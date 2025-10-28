svy_dumb <- function(data,
                       ymin = 0,
                       ymax = 100,
                       lang = "en",
                       main_title = "",
                       source_info = "",
                       subtitle = "",
                       sort = "wave2",
                       order = "hi-lo",
                       color_scheme = c("#008381", "#A43D6A"),
                       subtitle_h_just = 40,
                       subtitle_v_just = -18,
                       text_nudge = 6,
                       mean = FALSE,
                       drop_singles = FALSE){
  if(drop_singles == TRUE) {
    data = complete.cases(data)
  }
  if(sort == "diff"){
    data$diff = data$prop2-data$prop1
    data = data[order(-data$diff),]
  } else if(sort == "wave2"){
    data = data[order(-data$prop2),]
  } else if(sort == "wave1"){
    data = data[order(-data$prop1),]
  } else if(sort == "alpha"){
    data = data[order(data$pais),]
  }
  if(order == "hi-lo"){
    data$pais = factor(data$pais, levels = rev(unique(data$pais)))
  } else if(order == "lo-hi") {
    data$pais = factor(data$pais, levels = unique(data$pais))
  }
  data$max1 = data$prop2 < data$prop1
  data$max1[is.na(data$max1)] <- FALSE
  names(color_scheme) = c(unique(data$wave1), unique(data$wave2))
  ggplot(data, aes(y=pais)) +
    geom_point(aes(x = prop1, color = names(color_scheme)[1]), size=4) +
    geom_point(aes(x = prop2, color = names(color_scheme)[2]), size=4) +
    geom_text(data = data[data$max1 == FALSE, ],
              aes(x = prop1, label = proplabel1),
              nudge_x = -text_nudge, color = color_scheme[1], size = 4, fontface = "bold") +
    geom_text(data = data[data$max1 == TRUE, ],
              aes(x = prop1, label = proplabel1),
              nudge_x = text_nudge, color = color_scheme[1], size = 4, fontface = "bold") +
    geom_text(data = data[data$max1 == TRUE, ],
              aes(x = prop2, label = proplabel2),
              nudge_x = -text_nudge, color = color_scheme[2], size = 4, fontface = "bold") +
    geom_text(data = data[data$max1 == FALSE, ],
              aes(x = prop2, label = proplabel2),
              nudge_x = text_nudge, color = color_scheme[2], size = 4, fontface = "bold") +
    geom_vline(xintercept = 0, color = "#dddddf") +
    {if (mean) {
      scale_x_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 1),
                                           labels = paste(seq(ymin,ymax, 1), "", sep=""),
                                           expand = c(0,0))
      } else {
        scale_x_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 20),
                           labels = paste(seq(ymin,ymax, 20), "%", sep=""),
                           expand = c(0,0))
      }} +
    scale_color_manual(
      values = color_scheme,
      breaks = c(unique(data$wave2), unique(data$wave1))) +
    labs(title = main_title,
         y = "",
         x = " ",
         subtitle = subtitle) +
    theme(text = element_text(size = 12),
          plot.title = element_text(size = 17, face = "bold"),
          plot.subtitle = element_text(size = 10, face = "italic"),
          plot.margin = margin(5.5, 20, 5.5, 5.5, "points"),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title.position = "plot",
          axis.text = element_text(size = 10, color = "#585860", margin=margin(r=5)),
          panel.grid.major.y = element_line(color = "#dddddf"),
          panel.background = element_rect(fill = "white"),
          legend.position="top",
          legend.text = element_text( color = "#585860"),
          legend.title = element_blank(),
          legend.justification='right',
          legend.key.size = unit(1, "line"),
          legend.key = element_blank(),
          legend.margin = margin(t=subtitle_v_just,b=0, subtitle_h_just, 0))
}



lpr_dumb2 <- function(data,
                      outcome,
                      xvar = "pais",
                      over,
                      rec = c(1, 1),
                      ci_level = 0.95,
                      mean = FALSE,
                      filesave = "",
                      cfmt = "",
                      sort = "prop2",
                      order = "hi-lo",
                      ttest = FALSE,
                      keep_nr = FALSE) {

  # Detect if it's a survey object
  is_survey <- "survey.design2" %in% class(data) || "survey.design" %in% class(data)
  z <- qnorm(1 - (1 - ci_level) / 2)

  # Handle keep_nr
  if (keep_nr) {
    data <- data %>%
      mutate(!!sym(outcome) := case_when(
        na_tag(!!sym(outcome)) %in% c("a", "b") ~ 99,
        TRUE ~ as.numeric(!!sym(outcome))
      ))
  }

  if (length(rec) == 1) rec <- c(rec, rec)

  ## ---------------- Wave 1 ----------------
  wave1 <- data %>%
    drop_na(.data[[xvar]]) %>%
    filter(.data[["wave"]] == over[1]) %>%
    group_by(pais = as_factor(.data[[xvar]]),
             wave1 = as.character(as_factor(.data[["wave"]]))) %>%
    {
      if (is_survey) {
        if (mean) {
          summarize(.,
                    prop1 = survey_mean(.data[[outcome]],
                                        na.rm = TRUE,
                                        vartype = "ci",
                                        level = ci_level)) %>%
            mutate(proplabel1 = if (cfmt != "") sprintf(cfmt, prop1)
                   else sprintf("%.2f", prop1))
        } else {
          summarize(.,
                    prop1 = survey_mean(between(.data[[outcome]], rec[1], rec[2]),
                                        na.rm = TRUE,
                                        vartype = "ci",
                                        level = ci_level) * 100) %>%
            mutate(proplabel1 = if (cfmt != "") sprintf(cfmt, round(prop1))
                   else sprintf("%.0f%%", round(prop1)))
        }
      } else {
        summarise(.,
                  n = sum(!is.na(.data[[outcome]])),
                  est = if (mean) mean(.data[[outcome]], na.rm = TRUE)
                  else mean(between(.data[[outcome]], rec[1], rec[2]), na.rm = TRUE) * 100,
                  sd = sd(.data[[outcome]], na.rm = TRUE),
                  .groups = "drop"
        ) %>%
          mutate(
            se = sd / sqrt(n),
            lb1 = est - z * se,
            ub1 = est + z * se,
            prop1 = est,
            proplabel1 = if (cfmt != "") sprintf(cfmt, prop1)
            else if (mean) sprintf("%.2f", prop1)
            else sprintf("%.0f%%", round(prop1))
          )
      }
    }

  ## ---------------- Wave 2 ----------------
  wave2 <- data %>%
    drop_na(.data[[xvar]]) %>%
    filter(.data[["wave"]] == over[2]) %>%
    group_by(pais = as_factor(.data[[xvar]]),
             wave2 = as.character(as_factor(.data[["wave"]]))) %>%
    {
      if (is_survey) {
        if (mean) {
          summarize(.,
                    prop2 = survey_mean(.data[[outcome]],
                                        na.rm = TRUE,
                                        vartype = "ci",
                                        level = ci_level)) %>%
            mutate(proplabel2 = if (cfmt != "") sprintf(cfmt, prop2)
                   else sprintf("%.2f", prop2))
        } else {
          summarize(.,
                    prop2 = survey_mean(between(.data[[outcome]], rec[1], rec[2]),
                                        na.rm = TRUE,
                                        vartype = "ci",
                                        level = ci_level) * 100) %>%
            mutate(proplabel2 = if (cfmt != "") sprintf(cfmt, round(prop2))
                   else sprintf("%.0f%%", round(prop2)))
        }
      } else {
        summarise(.,
                  n = sum(!is.na(.data[[outcome]])),
                  est = if (mean) mean(.data[[outcome]], na.rm = TRUE)
                  else mean(between(.data[[outcome]], rec[1], rec[2]), na.rm = TRUE) * 100,
                  sd = sd(.data[[outcome]], na.rm = TRUE),
                  .groups = "drop"
        ) %>%
          mutate(
            se = sd / sqrt(n),
            lb2 = est - z * se,
            ub2 = est + z * se,
            prop2 = est,
            proplabel2 = if (cfmt != "") sprintf(cfmt, prop2)
            else if (mean) sprintf("%.2f", prop2)
            else sprintf("%.0f%%", round(prop2))
          )
      }
    }

  ## ---------------- Merge ----------------
  dumb <- merge(wave1, wave2, by = "pais")

  ## ---------------- Sorting ----------------
  dumb <- dumb %>%
    {
      if (sort == "prop1") {
        if (order == "hi-lo") arrange(., desc(prop1))
        else arrange(., prop1)
      } else if (sort == "prop2") {
        if (order == "hi-lo") arrange(., desc(prop2))
        else arrange(., prop2)
      } else if (sort == "xv") {
        if (order == "hi-lo") arrange(., desc(match(pais, levels(pais))))
        else arrange(., match(pais, levels(pais)))
      } else if (sort == "diff") {
        if (order == "hi-lo") mutate(., diff = prop2 - prop1) %>% arrange(., desc(diff))
        else mutate(., diff = prop2 - prop1) %>% arrange(., diff)
      } else if (sort == "xl") {
        if (order == "hi-lo") arrange(., desc(as.character(pais)))
        else arrange(., as.character(pais))
      } else .
    }

  ## ---------------- T-tests ----------------
  if (ttest) {
    t_test_results <- dumb %>%
      mutate(se1 = (ub1 - lb1) / (2 * z),
             se2 = (ub2 - lb2) / (2 * z))

    t_test_results_df <- data.frame(test = character(),
                                    diff = numeric(),
                                    ttest = numeric(),
                                    pval = numeric(),
                                    stringsAsFactors = FALSE)

    for (i in 1:nrow(t_test_results)) {
      diff <- round(t_test_results$prop1[i] - t_test_results$prop2[i], 3)
      t_stat <- round(diff / sqrt(t_test_results$se1[i]^2 + t_test_results$se2[i]^2), 3)
      df <- (t_test_results$se1[i]^2 + t_test_results$se2[i]^2)^2 /
        ((t_test_results$se1[i]^4 / (nrow(data) - 1)) +
           (t_test_results$se2[i]^4 / (nrow(data) - 1)))
      p_value <- round(2 * pt(-abs(t_stat), df), 3)

      t_test_results_df <- rbind(t_test_results_df,
                                 data.frame(test = paste(t_test_results$pais[i], t_test_results$wave1[i], "vs",
                                                         t_test_results$pais[i], t_test_results$wave2[i]),
                                            diff = diff,
                                            ttest = t_stat,
                                            pval = p_value))
    }

    attr(dumb, "t_test_results") <- t_test_results_df
  }

  if (filesave != "") write.csv(dumb, filesave, row.names = FALSE)

  return(dumb)
}



