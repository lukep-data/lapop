######################################################

# "Dumbbell" Graph Pre-Processing #

######################################################

#' Dumbbell Graphs
#'
#' This function creates dataframes which can then be input in svy_dumb for
#' comparing means of a variable across x values and two time periods.
#' @param data A survey object.  The data that should be analyzed.
#' @param outcome Outcome variable of interest to be plotted across countries
#' and waves, supplied as a character string.
#' @param xvar Character. The grouping variable to be plotted
#' along the x-axis (technically, the vertical axis for svy_dumb). Usually
#' country (pais).  Default: "pais".
#' @param over Numeric.  A vector of values for "wave" that specify which two
#' waves should be included in the plot.
#' @param rec Numeric. The minimum and maximum values of the outcome variable that
#' should be included in the numerator of the percentage.  For example, if the variable
#' is on a 1-7 scale and rec is c(5, 7), the function will show the percentage who chose
#' an answer of 5, 6, 7 out of all valid answers.  Can also supply one value only,
#' to produce the percentage that chose that value out of all other values.
#' Default: c(1, 1).
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' recoding to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Character. Changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param sort Character. On what value the bars are sorted.
#' Options are "prop1" (for the value of the outcome variable in wave 1), "prop2"
#' (default; for the value of the outcome variable in wave 2), "xv" (for
#' the underlying values of the x variable), "xl" (for the labels of the x variable,
#' i.e., alphabetical), and "diff" (for the difference between the outcome between
#' the two waves).
#' @param order Character.  How the bars should be sorted.  Options are "hi-lo"
#' (default) or "lo-hi".
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all pais-wave combinations and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#'
#' @return Returns a data frame, with data formatted for visualization by svy_dumb
#'
#' @examples
#'
#' \dontrun{lpr_dumb(gm2123,
#' outcome = "ing4",
#' rec = c(5, 7),
#' over = c(2021, 2023),
#' sort = "diff",
#' ttest = TRUE)}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@hdrinc.com}


dumb_helper <- function(data,
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
