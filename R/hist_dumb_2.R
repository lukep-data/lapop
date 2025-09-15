svy_hist2 <- function(data,
                      outcome_var = prop,
                      label_var   = proplabel,
                      cat_var     = cat,
                      text_size   = 4,
                      yminmax     = NULL,
                      lang        = "en",
                      main_title  = "",
                      subtitle    = "",
                      source_info = "",
                      order       = FALSE,
                      color_scheme = "#377F99") {

  # Make a stable category column for plotting/order
  if (order) {
    data <- data %>% mutate(cat_ord = reorder({{cat_var}}, n))
  } else {
    data <- data %>% mutate(cat_ord = factor({{cat_var}}, levels = rev(unique({{cat_var}}))))
  }

  data_in  <- data %>% filter({{outcome_var}} >= 5)  # inside, white
  data_out <- data %>% filter({{outcome_var}} <  5)  # outside, black

  # small offset (pre-flip y) to push outside labels just past the bar end
  offset <- if (nrow(data) > 0) max(data$n, na.rm = TRUE) * 0.02 else 0

  ggplot(data = data, aes(x = cat_ord, y = n)) +
    geom_bar(stat = "identity", width = 0.4, fill = color_scheme) +

    # inside labels centered in the bar
    geom_text(
      data = data_in,
      aes(label = {{label_var}}, y = n/2),   # center inside bar
      color = "#FFFFFF",
      fontface = "bold",
      size = text_size
    ) +

    # outside labels placed at bar end and nudged outward (use nudge_y pre-flip)
    geom_text(
      data = data_out,
      aes(label = {{label_var}}, y = n),
      nudge_y = offset,      # IMPORTANT: nudge_y (pre-flip) -> moves horizontally after coord_flip
      hjust = 0,             # left-align so text starts at the nudged point
      vjust = 0.5,
      color = "#000000",
      fontface = "bold",
      size = text_size
    ) +

    ylab("Total Responses") +

    # add a little breathing room on the numeric axis so outside labels don't get clipped
    { if (is.null(yminmax)) scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
      else ylim(yminmax) } +

    labs(title = main_title, subtitle = subtitle) +
    coord_flip() +
    theme(
      legend.position = "bottom",
      panel.background = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.25, color = "#A8A99E"),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 10),
      axis.text = element_text(size = 10),
      plot.subtitle = element_text(size = 10, face = "italic"),
      plot.margin = unit(c(t = 0, r = 0.25, b = 0, l = 0.25), "in"),
      plot.title.position = "plot",
      title = element_text(size = 12)
    )
}




# lapop_dumb2 <- function(data,
#                        ymin = 0,
#                        ymax = 100,
#                        lang = "en",
#                        main_title = "",
#                        source_info = "",
#                        subtitle = "",
#                        sort = "wave2",
#                        order = "hi-lo",
#                        color_scheme = c("#008381", "#A43D6A"),
#                        subtitle_h_just = 40,
#                        subtitle_v_just = -18,
#                        text_nudge = 6,
#                        drop_singles = FALSE){
#   if(drop_singles == TRUE) {
#     data = complete.cases(data)
#   }
#   if(sort == "diff"){
#     data$diff = data$prop2-data$prop1
#     data = data[order(-data$diff),]
#   } else if(sort == "wave2"){
#     data = data[order(-data$prop2),]
#   } else if(sort == "wave1"){
#     data = data[order(-data$prop1),]
#   } else if(sort == "alpha"){
#     data = data[order(data$pais),]
#   }
#   if(order == "hi-lo"){
#     data$pais = factor(data$pais, levels = rev(unique(data$pais)))
#   } else if(order == "lo-hi") {
#     data$pais = factor(data$pais, levels = unique(data$pais))
#   }
#   data$max1 = data$prop2 < data$prop1
#   data$max1[is.na(data$max1)] <- FALSE
#   names(color_scheme) = c(unique(data$wave1), unique(data$wave2))
#   ggplot(data, aes(y=pais)) +
#     geom_point(aes(x = prop1, color = names(color_scheme)[1]), size=4) +
#     geom_point(aes(x = prop2, color = names(color_scheme)[2]), size=4) +
#     geom_text(data = data[data$max1 == FALSE, ],
#               aes(x = prop1, label = proplabel1),
#               nudge_x = -text_nudge, color = color_scheme[1], size = 4, fontface = "bold") +
#     geom_text(data = data[data$max1 == TRUE, ],
#               aes(x = prop1, label = proplabel1),
#               nudge_x = text_nudge, color = color_scheme[1], size = 4, fontface = "bold") +
#     geom_text(data = data[data$max1 == TRUE, ],
#               aes(x = prop2, label = proplabel2),
#               nudge_x = -text_nudge, color = color_scheme[2], size = 4, fontface = "bold") +
#     geom_text(data = data[data$max1 == FALSE, ],
#               aes(x = prop2, label = proplabel2),
#               nudge_x = text_nudge, color = color_scheme[2], size = 4, fontface = "bold") +
#     geom_vline(xintercept = 0, color = "#dddddf") +
#     scale_x_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 20), labels = paste(seq(ymin,ymax, 20), "%", sep=""), expand = c(0,0)) +
#     scale_color_manual(values = color_scheme, breaks = levels(pais))) +
#     labs(title = main_title,
#          y = "",
#          x = " ",
#          subtitle = subtitle) +
#     theme(text = element_text(size = 12),
#           plot.title = element_text(size = 17, face = "bold"),
#           plot.subtitle = element_text(size = 10, face = "italic"),
#           plot.margin = margin(5.5, 20, 5.5, 5.5, "points"),
#           axis.title.y = element_blank(),
#           axis.ticks = element_blank(),
#           plot.title.position = "plot",
#           axis.text = element_text(size = 10, color = "#585860", margin=margin(r=5)),
#           panel.grid.major.y = element_line(color = "#dddddf"),
#           panel.background = element_rect(fill = "white"),
#           legend.position="top",
#           legend.text = element_text( color = "#585860"),
#           legend.title = element_blank(),
#           legend.justification='right',
#           legend.key.size = unit(1, "line"),
#           legend.key = element_blank(),
#           legend.margin = margin(t=subtitle_v_just,b=0, subtitle_h_just, 0))
# }





svy_stack2 <- function(data, outcome_var = data$prop, prop_labels = data$proplabel,
                      var_labels = data$varlabel, value_labels = data$vallabel,
                      lang = "en",
                      main_title = "",
                      subtitle = "",
                      source_info = "",
                      ylab_text = "",
                      text_size = 4,
                      rev_values = FALSE,
                      rev_variables = FALSE,
                      hide_small_values = TRUE,
                      order_bars = FALSE,
                      subtitle_h_just = -40,
                      fixed_aspect_ratio = TRUE,
                      legendnrow = 1,
                      color_scheme = c("#2e697d", "#4298b5", "#A8A99E", "#C8102E", "#810a1e")){
  if(!inherits(var_labels, "character") & !inherits(var_labels, "factor")){
    var_labels = as_factor(var_labels)
    data$varlabels = as_factor(data$varlabel)
  }
  if (!inherits(value_labels, "factor")) {
    value_labels <- factor(value_labels, levels = unique(value_labels))
    data$vallabel <- factor(data$vallabel, levels = unique(data$vallabel))
  }
  mycolors = rev(color_scheme[seq_along(unique(value_labels))])

  original_levels <- levels(data$vallabel)

  if (rev_values == TRUE) {
    value_labels <- factor(value_labels, levels = original_levels)
  } else {
    value_labels <- factor(value_labels, levels = rev(original_levels))
  }

  data$vallabel <- value_labels

  positions = rev(unique(var_labels))

  data_in  <- data %>% filter(prop >= 5)
  data_out <- data %>% filter(prop <  5)
  offset <- if (nrow(data) > 0) max(data$prop, na.rm = TRUE) * 0.01 else 0


  ggplot(data = data, aes(x = var_labels,
                          y = prop, fill = value_labels)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(data = data_in,
              aes(x = varlabel, y = prop, fill = vallabel, label = proplabel),
              position = position_stack(vjust = 0.5),
              color = "#FFFFFF",
              fontface = "bold",
              size = text_size) +
    geom_text(data = data_out,
              aes(x = varlabel, y = prop, fill = vallabel, label = proplabel),
              nudge_y = offset,      # IMPORTANT: nudge_y (pre-flip) -> moves horizontally after coord_flip
              hjust = 0,             # left-align so text starts at the nudged point
              vjust = 0.5,
              color = "#000000",
              fontface = "bold",
              size = text_size) +

    coord_flip() +
    labs(title = main_title,
         subtitle = subtitle) +
    ylab(ylab_text) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                       limits = c(0,100.1),
                       labels = c("0%", "25%", "50%", "75%", "100%"),
                       expand = c(0,0)) +
    scale_fill_manual(guide = guide_legend(reverse = T, nrow = legendnrow, byrow = TRUE),
                      drop = FALSE,
                      values = mycolors) +
    scale_x_discrete(limits = positions, expand = c(0, 0)) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(size = 0.25, color = "#A8A99E"),
          plot.subtitle = element_text(size = 10, face = "italic"),
          axis.ticks = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10),
          axis.text = element_text(size = 10),
          strip.text = element_text(size = 10, face = "bold"),
          strip.background = element_blank(),
          panel.spacing = unit(2.5, "lines"),
          panel.grid.major.x = element_line(size = 0.25, color = "#A8A99E"),
          panel.border = element_rect(size = 0.25, color = "#A8A99E", fill = NA),
          legend.margin = margin(l = subtitle_h_just),
          plot.title.position = "plot",
          plot.margin = unit(c(t = 0, r = 0.25, b = 0, l = 0.25), "in"))

}



