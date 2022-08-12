#######################################

# LAPOP Multi-Line Time Series Graphs #

#######################################


#' @include lapop_fonts.R
NULL

#'
#' LAPOP Multi-line Time-Series Graphs
#'
#' Documentation TBD
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled wave (survey wave/year; character vector), prop (outcome variable; numeric),
#' proplabel (text of outcome variable; character); lb (lower bound of estimate; numeric),
#' and ub (upper bound of estimate; numeric). Default: None (must be supplied).
#'
#'@export
#'@importFrom ggplot2 ggplot
#'@importFrom plyr round_any
#'@importFrom magick image_read
#'@importFrom ggplotify as.ggplot
#'@importFrom ggtext element_markdown
#'@importFrom pracma interp1
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'


lapop_tsmulti <- function(data, outcome_var = data$prop, lower_bound = data$lb, upper_bound = data$ub,
                     wave_var = as.character(data$wave), label_var = data$proplabel, point_var = data$prop,
                     varlabel = data$varlabel,
                     ymin = plyr::round_any(min(outcome_var, na.rm = TRUE)-5, 5, f = floor),
                     ymax = plyr::round_any(max(outcome_var, na.rm = TRUE)+5, 5, f = ceiling),
                     main_title = "",
                     source_info = "",
                     subtitle = "",
                     lang = "en",
                     subtitle_h_just = 40,
                     subtitle_v_just = -20,
                     color_scheme = c("#7030A0", "#3CBC70", "#1F968B", "#95D840")){
  #interpolate data for missing waves are still plotted on the x-axis (without data)
  if(sum(is.na(outcome_var)) > 0) {
    outcome_var = zoo::na.approx(outcome_var)
  }
  #limit colors to number of variables in varlabels (e.g. countries)
  mycolors = color_scheme[seq_along(unique(varlabel))]
  #specify color of text labels based on number of variables in varlabels and their order (alphabetical)
  textcolors = mycolors[order(unique(varlabel), decreasing = TRUE)]
  textcolors = rep(textcolors, each = length(unique(wave_var)))
  # create variable with label for final data points in series
  end_labels = ifelse(wave_var == max(wave_var), label_var, NA)
  ggplot(data, aes(x = wave_var, y = outcome_var, group = varlabel)) +
    geom_line(aes(color = varlabel), size = 1, alpha=0.48, show.legend = FALSE) +
    geom_point(aes(y = point_var, color = varlabel), size = 3.5, alpha=0.48, key_glyph = draw_key_blank) +
    scale_color_manual(labels = paste("<span style='color:",
                                      mycolors,
                                      "'>",
                                      sort(unique(varlabel)),
                                      "</span>"),
                       values = mycolors) +
    geom_text_repel(aes(label = end_labels, fontface= "bold"), color = textcolors,
              size = 4.5, nudge_x = 1, direction = "y") +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 10), labels = paste(seq(ymin,ymax,10), "%", sep=""), expand = c(0,0)) +
    labs(title = main_title,
         caption = paste0(ifelse(lang == "es", "Fuente: Bar\u00f3metro de las Am\u00e9ricas ", "Source: AmericasBarometer "),
                          source_info),
         x = " ",
         y = " ",
         subtitle = subtitle) +
    theme_minimal() +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 18, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0.02, family = "roboto-light", color="#545454"),
          plot.subtitle = element_text(size = 14, family = "nunito-light", color="#545454"),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14, color = "#545454"),
          panel.grid = element_line(color = "#D1D3D4"),
          panel.border = element_rect(linetype = "solid", color = "#D1D3D4", fill = NA),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='right',
          legend.margin = margin(t=subtitle_v_just,b=0, subtitle_h_just, 0),
          legend.spacing.x = unit(0.2, 'cm'),
          legend.text=element_markdown(family = "nunito-light", face = "bold"))
}

