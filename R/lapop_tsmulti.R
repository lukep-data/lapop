#######################################

# LAPOP Multi-Line Time Series Graphs #

#######################################


#' @include lapop_fonts.R
NULL

#'
#' LAPOP Multi-line Time-Series Graphs
#'
#' This function creates a time series graph utilizing multiple lines representing values of
#' an outcome variable for different values of a secondary variable -- for example, support for
#' democracy over time by country.  This function is designed to be used for
#' AmericasBarometer data.  The maximum number of lines is four.  Unlike the lapop_ts()
#' single-line time series graph, this function will not print confidence lines nor will
#' it show text values for each year (just the final/most recent year).
#'
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled varlabel (values of secondary variable which will be used to make each line; character),
#' wave (survey wave/year; character), prop (outcome variable; numeric),
#' proplabel (text of outcome variable; character). Default: None (must be supplied).
#' @param varlabel,wave_var,outcome_var,label_var,point_var Character,
#' character, numeric, character, numeric. Each component of the data to be plotted
#' can be manually specified in case the default columns in the data frame should
#' not be used (if, for example, the values for a given variable were altered
#' and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: dynamic.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: AmericasBarometer" in the bottom-left corner of the graph.
#' Default: None (only "Source: AmericasBarometer" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent of Mexicans who agree...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  #' Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param subtitle_h_just,subtitle_v_just Numeric.  Changes location of legend
#'  (secondary variable labels).  Defaults: 40, -20.
#' @param color_scheme Character.  Color of lines and dots.  Takes hex number, beginning with "#".
#' Must specify four values, even if four are not used.
#' Default: c("#7030A0", "#3CBC70", "#1F968B", "#95D840").

#'

#'
#'
#'@export
#'@importFrom ggplot2 ggplot
#'@importFrom plyr round_any
#'@importFrom magick image_read
#'@importFrom ggplotify as.ggplot
#'@importFrom ggtext element_markdown
#'@importFrom zoo na.approx
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'


lapop_tsmulti <- function(data, varlabel = data$varlabel, wave_var = as.character(data$wave),
                          outcome_var = data$prop, label_var = data$proplabel,
                          point_var = data$prop,
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

