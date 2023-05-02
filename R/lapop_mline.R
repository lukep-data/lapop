#######################################

# LAPOP Multi-Line Time Series Graphs #

#######################################

#' @rdname lapop-deprecated
#' @section \code{lapop_tsmulti}:
#' For \code{lapop
#' _tsmulti}, use \code{\link{lapop_mline}}.
#'
#' @export

lapop_tsmulti <- function(data, varlabel = data$varlabel, wave_var = as.character(data$wave),
                          outcome_var = data$prop, label_var = data$proplabel,
                          point_var = data$prop,
                          ymin = 0,
                          ymax = 100,
                          main_title = "",
                          source_info = "",
                          subtitle = "",
                          lang = "en",
                          legend_h_just = 40,
                          legend_v_just = -20,
                          subtitle_h_just = 0,
                          color_scheme = c("#7030A0", "#3CBC70", "#1F968B", "#95D840")){
  .Deprecated("lapop_mline")
  lapop_mline(data = data, varlabel = varlabel, wave_var = wave_var,
              outcome_var = outcome_var, label_var = label_var,
              point_var = point_var,
              ymin = ymin,
              ymax = ymax,
              main_title = main_title,
              source_info = source_info,
              subtitle = subtitle,
              lang = lang,
              legend_h_just = legend_h_just,
              legend_v_just = legend_v_just,
              subtitle_h_just = subtitle_h_just,
              color_scheme = color_scheme
  )
}

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
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: 0, 100.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: AmericasBarometer" in the bottom-left corner of the graph.
#' Default: None (only "Source: AmericasBarometer" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent of Mexicans who agree...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  #' Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param legend_h_just,legend_v_just Numeric.  Changes location of legend. From 0 to 100.
#'  (secondary variable labels).  Defaults: 40, -20.
#' @param subtitle_h_just Numeric.  Moves subtitle left to right.  From 0 to 1.
#'  (secondary variable labels).  Defaults: 0 (left justify).
#' @param color_scheme Character.  Color of lines and dots.  Takes hex number, beginning with "#".
#' Must specify four values, even if four are not used.
#' Default: c("#7030A0", "#3CBC70", "#1F968B", "#95D840").
#'
#'@examples
#'df <- data.frame(varlabel = c(rep("Honduras", 9), rep("El Salvador", 9),
#'                              rep("Mexico", 9), rep("Guatemala", 9)),
#'                 wave = rep(c("2004", "2006", "2008", "2010", "2012",
#'                              "2014", "2016/17", "2018/19", "2021"), 4),
#'                 prop = c(19, 24, 21, 15, 11, 32, 41, 38, 54, 29, 29, 25,
#'                          24, 24, 28, 36, 26, 32, 14, 16, 14, 16, 9, 14,
#'                          18, 19, 26, 21, 15, 18, 20, 14, 18, 17, 25, 36),
#'                 proplabel = c("19%", "24%", "21%", "15%", "11%", "32%",
#'                               "41%", "38%", "54%", "29%", "29%", "25%",
#'                               "24%", "24%", "28%", "36%", "26%", "32%",
#'                               "14%", "16%", "14%", "16%", "9%", "14%",
#'                               "18%", "19%", "26%", "21%", "15%", "18%",
#'                               "20%", "14%", "18%", "17%", "25%", "36%"))
#'
#'lapop_mline(df,
#'              main_title = "Intentions to emigrate in Guatemala, Honduras and Mexico reached\n their highest levels in the AmericasBarometer series",
#'              subtitle = "% who intend to migrate in:",
#'              source_info = "GM 2004-2021")
#'
#'@export
#'@importFrom ggplot2 ggplot
#'@importFrom ggtext element_markdown
#'@importFrom ggrepel geom_text_repel
#'@importFrom zoo na.approx
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'


lapop_mline <- function(data, varlabel = data$varlabel, wave_var = as.character(data$wave),
                          outcome_var = data$prop, label_var = data$proplabel,
                          point_var = data$prop,
                          ymin = 0,
                          ymax = 100,
                          main_title = "",
                          source_info = "",
                          subtitle = "",
                          lang = "en",
                          legend_h_just = 40,
                          legend_v_just = -20,
                          subtitle_h_just = 0,
                          color_scheme = c("#7030A0", "#3CBC70", "#1F968B", "#95D840")){
  if(class(varlabel) != "character" & class(varlabel) != "factor"){
    varlabel = as.character(varlabel)
    data$varlabels = as.character(data$varlabel)
  }
  #interpolate data for missing waves are still plotted on the x-axis (without data)
  if(sum(is.na(outcome_var)) > 0) {
    # outcome_var = zoo::na.approx(outcome_var)
    outcome_var = ifelse(is.na(outcome_var) & wave_var != max(wave_var) & wave_var != min(wave_var), zoo::na.approx(outcome_var), outcome_var)
  }
  varlabel = factor(varlabel, levels = unique(varlabel))
  #limit colors to number of variables in varlabels (e.g. countries)
  mycolors = color_scheme[seq_along(unique(varlabel))]
  #specify color of text labels based on number of variables in varlabels and their order (alphabetical)
  textcolors = rep(mycolors, each = length(unique(wave_var)))
  # create variable with label for final data points in series
  end_labels = ifelse(wave_var == max(wave_var), label_var, NA)
  ggplot(data, aes(x = wave_var, y = outcome_var, group = varlabel)) +
    geom_line(aes(color = varlabel), size = 1, alpha=0.48, show.legend = FALSE) +
    geom_point(aes(y = point_var, color = varlabel), size = 3.5, alpha=0.48, key_glyph = draw_key_blank) +
    scale_color_manual(breaks = levels(varlabel),
                       labels = paste("<span style='color:",
                                      mycolors,
                                      "'>",
                                      levels(varlabel),
                                      "</span>"),
                       values = mycolors) +
    ggrepel::geom_text_repel(aes(label = end_labels, fontface= "bold"), color = textcolors,
              size = 4.5, nudge_x = 1, direction = "y") +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 10), labels = paste(seq(ymin,ymax,10), "%", sep=""), expand = c(0,0)) +
    labs(title = main_title,
         caption = paste0(ifelse(lang == "es", "Fuente: ", "Source: "),
                          source_info),
         x = " ",
         y = " ",
         subtitle = subtitle) +
    theme_minimal() +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 18, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0.02, family = "roboto-light", color="#545454"),
          plot.subtitle = element_text(size = 14, family = "nunito-light", color="#545454", hjust = subtitle_h_just),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14, color = "#545454"),
          panel.grid = element_line(color = "#D1D3D4"),
          panel.border = element_rect(linetype = "solid", color = "#D1D3D4", fill = NA),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='right',
          legend.margin = margin(t=legend_v_just,b=0, legend_h_just, 0),
          legend.spacing.x = unit(0.2, 'cm'),
          legend.text=element_markdown(family = "nunito-light", face = "bold"))
}

