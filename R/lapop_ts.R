#######################################

# LAPOP Time-Series Graphs #

#######################################


#' @include lapop_fonts.R
NULL

#'
#' LAPOP Time-Series Graphs
#'
#' This function creates time series graphs using LAPOP formatting.  If there are waves missing at the
#' beginning or end of the series, the function will omit those waves from the graph (i.e.,
#' the x-axis will range from the earliest wave for which data is supplied to the latest).  If there are
#' waves missing in the middle of the series, those waves will be displayed on the x-axis, but no data will be
#' shown.
#'
#' The input data must have a specific format to produce a graph.  It must include columns for
#' the survey wave (wave), the outcome variable (prop), the lower bound of the estimate (lb),
#' the upper bound of the estimate (ub), and a string for the outcome variable label (proplabel).
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled wave (survey wave/year; character vector), prop (outcome variable; numeric),
#' proplabel (text of outcome variable; character); lb (lower bound of estimate; numeric),
#' and ub (upper bound of estimate; numeric). Default: None (must be supplied).
#' @param wave_var,outcome_var,label_var,lower_bound,upper_bound,point_var Character, numeric, character,
#' numeric, numeric,point_var. Each component of the data to be plotted can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: dynamic.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: AmericasBarometer" in the bottom-left corner of the graph.
#' Default: None (only "Source: AmericasBarometer" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., as "Percent of Mexicans who agree...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  #' Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of lines and dots.  Takes hex number, beginning with "#".
#' Default: "#3CBC70" (green).
#' @return Returns an object of class \code{ggplot}, a ggplot line graph showing
#' values of a variable over time.
#'
#' #' @examples
#' df <- data.frame(wave = c("2008", "2010", "2016/17", "2018/19", "2021"),
#' prop = c(23.2, 14.4, 35.8, 36.6, 40),
#' proplabel = c("23.2%", "14.4%", "35.8%", "36.6%", "40.0%"),
#' lb = c(20.2, 11.9, 33.3, 33.1, 38),
#' ub = c(26.2, 16.9, 38.3, 40.1, 42)
#' )
#'
#' lapop_ts(df,
#'  main_title = "Ecuadorians are becoming more interested in politics",
#'  subtitle = "% politically interested",
#'  source_info = "Ecuador 2006-2021",
#'  ymin = 0,
#'  ymax = 55
#'  )
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


lapop_ts <- function(data, outcome_var = data$prop, lower_bound = data$lb, upper_bound = data$ub,
                     wave_var = data$wave, label_var = data$proplabel, point_var = data$prop,
                     ymin = round_any(min(outcome_var)-5, 5, f = floor),
                     ymax = round_any(max(outcome_var)+5, 5, f = ceiling),
                     main_title = "",
                     source_info = "",
                     subtitle = "",
                     lang = "en",
                     color_scheme = "#3CBC70"){
  #the following lines detect if there's missing waves in the middle of a time series
  #if so, we need to do some interpolation so the missing waves are still plotted on the x-axis (without data)
  allwaves = c("2004", "2006", "2008", "2010", "2012", "2014", "2016/17", "2018/19", "2021", "2022")
  waves_test = allwaves[which(min(data$wave) == allwaves):which(max(data$wave) == allwaves)]
  if(length(data$wave) != length(waves_test)) {
    data = merge(data, data.frame(waves_test), by.x="wave", by.y = "waves_test", all.x = TRUE, all.y = TRUE)
    data$outcome_var = with(data, interp1(seq_along(wave_var), outcome_var, seq_along(wave_var), "linear"))
    data$lower_bound = with(data, interp1(seq_along(wave_var), lower_bound, seq_along(wave_var), "linear"))
    data$upper_bound = with(data, interp1(seq_along(wave_var), upper_bound, seq_along(wave_var), "linear"))
    data = data[!rowSums(!is.na(data)) <= 1, ]
    outcome_var = data$outcome_var
    lower_bound = data$lower_bound
    upper_bound = data$upper_bound
  }
  #now we stop dealing with missing data
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'>interval</span>"))
  #and turn to creating the graph
  ggplot(data=data, aes(x=wave_var, y=outcome_var)) +
    geom_line(aes(group = 1), color=color_scheme, size = 1, alpha=0.48) +
    geom_line(aes(group = 1, y =lower_bound), color=color_scheme, size = 1, alpha=0.48, lty="dashed") +
    geom_line(aes(group = 1, y= upper_bound), color=color_scheme, size = 1, alpha=0.48, lty="dashed") +
    geom_point(aes(y = point_var, color = " "), size = 3.5, alpha=0.48, key_glyph = "point") +
    scale_color_manual(values = color_scheme,
                       labels = paste0("<span style='color:#545454; font-size:13pt'> ",
                                       subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text)) +

    geom_text(aes(label=label_var, fontface= "bold"), color=color_scheme,  size = 5, vjust = -2.1) +
    scale_x_discrete(limits = wave_var) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 10), labels = paste(seq(ymin,ymax,10), "%", sep=""), expand = c(0,0)) +
    labs(title = main_title,
         caption = paste0(ifelse(lang == "es", "Fuente: Bar\u00f3metro de las Am\u00e9ricas ", "Source: AmericasBarometer "),
                          source_info),
         x = " ",
         y = " ") +
    theme_minimal() +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 18, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0.02, family = "roboto-light", color="#545454"),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14, color = "#545454"),
          panel.grid = element_line(color = "#D1D3D4"),
          panel.border = element_rect(linetype = "solid", color = "#D1D3D4", fill = NA),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='left',
          legend.margin = margin(t=0, b=0),
          legend.text=element_markdown(family = "nunito-light"))
}





