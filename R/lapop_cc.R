#######################################

# LAPOP Cross-Country Bar Graphs #

#######################################

#' @include lapop_fonts.R
#NULL

#' LAPOP Cross-Country Bar Graphs
#'
#' This function creates bar graphs for comparing values across countries using LAPOP formatting.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled vallabel (values of x-axis variable (e.g. pais); character vector), prop (outcome variable; numeric),
#' proplabel (text of outcome variable; character), lb (lower bound of estimate; numeric),
#' and ub (upper bound of estimate; numeric). Default: None (must be supplied).
#' @param vallabel,outcome_var,label_var,lower_bound,upper_bound Character, numeric, character,
#' numeric, numeric. Each component of the plot data can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: 0 to 100.
#' @param highlight Character.  Country of interest.  Will highlight (make darker) that country's bar.
#' Input must match entry in "vallabel" exactly. Default: None.
#' @param sort Character. Method of sorting bars.  Options: "hi-lo" (highest to lowest y value), "lo-hi" (lowest to highest),
#' "alpha" (alphabetical by vallabel/x-axis label). Default: Order of data frame.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: " in the bottom-left corner of the graph.
#' Default: None (only "Source: " will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "percentage of Mexicans who say...)".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of bars.  Takes hex number, beginning with "#".
#' Default: "#512B71" (purple).
#' @param label_size Numeric.  Size of text for data labels (percentages above bars).  Default: 5.
#'
#' @return Returns an object of class \code{ggplot}, a ggplot figure showing
#' average values of some variables across multiple countries.
#'
#' @examples
#' df <- data.frame(vallabel = c("PE", "CO", "BR", "PN", "GT", "DO", "MX", "BO", "EC",
#'                           "PY", "CL", "HN", "CR", "SV", "JA", "AR", "UY", "NI"),
#'                  prop = c(36.1, 19.3, 16.6, 13.3, 13, 11.1, 9.5, 9, 8.1, 8, 6.6,
#'                           5.7, 5.1, 3.4, 2.6, 1.9, 0.8, 0.2),
#'                  proplabel = c("36%" ,"19%" ,"17%" ,"13%" ,"13%" ,"11%" ,"10%",
#'                                "9%", "8%", "8%", "7%", "6%", "5%", "3%", "3%",
#'                                "2%", "1%", "0%"),
#'                  lb = c(34.9, 18.1, 15.4, 12.1, 11.8, 9.9, 8.3, 7.8, 6.9, 6.8,
#'                         5.4, 4.5, 3.9, 2.2, 1.4, 0.7, -0.4, -1),
#'                  ub = c(37.3, 20.5, 17.8, 14.5, 14.2, 12.3, 10.7, 10.2, 9.3,
#'                         9.2, 7.8, 6.95, 6.3, 4.6, 3.8, 3.1, 2, 1.4))
#'
#' lapop_cc(df,
#'          main_title = "Normalization of Initimate Partner Violence in Seven LAC Countries",
#'          subtitle = "% who say domestic violence is private matter",
#'          source_info = "2021",
#'          highlight = "BR",
#'          ymax = 50)
#'
#'@export
#'@import ggplot2
#'@importFrom ggtext element_markdown
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'





lapop_cc <- function(data, outcome_var = data$prop, lower_bound = data$lb, vallabel = data$vallabel,
                     upper_bound = data$ub, label_var = data$proplabel,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     highlight = "",
                     main_title = "",
                     source_info = "",
                     subtitle = "",
                     sort = "",
                     color_scheme = "#512B71",
                     label_size = 5){
  if(highlight != ""){
    data$hl_var = factor(ifelse(vallabel == highlight, 0, 1), labels = c("hl", "other"))
    fill_values = c(paste0(color_scheme, "47"), paste0(color_scheme, "20"))
  }
  else{
    data$hl_var = factor("other")
    fill_values = paste0(color_scheme, "47")
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$vallabel),]
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$vallabel),]
  }
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:#512B71; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:#512B71; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'> interval</span>"))
  ggplot(data=data, aes(x=factor(vallabel, levels = vallabel), y=prop, fill = hl_var)) +
    geom_bar(stat="identity", color = color_scheme, width = 0.6) +
    geom_text(aes(label=label_var, y = upper_bound), vjust= -0.5, size=label_size, fontface = "bold", color = color_scheme) +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.15, color = color_scheme, linetype = "solid") +
    scale_fill_manual(breaks = "other",
                      values = fill_values,
                      labels = paste0(" <span style='color:#545454; font-size:13pt'> ",
                                      subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text),
                      na.value = paste0(color_scheme, "90")) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title=main_title,
         y = "",
         x = "",
         caption = paste0(ifelse(lang == "es", "Fuente: ", "Source: "),
                          source_info)) +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 18, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0.02, family = "roboto-light", color="#545454"),
          panel.background = element_blank(),
          panel.border = element_rect(linetype = "solid", color = "#D1D3D4", fill = NA),
          axis.text = element_text(size = 14, color = "#545454", face = "bold"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='left',
          legend.margin = margin(t=0, b=0),
          legend.text = element_markdown(family = "nunito-light"))
}

