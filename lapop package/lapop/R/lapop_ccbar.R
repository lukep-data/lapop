#######################################

# LAPOP Cross-Country Bar Graphs #

#######################################



#' LAPOP Cross-Country Bar Graphs
#'
#' This function creates bar graphs for comparing values across countries using LAPOP formatting.
#'
#' The input data must have a specific format to produce a graph.  It must include columns for
#' the survey wave (wave), the outcome variable (prop), the lower bound of the estimate (lb),
#' the upper bound of the estimate (ub), and a string for the outcome variable label (proplabel).
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled wave (survey wave/year; character vector), prop (outcome variable; numeric),
#' proplabel (text of outcome variable; character); lb (lower bound of estimate; numeric),
#' and ub (upper bound of estimate; numeric). Default: None (must be supplied).
#' @param wave_var,outcome_var,label_var,lower_bound,upper_bound Character, numeric, character,
#' numeric, numeric. Each component of the data to be plotted can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: Nearest integer.
#' that is divisible by 5 above the maximum and below the minimum y value.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: AmericasBarometer" in the bottom-left corner of the graph.
#' Default: None (only "Source: AmericasBarometer" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., as "% of Mexicans who say...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  #' Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of lines and dots.  Takes hex number, beginning with "#".
#' Default: "#3CBC70" (green).
#' @return A ggplot graph.
#' @examples
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

lapop_cc <- function(data, outcome_var = data$prop, lower_bound = data$lb, pais = data$pais,
                     upper_bound = data$ub, label_var = data$proplabel,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     highlight = "",
                     main_title = "",
                     source_info = "",
                     subtitle = "",
                     sort = "",
                     color_scheme = "#512B71"){
  if(highlight != ""){
    data$hl_var = factor(ifelse(pais == highlight, 0, 1), labels = c("hl", "other"))
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
    data = data[order(data$pais),]
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$pais),]
  }
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:#512B71; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:#512B71; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'> interval</span>"))
  ggplot(data=data, aes(x=factor(pais, levels = pais), y=prop, fill = hl_var)) +
    geom_bar(stat="identity", color = color_scheme, width = 0.6) +
    geom_text(aes(label=label_var), vjust=-1.5, size=5, fontface = "bold", color = color_scheme) +
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
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas", "Source: AmericasBarometer"),
                          source_info)) +
    # {if(horizontal)coord_flip()} +
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

