#######################################

# Survey Stacked Bar Graph #

#######################################

#' Survey Stacked Bar Graphs
#'
#' This function creates a stacked bar graph using ggplot2, optimized for survey data.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled varlabel (name(s)/label(s) of variable(s) of interest; character), vallabel (names/labels of values for each variable; character),
#' prop (outcome variable value; numeric), and proplabel (text of outcome variable value; character).
#' Default: None (must be provided).
#' @param outcome_var,prop_labels,var_labels,value_labels Numeric, character, character, character.
#' Each component of the data to be plotted can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: " in the bottom-left corner of the graph.
#' Default: None (only "Source: " will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent who support...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of data bars for each value.  Allows up to 6 values.
#' Takes hex numbers, beginning with "#".
#' Default: c("#2D708E", "#008381", "#C74E49", "#784885", "#a43d6a","#202020")
#' (navy blue, turquoise, teal, green, sap green, pea soup).
#' @param subtitle_h_just Numeric.  Move the subtitle/legend text left (negative numbers) or right (positive numbers).
#' Ranges from -100 to 100.  Default: 0.
#' @param fixed_aspect_ratio Logical.  Should the aspect ratio be set to a specific value (0.35)?
#' This prevents bars from stretching vertically to fit the plot area.  Set to false when you have
#' a large number of bars (> 10).  Default: TRUE.
#' @param rev_variables Logical.  Should the order of the variables be reversed?  Default: FALSE.
#' @param rev_values Logical.  Should the order of the values for each variable be reversed?  Default: FALSE.
#' @return Returns an object of class \code{ggplot}, a ggplot stacked bar graph
#' @param small_values Character  Should labels for categories with less than 5 percent be hidden ('hide'),
#' nudged to the right ('nudge'), or "repelled" from other text ('repel')?  Default: 'hide'.
#' ?  Default: TRUE.
#' @param order_bars Logical.  Should categories be placed in descending order for each bar?  Default: FALSE.
#' showing the distributions of multiple categorical variables.
#' @param legendnrow Numeric.  How many rows for legend labels. Default: 1.
#' @examples
#'
#' df <- data.frame(varlabel = c(rep("Politicians can\nidentify voters", 5),
#'                               rep("Wealthy can\nbuy results", 5),
#'                               rep("Votes are\ncounted correctly", 5)),
#'                  vallabel = rep(c("Always", "Often", "Sometimes",
#'                                   "Never", "Other"), 3),
#'                  prop = c(36, 10, 19, 25, 10, 46, 10, 23, 11, 10, 35,
#'                           10, 32, 13, 10),
#'                  proplabel = c("36%", "10%", "19%", "25%", "10%", "46%",
#'                                "10%", "23%", "11%", "10%", "35%", "10%",
#'                                "32%", "13%", "10%"))
#'
#' svy_stack(df,
#'          main_title = "Trust in key features of the electoral process is low in Latin America",
#'          subtitle = "% believing it happens:",
#'          source_info = "2019")
#'
#'@export
#'@import ggplot2
#'@import ggtext
#'@import showtext
#'@importFrom stats reorder

#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'



svy_stack <- function(data, outcome_var = data$prop, prop_labels = data$proplabel,
                       var_labels = data$varlabel, value_labels = data$vallabel,
                       lang = "en",
                       main_title = "",
                       subtitle = "",
                       source_info = "",
                       ylab_text = "",
                       text_size = 4,
                       rev_values = FALSE,
                       rev_variables = FALSE,
                       small_values = 'hide',
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

  offset <- if (nrow(data) > 0) max(data$prop, na.rm = TRUE) * 0.01 else 0

  ggplot(data = data, aes(x = var_labels,
                          y = prop, fill = value_labels)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(label = ifelse(outcome_var >= 5, prop_labels, NA),
              position = position_stack(vjust = 0.5), color = "#FFFFFF",
              fontface = "bold", size = text_size) +
    {if (small_values == 'nudge') {
      geom_text(label = ifelse(outcome_var < 5, prop_labels, NA),
                nudge_y = offset,
                hjust = 0,
                vjust = 0.5,
                color = "#000000",
                fontface = "bold", size = text_size)
    }
    } +
    {if (small_values == 'repel') {
      ggrepel::geom_text_repel(label = ifelse(outcome_var < 5, prop_labels, NA),
                               position = position_stack(vjust = 0.5),
                               # hjust = 0,
                               # vjust = 0.5,
                               color = "#FFFFFF",
                               fontface = "bold", size = text_size / 1.3,
                               direction = "y",
                               force_pull = 0.2,
                               force = 5)
    }
    } +
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
