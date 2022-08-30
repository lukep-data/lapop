#######################################

# LAPOP Stacked Bar Graph #

#######################################

#' @include lapop_fonts.R
NULL

#' LAPOP Bar Graphs
#'
#' This function shows a stacked bar graph using LAPOP formatting.
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
#' which is added to the end of "Source: AmericasBarometer" in the bottom-left corner of the graph.
#' Default: None (only "Source: AmericasBarometer" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent who support...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of data bars for each value.  Allows up to 6 values.
#' Takes hex numbers, beginning with "#".
#' Default: c("#2D708E", "#1F9689", "#00ADA9", "#21A356", "#568424", "#ACB014")
#' (navy blue, turquoise, teal, green, sap green, pea soup).
#' @param subtitle_h_just Numeric.  Move the subtitle/legend text left (negative numbers) or right (positive numbers).
#' Ranges from -100 to 100.  Default: 0.
#' @param rev_variables Logical.  Should the order of the variables be reversed?  Default: FALSE.
#' @param rev_values Logical.  Should the order of the values for each variable be reversed?  Default: FALSE.
#' @return Returns an object of class \code{ggplot}, a ggplot stacked bar graph
#' @param hide_small_values Logical.  Should labels for categories with less than 5 percent be hidden?  Default: FALSE.
#' @param order_bars Logical.  Should categories be placed in descending order for each bar?  Default: FALSE.
#' showing the distributions of multiple categorical variables.
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
#' lapop_sb(df,
#'          main_title = "Trust in key features of the electoral process is low in Latin America",
#'          subtitle = "% believing it happens:",
#'          source_info = "2019")
#'
#'@export
#'@importFrom ggplot2 ggplot
#'@importFrom ggplotify as.ggplot
#'@importFrom ggtext element_markdown
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'



lapop_sb <- function(data, outcome_var = data$prop, prop_labels = data$proplabel,
                       var_labels = data$varlabel, value_labels = data$vallabel,
                       lang = "en",
                       main_title = "",
                       subtitle = "",
                       source_info = "",
                     rev_values = FALSE,
                     rev_variables = FALSE,
                     hide_small_values = FALSE,
                     order_bars = FALSE,
                     subtitle_h_just = 0,
                     color_scheme = c("#2D708E", "#1F9689", "#00ADA9", "#21A356", "#568424", "#ACB014")){
  mycolors = rev(color_scheme[seq_along(unique(value_labels))])
  if(rev_values == TRUE){
    value_labels = factor(value_labels, levels = unique(value_labels))
  } else{
    value_labels = factor(value_labels, levels = rev(unique(value_labels)))
  }
  positions = rev(unique(var_labels))
  if(order_bars == TRUE){
    var_labels = factor(var_labels, levels = unique(var_labels))
    data = data.frame(var_labels, value_labels, outcome_var, prop_labels)
    ggplot(data, aes(y = outcome_var, x = var_labels,
                      fill = reorder(value_labels, outcome_var), label = prop_labels)) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[1], ]) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[2], ]) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[3], ]) +
      # geom_bar(position = "stack", stat = "identity", width = 0.6) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[1], ],
                aes(label = prop_labels),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[2], ],
                aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[3], ],
                aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[1], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4,
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[2], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4,
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[3], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4,
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      coord_flip() +
      scale_fill_manual(values = mycolors, guide=guide_legend(reverse = TRUE)) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(title = main_title,
           y = "",
           x = " ",
           caption = paste0(ifelse(lang == "es", "Fuente: Bar\u00f3metro de las Am\u00e9ricas ", "Source: AmericasBarometer "),
                            source_info),
           subtitle = subtitle) +
      theme(text = element_text(size = 14, family = "roboto"),
            plot.title = element_text(size = 17, family = "nunito", face = "bold"),
            plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "roboto-light", color="#545454"),
            plot.subtitle = element_text(size = 14, family = "nunito-light", color="#545454"),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(margin=margin(r=0)),
            axis.ticks = element_blank(),
            aspect.ratio = 0.35,
            axis.text = element_text(size = 14, family = "roboto", color = "#545454", margin=margin(r=5)),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.text = element_text(family = "roboto", color = "#545454"),
            legend.title = element_blank(),
            legend.justification='left',
            legend.key.size = unit(1, "line"),
            legend.margin = margin(t=5,b=5, 0, subtitle_h_just))
  } else{
  ggplot(data, aes(fill = value_labels, y = outcome_var, x = var_labels, label = prop_labels)) +
    geom_bar(position = "stack", stat = "identity", width = 0.6) +
    geom_text(label = ifelse(outcome_var >= 5, prop_labels, NA),
              position = position_stack(vjust = 0.5), color = "#FFFFFF",
              fontface = "bold", size = 5) +
    ggrepel::geom_text_repel(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA),
                             position = position_stack(vjust = 0.5),
                             color = "#FFFFFF", segment.color = 'transparent',
                             fontface = "bold", size = 4,
                             direction = "y",
                             force_pull = 0.2, force = 5) +
    coord_flip() +
    scale_fill_manual(values = mycolors, guide=guide_legend(reverse = TRUE)) +
    scale_x_discrete(limits = positions, expand = c(0, 0)) +
    scale_y_continuous(expand = c(0.02, 0)) +
    labs(title = main_title,
         y = "",
         x = " ",
         caption = paste0(ifelse(lang == "es", "Fuente: Bar\u00f3metro de las Am\u00e9ricas ", "Source: AmericasBarometer "),
                          source_info),
         subtitle = subtitle) +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 17, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "roboto-light", color="#545454"),
          plot.subtitle = element_text(size = 14, family = "nunito-light", color="#545454"),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(margin=margin(r=0)),
          axis.ticks = element_blank(),
          aspect.ratio = 0.35,
          axis.text = element_text(size = 14, family = "roboto", color = "#545454", margin=margin(r=5)),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.text = element_text(family = "roboto", color = "#545454"),
          legend.title = element_blank(),
          legend.justification='left',
          legend.key.size = unit(1, "line"),
          legend.margin = margin(t=5,b=5, 0, subtitle_h_just))
  }
}
