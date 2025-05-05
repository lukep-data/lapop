#######################################

# Bar Graphs #

#######################################

#' Bar Graphs
#'
#' This function shows a bar graph for categorical variable frequencies.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled cat (labels of each category in variable; character),
#' prop (outcome variable value; numeric), and proplabel (text of outcome variable value; character).
#' Default: None (must be provided).
#' @param cat_var,outcome_var,label_var Character, numeric, character.
#' Each component of the data to be plotted can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Defaults: 0, 100.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: " in the bottom-left corner of the graph.
#' Default: None (only "Source: " will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent who agree that...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of bars.
#' Takes hex numbers, beginning with "#". Default: "#008381".
#' @param order Logical.  Should bars be ordered from most frequent response to least?  Default: FALSE.
#' @return Returns an object of class \code{ggplot}, a ggplot bar graph.
#' @examples
#'
#'df <- data.frame(
#'cat = c("Far Left", 1, 2, 3, 4, "Center", 6, 7, 8, 9, "Far Right"),
#'prop = c(4, 3, 5, 12, 17, 23, 15, 11, 5, 4, 1),
#'proplabel = c("4%", "3%", "5%", "12%", "17%", "23%", "15%", "11%", "5%", "4%", "1%")
#')
#'lapop_hist(df,
#'           main_title = "Most people identify as centrist",
#'           subtitle = "Distribution of ideological preferences",
#'           source_info = "Venezuela, 2019",
#'           ymax = 27
#')
#'
#'
#'@export
#'@import ggplot2
#'@import ggtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'



svy_hist <- function(data,
                     outcome_var = data$prop,
                     label_var = data$proplabel,
                     cat_var = data$cat,
                     yminmax = NULL,
                     lang = "en",
                     main_title = "",
                     subtitle = "",
                     source_info = "",
                     order = FALSE,
                     color_scheme = "#377F99"){

  if(order == TRUE){
    data = data[order(-data$prop), ]
    cat_var = cat_var[order(-outcome_var)]
    label_var = label_var[order(-outcome_var)]
    outcome_var = outcome_var[order(-outcome_var)]
  }

  ggplot(data = data) +
  geom_bar(mapping = aes(x = reorder(cat, n),
                         y = n),
           stat = "identity", width = 0.4, fill = color_scheme) +
  ylab("Total Responses") +
  {if(!is.null(yminmax)) ylim(yminmax)}+
  labs(title = main_title) +
  coord_flip() +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.25, color = "#A8A99E"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 10),
        title = element_text(size = 12))

}
