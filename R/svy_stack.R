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
#' @param hide_small_values Logical.  Should labels for categories with less than 5 percent be hidden?  Default: TRUE.
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
  if(!inherits(value_labels, "character") & !inherits(value_labels, "factor")){
    value_labels = as.character(value_labels)
    data$vallabel = as.character(data$vallabel)
  }
  mycolors = rev(color_scheme[seq_along(unique(value_labels))])
  if(rev_values == TRUE){
    value_labels = factor(value_labels, levels = unique(value_labels))
  } else{
    value_labels = factor(value_labels, levels = rev(unique(value_labels)))
  }
  positions = rev(unique(var_labels))

  ggplot(data = data, aes(x = var_labels,
                          y = prop, fill = value_labels)) +
    geom_bar(stat = "identity", width = 0.5) +
    coord_flip() +
    labs(title = main_title,
         subtitle = subtitle) +
    ylab("") +
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
          legend.text = element_text(size = 8),
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




stack_helper <- function(data,
                         outcome,
                         by = NULL,
                         sort = "xv",
                         order = "lo-hi",
                         filesave = "",
                         keep_nr = FALSE) {

  if (!is.null(by)) {
    # Exclude NA values from 'by' before looping
    valid_values <- sort(unique(data[[by]][!is.na(data[[by]])]))

    # Loop over all values of "by" variable - create a separate column for each value
    for (value in valid_values) {
      if (any(!is.na(data[[outcome]][data[[by]] == value]))) {

        column_name <- paste0("x_by_", value)

        data[[column_name]] <- replace(
          data[[outcome]],
          data[[by]] != value,
          NA
        )

        # Extract value label and assign it to newly created variables
        label_value <- names(which(attributes(data[[by]])$labels == value))

        if (length(label_value) == 0) {
          label_value <- as.character(value)  # Fallback if no label is found
        }

        attributes(data[[column_name]])$label <- label_value
      }
    }

    # Update outcome list to include the newly created "x_by_*" variables
    outcome <- grep("x_by", names(data), value = TRUE)
  }

  # Helper function to handle a single variable
  process_outcome <- function(data, outcome) {
    # Handle `keep_nr` logic
    if (keep_nr) {
      data <- data %>%
        mutate(!!sym(outcome) := case_when(
          na_tag(!!sym(outcome)) %in% c("a", "b") ~ 99, # Replace "NA(a)" and "NA(b)" with 99
          TRUE ~ as.numeric(!!sym(outcome))       # Keep other values unchanged
        ))
    }

    # Perform proportion calculations
    stack <- data %>%
      drop_na(outcome) %>%
      group_by(across(as_factor(outcome))) %>%
      summarise(
        n = n()
      ) %>%
      rename(vallabel = 1) %>%
      mutate(
        varlabel = if (!is.null(attributes(data[[outcome]])$label)) {
          attributes(data[[outcome]])$label
        } else {
          outcome
        },
        prop = n/sum(n),
        prop = prop * 100, # Convert to percentage
        proplabel = sprintf("%.0f%%", prop),
        vallabel = haven::as_factor(vallabel)
      ) %>%
      ungroup() %>%
      dplyr::select(varlabel, vallabel, prop, proplabel)

    # Sorting logic
    stack <- stack %>%
      {
        if (sort == "y") {
          if (order == "hi-lo") {
            arrange(., desc(prop))
          } else if (order == "lo-hi") {
            arrange(., prop)
          } else {
            .
          }
        } else if (sort == "xv") {
          if (order == "hi-lo") {
            arrange(., desc(vallabel))
          } else if (order == "lo-hi") {
            arrange(., vallabel)
          } else {
            .
          }
        } else if (sort == "xl") {
          if (order == "hi-lo") {
            arrange(., desc(as.character(vallabel)))
          } else if (order == "lo-hi") {
            arrange(., as.character(vallabel))
          } else {
            .
          }
        } else {
          .
        }
      }

    return(stack)
  }

  # Apply the purrr helper function to all outcomes and combine the results
  results <- map_dfr(outcome, ~ process_outcome(data, .x))

  # Save to file if required
  if (filesave != "") {
    write.csv(results, filesave, row.names = FALSE)
  }

  return(results)
}
