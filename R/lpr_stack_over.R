########################################

# LAPOP Stacked-Over Bar Graph Pre-Processing

########################################

#' LAPOP Stacked-Over Bar Graph Pre-Processing
#'
#' Like lpr_stack(), this function creates dataframes which can then be input
#' in lapop_stack() for plotting variables categories with a stacked bar graph.
#' Unlike lpr_stack(), which shows a bar for each specified outcome variable,
#' this function takes one outcome variable, and plots it across all values
#' of a secondary "over" variable.
#'
#' @param data  The data that should be analyzed. It requires a survey object
#' from lpr_data() function.
#' @param outcome Character. Outcome variable to be plotted.
#' @param over Character. Grouping variable.  The function breaks down the outcome
#' variable over distinct values of the "over" variable.
#' @param sort Character. On what value the bars are sorted. Options are "xv"
#' (default; sort on the underlying values of the value labels), "xl" (on the value
#' labels themselves, i.e. alphabetically), or "y" (on the proportions of the
#' outcome variable(s)).
#' @param order Character. How the bars should be sorted. Options are "lo-hi"
#' (default) or "hi-lo".
#' @param filesave Character. Path and file name to save the dataframe as csv.
#' @param keep_nr Logical. If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_stack
#'
#' @examples
#'
#' \dontrun{lapop_stack(data = gm, outcome = "aoj11", over = "wave"))}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'@import purrr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}




lpr_stack_over <- function(data,
                      outcome,
                      over,
                      sort = "xv",
                      order = "lo-hi",
                      filesave = "",
                      keep_nr = FALSE) {

  # Exclude NA values from 'over' before looping
  valid_values <- sort(unique(data$variables[[over]][!is.na(data$variables[[over]])]))


  #loop over all values of "over" variable - create a separate column for each value
  # column values will be the outcome variable if over == particular value, otherwise NA
  for (value in valid_values) {
    # if all values of outcome are NA for value of over, do not create new column
    if (any(!is.na(data$variables[[outcome]][data$variables[[over]] == value]))) {

      column_name <- paste0("x_over_", value)

      data$variables[[column_name]] <- replace(
      data$variables[[outcome]],
      data$variables[[over]] != value,
      NA
  )

  #Extract value label and assign it to newly created variables, which
  #will then be put into lpr_stack so it displays value labels of "over" variable
  #instead of its underlying values (e.g., "Mexico", "Guatemala", instead of "1", "2")
  label_value <- names(which(attributes(data$variables[[over]])$labels == value))

  if (length(label_value) == 0) {
    label_value <- as.character(value)  # Fallback to the value itself if no label is found
  }

  attributes(data$variables[[column_name]])$label <- label_value

    }
  }


  stack_df = lpr_stack(data = data,
                   outcome = grep("x_over", names(data$variables), value=TRUE),
                   sort = sort,
                   order = order,
                   filesave = filesave,
                   keep_nr = keep_nr)
  return(stack_df)
}


