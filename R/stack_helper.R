########################################

# Stacked Bar Graph Pre-Processing

########################################

#' Stacked Bar Graph Pre-Processing
#'
#' This function creates dataframes which can then be used to create stacked
#' bar plots.
#' If "by" is specified, the function shows the distribution of the outcome variable
#' by distinct values of a secondary variable.  If "by" is not provided, it will show
#' the distribution of all specified outcome variables.
#'
#' @param data  The data that should be analyzed.
#' @param outcome Character vector. Vector of variable(s) be plotted.
#' @param by Character. Grouping variable.  If by is provided, function will break
#' down the outcome variable by distinct values of the "by" variable (instead of
#' showing distribution of multiple outcome variables).
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
#' \dontrun{lapop_stack(data = gm, outcome = c("countfair1", "countfair3"))}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'@import purrr
#'
#'@author Luke Plutowski & Robert Vidigal

# # -----------------------------------------------------------------------
# stack_helper
# # -----------------------------------------------------------------------
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

    # Retrieve original factor levels from haven::as_factor
    factor_variable <- haven::as_factor(data[[outcome]])
    factor_levels <- levels(factor_variable)

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
        prop = n / sum(n) * 100,
        proplabel = sprintf("%.0f%%", prop),
        vallabel = factor(haven::as_factor(vallabel), levels = factor_levels)
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
