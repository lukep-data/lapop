########################################

# LAPOP Stacked Bar Graph Pre-Processing

########################################

#' LAPOP Stacked Bar Graph Pre-Processing
#'
#' This function creates dataframes which can then be input in lapop_stack() for
#' plotting variables categories with a stacked bar graph using LAPOP formatting.
#' If "by" is specified, the function shows the distribution of the outcome variable
#' by distinct values of a secondary variable.  If "by" is not provided, it will show
#' the distribution of all specified outcome variables.
#'
#' @param data  The data that should be analyzed. It requires a survey object
#' from lpr_data() function.
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
#'@author Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

# # -----------------------------------------------------------------------
# LPR_STACK
# # -----------------------------------------------------------------------
lpr_stack <- function(data,
                       outcome,
                       by = NULL,
                       sort = "xv",
                       order = "lo-hi",
                       filesave = "",
                       keep_nr = FALSE) {

  if (!is.null(by)) {
    # Exclude NA values from 'by' before looping
    valid_values <- sort(unique(data$variables[[by]][!is.na(data$variables[[by]])]))

    # Loop over all values of "by" variable - create a separate column for each value
    for (value in valid_values) {
      if (any(!is.na(data$variables[[outcome]][data$variables[[by]] == value]))) {

        column_name <- paste0("x_by_", value)

        data$variables[[column_name]] <- replace(
          data$variables[[outcome]],
          data$variables[[by]] != value,
          NA
        )

        # Extract value label and assign it to newly created variables
        label_value <- names(which(attributes(data$variables[[by]])$labels == value))

        if (length(label_value) == 0) {
          label_value <- as.character(value)  # Fallback if no label is found
        }

        attributes(data$variables[[column_name]])$label <- label_value
      }
    }

    # Update outcome list to include the newly created "x_by_*" variables
    outcome <- grep("x_by", names(data$variables), value = TRUE)
  }

  # Helper function to handle a single variable
  process_outcome <- function(data, outcome_var) {
    # Handle `keep_nr` logic
    if (keep_nr) {
      data <- data %>%
        mutate(!!sym(outcome_var) := case_when(
          na_tag(!!sym(outcome_var)) %in% c("a", "b") ~ 99, # Replace "NA(a)" and "NA(b)" with 99
          TRUE ~ as.numeric(!!sym(outcome_var))       # Keep other values unchanged
        ))
    }

    # Perform proportion calculations
    stack <- data %>%
      drop_na(!!sym(outcome_var)) %>%
      group_by(vallabel = as_factor(!!sym(outcome_var))) %>%
      summarise(
        prop = survey_mean(proportion = TRUE)
      ) %>%
      mutate(
        varlabel = if (!is.null(attributes(data$variables[[outcome_var]])$label)) {
          attributes(data$variables[[outcome_var]])$label
        } else {
          outcome_var
        },
        prop = prop * 100, # Convert to percentage
        proplabel = sprintf("%.0f%%", prop)
      ) %>%
      select(varlabel, vallabel, prop, proplabel) %>%
      ungroup()

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
