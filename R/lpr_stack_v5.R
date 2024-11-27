########################################

# LAPOP Stacked Bar Graph Pre-Processing

########################################

#' LAPOP Stacked Bar Graph Pre-Processing
#'
#' This function creates dataframes which can then be input in lapop_stack for
#' comparing values across countries with a stacked bar graph using LAPOP formatting.
#'
#' @param data  The data that should be analyzed. It requires a survey object from lpr_data function.
#' @param outcome List of variables be plotted.
#' @param filesave Character. Path and file name to save the dataframe as csv.
#' @param sort Character. On what value the bars are sorted: the x or the y.
#' Options are "y" (default; for the value of the outcome variable), "xv" (for
#' the underlying values of the x variable), "xl" (for the labels of the x variable,
#' i.e., alphabetical).
#' @param order Character. How the bars should be sorted. Options are "hi-lo"
#' (default) or "lo-hi".
#' @param keep_nr Logical. If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_stack
#'
#' @examples
#'
#' \dontrun{lapop_stack(data = gms, outcome = ing4, num = c(5, 7))}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'
#'@author Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

# # -----------------------------------------------------------------------
# LPR_STACK
# # -----------------------------------------------------------------------
lpr_stack <- function(data, 
                      outcome, 
                      sort = "y",
                      order = "hi-lo",
                      filesave = "",
                      keep_nr = FALSE) {
  library(srvyr)
  library(dplyr)
  library(purrr) # For iterating over variables
  
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
        prop = survey_mean(proportion = TRUE, na.rm = TRUE)
      ) %>%
      mutate(
        varlabel = attributes(data$variables[[outcome_var]])$label,
        variable = outcome_var, # Add a column to identify the variable
        prop = prop * 100, # Convert to percentage
        proplabel = sprintf("%.0f%%", prop)
      ) %>%
      select(variable, varlabel, vallabel, prop, proplabel) %>%
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
