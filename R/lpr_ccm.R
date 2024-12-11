#######################################

# LAPOP Cross-Country Bar Graph Pre-Processing #

#######################################

#' LAPOP Cross-Country Bar Graph Pre-Processing
#'
#' This function creates dataframes which can then be input in lapop_cc for
#' comparing values across countries with a bar graph using LAPOP formatting.
#'
#' @param data A survey object.  The data that should be analyzed.
#' @param outcome Character string.  Outcome variable of interest to be plotted
#' across countries.
#' @param xvar Default: pais_lab.
#' @param rec Numeric. The minimum and maximum values of the outcome variable that
#' should be included in the numerator of the percentage.  For example, if the variable
#' is on a 1-7 scale and rec is c(5, 7), the function will show the percentage who chose
#' an answer of 5, 6, 7 out of all valid answers.  Default: c(1, 1).
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' rescaling to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param sort Character. On what value the bars are sorted: the x or the y.
#' Options are "y" (default; for the value of the outcome variable), "xv" (for
#' the underlying values of the x variable), "xl" (for the labels of the x variable,
#' i.e., alphabetical).
#' @param order Character.  How the bars should be sorted.  Options are "hi-lo"
#' (default) or "lo-hi".
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all individual x levels and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_cc
#'
#' @examples
#'
#' \dontrun{lpr_cc(data = gms, outcome = ing4, num = c(5, 7))}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}





lpr_ccm <- function(data,
                    outcome_vars,
                    xvar = "pais_lab",
                    rec1 = c(1, 1),
                    rec2 = c(1, 1),
                    rec3 = c(1, 1),
                    ci_level = 0.95,
                    mean = FALSE,
                    filesave = "",
                    cfmt = "",
                    sort = "y",
                    order = "hi-lo",
                    ttest = FALSE,
                    keep_nr = FALSE) {

  # Map rec arguments to outcome variables
  rec_list <- list(rec1, rec2, rec3)
  rec_map <- purrr::map2(outcome_vars, rec_list[1:length(outcome_vars)], ~ list(var = .x, rec = .y))

  # Handle NA recoding if keep_nr is TRUE
  if (keep_nr) {
    data <- data %>%
      mutate(across(all_of(outcome_vars), ~ case_when(
        na_tag(.) %in% c("a", "b") ~ 99,
        TRUE ~ as.numeric(.)
      )))
  }

  # Process each outcome variable with its respective rec
  ccm <- purrr::map_dfr(rec_map, function(mapping) {
    outcome <- mapping$var
    rec <- mapping$rec

    temp <- data %>%
      drop_na(!!sym(xvar)) %>%
      group_by(pais = as_factor(!!sym(xvar))) %>%
      {
        if (mean) {
          summarize(. ,
                    prop = survey_mean(!!sym(outcome),
                                       na.rm = TRUE,
                                       vartype = "ci",
                                       level = ci_level)) %>%
            mutate(proplabel = case_when(cfmt != "" ~ sprintf("%.1f", prop),
                                         TRUE ~ sprintf("%.1f", prop)))
        } else {
          summarize(. ,
                    prop = survey_mean(between(!!sym(outcome), rec[1], rec[2]),
                                       na.rm = TRUE,
                                       vartype = "ci",
                                       level = ci_level) * 100) %>%
            mutate(proplabel = case_when(cfmt != "" ~ sprintf("%.0f%%", round(prop)),
                                         TRUE ~ sprintf("%.0f%%", round(prop))))
        }
      } %>%
      filter(prop != 0) %>%
      rename(lb = prop_low, ub = prop_upp) %>%
      ungroup() %>%
      mutate(var = outcome)

    temp
  })
  # Sorting logic
  ccm = ccm %>%
    {
      if (sort == "y") {
          arrange(., if (order == "hi-lo") desc(prop) else prop)
      } else if (sort == "xl") {
          arrange(., if (order == "hi-lo") desc(as.character(pais)) else as.character(pais))
      } else if (sort == "xv") {
        arrange(., if (order == "hi-lo") desc(match(pais, levels(pais))) else match(pais, levels(pais)))
      } else {
        .
      }
    }

  # Save the results to a file if specified
  if (filesave != "") {
    write.csv(ccm, filesave, row.names = FALSE)
  }

  return(ccm)
}

x = lpr_ccm(gm23,
        outcome_vars = c("aoj11", "vic1ext", "pn4"),
        rec1 = c(1, 2),
        rec2 = c(1, 1),
        rec3 = c(1, 2),
        sort = "y")

x

lapop_ccm(x)
