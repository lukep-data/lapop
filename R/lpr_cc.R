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
#' @param xvar Character. Outcome variable will be broken down by this variable.
#' Default: pais_lab.
#' @param rec Numeric. The minimum and maximum values of the outcome variable that
#' should be included in the numerator of the percentage.  For example, if the variable
#' is on a 1-7 scale and rec is c(5, 7), the function will show the percentage who chose
#' an answer of 5, 6, 7 out of all valid answers.  Default: c(1, 1).
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' rescaling to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Character. changes the format of the numbers displayed above the bars.
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
#' \dontrun{lpr_cc(data = gms, outcome = "ing4", num = c(5, 7))}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}

lpr_cc = function(data,
                  outcome,
                  xvar = "pais_lab",
                  rec = c(1, 1),
                  ci_level = 0.95,
                  mean = FALSE,
                  filesave = "",
                  cfmt = "",
                  sort = "y",
                  order = "hi-lo",
                  ttest = FALSE,
                  keep_nr = FALSE) {

  # If keep_nr is TRUE, convert don't knows (NA(a)) and no answers (NA(b)) to
  # non-NA data (a value of 99).
  if (keep_nr) {
    data <- data %>%
      mutate(!!sym(outcome) := case_when(
        na_tag(!!sym(outcome)) %in% c("a", "b") ~ 99,
        TRUE ~ as.numeric(!!sym(outcome))
      ))
  }

  cc <- data %>%
    drop_na(!!sym(xvar)) %>%
    group_by(vallabel = as_factor(!!sym(xvar))) %>%
    {
      if (mean) {
        summarize(.,
                  prop = survey_mean(!!sym(outcome),
                                     na.rm = TRUE,
                                     vartype = "ci",
                                     level = ci_level)) %>%
          mutate(proplabel = if (cfmt != "") {
            sprintf(cfmt, prop)
          } else {
            sprintf("%.1f", prop)
          })
      } else {
        summarize(.,
                  prop = survey_mean(between(!!sym(outcome), rec[1], rec[2]),
                                     na.rm = TRUE,
                                     vartype = "ci",
                                     level = ci_level) * 100) %>%
          mutate(proplabel = if (cfmt != "") {
            sprintf(cfmt, round(prop))
          } else {
            sprintf("%.0f%%", round(prop))
          })}
      } %>%
    filter(prop != 0) %>%
    rename(lb = prop_low, ub = prop_upp) %>%
    ungroup() %>%  # Ungroup to avoid issues with arrange
    {
      if (sort == "y") {
        if (order == "hi-lo") {
          arrange(., desc(prop))
        } else if (order == "lo-hi") {
          arrange(., prop)
        }
      } else if (sort == "xv") {
        if (order == "hi-lo") {
          arrange(., desc(vallabel))
        } else if (order == "lo-hi") {
          arrange(., vallabel)
        }
      } else if (sort == "xl") {
        if (order == "hi-lo") {
          arrange(., desc(as.character(vallabel)))
        } else if (order == "lo-hi") {
          arrange(., as.character(vallabel))
        } else {
          .  # Return unchanged if no valid sorting option is selected
        }
      }
    }

  # Perform pairwise t-tests if requested
  if (ttest) {
    # Estimate standard error for each row using the confidence intervals
    cc <- cc %>%
      mutate(se = (ub - lb) / (2 * 1.96))

    # Initialize an empty data frame to store t-test results
    t_test_results <- data.frame(test = character(),
                                 diff = numeric(),
                                 ttest = numeric(),
                                 pval = numeric(),
                                 stringsAsFactors = FALSE)

    # Perform pairwise t-tests for each combination of rows
    for(i in 1:(nrow(cc) - 1)) {
      for(j in (i + 1):nrow(cc)) {
        # Extract values for the two rows being compared
        # adding a comment
        prop1 <- cc$prop[i]
        se1 <- cc$se[i]
        prop2 <- cc$prop[j]
        se2 <- cc$se[j]

        # Calculate difference, t-statistic, and degrees of freedom
        diff <- prop1 - prop2
        t_stat <- diff / sqrt(se1^2 + se2^2)
        df <- (se1^2 + se2^2)^2 / ((se1^2)^2 / (nrow(data) - 1) + (se2^2)^2 / (nrow(data) - 1))

        # Calculate p-value
        p_value <- 2 * pt(-abs(t_stat), df)

        # Store results in a data frame
        t_test_results <- rbind(t_test_results,
                                data.frame(test = paste(cc$vallabel[i], "vs", cc$vallabel[j]),
                                           diff = diff,
                                           ttest = t_stat,
                                           pval = p_value))
        attr(cc, "t_test_results") <- t_test_results
      }
    }
  }

  if (filesave != "") {
    write.csv(cc, filesave)
  }

  return(cc)
}





