#######################################

# LAPOP Multi-Line, Multi-Variable Time Series Graph Pre-Processing #

#######################################

#' LAPOP Multi-Line, Multi-Variable Time Series Graph Pre-Processing
#'
#' This function creates a dataframe which can then be input in lapop_mline. It
#' will show the values of between 1 and 4 variables across time (essentially,
#' it allows you to do lpr_ts for multiple variables).  The lpr_mline function
#' instead shows the value of one outcome variable, broken down by values of
#' a secondary variable.
#'
#' @param data A survey object.  The data that should be analyzed.
#' @param outcome Character vector.  Outcome variables of interest to be plotted
#' across time.
#' @param rec1,rec2,rec3,rec4 Numeric. The minimum and maximum values of the outcome
#' variable that should be included in the numerator of the percentage.
#' For example, if the variable is on a 1-7 scale and rec is c(5, 7), the
#' function will show the percentage who chose an answer of 5, 6, 7 out of
#' all valid answers.  Can also supply one value only, to produce the percentage
#' that chose that value out of all other values. Default: c(1, 1).
#' @param use_wave Logical.  If TRUE, will use "wave" for the x-axis; otherwise,
#' will use "year".  Default: FALSE.
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' rescaling to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Character. changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all individual x levels and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_mline
#'
#' @examples
#'
#' \dontrun{lpr_mline_mv(ttots,
#' outcome = c("ing4", "pn4", "b2", "vic1ext"),
#' rec1 = c(5, 7),
#' rec2 = c(1, 2),
#' rec3 = c(5, 7),
#' rec4 = c(1, 1),
#' use_wave = TRUE)}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}

lpr_mline_mv <- function(data,
                   outcome,
                   rec1 = c(1, 1),
                   rec2 = c(1, 1),
                   rec3 = c(1, 1),
                   rec4 = c(1, 1),
                   use_wave = FALSE,
                   ci_level = 0.95,
                   mean = FALSE,
                   filesave = "",
                   cfmt = "",
                   ttest = FALSE,
                   keep_nr = FALSE) {


  if (length(rec1) == 1) {
    rec1 = c(rec1, rec1)
  }
  if (length(rec2) == 1) {
    rec2 = c(rec2, rec2)
  }
  if (length(rec3) == 1) {
    rec3 = c(rec3, rec3)
  }
  if (length(rec4) == 1) {
    rec4 = c(rec4, rec4)
  }

  rec_list = list(rec1, rec2, rec3, rec4)

  result_list <- list()

  for (var in outcome) {
    temp <- lpr_ts(data = data,
                   outcome = var,
                   rec = rec_list[[which(outcome == var)]],
                   use_wave = use_wave,
                   mean = mean,
                   cfmt = cfmt,
                   keep_nr = keep_nr)
    temp$varlabel <- var
    result_list[[var]] <- temp
  }

  mline <- do.call(rbind, result_list)

  # Perform pairwise t-tests for each combination of rows
  if (ttest) {
    # Estimate standard error for each row using the confidence intervals
    mline <- mline %>%
      mutate(se = (ub - lb) / (2 * 1.96))

    # Initialize an empty data frame to store t-test results
    t_test_results <- data.frame(
      test = character(),
      diff = numeric(),
      t_stat = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )

    # Perform pairwise t-tests for each combination of rows
    for (i in 1:(nrow(mline) - 1)) {
      for (j in (i + 1):nrow(mline)) {
        # Extract values for the two rows being compared
        prop1 <- mline$prop[i]
        se1 <- mline$se[i]
        prop2 <- mline$prop[j]
        se2 <- mline$se[j]

        # Calculate difference, t-statistic, and degrees of freedom
        diff <- prop1 - prop2
        t_stat <- diff / sqrt(se1^2 + se2^2)
        df <- (se1^2 + se2^2)^2 /
          ((se1^2)^2 / (nrow(mline) - 1) + (se2^2)^2 / (nrow(mline) - 1))

        # Calculate p-value
        p_value <- 2 * pt(-abs(t_stat), df)

        # Store results in a data frame
        t_test_results <- rbind(
          t_test_results,
          data.frame(
            test = paste0(
              mline$varlabel[i], mline$wave[i],
              " vs ",
              mline$varlabel[j], mline$wave[j]
            ),
            diff = diff,
            t_stat = t_stat,
            p_value = p_value
          )
        )
      }
    }

    # Add t-test results as an attribute to the dataset
    attr(mline, "t_test_results") <- t_test_results
  }


  if (filesave != "") {
    write.csv(mline, filesave, row.names = FALSE)
  }

  return(mline)
}



