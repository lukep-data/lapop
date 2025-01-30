######################################################

# LAPOP "Multiple-Over" Breakdown Graph Pre-Processing #

######################################################

#' LAPOP "Multiple-Over" Breakdown Graphs
#'
#' This function creates dataframes which can then be input in lapop_mover for
#' comparing means across values of secondary variable(s) using LAPOP formatting.
#'
#' @param data A survey object.  The data that should be analyzed.
#' @param outcome Character. Outcome variable of interest to be plotted across secondary
#' variable(s).
#' @param grouping_vars A character vector specifying one or more grouping variables.
#' For each variable, the function calculates the average of the outcome variable,
#' broken down by the distinct values within the grouping variable(s).
#' @param rec Numeric. The minimum and maximum values of the outcome variable that
#' should be included in the numerator of the percentage.  For example, if the variable
#' is on a 1-7 scale and rec is c(5, 7), the function will show the percentage who chose
#' an answer of 5, 6, 7 out of all valid answers.  Can also supply one value only,
#' to produce the percentage that chose that value out of all other values.
#' Default: c(1, 1).
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' recoding to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all individual year-xvar levels and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_mover
#'
#' @examples
#'
#' \dontrun{lpr_mover(data = gm23,
#'  outcome = "ing4",
#'  grouping_vars = c("q1tc_r", "edad", "edre", "wealth"),
#'  num = c(5, 7)}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'@import purrr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}

lpr_mover <- function(data,
                      outcome,
                      grouping_vars,
                      rec = c(1, 1),
                      ci_level = 0.95,
                      mean = FALSE,
                      filesave = "",
                      cfmt = "",
                      ttest = FALSE,
                      keep_nr = FALSE
) {

  # If keep_nr is TRUE, convert don't knows (NA(a)) and no answers (NA(b)) to
  # non-NA data (a value of 99).
  if (keep_nr) {
    data <- data %>%
      mutate(!!outcome := case_when(
        na_tag(.data[[outcome]]) == "a" | na_tag(.data[[outcome]]) == "b" ~ 99,
        TRUE ~ as.numeric(.data[[outcome]])
      ))
  }

  if (length(rec) == 1) {
    rec = c(rec, rec)
  }

  # Calculate means for each individual breakdown variable
  calculate_means <- function(data, grouping_var, outcome_var) {
    data %>%
      drop_na(.data[[grouping_var]]) %>%
      group_by(vallabel = as_factor(.data[[grouping_var]])) %>%
      {
        if (mean) {
          summarize(.,
                    prop = survey_mean(.data[[outcome_var]],
                                       na.rm = TRUE,
                                       vartype = "ci",
                                       level = ci_level)
          ) %>%
            mutate(
              proplabel = case_when(cfmt != "" ~ sprintf("%.1f", prop),
                                    TRUE ~ sprintf("%.1f", prop))
            )
        } else {
          summarize(.,
                    prop = survey_mean(between(.data[[outcome_var]], rec[1], rec[2]),
                                       na.rm = TRUE,
                                       vartype = "ci",
                                       level = ci_level) * 100
          ) %>%
            mutate(
              proplabel = case_when(cfmt != "" ~ sprintf("%.0f%%", round(prop)),
                                    TRUE ~ sprintf("%.0f%%", round(prop)))
            )
        }
      } %>%
      mutate(
        varlabel = if (!is.null(attributes(data$variables[[grouping_var]])$label)) {
          attributes(data$variables[[grouping_var]])$label
        } else {
          grouping_var # Use the variable name as a fallback
        },
        vallabel = as.character(vallabel)
      ) %>%
      rename(lb = prop_low, ub = prop_upp) %>%
      select(varlabel, vallabel, prop, proplabel, lb, ub)
  }

  mover <- map_dfr(grouping_vars, ~ calculate_means(data, .x, outcome))

  if (filesave != "") {
    write.csv(mover, filesave)
  }

  # conduct pairwise t-tests if requested
  if (ttest) {
    mover <- mover %>%
      mutate(se = (ub - lb) / (2 * 1.96))

    t_test_results <- data.frame(
      varlabel = character(),
      test = character(),
      diff = numeric(),
      ttest = numeric(),
      pval = numeric(),
      stringsAsFactors = FALSE
    )

    varlabels <- unique(mover$varlabel)
    for (vl in varlabels) {
      mover_subset <- mover %>% filter(varlabel == vl)

      for (i in 1:(nrow(mover_subset) - 1)) {
        for (j in (i + 1):nrow(mover_subset)) {
          prop1 <- mover_subset$prop[i]
          se1 <- mover_subset$se[i]
          prop2 <- mover_subset$prop[j]
          se2 <- mover_subset$se[j]

          diff <- prop1 - prop2
          t_stat <- diff / sqrt(se1^2 + se2^2)
          df <- (se1^2 + se2^2)^2 / ((se1^2)^2 / (nrow(data) - 1) + (se2^2)^2 / (nrow(data) - 1))

          p_value <- 2 * pt(-abs(t_stat), df)

          t_test_results <- rbind(t_test_results,
                                  data.frame(varlabel = vl,
                                             test = paste(mover_subset$vallabel[i], "vs", mover_subset$vallabel[j]),
                                             diff = diff,
                                             ttest = t_stat,
                                             pval = p_value))
        }
      }
    }

    # Attach t-test results as an attribute
    attr(mover, "t_test_results") <- t_test_results
  }

  return(mover)
}





