######################################################

# LAPOP "Dumbbell" Graph Pre-Processing #

######################################################

#' LAPOP Dumbbell Graphs
#'
#' This function creates dataframes which can then be input in lapop_dumb for
#' comparing means of a variable across countries and two waves using LAPOP formatting.

#' @param data A survey object.  The data that should be analyzed.
#' @param outcome Outcome variable of interest to be plotted across countries
#' and waves, supplied as a character string.
#' @param xvar Character. The grouping variable to be plotted
#' along the x-axis (technically, the vertical axis for lapop_dumb). Usually
#' country (pais).  Default: "pais".
#' @param over Numeric.  A vector of values for "wave" that specify which two
#' waves should be included in the plot.
#' @param rec Numeric. The minimum and maximum values of the outcome variable that
#' should be included in the numerator of the percentage.  For example, if the variable
#' is on a 1-7 scale and rec is c(5, 7), the function will show the percentage who chose
#' an answer of 5, 6, 7 out of all valid answers.  Default: c(1, 1).
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' recoding to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param sort Character. On what value the bars are sorted.
#' Options are "prop1" (for the value of the outcome variable in wave 1), "prop2"
#' (default; for the value of the outcome variable in wave 2), "xv" (for
#' the underlying values of the x variable), "xl" (for the labels of the x variable,
#' i.e., alphabetical), and "diff" (for the difference between the outcome between
#' the two waves).
#' @param order Character.  How the bars should be sorted.  Options are "hi-lo"
#' (default) or "lo-hi".
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all individual year-xvar levels and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_dumb
#'
#' @examples
#'
#' \dontrun{lpr_dumb(gm2123,
#' outcome = "ing4",
#' rec = c(5, 7),
#' over = c(2021, 2023),
#' sort = "diff",
#' ttest = TRUE)}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}


lpr_dumb = function(data,
                  outcome,
                  xvar = "pais",
                  over,
                  rec = c(1, 1),
                  ci_level = 0.95,
                  mean = FALSE,
                  filesave = "",
                  cfmt = "",
                  sort = "prop2",
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

    wave1 = data %>%
    drop_na(!!sym(xvar)) %>%
    filter(wave == over[1]) %>%
    group_by(pais = as_factor(!!sym(xvar)),
             wave1 = as.character(as_factor(wave))
    ) %>%
    {
      if (mean) {
        summarize(.,
                  prop1 = survey_mean(!!sym(outcome),
                                     na.rm = TRUE,
                                     vartype = "ci",
                                     level = ci_level)) %>%
          mutate(proplabel1 = case_when(cfmt != "" ~ sprintf("%.1f", prop1),
                                       TRUE ~ sprintf("%.1f", prop1)))
      } else {
        summarize(.,
                  prop1 = survey_mean(between(!!sym(outcome), rec[1], rec[2]),
                                     na.rm = TRUE,
                                     vartype = "ci",
                                     level = ci_level) * 100) %>%
          mutate(proplabel1 = case_when(cfmt != "" ~ sprintf("%.0f%%", round(prop1)),
                                       TRUE ~ sprintf("%.0f%%", round(prop1))))
      }
    } %>%
    filter(prop1 != 0) %>%
    rename(., lb1 = prop1_low, ub1 = prop1_upp)

  wave2 = data %>%
    drop_na(!!sym(xvar)) %>%
    filter(wave == over[2]) %>%
    group_by(pais = as_factor(!!sym(xvar)),
             wave2 = as.character(as_factor(wave))
    ) %>%
    {
      if (mean) {
        summarize(.,
                  prop2 = survey_mean(!!sym(outcome),
                                     na.rm = TRUE,
                                     vartype = "ci",
                                     level = ci_level)) %>%
          mutate(proplabel2 = case_when(cfmt != "" ~ sprintf("%.1f", prop2),
                                        TRUE ~ sprintf("%.1f", prop2)))
      } else {
        summarize(.,
                  prop2 = survey_mean(between(!!sym(outcome), rec[1], rec[2]),
                                      na.rm = TRUE,
                                      vartype = "ci",
                                      level = ci_level) * 100) %>%
          mutate(proplabel2 = case_when(cfmt != "" ~ sprintf("%.0f%%", round(prop2)),
                                        TRUE ~ sprintf("%.0f%%", round(prop2))))
      }
    } %>%
    filter(prop2 != 0) %>%
    rename(., lb2 = prop2_low, ub2 = prop2_upp)

  dumb = merge(wave1, wave2, by = "pais")

  dumb = dumb %>%
    {
      if (sort == "prop1") {
        if (order == "hi-lo") {
          arrange(., desc(prop1))
        } else if (order == "lo-hi") {
          arrange(., prop1)
        }
      } else if (sort == "prop2") {
        if (order == "hi-lo") {
          arrange(., desc(prop2))
        } else if (order == "lo-hi") {
          arrange(., prop2)
        }
      } else if (sort == "xv") {
        if (order == "hi-lo") {
          arrange(., desc(match(pais, levels(pais))))
        } else if (order == "lo-hi") {
          arrange(., match(pais, levels(pais)))
        }
      } else if (sort == "diff") {
        if (order == "hi-lo") {
          mutate(., diff = prop2 - prop1) %>%
          arrange(., desc(diff))
        } else if (order == "lo-hi") {
          mutate(., diff = prop2 - prop1) %>%
          arrange(., diff)
        }
      } else if (sort == "xl") {
        if (order == "hi-lo") {
          arrange(., desc(as.character(xvar)))
        } else if (order == "lo-hi") {
          arrange(., as.character(xvar))
        } else {
          .  # Return unchanged if no valid sorting option is selected
        }
      }
    }


  if (ttest) {
    dumb = dumb %>%
      mutate(se1 = (ub1 - lb1) / (2 * 1.96),
             se2 = (ub2 - lb2) / (2 * 1.96))

    dumb = dumb %>%
      rowwise() %>%
      mutate(diff = prop1 - prop2,
             ttest = diff / sqrt(se1^2 + se2^2),
             df = (se1^2 + se2^2)^2 /
               ((se1^2)^2 / (nrow(data) - 1) + (se2^2)^2 / (nrow(data) - 1)),
             pval = 2 * pt(-abs(ttest), df)) %>%
      ungroup() %>%
      select(-se1, -se2, -df)
  }

  if (filesave != "") {
    write.csv(dumb, filesave)
  }

  return(dumb)
}



