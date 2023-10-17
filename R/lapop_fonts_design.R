#######################################

# LAPOP Visualization Templates #

#######################################


#' LAPOP Fonts (design)
#'
#' This function loads fonts needed for LAPOP graph formatting.
#'
#' @param showtext_end Logical.  If you were previously using lapop_fonts, turn to TRUE so
#' it stops using the glyph rendering of fonts and starts using text.
#'
#'@import sysfonts
#'@import systemfonts
#'
#'@export
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'@return No return value, called for side effects
#'@examples
#'\dontrun{lapop_fonts_design()}
#'


lapop_fonts_design <- function(showtext_end = FALSE){
  if(showtext_end == TRUE){
    showtext_end()
  }
  systemfonts::register_variant(
    name = "roboto-light",
    family = "roboto",
    weight = "light"
  )
  systemfonts::register_variant(
    name = "nunito-light",
    family = "nunito",
    weight = "light"
  )
}
