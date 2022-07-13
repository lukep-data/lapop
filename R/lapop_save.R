#######################################

# LAPOP Save #

#######################################

#' LAPOP Save
#'
#' This function creates exports graphs created using the LAPOP templates.
#'
#' @param figure Ggplot object.
#' @param filename File path + name to be saved + .filetype.
#' @param format Character.  Options: "png", "svg". Default = "svg".
#' @param Logo Logical.  Should logo be added to plot?  Default: FALSE.
#' @param width_px Numeric. Width in pixels.  Default: 750.
#' @param height_px Numeric.  Height in pixels.
#'
#' @return Saves a file to provided directory.
#'
#' @examples
#' #'df <- data.frame(
#'cat = c("Far Left", 1, 2, 3, 4, "Center", 6, 7, 8, 9, "Far Right"),
#'prop = c(4, 3, 5, 12, 17, 23, 15, 11, 5, 4, 1),
#'proplabel = c("4%", "3%", "5%", "12%", "17%", "23%", "15%", "11%", "5%", "4%", "1%")
#')
#' myfigure <- lapop_hist(df,
#'           main_title = "Centrists are a plurality among Peruvians",
#'           subtitle = "Distribution of ideological preferences",
#'           source_info = "Peru, 2019",
#'           ymax = 27
#')
#' lapop_save(myfigure, "fig1.svg", format = "svg", width_px = 800)
#'
#'@export
#'

lapop_save <- function(figure, filename,
                       format = "svg",
                       logo = FALSE,
                       width_px = 750,
                       height_px = 500){
  dev.new()
  if(format == "svg"){
    svg(filename, width = width_px/96, height = height_px/96)
  } else if(format == "png"){
    png(filename, width = width_px/96, height = height_px/96)
  }
  print(figure)
  grid::grid.raster(logo, x = 0.95, y = 0.02,
                      just = c('right', 'bottom'),
                      width = unit(0.3 * 1.66, 'inches'), height = unit(0.3, 'inches'))
  dev.off()
}
