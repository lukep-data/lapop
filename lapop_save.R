lapop_save <- function(figure, filename, 
                       format = "svg", 
                       logo = TRUE,
                       width_px = 750,
                       height_px = 500){
  dev.new()
  if(format == "svg"){
    svg(filename, width = width_px/96, height = height_px/96)
  } else if(format == "png"){
    png(filename, width = width_px/96, height = height_px/96)
  }
  figure
  grid::grid.raster(logo, x = 0.95, y = 0.02,
                      just = c('right', 'bottom'),
                      width = unit(0.3 * 1.66, 'inches'), height = unit(0.3, 'inches')) 
  dev.off()
}
