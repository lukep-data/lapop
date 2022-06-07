logoloc <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\3_Media\Logos\lapop\lapop-full-color-00ada9.svg)")  # logo location and file name
logo <- image_read(logoloc)

lp_save <- function(figure, filename, format = "svg"){
  dev.new()
  if(format == "svg"){
    svg(filename, width = 750/96, height = 500/96)
  } else if(format == "png"){
    png(filename, width = 750/96, height = 500/96)
  }
  print(figure)
  grid::grid.raster(logo, x = 0.95, y = 0.02,
                    just = c('right', 'bottom'),
                    width = unit(0.3 * 1.66, 'inches'), height = unit(0.3, 'inches'))
  dev.off()
}
