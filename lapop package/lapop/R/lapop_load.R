#######################################

# LAPOP Visualization Templates #

#######################################

####### load packages, fonts, templates (No modifications necessary, just run)

# must run install.packages on first time
library(ggplot2) # for graphics
library(showtext) # for adding custom texts
library(magick) # for adding logo to plot
library(plyr) # for data manipulation (round_any() function)
library(ggtext) # for adding markdown in labels
library(pracma) # for interpolation of missing data
library(ggplotify) # for as.ggplot function
library(roxygen2)
# library(dplyr)

# logo <- image_read(paste0(template_dir, "lapop-full-color-00ada9.svg"))

font_add_google("nunito", "nunito")
font_add_google("roboto", "roboto")
font_add_google("roboto", family = "roboto-light", regular.wt = 300)
font_add_google("nunito", family = "nunito-light", regular.wt = 300)
showtext_auto()


