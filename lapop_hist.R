#######################################

# LAPOP Histograms Graphs #

#######################################

#first, change these to wherever the lapop logo and your .csv data files are located
logoloc <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\lapop-full-color-00ada9.png)")  # logo location and file name
csvdir <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\Histogram\Data Files)") # path with csv files

setwd(csvdir)

#load packages
#On first run, you need to install the packages (remove # and run code below)
# install.packages("ggplot2")
# install.packages("showtext")
# install.packages("magick")
# install.packages("ggtext")
# install.packages("plyr")
library(ggplot2) # for graphics
library(showtext) # for adding custom texts
library(magick) # for adding logo to plot 
library(plyr) # for data manipulation (round_any function)
# library(ggtext) # for adding markdown in labels

logo <- image_read(logoloc)

#run these lines to be load in nunito and roboto fonts
font_add_google("nunito", "nunito")
font_add_google("roboto", "roboto")
font_add_google("roboto", family = "roboto-light", regular.wt = 300)
font_add_google("nunito", family = "nunito-light", regular.wt = 300)

showtext_auto()

#read in .csv data produced by Stata
df_hist <- read.csv("hist_example_data.csv", fileEncoding = "UTF-8-BOM")
# data should be in a csv, with five columns: 
# cat (character) 
# prop (numeric)
# proplabel (character) - the estimates as characters, with a percentage symbol


df_hist


lapop_hist <- function(data, outcome_var = data$prop, label_var = data$proplabel,
                       cat_var = data$cat, 
                       ymin = 0,
                       ymax = round_any(max(outcome_var)+5, 5, f = ceiling),
                       lang = "en",
                       main.title = "",
                       subtitle = "",
                       source.info = "",
                       order = ""){
  if(order == TRUE){
    data = data[order(-data$prop), ]
  }
  ggplot(data, aes(x=factor(cat_var, levels = cat), y = outcome_var)) +
    geom_bar(stat = "identity", color = "#1F968B", fill = "#1F968B28", width = 0.75) +
    geom_text(aes(label=label_var), vjust=-0.5, size = 5, fontface = "bold", color = "#009A97") +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
    labs(title=main.title,
         y = "",
         x = "",
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas ", "Source: AmericasBarometer "),
                          source.info),
         subtitle = subtitle) +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 18, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "roboto-light", color="#545454"),
          plot.subtitle = element_text(size = 13, family = "nunito-light", color="#545454"), 
          axis.title.y = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          # axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 14, family = "roboto-light", color = "#545454"),
          panel.grid = element_line(color = "#D1D3D4"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank())
  }

myplot <- lapop_hist(df_hist,
           main.title = "Centrists are a plurality among Peruvians",
           subtitle = "Distribution of ideological preferences",
           ymax = 27,
           source.info = "Peru, 2019",
           lang = "en")

myplot

#save file
dev.new()
svg("ABPER2019_L1.png", width = 750/96, height = 450/96)
myplot
grid::grid.raster(logo, x = 0.97, y = 0.02, 
                  just = c('right', 'bottom'), 
                  width = unit(0.4, 'inches'), height = unit(0.4 * 450/750, 'inches'))
dev.off()

#lapop logo is 368x222 pixels = 1.657 ratio (or 0.603)
