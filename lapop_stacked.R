#######################################

# LAPOP Histograms Graphs #

#######################################

#first, change these to wherever the lapop logo and your .csv data files are located
logoloc <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\lapop-full-color-00ada9.png)")  # logo location and file name
csvdir <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\Stacked Bar\Data Files)") # path with csv files

setwd(csvdir)

#load packages
#On first run, you need to install the packages (remove # and run code below)
# install.packages("ggplot2")
# install.packages("showtext")
# install.packages("magick")
# install.packages("ggtext")
library(ggplot2) # for graphics
library(showtext) # for adding custom texts
library(magick) # for adding logo to plot 
# library(ggtext) # for adding markdown in labels

logo <- image_read(logoloc)

#run these lines to be load in nunito and roboto fonts
font_add_google("nunito", "nunito")
font_add_google("roboto", "roboto")
font_add_google("roboto", family = "roboto-light", regular.wt = 300)
font_add_google("nunito", family = "nunito-light", regular.wt = 300)

showtext_auto()

#read in .csv data produced by Stata
df_sb <- read.csv("stackedbar_example_data.csv", fileEncoding = "UTF-8-BOM")
# data should be in a csv, with five columns: 
# var label (character) - name of the variable 
# vallabel (character) - name of each value label 
# prop (numeric) - proportion in each category, within one variable
# proplabel (character) - the estimates as characters, with a percentage symbol


lapop_sb <- function(data, outcome_var = data$prop, prop_labels = data$proplabel,
                       var_labels = data$varlabel, value_labels = data$vallabel, 
                       lang = "en",
                       main.title = "",
                       subtitle = "",
                       source.info = "",
                     rev.values = FALSE,
                     rev.variables = FALSE){
  bar_colors = c("#2D708E", "#1F9689", "#00ADA9", "#21A356", "#568424", "#ACB014")
  mycolors = rev(bar_colors[1:length(unique(value_labels))])
  if(rev.values == TRUE){
    value_labels = factor(value_labels, levels = unique(value_labels))
  } else{
    value_labels = factor(value_labels, levels = rev(unique(value_labels)))
  }
  positions = rev(unique(var_labels))
  ggplot(data, aes(fill = value_labels, y = outcome_var, x = var_labels, label = prop_labels)) + 
    geom_bar(position = "stack", stat = "identity", width = 0.45) + 
    geom_text(position = position_stack(vjust = 0.5), color = "#FFFFFF", fontface = "bold", size = 5) +
    coord_flip() + 
    scale_fill_manual(values = mycolors, guide=guide_legend(reverse = TRUE)) +
    scale_x_discrete(limits = positions, expand = c(0,0)) + 
    labs(title = main.title,
         y = "",
         x = " ",
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas ", "Source: AmericasBarometer "),
                          source.info),
         subtitle = subtitle) +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 17, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "roboto-light", color="#545454"),
          plot.subtitle = element_text(size = 14, family = "nunito-light", color="#545454"),
          # axis.title = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 14, family = "roboto", color = "#545454", margin=margin(r=5)),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.text = element_text(family = "roboto", color = "#545454"),
          legend.title = element_blank(),
          legend.justification='left',
          legend.key.size = unit(1, "line"),
          legend.margin = margin(t=5,b=5, 0, 0))
}

# add breaks in labels with \n
df_sb$varlabel2 <- ifelse(df_sb$varlabel == "Politicians can identify voters", "Politicians can \n identify voters", df_sb$varlabel)
df_sb$varlabel2 <- ifelse(df_sb$varlabel == "The wealthy buy the results", "The wealthy buy \nthe results", df_sb$varlabel2)
df_sb$varlabel2 <- ifelse(df_sb$varlabel == "Votes are counted correctly", "Votes are counted \ncorrectly", df_sb$varlabel2)

myplot <- lapop_sb(df_sb,
                   main.title = "Trust in key features of the electoral process is low in Latin America",
                   subtitle = "% believing it happens:",
                   var_labels = df_sb$varlabel2, 
                   source.info = "2019", 
                   rev.values = FALSE)

myplot

#save file
dev.new()
svg("AB2019_COUNTFAIR.svg", width = 750/96, height = 450/96)
myplot
grid::grid.raster(logo, x = 0.97, y = 0.02, 
                  just = c('right', 'bottom'), 
                  width = unit(0.4, 'inches'), height = unit(0.4 * 450/750, 'inches'))
dev.off()

#lapop logo is 368x222 pixels = 1.657 ratio (or 0.603)
