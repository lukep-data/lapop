#######################################

# LAPOP Cross-Country Bar Graphs #

#######################################

#first, change these to wherever the lapop logo and your .csv data files are located
logoloc <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\lapop-full-color-00ada9.png)")  # logo location and file name
csvdir <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\Cross-Country Histogram\Data Files)") # path with csv files

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
library(ggtext) # for adding markdown in labels

logo <- image_read(logoloc)

#run these lines to be load in nunito and roboto fonts
font_add_google("nunito", "nunito")
font_add_google("roboto", "roboto")
font_add_google("roboto", family = "roboto-light", regular.wt = 300)
font_add_google("nunito", family = "nunito-light", regular.wt = 300)

showtext_auto()

#read in .csv data produced by Stata
df_cc <- read.csv("cc_example_data.csv", fileEncoding = "UTF-8-BOM")
# data should be in a csv, with five columns: 
# pais (character) 
# prop (numeric)
# proplabel (character) - the estimates as characters, with a percentage symbol
# lower bound  of estimate (numeric)
# upper bound of estimate (numeric)



#"#512B71" (purple, for Insights)
#  "#8ABABB" (green-gray, for other reports)

# run this once to create the function for cross-country graphs
lapop_cc <- function(data, outcome_var = data$prop, lower_bound = data$lb, pais = data$pais, 
                     upper_bound = data$ub, label_var = data$proplabel,
                     # horizontal = FALSE,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     highlight = "", 
                     main.title = "",
                     source.info = "",
                     subtitle = "",
                     sort = ""){
  if(highlight != ""){
    data$hl_var = factor(ifelse(pais == highlight, 0, 1), labels = c("hl", "other"))
    fill_values = c("#512B7147", "#512B7120")
  }
  else{
    data$hl_var = factor("other")
    fill_values = "#512B7147"
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$pais),]
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$pais),]
  }
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:#512B71; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:#512B71; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'> interval</span>"))
  ggplot(data=data, aes(x=factor(pais, levels = pais), y=prop, fill = hl_var)) +
    geom_bar(stat="identity", color = "#512B71", width = 0.6) +
    geom_text(aes(label=label_var), vjust=-1.5, size=5, fontface = "bold", color = "#512B71") +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.15, color = "#512B71", linetype = "solid") +
    scale_fill_manual(breaks = "other",
                      values = fill_values,
                      labels = paste0(" <span style='color:#545454; font-size:13pt'> ",
                                      subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text),
                      na.value = "#512B7190") +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title=main.title,
         y = "",
         x = "",
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas", "Source: AmericasBarometer"),
                          source.info)) +
    # {if(horizontal)coord_flip()} + 
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 18, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0.02, family = "roboto-light", color="#545454"),
          panel.background = element_blank(),
          panel.border = element_rect(linetype = "solid", color = "#D1D3D4", fill = NA),
          axis.text = element_text(size = 14, color = "#545454", face = "bold"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='left',
          legend.margin = margin(t=0, b=0),
          legend.text = element_markdown(family = "nunito-light"))
}

#bare bones, default graph
lapop_cc(df_cc)

df_ccr <- df_cc[sample(1:nrow(df_cc)), ]


#graph with custom options, saved as an object called "myplot"
myplot <- lapop_cc(df_ccr,
                   main.title = "In Peru, there is widespread tolerance for shutting down \nCongress when corruption is rampant",
                   subtitle = "% tolerating closure of Congress",
                   highlight = "PE",
                   ymin = 0,
                   ymax = 45,
                   lang = "en",
                   source.info = "2019",
                   sort = "hi-lo")
myplot

#save file
dev.new()
svg("AB2019_JC15.svg", width = 750/96, height = 500/96)
myplot
grid::grid.raster(logo, x = 0.97, y = 0.02, 
                  just = c('right', 'bottom'), 
                  width = unit(0.4, 'inches'), height = unit(0.4 * 2/3, 'inches'))
dev.off()

#lapop logo is 368x222 pixels = 1.657 ratio (or 0.603)
