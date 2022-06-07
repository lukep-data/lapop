#######################################

# LAPOP Time-Series Graphs #

#######################################

#first, change these to wherever the lapop logo and your .csv data files are located
logoloc <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\lapop-full-color-00ada9.png)")  # logo location and file name
csvdir <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\Time Series Graphs\Data Files)") # path with csv files

setwd(csvdir)

#load packages.  
#On first run, you need to install the packages (remove # and run code below)
# install.packages("ggplot2")
# install.packages("plyr")
# install.packages("showtext")
# install.packages("pracma")
library(ggplot2) # for graphics
library(plyr) # for data manipulation (round_any function)
library(showtext) # for adding custom texts
library(pracma) # for interpolation of missing data
library(magick) # for reading in lapop logo 

logo <- image_read(logoloc)

#run these lines to load in nunito and roboto fonts
font_add_google("nunito", "nunito")
font_add_google("roboto", "roboto")
font_add_google("roboto", family = "roboto-light", regular.wt = 300)

showtext_auto()

#read in data
df_full <- read.csv("ts_example_data.csv", fileEncoding = "UTF-8-BOM")
# data should be in a csv, with five columns: 
# wave (character) 
# prop (numeric)
# proplabel (character) - the estimates as characters, with a percentage symbol
# lower bound  of estimate (numeric)
# upper bound of estimate (numeric)

lapop_ts <- function(data, outcome_var = data$prop, lower_bound = data$lb, upper_bound = data$ub,
                     wave_var = data$wave, label_var = data$proplabel, point_var = data$prop, 
                     ymin = round_any(min(outcome_var)-5, 5, f = floor),
                     ymax = round_any(max(outcome_var)+5, 5, f = ceiling),
                     main.title = "",
                     source.info = "",
                     subtitle = "",
                     lang = "en"){
  #the following lines detect if there's missing waves in the middle of a time series
  #if so, we need to do some interpolation so the missing waves are still plotted on the x-axis (without data)
  allwaves = c("2006", "2008", "2010", "2012", "2014", "2016/17", "2018/19", "2021")
  waves_test = allwaves[which(min(data$wave) == allwaves):which(max(data$wave) == allwaves)]
  if(length(data$wave) != length(waves_test)) {
    data = merge(data, data.frame(waves_test), by.x="wave", by.y = "waves_test", all.x = TRUE, all.y = TRUE)
    data$outcome_var = with(data, interp1(seq_along(wave_var), outcome_var, seq_along(wave_var), "linear"))
    data$lower_bound = with(data, interp1(seq_along(wave_var), lower_bound, seq_along(wave_var), "linear"))
    data$upper_bound = with(data, interp1(seq_along(wave_var), upper_bound, seq_along(wave_var), "linear"))
    data = data[!rowSums(!is.na(data)) <= 1, ]
    outcome_var = data$outcome_var
    lower_bound = data$lower_bound
    upper_bound = data$upper_bound
  }
  #now we stop dealing with missing data
  # ci_text = ifelse(lang == "es", "intervalo de confianza", "confidence interval")
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:#3CBC70; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:#3CBC70; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'>interval</span>"))
  #and turn to creating the graph
  ggplot(data=data, aes(x=wave_var, y=outcome_var)) + 
    geom_line(aes(group = 1), color="#3CBC70", size = 1, alpha=0.48) +
    geom_line(aes(group = 1, y =lower_bound), color="#3CBC70", size = 1, alpha=0.48, lty="dashed") +
    geom_line(aes(group = 1, y= upper_bound), color="#3CBC70", size = 1, alpha=0.48, lty="dashed") +
    geom_point(aes(y = point_var, color = " "), size = 3.5, alpha=0.48, key_glyph = "point") +
    scale_color_manual(values = "#3CBC70",
                       labels = paste0("<span style='color:#545454; font-size:13pt'> ",
                                       subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text)) +
    
    geom_text(aes(label=label_var, fontface= "bold"), color="#3CBC70",  size = 5, vjust = -2.1) +
    scale_x_discrete(limits = wave_var) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 10), labels = paste(seq(ymin,ymax,10), "%", sep=""), expand = c(0,0)) +
    labs(title = main.title,
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas ", "Source: AmericasBarometer "),
                          source.info),
         x = " ",
         y = " ") +
    theme_minimal() +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 18, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0.02, family = "roboto-light", color="#545454"),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14, color = "#545454"),
          panel.grid = element_line(color = "#D1D3D4"),
          panel.border = element_rect(linetype = "solid", color = "#D1D3D4", fill = NA),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='left',
          legend.margin = margin(t=0, b=0),
          legend.text=element_markdown(family = "nunito-light"))
} 

## now, you can create graphs with custom options using the above function
# basic graph
# lapop_ts(df_full)

#graph with custom options
fig1 <- lapop_ts(df_full,
         main.title = "Ecuadorians are becoming more interested in politics",
         subtitle = "% politically interested",
         source.info = "Ecuador, 2006-2021",
         ymin = 0,
         ymax = 55,
         lang = "en") 
fig1

# grid::grid.raster(logo, x = 0.97, y = 0.02, just = c('right', 'bottom'), width = unit(0.55, 'inches'))


#missing data at beginning/end of series
df_mis_end <- read.csv("ts_example_data-miss-end.csv", fileEncoding = "UTF-8-BOM")

fig2 <- lapop_ts(df_mis_end,
                 main.title = "Ecuadorians are becoming more interested in politics",
                 subtitle = "% politically interested",
                 source.info = "Ecuador 2006-2021",
                 ymin = 0,
                 ymax = 55,
                 lang = "es")

fig2


#missing data in middle of series
df_mis_mid <- read.csv("ts_example_data-miss-midend.csv", fileEncoding = "UTF-8-BOM")


fig3 <- lapop_ts(df_mis_mid, 
                 main.title = "Ecuadorians are becoming more interested in politics",
                 subtitle = "% politically interested",
                 source.info = "Ecuador 2006-2021",
                 ymin = 0,
                 ymax = 55,
                 lang = "en")

fig3

#save file
dev.new()
png("ABECU2019_POL1.png", width = 750, height = 500)
fig3
grid::grid.raster(logo, x = 0.97, y = 0.02, 
                  just = c('right', 'bottom'), 
                  width = unit(0.5, 'inches'), height = unit(0.5 * 2/3, 'inches'))
dev.off()

#lapop logo is 368x222 pixels = 1.657 ratio (or 0.603)






