#######################################

# LAPOP Regression Graphs #

#######################################

#first, change these to wherever the lapop logo and your .csv data files are located
logoloc <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\3_Media\Logos\lapop\lapop-full-color-00ada9.svg)")  # logo location and file name
csvdir <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\Regression Graph\Data Files)") # path with csv files

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
library(plyr) # for data manipulation (round_any() function)
library(ggtext) # for adding markdown in labels

logo <- image_read(logoloc)



#run these lines to be load in nunito and roboto fonts
font_add_google("nunito", "nunito")
font_add_google("roboto", "roboto")
font_add_google("roboto", family = "roboto-light", regular.wt = 300)
font_add_google("nunito", family = "nunito-light", regular.wt = 300)

showtext_auto()

#read in .csv data produced by Stata
df_reg <- read.csv("regression_example_data.csv")
# data should be in a csv, with five columns: 
# varlabel (character) - name of the variable 
# coef (numeric) - proportion in each category, within one variable
# proplabel (character) - the estimates as characters, with a percentage symbol
# lb (numeric) - 95% CI lower bound
# ub (numeric) - 95% CI upper bound

df_reg$varlabel <- df_reg[, 1]
df_reg$varlabel[5] <- "Intimate \n Partner"

#"#512B71" (purple, for Insights)
#  "#2D708E" (blue, for other reports)
lapop_coef <- function(data, 
                       lang = "en",
                       main.title = "",
                       subtitle = "",
                       source.info = "",
                       ymin = NULL,
                       ymax = NULL,
                       pred.prob = FALSE){
  data$varlabel = factor(data$varlabel, levels = rev(unique(data$varlabel)))
  data$sig = ifelse(data$pvalue < 0.05, FALSE, TRUE)
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:#512B71; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:#512B71; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'>interval</span>"))
  ggplot(data, aes(x = varlabel, y = coef)) +
    geom_hline(yintercept = 0, color = gray(1/2), lty = 2) +
    geom_errorbar(aes(x=varlabel, ymin = lb, ymax = ub), width = 0.3, lty = 1, color = "#512B71") + 
    geom_point(aes(x = varlabel, y = coef, fill = sig), color = "black", size = 5.5, shape = 21) + 
    geom_text(aes(label = proplabel, vjust = -1.25), size = 5, color = "#512B71", fontface = "bold") + 
    scale_fill_manual(values = "#512B71",
                      labels = paste0(" <span style='color:#545454; font-size:13pt'> ",
                                      ifelse(lang == "es", ifelse(pred.prob == TRUE, "Probabilidades pronosticadas", "Coeficientes de regresión"),
                                             ifelse(pred.prob == TRUE, "Predicted probabilities", "Regression coefficients")),
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text),
                      limits = "FALSE",
                      na.value = "white") +
    coord_flip() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title = main.title,
         y = " ",
         x = " ",
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas", "Source: AmericasBarometer"),
                          source.info)) +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 18, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "roboto-light", color="#545454"),
          plot.subtitle = element_text(size = 14, family = "nunito-light", color="#545454"),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 14, family = "roboto", color = "#545454"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification='left',
          # legend.margin = margin(t=0, b=0),
          legend.text = element_markdown(family = "nunito-light"),
          legend.key=element_blank(),
          legend.margin=margin(0, 0, 0, -30))
  }
  
myplot <- lapop_coef(df_reg,
           main.title = "Demographic and Socioeconomic Predictors of Normalizing IPV",
           subtitle = "Regression Coefficients",
           ymin = -0.3,
           ymax = 0.22,
           pred.prob = FALSE,
           source.info = "GM 2021",
           lang = "en") 
 
myplot

#save file
dev.new()
svg("ABGM2021_IPV-black.svg", width = 750/96, height = 500/96)
myplot
grid::grid.raster(logo, x = 0.95, y = 0.02,
                  just = c('right', 'bottom'),
                  width = unit(0.3 * 1.66, 'inches'), height = unit(0.3, 'inches'))
dev.off()


myplot


#lapop logo is 368x222 pixels = 1.657 ratio (or 0.603)
