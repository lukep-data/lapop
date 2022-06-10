#######################################

# LAPOP Visualization Templates #

#######################################



####### Set directories

# set directory for current project (where data files are stored & where images will be saved)
project_dir <- (r"(C:\Users\plutowl\Desktop\Insights Example)") # path with csv files
setwd(project_dir)

# specify directory where R scripts for viz templates are held
template_dir <- (r"(C:\Users\plutowl\Box\LAPOP Visualizations\lapop-viz\)")

# can manually specify location of logo image file - otherwise use one in the template directory
# logo_dir <- (r"(C:\Users\plutowl\Box\LAPOP Shared\4_Resources\5_Code\R Visualizations\lapop-full-color-00ada9.svg)")  # logo location and file name


####### load packages, fonts, templates (No modifications necessary, just run)

# must run install.packages on first time 
# install.packages(c("ggplot2", "showtext", "magick", "plyr", "ggtext", "pracma"))
library(ggplot2) # for graphics
library(showtext) # for adding custom texts
library(magick) # for adding logo to plot
library(plyr) # for data manipulation (round_any() function)
library(ggtext) # for adding markdown in labels
library(pracma) # for interpolation of missing data


logo <- image_read(paste0(template_dir, "lapop-full-color-00ada9.svg"))

font_add_google("nunito", "nunito")
font_add_google("roboto", "roboto")
font_add_google("roboto", family = "roboto-light", regular.wt = 300)
font_add_google("nunito", family = "nunito-light", regular.wt = 300)
showtext_auto()


source(paste0(template_dir, "lapop_save.R"))
source(paste0(template_dir, "lapop_coefplot.R"))
source(paste0(template_dir, "lapop_ccbar.R"))
source(paste0(template_dir, "lapop_ts.R"))
source(paste0(template_dir, "lapop_stacked.R"))
source(paste0(template_dir, "lapop_hist.R"))


####### Figure types and examples

## Cross-country bar graph
cc_ex <- read.csv("gr1.csv")

lapop_cc(cc_ex)

cc1 <- lapop_cc(cc_ex,
                main_title = "Normalization of Initimate Partner Violence in Seven LAC Countries",
                subtitle = "% who say domestic violence is private matter",
                source_info = ", 2021",
                highlight = "SV",
                # outcome_var = data$prop, lower_bound = data$lb, pais = data$pais, upper_bound = data$ub, label_var = data$proplabel,
                # ymin = 0,
                # ymax = 100,
                # lang = "en", #options: en, es
                # sort = "", #options: hi-lo, lo-hi, alpha
                # color_scheme = "#512B71"
                )

cc1

lapop_save(cc1, "fig1.svg")


## Regression coefficient plot
gr2 <- read.csv("gr2.csv")

lapop_coef(gr2)

# To add break lines on labels, insert \n  
# gr2$varlabel[1] <- "Intimate\nPartner"

# to flip order of data, use this code
# gr2 <- gr2[order(nrow(gr2):1),]

reg1 <- lapop_coef(gr2,
                   main_title = "Demographic and Socioeconomic Predictors of Normalizing IPV",
                   pred_prob = TRUE, # doesn't change data calculation, just subtitle text
                   source_info = ", 2021",
                   ymin = -0.3,
                   ymax = 0.2,
                   # coef_var = data$coef, label_var = data$proplabel,
                   # varlabel_var = data$varlabel, lb = data$lb, ub = data$ub, 
                   # pval_var = data$pvalue, 
                   # lang = "en",
                   # subtitle = "",
                   # color_scheme = "#512B71",
                   # subtitle_h_just = 0, # moves subtitle to left or right (-90 to 90)
                   )

reg1

lapop_save(reg1, "reg1.svg",
           # logo = TRUE,
           # format = "svg", #options: svg, png
           # width_px = 750,
           # height_px = 500
           )



# To translate into Spanish, can either change labels in Stata or:
# manually change labels in the data frame
gr2_es <- gr2
gr2_es$varlabel <- c("Hombre", "Edad", "Educación", "Riqueza", "Compañero/a\níntimo/a")
# Then, translate the title and change add lang = "es" 

reg1_es <- lapop_coef(gr2_es,
                      main_title = "Predictores demográficos y socioeconómicos de normalizar \nla violencia doméstica",
                      pred_prob = TRUE,
                      source_info = ", 2021",
                      ymin = -0.3,
                      ymax = 0.2,
                      lang = "es")


## Stacked Bar Graph 
df_sb <- read.csv("stackedbar_example_data.csv")

# df_sb$varlabel <- df_sb$ï..varlabel

# add breaks in labels with \n
df_sb$varlabel2 <- ifelse(df_sb$varlabel == "Politicians can identify voters", "Politicians can \n identify voters", df_sb$varlabel)
df_sb$varlabel2 <- ifelse(df_sb$varlabel == "The wealthy buy the results", "The wealthy buy \nthe results", df_sb$varlabel2)
df_sb$varlabel2 <- ifelse(df_sb$varlabel == "Votes are counted correctly", "Votes are counted \ncorrectly", df_sb$varlabel2)

fig2 <- lapop_sb(df_sb,
                   main_title = "Trust in key features of the electoral process is low in Latin America",
                   subtitle = "% believing it happens:",
                   var_labels = df_sb$varlabel2,
                   source_info = "2019"
                   # outcome_var = data$prop, prop_labels = data$proplabel,
                   # var_labels = data$varlabel, value_labels = data$vallabel,
                   # lang = "en",
                   # rev.values = FALSE,
                   # rev.variables = FALSE,
                   # subtitle_h_just = 0, 
                   # color_scheme = c("#2D708E", "#1F9689", "#00ADA9", "#21A356", "#568424", "#ACB014")
)

fig2

## Time Series Graph
df_mis_mid <- read.csv("ts_example_data-miss-midend.csv")

# df_mis_mid$wave <- df_mis_mid$ï..wave

fig3 <- lapop_ts(df_mis_mid,
                 main_title = "Ecuadorians are becoming more interested in politics",
                 subtitle = "% politically interested",
                 source_info = "Ecuador 2006-2021",
                 ymin = 0,
                 ymax = 55
                 # lang = "en",
                 # data, outcome_var = data$prop, lower_bound = data$lb, upper_bound = data$ub,
                 # wave_var = data$wave, label_var = data$proplabel, point_var = data$prop, 
                 # color_scheme = "#3CBC70"
                 )

fig3 


## Bar graph

#read in .csv data produced by Stata
df_hist <- read.csv("hist_example_data.csv")

# df_hist$cat <- df_hist$ï..cat

myplot <- lapop_hist(df_hist,
                     main_table = "Centrists are a plurality among Peruvians",
                     subtitle = "Distribution of ideological preferences",
                     source_info = "Peru, 2019",
                     ymax = 27,
                     # outcome_var = data$prop, label_var = data$proplabel,
                     # cat_var = data$cat, 
                     # ymin = 0,
                     # lang = "en",
                     # order = FALSE,
                     # color_scheme = "#1F968B"
                     )


## Still to come: barbell plot, demographic breakdown
