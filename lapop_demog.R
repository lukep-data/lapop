#######################################

# LAPOP Demographic Breakdown Graph #

#######################################


df <- read.csv("C:/Users/plutowl/Box/LAPOP Visualizations/lapop-viz/Example Data Files/demog_example_data.csv")
names(df)[1] <- "varlabel"

lapop_demog <- function(data, 
                        lang = "en",
                        main_title = "",
                        subtitle = "",
                        source_info = "",
                        rev_values = FALSE,
                        rev_variables = FALSE,
                        subtitle_h_just = 0, 
                        ymin = 0,
                        ymax = 100,
                        color_scheme = c("#7030A0", "#00ADA9", "#3CBC70", "#7EA03E", "#568424", "#ACB014")){
  # color_scheme = c("#2D708E", "#1F9689", "#00ADA9", "#21A356", "#568424", "#ACB014")
  data$varlabel = factor(data$varlabel, levels = unique(data$varlabel))
  data$vallabel = factor(data$vallabel, levels = unique(data$vallabel))
  mycolors = color_scheme[1:length(unique(data$varlabel))]
  # names(mycolors) <- letters[1:length(mycolors)]
  # print(mycolors)
  # if(rev_values == TRUE){
  #   value_labels = factor(value_labels, levels = unique(value_labels))
  # } else{
  #   value_labels = factor(value_labels, levels = rev(unique(value_labels)))
  # }
  # positions = rev(unique(var_labels))
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:#545454; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:#545454; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'>interval</span>"))
  p = ggplot(data, aes(x = vallabel, y = prop, color = factor(varlabel), label = proplabel)) + 
    geom_point(alpha=0.47, key_glyph = "point") + 
    facet_grid(cols = vars(varlabel), scales = "free", space = "free") + 
    geom_errorbar(aes(ymin=lb, ymax=ub), width = 0.2, show.legend = FALSE) + 
    geom_text(aes(y = ub), fontface = "bold", size = 5, vjust = -0.8, show.legend = FALSE) +
    scale_color_manual(values = mycolors,
                       labels = paste0("<span style='color:#545454; font-size:13pt'> ",
                                       subtitle,
                                       "<span style='color:#FFFFFF00'>--------</span>",
                                       ci_text),
                       guide = guide_legend(override.aes = list(shape = 16,
                                                                color = c("black", "white", "white", "white"),
                                                                fill = c("gray", "white", "white", "white")))) +
    scale_y_continuous(limits = c(ymin, ymax), 
                       breaks = seq(ymin, ymax, ifelse(ymax - ymin <= 50, 5, 10)), 
                       expand = c(0,0)) +
    geom_vline(xintercept = seq(0.5, length(data$vallabel), by = 1), color="#D1D3D4", size = 0.25) + 
    labs(title = main_title,
         y = "",
         x = " ",
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas ", "Source: AmericasBarometer "),
                          source_info)) +
    theme(text = element_text(size = 14, family = "roboto"),
          plot.title = element_text(size = 17, family = "nunito", face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "roboto-light", color="#545454"),
          plot.subtitle = element_text(size = 14, family = "nunito-light", color="#545454"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=.25, color="#D1D3D4"),
          # panel.border = element_rect(linetype = "solid", color = "#D1D3D4", fill = NA),
          panel.background = element_rect(fill = "white"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 14, family = "roboto", color = "#545454"),
          # legend.position = "none",
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification='left',
          legend.margin=margin(0, 0, -5, -20-subtitle_h_just),
          legend.text=element_markdown(family = "nunito-light"),
          legend.key=element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          strip.text = element_text(size = 14),
          strip.background = element_blank()
    )
  #this code chunk will change the colors of the headers of the facets
  g = ggplot_gtable(ggplot_build(p))
  strips = which(grepl('strip-', g$layout$name))
  for (i in seq_along(strips)) {
    l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    g$grobs[[strips[i]]]$grobs[[1]]$children[[l]]$children[[1]]$gp$col <- mycolors[i]
  }
  as.ggplot(g)
}

fig1 <- lapop_demog(df,
            main_title = "More educated, men, and younger individuals in the LAC region are the\nmost likely to be crime victims",
            subtitle = "% victim of a crime",
            ymin = 0, 
            ymax = 40)


lapop_save(fig1, "C:/Users/plutowl/Box/LAPOP Visualizations/lapop-viz/Example Images/lapop_demogfig1.svg",
           width_px = 800)

