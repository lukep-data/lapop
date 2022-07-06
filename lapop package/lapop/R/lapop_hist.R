#######################################

# LAPOP Histograms Graphs #

#######################################


lapop_hist <- function(data, outcome_var = data$prop, label_var = data$proplabel,
                       cat_var = data$cat,
                       ymin = 0,
                       ymax = round_any(max(outcome_var)+5, 5, f = ceiling),
                       lang = "en",
                       main_title = "",
                       subtitle = "",
                       source_info = "",
                       order = FALSE,
                       color_scheme = "#1F968B"){
  if(order == TRUE){
    data = data[order(-data$prop), ]
    cat_var = cat_var[order(-outcome_var)]
    label_var = label_var[order(-outcome_var)]
    outcome_var = outcome_var[order(-outcome_var)]
  }
  ggplot(data, aes(x=factor(cat_var, levels = cat_var), y = outcome_var)) +
    geom_bar(stat = "identity", color = color_scheme, fill = paste0(color_scheme, "28"), width = 0.75) +
    geom_text(aes(label=label_var), vjust=-0.5, size = 5, fontface = "bold", color = color_scheme) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
    labs(title=main_title,
         y = "",
         x = "",
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas ", "Source: AmericasBarometer "),
                          source_info),
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
