#######################################

# LAPOP Histogram #

#######################################

lapop_sb <- function(data, outcome_var = data$prop, prop_labels = data$proplabel,
                       var_labels = data$varlabel, value_labels = data$vallabel, 
                       lang = "en",
                       main_title = "",
                       subtitle = "",
                       source_info = "",
                     rev_values = FALSE,
                     rev_variables = FALSE,
                     subtitle_h_just = 0, 
                     color_scheme = c("#2D708E", "#1F9689", "#00ADA9", "#21A356", "#568424", "#ACB014")){
  # color_scheme = c("#2D708E", "#1F9689", "#00ADA9", "#21A356", "#568424", "#ACB014")
  mycolors = rev(color_scheme[1:length(unique(value_labels))])
  if(rev_values == TRUE){
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
    labs(title = main_title,
         y = "",
         x = " ",
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas ", "Source: AmericasBarometer "),
                          source_info),
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
          legend.margin = margin(t=5,b=5, 0, subtitle_h_just))
}

