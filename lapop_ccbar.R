#######################################

# LAPOP Cross-Country Bar Graphs #

#######################################

# run this once to create the function for cross-country graphs
lapop_cc <- function(data, outcome_var = data$prop, lower_bound = data$lb, pais = data$pais, 
                     upper_bound = data$ub, label_var = data$proplabel,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     highlight = "", 
                     main_title = "",
                     source_info = "",
                     subtitle = "",
                     sort = "",
                     color_scheme = "#512B71"){
  if(highlight != ""){
    data$hl_var = factor(ifelse(pais == highlight, 0, 1), labels = c("hl", "other"))
    fill_values = c(paste0(color_scheme, "47"), paste0(color_scheme, "20"))
  }
  else{
    data$hl_var = factor("other")
    fill_values = paste0(color_scheme, "47")
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
    geom_bar(stat="identity", color = color_scheme, width = 0.6) +
    geom_text(aes(label=label_var), vjust=-1.5, size=5, fontface = "bold", color = color_scheme) +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.15, color = color_scheme, linetype = "solid") +
    scale_fill_manual(breaks = "other",
                      values = fill_values,
                      labels = paste0(" <span style='color:#545454; font-size:13pt'> ",
                                      subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text),
                      na.value = paste0(color_scheme, "90")) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title=main_title,
         y = "",
         x = "",
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas", "Source: AmericasBarometer"),
                          source_info)) +
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

