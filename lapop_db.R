#######################################

# LAPOP Dumbbell Graphs #

#######################################

lapop_db <- function(data, 
                      ymin = 0,
                      ymax = 100,
                      lang = "en",
                      highlight = "", 
                      main_title = "",
                      source_info = "",
                      subtitle = "",
                     sort = "wave2", 
                      order = "hi-lo",
                      color_scheme = c("#3CBC70", "#482677"),
                      subtitle_h_just = 40,
                     subtitle_v_just = -18){
  if(sort == "diff"){
    data$diff = data$prop2-data$prop1
    data = data[order(-data$diff),]
  } else if(sort == "wave2"){
    data = data[order(-data$prop2),]
  } else if(sort == "wave1"){
    data = data[order(-data$prop1),]
  } else if(sort == "alpha"){
    data = data[order(data$pais),]
  }
  if(order == "hi-lo"){ 
    data$pais = factor(data$pais, levels = rev(unique(data$pais)))
  } else if(order == "lo-hi") {
    data$pais = factor(data$pais, levels = unique(data$pais))
  }
  data$max1 = data$prop2 < data$prop1
  data$max1[is.na(data$max1)] <- FALSE
  names(color_scheme) = c(unique(data$wave1), unique(data$wave2))
    ggplot(data, aes(y=pais)) +
      geom_point(aes(x = prop1, color = names(color_scheme)[1]), size=4) +
      geom_point(aes(x = prop2, color = names(color_scheme)[2]), size=4) +
      geom_text(data = data[data$max1 == FALSE, ], 
                aes(x = prop1, label = proplabel1), 
                nudge_x = -6, color = color_scheme[1], size = 5, fontface = "bold") +
      geom_text(data = data[data$max1 == TRUE, ], 
                aes(x = prop1, label = proplabel1), 
                nudge_x = 6, color = color_scheme[1], size = 5, fontface = "bold") +
      geom_text(data = data[data$max1 == TRUE, ], 
                aes(x = prop2, label = proplabel2), 
                nudge_x = -6, color = color_scheme[2], size = 5, fontface = "bold") +
      geom_text(data = data[data$max1 == FALSE, ], 
                aes(x = prop2, label = proplabel2), 
                nudge_x = 6, color = color_scheme[2], size = 5, fontface = "bold") +
      geom_vline(xintercept = 0, color = "#D1D3D4") +
      scale_x_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 20), labels = paste(seq(ymin,ymax, 20), "%", sep=""), expand = c(0,0)) +
      scale_color_manual(values = color_scheme) +
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
            plot.margin = margin(5.5, 20, 5.5, 5.5, "points"),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 12, family = "roboto", color = "#545454", margin=margin(r=5)),
            panel.grid.major.y = element_line(color = "#D1D3D480"),
            panel.background = element_rect(fill = "white"),
            legend.position="top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.text = element_text(family = "roboto", color = "#545454"),
            legend.title = element_blank(),
            legend.justification='right',
            legend.key.size = unit(1, "line"),
            legend.key = element_blank(),
            legend.margin = margin(t=subtitle_v_just,b=0, subtitle_h_just, 0))
}
