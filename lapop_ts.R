#######################################

# LAPOP Time-Series Graphs #

#######################################


lapop_ts <- function(data, outcome_var = data$prop, lower_bound = data$lb, upper_bound = data$ub,
                     wave_var = data$wave, label_var = data$proplabel, point_var = data$prop, 
                     ymin = round_any(min(outcome_var)-5, 5, f = floor),
                     ymax = round_any(max(outcome_var)+5, 5, f = ceiling),
                     main_title = "",
                     source_info = "",
                     subtitle = "",
                     lang = "en",
                     color_scheme = "#3CBC70"){
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
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'>interval</span>"))
  #and turn to creating the graph
  ggplot(data=data, aes(x=wave_var, y=outcome_var)) + 
    geom_line(aes(group = 1), color=color_scheme, size = 1, alpha=0.48) +
    geom_line(aes(group = 1, y =lower_bound), color=color_scheme, size = 1, alpha=0.48, lty="dashed") +
    geom_line(aes(group = 1, y= upper_bound), color=color_scheme, size = 1, alpha=0.48, lty="dashed") +
    geom_point(aes(y = point_var, color = " "), size = 3.5, alpha=0.48, key_glyph = "point") +
    scale_color_manual(values = color_scheme,
                       labels = paste0("<span style='color:#545454; font-size:13pt'> ",
                                       subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text)) +
    
    geom_text(aes(label=label_var, fontface= "bold"), color=color_scheme,  size = 5, vjust = -2.1) +
    scale_x_discrete(limits = wave_var) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 10), labels = paste(seq(ymin,ymax,10), "%", sep=""), expand = c(0,0)) +
    labs(title = main_title,
         caption = paste0(ifelse(lang == "es", "Fuente: Barómetro de las Américas ", "Source: AmericasBarometer "),
                          source_info),
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






