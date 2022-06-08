#######################################

# LAPOP Regression Graphs #

#######################################


lapop_coef <- function(data, coef_var = data$coef, label_var = data$proplabel,
                       varlabel_var = data$varlabel, lb = data$lb, ub = data$ub, 
                       pval_var = data$pvalue, 
                       lang = "en",
                       main_title = "",
                       subtitle = "",
                       source.info = "",
                       ymin = NULL,
                       ymax = NULL,
                       pred_prob = FALSE,
                       color_scheme = "#512B71",
                       subtitle.h.just = 0){
  varlabel_var = factor(varlabel_var, levels = rev(unique(varlabel_var)))
  sig = ifelse(pval_var < 0.05, FALSE, TRUE)
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#545454; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#545454'>interval</span>"))
  
  ggplot(data, aes(x = varlabel_var, y = coef_var)) +
    geom_hline(yintercept = 0, color = gray(1/2), lty = 2) +
    geom_errorbar(aes(x=varlabel_var, ymin = lb, ymax = ub), width = 0.3, lty = 1, color = color_scheme) + 
    geom_point(aes(x = varlabel_var, y = coef_var, fill = sig), color = "black", size = 5.5, shape = 21) + 
    geom_text(aes(label = label_var, vjust = -1.25), size = 5, color = color_scheme, fontface = "bold") + 
    scale_fill_manual(values = color_scheme,
                      labels = paste0(" <span style='color:#545454; font-size:13pt'> ",
                                      ifelse(lang == "es", ifelse(pred_prob == TRUE, "Probabilidades pronosticadas", "Coeficientes de regresión"),
                                             ifelse(pred_prob == TRUE, "Predicted probabilities", "Regression coefficients")),
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text),
                      limits = "FALSE",
                      na.value = "white") +
    coord_flip() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title = main_title,
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
          legend.margin=margin(0, 0, 0, -30-subtitle.h.just))
  }
  

