#Plots validation (48 hours)

ggplot(ee, aes(x, V1)) + 
    geom_segment(aes(xend=x,yend=upper_rainP,colour = "Observed rainfall"),size = 4, lineend = "butt") +
    geom_ribbon(aes(ymin=top5min_forplot[range]*q_scalefact, ymax=top5max_forplot[range]*q_scalefact,
                    x=range, fill = "BehavMemb"), alpha = 0.4) +
    geom_line(aes(x=range, y=disch_med_forplot_valid[range]*q_scalefact, colour = "Sim median"),linetype="dashed", size=0.6) +
    geom_line(aes(x=range, y=disch_obs_forplot*q_scalefact, colour = "Observed disch"),size=0.9) +
    scale_y_continuous(sec.axis = sec_axis(~., name = "Rainfall [mm/h]",breaks = breakz_y,labels=laby2),
                       breaks = breakz_y) + 
    scale_x_continuous(labels=tage2, breaks = breakz_x) + 
    labs(x = "Simulation time", y=expression(paste("Discharge [x 10"^" -1", " mm/h]")), title="Rainfall-runoff", subtitle=subkopft, 
         caption=captionfig, colour="Timeseries", fill="Simulated discharge", linetype="Timeseries") +
    scale_color_manual(values=c("#B46D6D", "#6DC7CB", "#626262")) +  #obs disch, rainfall, sim disch median
    scale_fill_manual(values=c("#4A4A4A")) +   # top5 
    theme(plot.title = element_text(size=25),
          plot.subtitle = element_text(size=16),
          axis.title.x = element_text(size=20),
          axis.text.x  = element_text(size=15,angle=345, vjust=1, hjust=0),
          axis.title.y = element_text(size=20),    # title
          axis.text.y  = element_text(size=19),     # numbers
          legend.title = element_text(size=19),
          legend.text = element_text(size=20)) +
    guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)) +
    ggsave(dateiname, width = 10, height = 6.12, dpi=300)