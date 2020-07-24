#Two-month plots

ggplot(eeP, aes(x, y)) + 
      geom_segment(aes(xend=x,yend=upper_rainP, colour = "Observed rainfall"),size = 0.5, lineend = "butt" ) +
      geom_ribbon(aes(ymin=top5min_forplot[range]*q_scalefact, ymax=top5max_forplot[range]*q_scalefact, x=range, fill = "BehavMemb"), alpha=0.4) +
      geom_line(aes(x=range, y=disch_median_forplot[range]*q_scalefact, colour = "Sim median"),linetype="dashed", size=0.6) +
      geom_line(aes(x=range, y=disch_obs_forplot[range]*q_scalefact, colour = "Observed disch"),size=0.6) +
      scale_y_continuous(sec.axis = sec_axis(~., name = "Rainfall [mm/h]",breaks = breakz_yP,labels=laby2P),
                         breaks = breakz_yP) + 
      scale_x_continuous(breaks=seq(inicial,contador-1, by=intervx),labels=tage) +
      labs(x = "Simulation time [h]", y=expression(paste("Discharge [x 10"^" -1", " mm/h]")), title="Rainfall-runoff", subtitle=subkopft,
           caption=captionfig, colour="Timeseries", fill="Envelopes") +
      scale_color_manual(values=c("#B46D6D", "#6DC7CB", "#626262")) +
      scale_fill_manual(values=c("#4A4A4A")) +   # top5 
      theme(plot.title = element_text(size=25),
            plot.subtitle = element_text(size=16),
            axis.title.x = element_text(size=20),
            axis.text.x  = element_text(size=19,angle=345, vjust=1, hjust=0),
            axis.title.y = element_text(size=20),    # title
            axis.text.y  = element_text(size=19),     # numbers
            legend.title = element_text(size=19),
            legend.text = element_text(size=20)) +
      guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)) +
      ggsave(dateiname, width = 10, height = 6)