#################################
# Burden figures
################################# 

###############################
# Big panels:
## All regions, with each age and all ages. (hospsens 2 only, hospsens 3 for internal record)
## Then by region (hospsens 2 for supplement, hospsens 3 for internal record)
###############################

td_bu3$model = factor(td_bu3$model)
td_bu3$modelb = factor(td_bu3$model, levels = rev(levels(td_bu3$model)))

td_bu3$age = factor(td_bu3$age)
td_bu3$age = factor(td_bu3$age, levels = levels(td_bu3$age)[c(4,1,3,2)])
td_bu3$ageb = factor(td_bu3$age, levels = rev(levels(td_bu3$age)))

  for (i in 1:4){
    p = ggplot(data=td_bu3[td_bu3$region==unique(td_bu3$region)[i],], 
               aes(x=outcome2, y=est, colour=modelb)) + 
      themebar2 + theme(axis.text.x = element_text(color="black", hjust=1, size=8, angle=30),
                        axis.text.y = element_text(face="bold", color="black", size=8, angle=0),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_line(colour="gray90", linetype = "dotted"),
                        strip.background = element_rect(fill = "white"),
                        strip.text.y = element_text(angle = 0)) +
      geom_errorbar(aes(ymin=lci, ymax=hci), width=.15, position = position_dodge(width = 1)) + 
      geom_text(aes(label=lbl), position = position_dodge(width = 1), vjust=-0.65, hjust=0.6, size=2.8, show.legend = FALSE, fontface="bold") +
      facet_grid(age~country_total, scales="free") + coord_flip() +
      scale_colour_manual(values=c("darkorange2", "forestgreen")) + 
      xlab("") + ylab("Cases (log-scale)") + 
      scale_y_log10(limits=c(10, 1.5e9), breaks=10^c(1:8), 
                    labels=c("10", "100", "1,000", "10,000", "100,000", "1M", "10M", "100M")) + 
      guides(colour=guide_legend(title = "", title.position="left", reverse=T)) 
      
    ggsave(paste(plotpre_fig, "epicode45_burden_figures/BigI_totals_byagegroup_", unique(td_bu3$region)[i], ".eps", sep=""), 
           plot = grid.draw(p), device="eps", width=7, height=7.5, units="in")
  }

###############################
# Big panels:
## By region, all ages
## hospsens 2 for main paper, hospsens 1 and 3 for supplement.
###############################

  p = ggplot(data=td_bu3[td_bu3$age=="Ages: All",], 
             aes(x=outcome2, y=est, colour=modelb)) + 
    themebar2 + theme(axis.text.x = element_text(color="black", hjust=1, size=8, angle=30),
                      axis.text.y = element_text(face="bold", color="black", size=8, angle=0),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_line(colour="gray90", linetype = "dotted"),
                      strip.background = element_rect(fill = "white"),
                      strip.text.y = element_text(angle = 0)) +
    geom_errorbar(aes(ymin=lci, ymax=hci), width=.15, position = position_dodge(width = 1)) + 
    geom_text(aes(label=lbl), position = position_dodge(width = 1), vjust=-0.65, hjust=0.6, size=2.8, show.legend = FALSE, fontface="bold") +
    facet_grid(region~country_total, scales="free") + coord_flip() +
    scale_colour_manual(values=c("darkorange2", "forestgreen")) + 
    xlab("") + ylab("Cases (log-scale)") + 
    scale_y_log10(limits=c(10, 1.5e9), breaks=10^c(1:8), labels=c("10", "100", "1,000", "10,000", "100,000", "1M", "10M", "100M")) + 
    guides(colour=guide_legend(title = "", title.position="left", reverse=T)) 
 
  ggsave(paste(plotpre_fig, "epicode45_burden_figures/BigII_all_ages_incomegroup", ".eps", sep=""), 
         plot = grid.draw(p), device="eps", width=7, height=7.5, units="in")


###############################
# Small panels:
## For main body, all ages at once
## and now, include potential sensitivity graphs
###############################

p = ggplot(data=td_bu2, 
           aes(x=region2, y=est, colour=model2)) + 
  geom_point(position = position_dodge(width = 0.75), size=2) + 
  themebar2 + theme(legend.title = element_text(size = 8, face = "bold"),
                    legend.text = element_text(size = 8, lineheight=0.8),
                    axis.text.x = element_text(color="black", hjust=1, size=8, angle=30),
                    axis.text.y = element_text(face="bold", color="black", size=8, angle=0),
                    panel.grid.minor = element_blank(),
                    strip.background = element_rect(fill = "white")) +
  geom_errorbar(aes(ymin=lci, ymax=hci), width=.15, position = position_dodge(width = .75)) + 
  geom_text(aes(y=textpos, label=lbl), position = position_dodge(width = 0.75), vjust=-0.65, hjust=0.5, size=2.6, show.legend = FALSE, fontface="bold") +
  facet_grid(.~outcome, scales="free") + coord_flip(clip="on") +
  scale_colour_manual(values=Set0) + theme(strip.text.y = element_text(angle = 0)) + 
  xlab("") + ylab("") + scale_y_log10(labels=scales::comma) +
  # scale_y_log10(limits=c(1e3, 1e8), breaks=10^c(3:8), labels=c("1,000", "10,000", "100,000", "1M", "10M", "100M")) + 
  guides(colour=guide_legend(title = "", title.position="left", reverse=T, nrow=2, byrow=T)) 

ggsave(paste(plotpre_fig, "epicode45_burden_figures/smallI_allages_incomegroup", ".eps", sep=""), 
       plot = grid.draw(p), device="eps", width=8, height=6, units="in", dpi=300)


###########################
# Small Panels:
## Now with age groups (using td_bu3)
###########################

for (i in 1:4){
p = ggplot(data=td_bu3[td_bu3$region==unique(td_bu3$region)[i],], 
           aes(x=ageb, y=est, colour=model2b)) + 
  geom_point(position = position_dodge(width = 0.75), size=2) + 
  themebar2 + theme(legend.title = element_text(size = 8, face = "bold"),
                    legend.text = element_text(size = 8, lineheight=0.8),
                    axis.text.x = element_text(color="black", hjust=1, size=8, angle=30),
                    axis.text.y = element_text(face="bold", color="black", size=8, angle=0),
                    panel.grid.minor = element_blank(),
                    strip.background = element_rect(fill = "white")) +
  geom_errorbar(aes(ymin=lci, ymax=hci), width=.15, position = position_dodge(width = .75)) + 
  geom_text(aes(y=textpos, label=lbl), position = position_dodge(width = 0.75), vjust=-0.65, hjust=0.5, size=2.6, show.legend = FALSE, fontface="bold") +
  facet_grid(.~outcome, scales="free") + coord_flip(clip="on") +
  scale_colour_manual(values=Set0) + theme(strip.text.y = element_text(angle = 0)) + 
  xlab("") + ylab("") + scale_y_log10(labels=scales::comma) +
  # scale_y_log10(limits=c(1e3, 1e8), breaks=10^c(3:8), labels=c("1,000", "10,000", "100,000", "1M", "10M", "100M")) + 
  guides(colour=guide_legend(title = "", title.position="left", reverse=T, nrow=2, byrow=T)) 

ggsave(paste(plotpre_fig, "epicode45_burden_figures/smallII_byage_", unique(td_bu3$region)[i], ".eps", sep=""), 
       plot = grid.draw(p), device="eps", width=8, height=6, units="in", dpi=300)

}
