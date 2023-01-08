
# Splines of incidence of all cases, hospitalized cases, and fatal cases.

#############################
## Ribbon plots, models 1 and 2 overlaid on one another
## consider restricting to <2 years of age
#############################

if (!dir.exists(file.path(paste(plotpre_fig, "epicode46_ribbons", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode46_ribbons", sep="")))
}

tmp_dems = cbind(rep(lxt$ALL$pop[1:60]/sum(lxt$ALL$pop[1:60])*pop_all/10, each=10), 
                 rep(lxt$LIC$pop[1:60]/sum(lxt$LIC$pop[1:60])*pop_lic/10, each=10),
                 rep(lxt$LMIC$pop[1:60]/sum(lxt$LMIC$pop[1:60])*pop_lmic/10, each=10),
                 rep(lxt$UMIC$pop[1:60]/sum(lxt$UMIC$pop[1:60])*pop_umic/10, each=10))

burden2 = sweep(burden, c(1,5), 1e3/tmp_dems, "*")

burden_ribbon = apply(burden2, c(1,3:5), "quantile", c(0.5, 0.025, 0.975))
burden_ribbon_df = burden_ribbon %>% cubelyr::as.tbl_cube(met_name="Val") %>% as_tibble %>% 
                    pivot_wider(id_cols = c("age", "outcome", "model", "region"),
                                names_from = Var1, values_from = Val) %>% 
                    filter(region!="All") %>% 
                    dplyr::rename(est = `50%`, lci = `2.5%`, hci = `97.5%`) %>%
                    mutate(outcome = factor(outcome, levels = c("disease", "hosp", "deaths"),
                           labels = c("Cases", "Hospitalizations", "In-hospital deaths"))) %>% 
                    mutate(model = factor(model, levels = c("com_inc", "hosp_inc"),
                                                 labels = c("Outcome Model (OM) I", "Outcome Model (OM) II"))) 
# truncate picture
burden_ribbon_df$lci[burden_ribbon_df$lci<0.01] = 0.01
burden_ribbon_df$hci[burden_ribbon_df$hci<0.01] = 0.01
burden_ribbon_df$est[burden_ribbon_df$est<0.01] = 0.01
burden_ribbon_df$hci[burden_ribbon_df$hci>2000] = 2000

p = ggplot(data=burden_ribbon_df, aes(x=age, ymin=lci, ymax=hci, fill=model)) + 
  geom_ribbon(alpha=0.5, size=0) + 
  themebar2 + theme(axis.text = element_text(size = 8, face = "bold"),
                    strip.text.y = element_text(angle = 0),
                    legend.direction = "horizontal",
                    strip.background = element_rect(fill = "white"),
                    panel.grid.minor =  element_blank()) + 
  facet_grid(region~outcome) + # ncol=4
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_fill_manual(values=c("lightgreen", "tan1")) + 
  guides(fill=guide_legend(title = "", title.position="left")) + 
  scale_y_log10(limits = c(0.01, 2000), breaks=10^(-2:3), labels=format(10^(-2:3), drop0trailing=T)) + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) # + 
# scale_y_continuous(breaks=seq(0, 1000, 200), limits = c(0, 1000)) 

png(filename = paste(plotpre_fig, "epicode46_ribbons/outcome_ribbons.png", sep=""),
    width = 7, height = 7.5, units = 'in', res=500,
    family = "Arial")
grid.newpage()
print(grid.draw(p))
dev.off()
