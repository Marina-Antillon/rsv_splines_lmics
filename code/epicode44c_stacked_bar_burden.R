## creating stacked bars of burden for 3-month age groups. Makes figures 5 and Fig E of supplement 3.

prct_90d_5 = (apply(burden[0:31,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "0-<3 months")
prct_90_180d_5 = (apply(burden[32:61,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "3-<6 months")
prct_120d_5 = (apply(burden[0:41,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>%
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>%
  pivot_wider(names_from=Var1, values_from = value) %>%
  mutate(measure = "0-<4 months")
prct_150d_5 = (apply(burden[0:51,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>%
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>%
  pivot_wider(names_from=Var1, values_from = value) %>%
  mutate(measure = "0-<5 months")
prct_180d_5 = (apply(burden[0:61,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>%
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>%
  pivot_wider(names_from=Var1, values_from = value) %>%
  mutate(measure = "0-<6 months")

prct_180_270d_5 = (apply(burden[62:91,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "6-<9 months")
prct_270_365d_5 = (apply(burden[92:121,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "9-<12 months")

prct_180_365d_5 = (apply(burden[62:121,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "6-<12 months")
prct_1_5 = (apply(burden[122:600,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "12-59 months")

prods_u5 = bind_rows(prct_90d_5, prct_90_180d_5, prct_180_270d_5, prct_270_365d_5, prct_1_5)
prods_u5$measure = factor(prods_u5$measure, levels = c("0-<3 months", "3-<6 months", "6-<9 months", "9-<12 months", "12-59 months"))
prods_u5$outcome = factor(prods_u5$outcome, levels = c("disease", "hosp", "deaths"))
levels(prods_u5$outcome) = c("Cases", "Hospitalizations", "In-hospital deaths")
prods_u5$model = factor(prods_u5$model, levels=c("hosp_inc", "com_inc"))
levels(prods_u5$model) = c("Outcome model (OM) II", "Outcome model (OM) I")
prods_u5$lbl = apply(prods_u5[,c("est", "lci", "hci")], 1, ci_string_dec, 2)
prods_u5$textpos = 0.5*(prods_u5$lci + prods_u5$hci)
prods_u5$textpos[prods_u5$textpos<0.2] = 0.2

p = ggplot(data=prods_u5, aes(x=outcome, y=est, colour=model)) +
  themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white")) + 
  geom_point(position = position_dodge(width = 1), size=1) + 
  geom_errorbar(aes(ymin=lci, ymax=hci), width=0.2, position = position_dodge(width = 1)) + 
  facet_grid(region~measure) + 
  geom_text(aes(y=textpos, label=lbl), position = position_dodge(width = 1), 
            vjust=-0.65, hjust=0.5, size=2.8, show.legend = FALSE, fontface="bold") +
  scale_colour_manual(values=c("darkorange2", "forestgreen")) + 
  xlab(NULL) + ylab("Age in months") + coord_flip(clip="off") + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0,1)) + 
  scale_x_discrete(limits = rev(levels(prods_u5$outcome))) +
  guides(colour=guide_legend(title = "", title.position="left", reverse=T)) 

ggsave(paste0(plotpre_fig, "percent_covered_products.eps"), 
       plot = grid.draw(p), device="eps", width=14, height=7.5, units="in")

prods_u5 = bind_rows(prct_90d_5, prct_90_180d_5, prct_180_270d_5, prct_270_365d_5, prct_1_5)
prods_u5$measure = factor(prods_u5$measure, levels = c("0-<3 months", "3-<6 months", "6-<9 months", "9-<12 months", "12-59 months"))
prods_u5$outcome = factor(prods_u5$outcome, levels = c("disease", "hosp", "deaths"))
levels(prods_u5$outcome) = c("Cases", "Hospitalizations", "In-hospital deaths")
prods_u5$model = factor(prods_u5$model, levels=c("com_inc", "hosp_inc"))
levels(prods_u5$model) = c("Outcome model (OM) I", "Outcome model (OM) II")
prods_u5$lbl = paste0(round(prods_u5$est*100, 0), "%")
# prods_u5$textpos = 0.5*(prods_u5$lci + prods_u5$hci)
# prods_u5$textpos[prods_u5$textpos<0.2] = 0.2

p=ggplot(data=prods_u5, aes(x=factor(outcome), y=est, fill=measure)) +
  themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE), alpha=0.7) + 
  facet_grid(region~model) + 
  scale_fill_manual(values=c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#35978f")) + 
  geom_text(aes(label = lbl), size = 2.5, position = position_stack(reverse = TRUE, vjust = 0.5), fontface="bold") + 
  xlab(NULL) + ylab("Proportion of cases <5 in each age bracket") + coord_flip(clip="off") + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0,1.01)) + 
  scale_x_discrete(limits = rev(levels(prods_u5$outcome))) +
  guides(fill=guide_legend(title = "", title.position="left", reverse=F))

ggsave(paste0(plotpre_fig, "percent_covered_products_bar.pdf"), 
       plot = grid.draw(p), device="pdf", width=7.5, height=5.5, units="in")

load(paste0(plotpre_out, "epicode44b_time_to_perc_cases/burden_sev_results2.Rdata"))

prct_90d_5 = (apply(burden[0:31,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "0-<3 months")
prct_90_180d_5 = (apply(burden[32:61,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "3-<6 months")
prct_180_270d_5 = (apply(burden[62:91,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "6-<9 months")
prct_270_365d_5 = (apply(burden[92:121,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "9-<12 months")
prct_180_365d_5 = (apply(burden[62:121,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "6-<12 months")
prct_1_5 = (apply(burden[122:600,,,,], 2:5, "sum")/apply(burden[,,,,], 2:5, "sum")) %>% 
  apply(2:4, meanpi) %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=Var1, values_from = value) %>% 
  mutate(measure = "12-59 months")

prods_u5 = bind_rows(prct_90d_5, prct_90_180d_5, prct_180_270d_5, prct_270_365d_5, prct_1_5)
prods_u5$measure = factor(prods_u5$measure, levels = c("0-<3 months", "3-<6 months", "6-<9 months", "9-<12 months", "12-59 months"))
prods_u5$outcome = factor(prods_u5$outcome, levels = c("comm_sev", "comm_vsev", "hosp_sev", "hosp_vsev"))
levels(prods_u5$outcome) = c("Community severe", "Community very severe", "Hospital severe", "Hospital very severe")
prods_u5$model = factor(prods_u5$model, levels=c("com_inc", "hosp_inc"))
levels(prods_u5$model) = c("Outcome model (OM) I", "Outcome model (OM) II")
prods_u5$lbl = paste0(round(prods_u5$est*100, 0), "%")
prods_u5$textpos = 0.5*(prods_u5$lci + prods_u5$hci)
prods_u5$textpos[prods_u5$textpos<0.2] = 0.2

p = ggplot(data=prods_u5, aes(x=outcome, y=est, colour=model)) +
  themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white")) + 
  geom_point(position = position_dodge(width = 1), size=1) + 
  geom_errorbar(aes(ymin=lci, ymax=hci), width=0.2, position = position_dodge(width = 1)) + 
  facet_grid(region~measure) + 
  geom_text(aes(y=textpos, label=lbl), position = position_dodge(width = 1), 
            vjust=-0.65, hjust=0.5, size=2.8, show.legend = FALSE, fontface="bold") +
  scale_colour_manual(values=c("darkorange2", "forestgreen")) + 
  xlab(NULL) + ylab("Age in months") + coord_flip(clip="off") + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0,1)) + 
  scale_x_discrete(limits = rev(levels(prods_u5$outcome))) +
  guides(colour=guide_legend(title = "", title.position="left", reverse=T)) 

ggsave(paste0(plotpre_fig, "percent_covered_products_SEV.eps"), 
       plot = grid.draw(p), device="eps", width=14, height=7.5, units="in")

p=ggplot(data=prods_u5, aes(x=factor(outcome), y=est, fill=measure)) +
  themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE), alpha=0.7) + 
  facet_grid(region~model) + 
  scale_fill_manual(values=c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#35978f")) + 
  geom_text(aes(label = lbl), size = 2.5, position = position_stack(reverse = TRUE, vjust = 0.5), fontface="bold") + 
  xlab(NULL) + ylab("Proportion of cases <5 in each age bracket") + coord_flip(clip="off") + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0,1)) + 
  scale_x_discrete(limits = rev(levels(prods_u5$outcome))) +
  guides(fill=guide_legend(title = "", title.position="left", reverse=F))

ggsave(paste0(plotpre_fig, "percent_covered_products_SEV_bar.pdf"), 
       plot = grid.draw(p), device="pdf", width=7.5, height=6.5, units="in")
