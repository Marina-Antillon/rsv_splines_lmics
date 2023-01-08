# Base Ribbons, severity outcomes...

# read in sev_inc ----
load(file=paste(plotpre_out, "epicode01b_comm_prob_sev/inc_sev_global_predictions.Rdata", sep=""))

inc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(inc_ribbons_global) = c("est", "lowci", "hici")
inc_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
inc_ribbons_global$mos = seq(0.1, 60, 0.1)
inc_ribbons_global$est[inc_ribbons_global$est<0.001] = 0.001
inc_ribbons_global$mean[inc_ribbons_global$mean<0.001] = 0.001
inc_ribbons_global$lowci[inc_ribbons_global$lowci<0.001] = 0.001

gr_sev = ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1))

gr_sev_logit = ggplot(data=inc_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  xlab("Age in months") + ylab("Probability of severity\n(log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 

# read in vsev_inc ----
load(file=paste(plotpre_out, "epicode01b_comm_prob_sev/inc_vsev_global_predictions.Rdata", sep=""))

inc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(inc_ribbons_global) = c("est", "lowci", "hici")
inc_ribbons_global$mean = apply(allpred, 1, mean)
inc_ribbons_global$mos = seq(0.1, 60, 0.1)
inc_ribbons_global$est[inc_ribbons_global$est<0.001] = 0.001
inc_ribbons_global$mean[inc_ribbons_global$mean<0.001] = 0.001
inc_ribbons_global$lowci[inc_ribbons_global$lowci<0.001] = 0.001

gr_vsev = ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1))

gr_vsev_logit = ggplot(data=inc_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  xlab("Age in months") + ylab("Probability of severity\n(log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 

# read in sev_hosp ----

load(file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_sev_global_predictions.Rdata", sep=""))

inc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(inc_ribbons_global) = c("est", "lowci", "hici")
inc_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
inc_ribbons_global$mos = seq(0.1, 60, 0.1)
inc_ribbons_global$est[inc_ribbons_global$est<0.001] = 0.001
inc_ribbons_global$mean[inc_ribbons_global$mean<0.001] = 0.001
inc_ribbons_global$lowci[inc_ribbons_global$lowci<0.001] = 0.001

gr_sevhosp = ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1))

gr_sevhosp_logit = ggplot(data=inc_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  xlab("Age in months") + ylab("Probability of severity\n(log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 

# read in vsev_hosp ----

load(file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_global_predictions.Rdata", sep=""))

inc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(inc_ribbons_global) = c("est", "lowci", "hici")
inc_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
inc_ribbons_global$mos = seq(0.1, 60, 0.1)
inc_ribbons_global$est[inc_ribbons_global$est<0.001] = 0.001
inc_ribbons_global$mean[inc_ribbons_global$mean<0.001] = 0.001
inc_ribbons_global$lowci[inc_ribbons_global$lowci<0.001] = 0.001

load(file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_lic_predictions.Rdata", sep=""))
load(file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_lmic_predictions.Rdata", sep=""))
load(file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_umic_predictions.Rdata", sep=""))

# columns: lowci, hici, mos, econ
tmp1 = data.frame(t(apply(allpred_lic, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp1) = c("est", "lowci", "hici")
tmp1$mean = apply(allpred_lic, 1, mean)
tmp1$Economic_setting = "Lower income"
tmp2 = data.frame(t(apply(allpred_lmic, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp2) = c("est", "lowci", "hici")
tmp2$mean = apply(allpred_lmic, 1, mean)
tmp2$Economic_setting = "Lower middle income"
tmp3 = data.frame(t(apply(allpred_umic, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp3) = c("est", "lowci", "hici")
tmp3$mean = apply(allpred_umic, 1, mean)
tmp3$Economic_setting = "Upper middle income"

inc_ribbons = rbind(tmp1, tmp2, tmp3) # rbind(tmp1, tmp2, tmp3, tmp4)
inc_ribbons$mos = rep(seq(0.1, 60, 0.1), times=3)
inc_ribbons$Economic_setting = factor(inc_ribbons$Economic_setting)
inc_ribbons$est[inc_ribbons$est<0.001] = 0.001
inc_ribbons$mean[inc_ribbons$mean<0.001] = 0.001
inc_ribbons$lowci[inc_ribbons$lowci<0.001] = 0.001

region_labels = c(`Lower income` = "LIC", 
                  `Lower middle income` = "LMIC",
                  `Upper middle income` = "UMIC")

gr_vsevhosp = ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  geom_ribbon(data=inc_ribbons, aes(x=mos, ymin=lowci, ymax=hici), alpha=0.5, size=0, fill = "lightskyblue") +
  geom_line(data=inc_ribbons, aes(y=mean), color="mediumblue", linetype="dashed") + 
  geom_line(data=inc_ribbons, aes(y=est), color="mediumblue") +
  facet_wrap(~Economic_setting, ncol=3, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1))

gr_vsevhosp_logit = ggplot(data=inc_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  geom_ribbon(data=inc_ribbons, aes(x=mos, ymin=logit(lowci), ymax=logit(hici)), alpha=0.5, size=0, fill = "lightskyblue") +
  geom_line(data=inc_ribbons, aes(y=logit(mean)), color="mediumblue", linetype="dashed") + 
  geom_line(data=inc_ribbons, aes(y=logit(est)), color="mediumblue") +
  facet_wrap(~Economic_setting, ncol=3, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("Probability of severity\n(log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 

ggarrange(ggarrange(gr_sev_logit + xlab(NULL) + ylab("Probability of severity\n(log-odds scale)"), 
                    gr_vsev_logit + xlab(NULL) + ylab(NULL) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()), 
                    gr_sevhosp_logit + xlab(NULL) + ylab(NULL) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()), 
                    nrow=1, ncol = 3, labels = c('A', 'B', 'C'), align="h", widths = c(1.05,0.8,0.8), # alignment h or v?
                    font.label = list(size = 10, color = "black")), 
          gr_vsevhosp_logit + ylab("Probability of severity\n(log-odds scale)"),
          nrow=2, ncol = 1, labels = c('', 'D'), heights = c(0.8, 1),
          font.label = list(size = 10, color = "black"))

ggsave(paste(plotpre_fig, "base_ribbons_severity.pdf", sep=""), 
       width = 7, height = 5, units = 'in', dpi=600)

