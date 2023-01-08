# Base Ribbons, main outcomes

# read in inc ----

load(file=paste(plotpre_out, "epicode01a_comm_inc/inc_global_predictions.Rdata", sep=""))
load(file=paste(plotpre_out, "epicode01a_comm_inc/inc_lmic_predictions.Rdata", sep=""))
load(file=paste(plotpre_out, "epicode01a_comm_inc/inc_umic_predictions.Rdata", sep=""))

# incidence at peak
meanpi(allpred_lmic[apply(allpred_lmic, 2, which.max),])
meanpi(allpred_umic[apply(allpred_lmic, 2, which.max),])

# sanity check
# is this the peak in the graph: meanpi(apply(allpred_lmic, 2, which.max))/10 yes

# columns: lowci, hici, mos, econ
tmp1 = data.frame(t(apply(allpred_lmic, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp1) = c("est", "lowci", "hici")
tmp1$mean = apply(allpred_lmic, 1, mean)
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
inc_ribbons$est[inc_ribbons$est<0.1] = 0.1
inc_ribbons$mean[inc_ribbons$mean<0.1] = 0.1
inc_ribbons$lowci[inc_ribbons$lowci<0.1] = 0.1

inc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(inc_ribbons_global) = c("est", "lowci", "hici")
inc_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
inc_ribbons_global$mos = seq(0.1, 60, 0.1)
inc_ribbons_global$est[inc_ribbons_global$est<0.1] = 0.1
inc_ribbons_global$mean[inc_ribbons_global$mean<0.1] = 0.1
inc_ribbons_global$lowci[inc_ribbons_global$lowci<0.1] = 0.1

region_labels = c(`Lower income` = "LIC", 
                  `Lower middle income` = "LMIC",
                  `Upper middle income` = "UMIC")

gr_inc = ggplot(data=inc_ribbons, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold")) +
  geom_ribbon(alpha=0.5, size=0, fill = "lightskyblue") +
  geom_line(aes(y=mean), color="mediumblue", linetype="dashed") + 
  geom_line(aes(y=est), color="mediumblue") + 
  geom_ribbon(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici), 
              alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(data=inc_ribbons_global, aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(data=inc_ribbons_global, aes(y=est), color="red4") + 
  facet_wrap(Economic_setting~., labeller=labeller(Economic_setting=as_labeller(region_labels)), ncol=3) + # ncol=4
  xlab("Age in months") + ylab("Cases per\n1,000 person-years") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1000, 200)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1000))

gr_inc_log = ggplot(data=inc_ribbons, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold")) +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "lightskyblue") +
  geom_line(aes(y=mean), color="mediumblue", linetype="dashed") + 
  geom_line(aes(y=est), color="mediumblue") + 
  geom_ribbon(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici), 
              alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(data=inc_ribbons_global, aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(data=inc_ribbons_global, aes(y=est), color="red4") + 
  facet_wrap(Economic_setting~.,labeller=labeller(Economic_setting=as_labeller(region_labels)), ncol=3) + # ncol=4
  xlab("Age in months") + ylab("Cases per\n1,000person-years\n(log-scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_log10(breaks=c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100", "1,000")) + 
  coord_cartesian(ylim=c(0.1,1000))

# read in hosprob ----

load(paste(plotpre_out, "epicode11_hosp_prob/hosp_global_predictions.Rdata", sep=""))

# columns: lowci, hici, mos, econ
tmp1 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp1) = c("est", "lowci", "hici")
tmp1$mean = apply(allpred, 1, mean)
tmp1$Economic_setting = "Lower income"
tmp2 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp2) = c("est", "lowci", "hici")
tmp2$mean = apply(allpred, 1, mean)
tmp2$Economic_setting = "Lower middle income"
tmp3 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp3) = c("est", "lowci", "hici")
tmp3$mean = apply(allpred, 1, mean)
tmp3$Economic_setting = "Upper middle income"

hosp_ribbons = rbind(tmp1, tmp2, tmp3) # rbind(tmp1, tmp2, tmp3, tmp4)
hosp_ribbons$mos = rep(seq(0.1, 60, 0.1), times=3)
hosp_ribbons$Economic_setting = factor(hosp_ribbons$Economic_setting)
# hosp_ribbons$Economic_setting = factor(hosp_ribbons$Economic_setting, 
#                                       levels(hosp_ribbons$Economic_setting)[c(2,3,4,1)])
# hosp_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
# colnames(hosp_ribbons_global) = c("est", "lowci", "hici")
# hosp_ribbons_global$mean = apply(allpred, 1, mean)
# hosp_ribbons_global$mos = seq(0.1, 60, 0.1)

region_labels = c(`Lower income` = "LIC", 
                  `Lower middle income` = "LMIC",
                  `Upper middle income` = "UMIC")

gr_hosprob = ggplot(data=hosp_ribbons, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold")) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  facet_wrap(Economic_setting~.,labeller=labeller(Economic_setting=as_labeller(region_labels)), ncol=3) + # ncol=4
  xlab("Age in months") + ylab("Pr. of Hosp") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2), limits = c(0, 1)) 

gr_hosprob_logit = ggplot(data=hosp_ribbons, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold")) +
  theme(panel.grid.minor = element_blank()) + 
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") + 
  facet_wrap(Economic_setting~.,labeller=labeller(Economic_setting=as_labeller(region_labels)), ncol=3) + # ncol=4
  xlab("Age in months") + ylab("Pr. of Hosp\n(log-odds)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5)), 
                     labels = c("0.01","0.025","0.05","0.1", "0.25", "0.5"), 
                     limits = c(-5, 0.5)) 

# read in hospinc ----
load(paste(plotpre_out_hinc, "epicode12a_hospinc/hospinc_global_predictions.Rdata", sep=""))
# load(paste(plotpre_out_hinc, "epicode12a_hospinc/hospinc_lic_predictions.Rdata", sep=""))
# load(paste(plotpre_out_hinc, "epicode12a_hospinc/hospinc_lmic_predictions.Rdata", sep=""))
# load(paste(plotpre_out_hinc, "epicode12a_hospinc/hospinc_umic_predictions.Rdata", sep=""))

meanpi(allpred[apply(allpred, 2, which.max),])

# columns: lowci, hici, mos, econ
tmp1 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975)))) #allpred_lic
colnames(tmp1) = c("est", "lowci", "hici")
tmp1$mean = apply(allpred, 1, mean) # allpred_lic
tmp1$Economic_setting = "Lower income"
tmp2 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975)))) # allpred_lmic
colnames(tmp2) = c("est", "lowci", "hici")
tmp2$mean = apply(allpred, 1, mean) # allpred_lmic
tmp2$Economic_setting = "Lower middle income"
tmp3 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975)))) # allpred_umic
colnames(tmp3) = c("est", "lowci", "hici")
tmp3$mean = apply(allpred, 1, mean) # allpred_umic
tmp3$Economic_setting = "Upper middle income"

hinc_ribbons = rbind(tmp1, tmp2, tmp3) # rbind(tmp1, tmp2, tmp3, tmp4)
hinc_ribbons$mos = rep(seq(0.1, 60, 0.1), times=3)
hinc_ribbons$Economic_setting = factor(hinc_ribbons$Economic_setting)
hinc_ribbons$est[hinc_ribbons$est<0.1] = 0.1
hinc_ribbons$mean[hinc_ribbons$mean<0.1] = 0.1
hinc_ribbons$lowci[hinc_ribbons$lowci<0.1] = 0.1
hinc_ribbons$est[hinc_ribbons$est>1000] = 1000
hinc_ribbons$hici[hinc_ribbons$hici>1000] = 1000
hinc_ribbons$mean[hinc_ribbons$mean>1000] = 1000

# hinc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
# colnames(hinc_ribbons_global) = c("est", "lowci", "hici")
# hinc_ribbons_global$mean = apply(allpred, 1, mean)
# # cfr_ribbons_global$Economic_setting = "Global"
# hinc_ribbons_global$mos = seq(0.1, 60, 0.1)
# hinc_ribbons_global$est[hinc_ribbons_global$est<0.1] = 0.1
# hinc_ribbons_global$mean[hinc_ribbons_global$mean<0.1] = 0.1
# hinc_ribbons_global$lowci[hinc_ribbons_global$lowci<0.1] = 0.1
# hinc_ribbons_global$est[hinc_ribbons_global$est>1000] = 1000
# hinc_ribbons_global$hici[hinc_ribbons_global$hici>1000] = 1000
# hinc_ribbons_global$mean[hinc_ribbons_global$mean>1000] = 1000

region_labels = c(`Lower income` = "LIC", 
                  `Lower middle income` = "LMIC",
                  `Upper middle income` = "UMIC")

gr_hinc = ggplot(data=hinc_ribbons, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(panel.grid.minor = element_blank()) + 
  theme(axis.text = element_text(size = 8, face = "bold")) +
  geom_ribbon(aes(x=mos, ymin=lowci, ymax=hici), 
              alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  # geom_ribbon(data=hinc_ribbons, alpha=0.5, size=0, fill = "lightskyblue") +
  # geom_line(data=hinc_ribbons, aes(y=mean), color="mediumblue", linetype="dashed") + 
  # geom_line(data=hinc_ribbons, aes(y=est), color="mediumblue") + 
  facet_wrap(Economic_setting~., ncol=3, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("Cases per\n1,000 person-years") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(limits = c(0, 1000)) 

gr_hinc_log = ggplot(data=hinc_ribbons, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(panel.grid.minor = element_blank()) + 
  theme(axis.text = element_text(size = 8, face = "bold")) +
  geom_ribbon(aes(x=mos, ymin=lowci, ymax=hici), 
              alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  # geom_ribbon(data=hinc_ribbons, alpha=0.5, size=0, fill = "lightskyblue") +
  # geom_line(data=hinc_ribbons, aes(y=mean), color="mediumblue", linetype="dashed") + 
  # geom_line(data=hinc_ribbons, aes(y=est), color="mediumblue") + 
  facet_wrap(Economic_setting~., ncol=3, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("Cases per\n1,000 person-years\n(log-scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels=c("0.1", "1", "10", "100", "1,000")) 

# read in hCFR ----
load(paste(plotpre_out, "epicode21_cfr/cfr_global_predictions.Rdata", sep=""))
# load(paste(plotpre_out, "epicode21_cfr/cfr_lic_predictions.Rdata", sep=""))
# load(paste(plotpre_out, "epicode21_cfr/cfr_lmic_predictions.Rdata", sep=""))
# load(paste(plotpre_out, "epicode21_cfr/cfr_umic_predictions.Rdata", sep=""))

# columns: lowci, hici, mos, econ
tmp1 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp1) = c("est", "lowci", "hici")
tmp1$mean = apply(allpred, 1, mean)
tmp1$Economic_setting = "Lower income"
tmp2 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp2) = c("est", "lowci", "hici")
tmp2$mean = apply(allpred, 1, mean)
tmp2$Economic_setting = "Lower middle income"
tmp3 = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp3) = c("est", "lowci", "hici")
tmp3$mean = apply(allpred, 1, mean)
tmp3$Economic_setting = "Upper middle income"

cfr_ribbons = rbind(tmp1, tmp2, tmp3) # rbind(tmp1, tmp2, tmp3, tmp4)
cfr_ribbons$mos = rep(seq(0.1, 60, 0.1), times=3)
cfr_ribbons$Economic_setting = factor(cfr_ribbons$Economic_setting)

cfr_ribbons$est[cfr_ribbons$est<0.001] = 0.001
cfr_ribbons$mean[cfr_ribbons$mean<0.001] = 0.001
cfr_ribbons$lowci[cfr_ribbons$lowci<0.001] = 0.001

gr_hcfr = ggplot(data=cfr_ribbons, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold")) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  facet_wrap(Economic_setting~., ncol=3, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("hCFR") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=seq(0, 0.05, 0.01)) +
  coord_cartesian(ylim=c(0, 0.05))

gr_hcfr_logit = ggplot(data=cfr_ribbons, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold")) +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  facet_wrap(Economic_setting~., ncol=3, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("hCFR\n(log-odds)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.001, 0.0025, 0.005, 0.01, 0.025, 0.05)), 
                     labels = c("0.001", "0.0025", "0.005","0.01", "0.025", "0.05"), 
                     limits = c(-7, -3)) +
  coord_cartesian(ylim=c(-7,-3))

# numbers for text in paper---

inc_ribbons[inc_ribbons$mos==0.5,]
hinc_ribbons[hinc_ribbons$mos==0.5,]
hosp_ribbons[hosp_ribbons$mos==0.5,]
hosp_ribbons[hosp_ribbons$mos==6.0,]
hosp_ribbons[hosp_ribbons$mos==12.0,]
hosp_ribbons[hosp_ribbons$mos==60,]
# incidence at peaks is above just after I bring in the matrices for hospinc and inc

cfr_ribbons[cfr_ribbons$mos==0.5,]
cfr_ribbons[cfr_ribbons$mos==6.0,]
cfr_ribbons[cfr_ribbons$mos==12.0,]
cfr_ribbons[cfr_ribbons$mos==60,]

# Make main plot ------------

library(ggpubr)

ggarrange(gr_inc_log + xlab(NULL) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()), 
          gr_hinc_log + xlab(NULL) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + theme(strip.background = element_blank(), strip.text = element_blank()), 
          gr_hosprob_logit + xlab(NULL) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + theme(strip.background = element_blank(), strip.text = element_blank()), 
          gr_hcfr_logit + theme(strip.background = element_blank(), strip.text = element_blank()),
          nrow=4, labels = c('A', 'B', 'C', 'D'), align="v", heights=c(1,0.85,0.85,1),
          font.label = list(size = 10, color = "black"))
ggsave(paste(plotpre_fig, "base_ribbons.pdf", sep=""), 
               width = 7, height = 8, units = 'in', dpi=600)

