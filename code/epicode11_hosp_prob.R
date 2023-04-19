# Hospitalization PROBABILITY

######################
## Make directories to 
## store output if it 
## doesn't already exist.
######################

if (!dir.exists(file.path(paste(plotpre_fig, "epicode11_hosp_prob", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode11_hosp_prob", sep="")))
}

if (!dir.exists(file.path(paste(plotpre_out, "epicode11_hosp_prob", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode11_hosp_prob", sep="")))
}

################################
## Estimate knot positions
## Logistic regression: Homaira, Nokes, Zar
################################

hosp = read.csv("./data/hospitalizations.csv")

hosp$est = hosp$hosp/hosp$rsv_cases
hosp$lci = binconf(hosp$hosp, hosp$rsv_cases, method="exact")[,"Lower"]
hosp$uci = binconf(hosp$hosp, hosp$rsv_cases, method="exact")[,"Upper"]

hosp$study_no = as.factor(as.numeric(hosp$citation))

hosp$Title = paste(hosp$Location, "\n(", hosp$Author, ", ", hosp$Year, ")", 
                        "\n", hosp$Study_period, sep="")

# sensitivity analysis about midpoints
if (midpt_sensitivity == T){
hosp$midpoint = 0.5*(hosp$midpoint+hosp$midpoint_sens)
}

# take out Wu 2015 and Bigogo 2013 and Homaira
hosp_val = hosp[hosp$citation %in% c("Wu 2015", "Bigogo 2013", "Homaira 2012"),]
hosp = hosp[!(hosp$citation %in% c("Wu 2015", "Bigogo 2013", "Homaira 2012")),]
hosp$study_no = as.numeric(factor(hosp$citation))
hosp$Economic_setting = factor(hosp$Economic_setting)

hosp$dummy=1

b_gamm = gamm4(cbind(hosp, rsv_cases)~s(log(midpoint), k=-1, bs=bs_type) + 
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=hosp, family=binomial(link="logit"), REML=T)
check_overdispersion(b_gamm$mer) 

v_gamm = gamm4(cbind(hosp, rsv_cases)~s(log(midpoint), k=-1, bs=bs_type, by=Economic_setting) + Economic_setting + 
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=hosp, family=binomial(link="logit"), REML=T)
check_overdispersion(v_gamm$mer) 

v1_gamm = gamm4(cbind(hosp, rsv_cases)~s(log(midpoint), k=-1, bs=bs_type, by=Economic_setting) + Economic_setting + 
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=Economic_setting),
               random=~(Economic_setting|study_no), data=hosp, family=binomial(link="logit"), REML=T)
check_overdispersion(v1_gamm$mer) 

modcomp = anova(v1_gamm$mer, v_gamm$mer, b_gamm$mer)
write.csv(modcomp, file=paste(plotpre_out, "epicode12_hosp_prob/modcomp_hospassume1.csv", sep=""))

newdata = data.frame(midpoint=seq(0.1, 60, 0.1), 
                     Cases=1000,
                     study_no=2, dummy=0)
bpred = data.frame(predict.gam(b_gamm$gam, newdata=newdata, se.fit=T))
bpred$lfit = bpred$fit-1.96*bpred$se.fit
bpred$ufit = bpred$fit+1.96*bpred$se.fit

bpred=cbind(newdata, bpred)

matplot(bpred$midpoint,logistic(as.matrix(bpred[,c("fit", "lfit", "ufit")])), 
        type="l", lty=1, ylim=c(0,1))
points(hosp$midpoint, hosp$est, pch=20)

# this predicts the basis at new values
tmp=predict.gam(b_gamm$gam, newdata=newdata, type="lpmatrix", se.fit=T)
coef(b_gamm$gam)
b_gamm$gam$Vp # seems to be the same as vcov(b$gam,unconditional=TRUE)
# gam.check(b_gamm$gam)
# uncertainty for smoothing parameter:
vcov(b_gamm$gam,unconditional=TRUE)

b_gamm$gam$Vc 
# Under ML or REML smoothing parameter estimation it is 
# possible to correct the covariance matrix Vp for smoothing 
# parameter uncertainty. This is the corrected version.

somebetas = rmvnorm(n=n_iter, coef(b_gamm$gam), b_gamm$gam$Vp)
someiterates = (tmp %*% t(somebetas))
allpred = logistic(someiterates)
matplot(allpred, type="l", lty=1, col=rgb(0,0,0,alpha=0.01))

allpred_df_global = data.frame(pred=as.vector(allpred[seq(5, 595, 10),]), 
                               mos=rep(seq(0.5, 59.5, 1), times=dim(allpred)[2]), 
                               iter=rep(1:dim(allpred)[2], each=length(seq(0.5, 59.5, 1))))

write.csv(allpred_df_global, file=paste(plotpre_out, "epicode11_hosp_prob/hosp_global_predictions.csv", sep=""))
save(allpred_df_global, allpred, file=paste(plotpre_out, "epicode11_hosp_prob/hosp_global_predictions.Rdata", sep=""))

################################
# PLOTS
################################

jpeg(filename = paste(plotpre_fig, "epicode11_hosp_prob/hosp_global.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=hosp, aes(x=midpoint, y=est)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=hosp, aes(x=midpoint, ymin=lci, ymax=uci), width=.25) + 
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode11_hosp_prob/hosp_global_logit.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=hosp, aes(x=midpoint, y=logit(est+0.001))) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + 
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=logit(pred+0.001), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=hosp, aes(x=midpoint, ymin=logit(lci+0.001), ymax=logit(uci+0.001)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.001, 0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995, 0.999)), 
                     labels = c("0.001", "0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995", "0.999"), limits = c(-7, 7)) 
dev.off()

#######################
## Validation
## Only studies not in 
## the estimation sample
## Sutmoller was excluded altogether
#######################

jpeg(filename = paste(plotpre_fig, "epicode11_hosp_prob/hosp_validation.jpeg", sep=""), 
     width = 5, height = 3, units = 'in', res=600)
ggplot(data=hosp_val, aes(x=midpoint, y=est)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=2) + 
  geom_errorbar(data=hosp_val, aes(x=midpoint, ymin=lci, ymax=uci), width=.25) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0, 60, 12)) + scale_y_continuous(limits = c(0, 1)) +  
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients")
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode11_hosp_prob/hosp_validation_logit.jpeg", sep=""), 
     width = 7, height = 3, units = 'in', res=600)
ggplot(data=hosp_val, aes(x=midpoint, y=logit(est+0.001))) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + 
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=logit(pred+0.001), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=hosp_val, aes(x=midpoint, ymin=logit(lci+0.001), ymax=logit(uci+0.001)), width=.25) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=logit(c(0.001, 0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995, 0.999)), 
                     labels = c("0.001", "0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995", "0.999"), limits = c(-7, 7)) + 
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients (logit scale)")
dev.off()

# ***********************
# Ribbon plots ----------
# ***********************

load(file=paste(plotpre_out, "epicode11_hosp_prob/hosp_global_predictions.Rdata", sep=""))
hosp_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(hosp_ribbons_global) = c("est", "lowci", "hici")
hosp_ribbons_global$mean = apply(allpred, 1, mean)
hosp_ribbons_global$mos = seq(0.1, 60, 0.1)

region_labels = c(`Low income` = "LIC", 
                  `Lower middle income` = "LMIC",
                  `Upper middle income` = "UMIC")

jpeg(filename = paste(plotpre_fig, "epicode11_hosp_prob/hosp_ribbons.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=hosp_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2), limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode11_hosp_prob/hosp_ribbons_logit.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=hosp_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + 
  theme(panel.grid.minor = element_blank()) + 
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") + 
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients (logit scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.001, 0.0025, 0.005, 0.025, 0.01, 0.05, 0.1, 0.25, 0.5)), 
                     labels = c("0.001", "0.0025", "0.005", "0.025", "0.01","0.05","0.1", "0.25", "0.5"), limits = c(-7, 0.5)) 
dev.off()
