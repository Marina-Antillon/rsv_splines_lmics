### CFR Splines ------

######################
## Make directories -----
## to store output if it 
## doesn't already exist.
######################

if (!dir.exists(file.path(paste(plotpre_fig, "epicode21_cfr", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode21_cfr", sep="")))
}

if (!dir.exists(file.path(paste(plotpre_out, "epicode21_cfr", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode21_cfr", sep="")))
}

######################
## Call data in -----
## make into long-form
######################

load("./data/CFR_workingfile.Rdata")
cfr_all$study_no = 1:dim(cfr_all)[1]

cfr_all = cfr_all[substrRight(cfr_all$Study_period, 4)>1999,]


cases_vars = which(substr(colnames(cfr_all), 1, 5) == "Cases")
deaths_vars = which(substr(colnames(cfr_all), 1, 6) == "Deaths")
redundant_vars = which(substr(colnames(cfr_all), 1, 9) == "Redundant")
id_vars = which(colnames(cfr_all) %in% c("study_no", "Author", "Year", "Study_period", "Location", "Economic_setting"))

# cfr_all$Location[!is.na(cfr_all$Country)] = paste(cfr_all$Location[!is.na(cfr_all$Country)], cfr_all$Country[!is.na(cfr_all$Country)], sep=", ")
cfr_all$Location = trimws(cfr_all$Location, "right")

# cases
agegroups=c()
for (i in 1:length(colnames(cfr_all)[cases_vars])){
  agegroups[i] = paste(strsplit(colnames(cfr_all)[cases_vars], "_")[[i]][2], "_", 
                     strsplit(colnames(cfr_all)[cases_vars], "_")[[i]][3], sep="")
}

tmp_cases = pivot_longer(cfr_all[,c(id_vars, cases_vars)],
                         cols = starts_with("Cases"),
                         names_to = "Age_Groups",
                         names_prefix = "Cases_",
                         values_to="Cases")
tmp_cases = tmp_cases[!is.na(tmp_cases$Cases),]
rownames(tmp_cases) = NULL

# deaths
agegroups=c()
for (i in 1:length(colnames(cfr_all)[deaths_vars])){
  agegroups[i] = paste(strsplit(colnames(cfr_all)[deaths_vars], "_")[[i]][2], "_", 
                       strsplit(colnames(cfr_all)[deaths_vars], "_")[[i]][3], sep="")
}

tmp_deaths = pivot_longer(cfr_all[,c(id_vars, deaths_vars)],
                         cols = starts_with("Deaths"),
                         names_to = "Age_Groups",
                         names_prefix = "Deaths_",
                         values_to="Deaths")
tmp_deaths = tmp_deaths[!is.na(tmp_deaths$Deaths),]
rownames(tmp_deaths) = NULL

# redundancy vars
agegroups=c()
for (i in 1:length(colnames(cfr_all)[redundant_vars])){
  agegroups[i] = paste(strsplit(colnames(cfr_all)[redundant_vars], "_")[[i]][2], "_", 
                       strsplit(colnames(cfr_all)[redundant_vars], "_")[[i]][3], sep="")
}

tmp_red = pivot_longer(cfr_all[,c(id_vars, redundant_vars)],
                          cols = starts_with("Redundant"),
                          names_to = "Age_Groups",
                          names_prefix = "Redundant_",
                          values_to="Redundant")
tmp_red = tmp_red[!is.na(tmp_red$Redundant),]
rownames(tmp_red) = NULL

# join it all together
cfr_long = full_join(tmp_cases, tmp_deaths)
cfr_long = left_join(cfr_long, tmp_red)

sum(cfr_long$Redundant!=T, na.rm=T)
sum(tmp_red$Redundant!=T, na.rm=T)

######################
## Assign midpoints ----
######################

agemdpt = data.frame(Age_Groups = c("0d_27d", "28d_3m", "0m_3m", "3m_5m", "1m_5m", "0m_5m", 
                                    "6m_8m", "9m_11m", "6m_11m", "6m_23m", "1m_11m", "0m_11m",
                                    "12m_23m", "24m_35m", "36m_59m", "0m_23m", "0m_35m", "0m_47m", "12m_59m", 
                                    "24m_59m", "0m_59m", "1m_59m"),
                     midpoint = c(0.5, 2, 1.5, 4.5, 3.5, 3, 7.5, 10.5, 9, 15, 6.5, 6, 18, 30, 48, 12, 18, 
                                  24, 36, 42, 30, 30.5))

ageleft = data.frame(Age_Groups = c("0d_27d", "28d_3m", "0m_3m", "3m_5m", "1m_5m", "0m_5m", 
                                    "6m_8m", "9m_11m", "6m_11m", "6m_23m", "1m_11m", "0m_11m",
                                    "12m_23m", "24m_35m", "36m_59m", "0m_23m", "0m_35m", "0m_47m", "12m_59m", 
                                    "24m_59m", "0m_59m", "1m_59m"),
                     left = c(0, 1, 0, 3, 1, 0, 6, 9, 6, 6, 1, 0, 12, 24, 36, 0, 0, 
                              0, 12, 24, 0, 1))

ageright = data.frame(Age_Groups = c("0d_27d", "28d_3m", "0m_3m", "3m_5m", "1m_5m", "0m_5m", 
                                     "6m_8m", "9m_11m", "6m_11m", "6m_23m", "1m_11m", "0m_11m",
                                     "12m_23m", "24m_35m", "36m_59m", "0m_23m", "0m_35m", "0m_47m", "12m_59m", 
                                     "24m_59m", "0m_59m", "1m_59m"),
                      right = c(1, 3, 3, 6, 6, 6, 9, 12, 12, 23, 12, 12, 24, 36, 24, 36, 48, 
                                60, 60, 60, 60, 60))
cfr_long = left_join(cfr_long, agemdpt)
cfr_long = left_join(cfr_long, ageleft)
cfr_long = left_join(cfr_long, ageright)

## Titles for graphs
cfr_long$Title = paste(cfr_long$Location, "\n(", cfr_long$Author, ", ", cfr_long$Year, ")", 
                       "\n", cfr_long$Study_period, sep="")

cfr_long = cfr_long[!is.na(cfr_long$Cases),]
cfr_long = cfr_long[cfr_long$Cases>0,]

######################
# round all cases to 
# nearest whole (some are 
# X.9995 for some reason)
######################

cfr_long$Cases = round(cfr_long$Cases)
cfr_long$Deaths = round(cfr_long$Deaths)

cfr_long = cfr_long[cfr_long$Economic_setting!="High income",]
cfr_long$Economic_setting = factor(cfr_long$Economic_setting)
cfr_long$Economic_setting2 = as.character(cfr_long$Economic_setting)
cfr_long$Economic_setting2[cfr_long$Economic_setting2=="Lower income"] = "Lower middle income"
cfr_long$Economic_setting2 = factor(cfr_long$Economic_setting2)
# levels(cfr_long$Economic_setting)

# this cfr long is 799 and the other is 798 - haven't taken out the 0m-47m one

######################
## calculate CFR ----
######################

cfr_long$cfr = cfr_long$Deaths/cfr_long$Cases
cfr_long$cfr_lci = binconf(cfr_long$Deaths, cfr_long$Cases, method="exact")[,"Lower"]
cfr_long$cfr_uci = binconf(cfr_long$Deaths, cfr_long$Cases, method="exact")[,"Upper"]

cfr_long$study_no = factor(as.numeric(as.factor(cfr_long$Title)))

################################
## Logistic regression; Global analysis  ----
###############################

cfr_long_min = cfr_long[cfr_long$Redundant==F | is.na(cfr_long$Redundant),]
cfr_long_min = cfr_long_min[!(cfr_long_min$Age_Groups %in% c( "0m_47m")),] # "0m_35m",

cfr_long_min_all = cfr_long_min
cfr_long_min = cfr_long_min[cfr_long_min$Cases>0,]
cfr_long_min = cfr_long_min[cfr_long_min$study_no %in% as.numeric(names(table(cfr_long_min$study_no)))[table(cfr_long_min$study_no)>2],]
# Make it so it's 15 in total
# bigenough = cfr_long_min %>% group_by(study_no) %>% dplyr::summarise(n_total = sum(Cases))
# cfr_long_min = cfr_long_min[cfr_long_min$study_no %in% bigenough$study_no[bigenough$n_total>15],]

# the rest of the studies are for validation 
cfr_long_min_all = cfr_long_min_all[!(cfr_long_min_all$study_no %in% cfr_long_min$study_no),]

write.csv(cfr_long_min, "./data_long_cfr_train.csv")
write.csv(cfr_long_min_all, "./data_long_cfr_val.csv")

cfr_long_min$study_no = factor(as.numeric(as.factor(cfr_long_min$Title)))

if (midpt_sensitivity == T){
  for (i in 1:length(unique(cfr_long_min$study_no))){
    cfr_long_min$midpoint[cfr_long_min$study == i & cfr_long_min$midpoint==min(cfr_long_min$midpoint[cfr_long_min$study == i])] = 
      cfr_long_min$right[cfr_long_min$study == i & cfr_long_min$midpoint==min(cfr_long_min$midpoint[cfr_long_min$study == i])]
    
    cfr_long_min$midpoint[cfr_long_min$study == i & cfr_long_min$midpoint==max(cfr_long_min$midpoint[cfr_long_min$study == i])] = 
      cfr_long_min$left[cfr_long_min$study == i & cfr_long_min$midpoint==max(cfr_long_min$midpoint[cfr_long_min$study == i])]
  }
}

cfr_long_min$dummy=1

b_gamm = gamm4(cbind(Deaths, Cases)~s(log(midpoint), m=2, k=-1, bs=bs_type) + 
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=cfr_long_min, family=binomial(link="logit"))

# check_overdispersion(b_gamm$mer)
# gam.check(b_gamm$gam)
# hist(b_gamm$gam$residuals)
# plot(log(cfr_long_min$midpoint), b_gamm$gam$residuals)

v_gamm = gamm4(cbind(Deaths, Cases)~s(log(midpoint), m=2, k=-1, bs=bs_type, by=Economic_setting) + Economic_setting + 
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=cfr_long_min, family=binomial(link="logit"))

# check_overdispersion(v_gamm$mer)
# gam.check(v_gamm$gam)
# hist(v_gamm$gam$residuals)

v1_gamm = gamm4(cbind(Deaths, Cases)~s(log(midpoint), m=2, k=-1, bs=bs_type, by=Economic_setting) + Economic_setting + 
                  t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=Economic_setting),
                random=~(Economic_setting|study_no), data=cfr_long_min, family=binomial(link="logit"), 
                control=glmerControl(optCtrl = list(maxfun=2e4)))

# check_overdispersion(v1_gamm$mer)
# gam.check(v1_gamm$gam)
# hist(v1_gamm$gam$residuals)
# plot(v1_gamm$gam)

modcomp = anova(b_gamm$mer, v_gamm$mer, v1_gamm$mer)
write.csv(modcomp, file=paste(plotpre_out, "epicode22_cfr/modcomp.csv", sep=""))

# LIC and LMIC combined

v_gamm_comb = gamm4(cbind(Deaths, Cases)~s(log(midpoint), m=2, k=-1, bs=bs_type, by=Economic_setting2) + Economic_setting2 +
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=cfr_long_min, family=binomial(link="logit"))

v1_gamm_comb = gamm4(cbind(Deaths, Cases)~s(log(midpoint), m=2, k=-1, bs=bs_type, by=Economic_setting2) + Economic_setting2 +
                  t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=Economic_setting2),
                random=~(Economic_setting2|study_no), data=cfr_long_min, family=binomial(link="logit"),
                control=glmerControl(optCtrl = list(maxfun=2e4)))
modcomp_comb = anova(b_gamm$mer, v_gamm_comb$mer, v1_gamm_comb$mer)
# write.csv(modcomp_comb, file=paste(plotpre_out, "epicode22_cfr/modcomp_comb.csv", sep=""))
# draw the contrast. If it is qualitatively the same, just say so in the letter and let it be.

newdata = data.frame(midpoint=seq(0.1, 60, 0.1), Cases=100, study_no=10, dummy=0)
bpred = data.frame(predict.gam(b_gamm$gam, newdata=newdata, se.fit=T))
bpred$lfit = bpred$fit-1.96*bpred$se.fit
bpred$ufit = bpred$fit+1.96*bpred$se.fit

bpred=cbind(newdata, bpred)

matplot(log(bpred$midpoint),logistic(as.matrix(bpred[,c("fit", "lfit", "ufit")])), type="l", lty=1, ylim=c(0,.4))
points(log(cfr_long_min$midpoint), cfr_long_min$cfr, pch=20)

# this predicts the basis at new values
tmp=predict.gam(b_gamm$gam, newdata=newdata, type="lpmatrix", se.fit=T)
# coef(b_gamm$gam)
# b_gamm$gam$Vp # seems to be the same as # vcov(b_gamm$gam,unconditional=TRUE)
# gam.check(b_gamm$gam)
# uncertainty for smoothing parameter:
# vcov(b_gamm$gam,unconditional=TRUE)
# b_gamm$gam$Vc 
# Under ML or REML smoothing parameter estimation it is 
# possible to correct the covariance matrix Vp for smoothing 
# parameter uncertainty. This is the corrected version.

somebetas = rmvnorm(n=n_iter, coef(b_gamm$gam), b_gamm$gam$Vp)
someiterates = (tmp %*% t(somebetas))
allpred = logistic(someiterates)
matplot(seq(0.1, 60, 0.1), allpred, type="l", ylim=c(0, .1), lty=1, col=rgb(0,0,0,alpha=0.1))

allpred_df_global = data.frame(pred=as.vector(allpred[seq(5, 595, 10),]), 
                               mos=rep(seq(0.5, 59.5, 1), times=dim(allpred)[2]), 
                               iter=rep(1:dim(allpred)[2], each=length(seq(0.5, 59.5, 1))))

write.csv(allpred_df_global, file=paste(plotpre_out, "epicode21_cfr/cfr_global_predictions.csv", sep=""))
save(allpred_df_global, allpred, file=paste(plotpre_out, "epicode21_cfr/cfr_global_predictions.Rdata", sep=""))

# Now global only -------

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lic.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
print(ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Lower income",], 
             aes(x=midpoint, y=cfr)) + themebar2 +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
                  aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
        xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients") + 
        scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
        scale_y_continuous(limits = c(0, 1)))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lic_logit.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
print(ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Lower income",], 
             aes(x=midpoint, y=logit(cfr))) + themebar2 +
        theme(panel.grid.minor =  element_blank()) +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
                  aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        geom_errorbar(aes(x=midpoint, ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
        xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (log-odds scale)") + 
        scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
        scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                           labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                      "0.75", "0.95", "0.99", "0.995")) +
        coord_cartesian(ylim=c(-7, 7)))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lic_val.jpeg", sep=""), 
     width = 7, height = 3.5, units = 'in', res=600)
print(ggplot(data=cfr_long_min_all[cfr_long_min_all$Economic_setting=="Lower income",], 
             aes(x=midpoint, y=cfr)) + themebar2 +
        theme(panel.grid.minor =  element_blank()) +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
                  aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
        xlab("Age in months") + ylab("Probability of Death among\nHospitalized Patients") + 
        scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
        scale_y_continuous(limits = c(0, 1)))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lic_logit_val.jpeg", sep=""), 
     width = 7, height = 3.5, units = 'in', res=600)
ggplot(data=cfr_long_min_all[cfr_long_min_all$Economic_setting=="Lower income",], 
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among\nHospitalized Patients (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7))
dev.off()

# Now global only -----

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lmic_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Lower middle income" & 
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Lower middle income"])[1:9],], 
       aes(x=midpoint, y=cfr)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lmic_B.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Lower middle income" & 
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Lower middle income"])[10:18],], 
       aes(x=midpoint, y=cfr)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lmic_C.jpeg", sep=""),
     width = 7, height = 5.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Lower middle income" &
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Lower middle income"])[19:24],],
       aes(x=midpoint, y=cfr)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,],
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +
  geom_point(size=1) + facet_wrap(~Title, ncol=3) +
  geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) +
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients") +
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + scale_y_continuous(limits = c(0, 1))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lmic_Alogit.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Lower middle income" & 
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Lower middle income"])[1:9],], 
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lmic_Blogit.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Lower middle income" & 
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Lower middle income"])[10:18],], 
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lmic_Clogit.jpeg", sep=""),
     width = 7, height = 5.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Lower middle income" &
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Lower middle income"])[19:24],],
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,],
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
  geom_point(size=1) + facet_wrap(~Title, ncol=3) +
  geom_errorbar(aes(x=midpoint, ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) +
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (log-odds scale)") +
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) +
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)),
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50",
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7))
dev.off()

# Validation -----
# unique(cfr_long_min_all$Title[cfr_long_min_all$Economic_setting=="Lower middle income"])

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lmic_valA.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
print(ggplot(data=cfr_long_min_all[cfr_long_min_all$Economic_setting=="Lower middle income" & 
                                     cfr_long_min_all$Title %in% unique(cfr_long_min_all$Title[cfr_long_min_all$Economic_setting=="Lower middle income"])[1:8],], 
             aes(x=midpoint, y=cfr)) + themebar2 +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
                  aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
        xlab("Age in months") + ylab("Probability of Death among\nHospitalized Patients") + 
        scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
        scale_y_continuous(limits = c(0, 1)))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_lmic_logit_valA.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min_all[cfr_long_min_all$Economic_setting=="Lower middle income"& 
                               cfr_long_min_all$Title %in% unique(cfr_long_min_all$Title[cfr_long_min_all$Economic_setting=="Lower middle income"])[1:8],], 
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among\nHospitalized Patients (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()

# Upper middle income -----
unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Upper middle income"])
# 9 and 9, and 7

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Upper middle income" &
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Upper middle income"])[1:9],], 
       aes(x=midpoint, y=cfr)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_Alogit.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Upper middle income" & 
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Upper middle income"])[1:9],], 
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, y=logit(cfr), ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_B.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Upper middle income" &
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Upper middle income"])[10:18],], 
       aes(x=midpoint, y=cfr)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_Blogit.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Upper middle income" & 
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Upper middle income"])[10:18],], 
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, y=logit(cfr), ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_C.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Upper middle income" &
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Upper middle income"])[19:25],], 
       aes(x=midpoint, y=cfr)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_Clogit.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min[cfr_long_min$Economic_setting=="Upper middle income" & 
                           cfr_long_min$Title %in% unique(cfr_long_min$Title[cfr_long_min$Economic_setting=="Upper middle income"])[19:25],], 
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, y=logit(cfr), ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()

# Validation (by economic group) -----

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_val_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min_all[cfr_long_min_all$Economic_setting=="Upper middle income" & 
                               cfr_long_min_all$Title %in% unique(cfr_long_min_all$Title[cfr_long_min_all$Economic_setting=="Upper middle income"])[1:9],], 
       aes(x=midpoint, y=cfr)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (logit scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(limits = c(0, 1))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_logit_val_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
ggplot(data=cfr_long_min_all[cfr_long_min_all$Economic_setting=="Upper middle income" & 
                               cfr_long_min_all$Title %in% unique(cfr_long_min_all$Title[cfr_long_min_all$Economic_setting=="Upper middle income"])[1:9],], 
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (logit scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_val_B.jpeg", sep=""), 
     width = 7, height = 9.5, units = 'in', res=600)
ggplot(data=cfr_long_min_all[cfr_long_min_all$Economic_setting=="Upper middle income" & 
                               cfr_long_min_all$Title %in% unique(cfr_long_min_all$Title[cfr_long_min_all$Economic_setting=="Upper middle income"])[10:19],],
       aes(x=midpoint, y=cfr)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=cfr_lci, ymax=cfr_uci), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (logit scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(limits = c(0, 1))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_umic_logit_val_B.jpeg", sep=""), 
     width = 7, height = 9.5, units = 'in', res=600)
ggplot(data=cfr_long_min_all[cfr_long_min_all$Economic_setting=="Upper middle income" & 
                               cfr_long_min_all$Title %in% unique(cfr_long_min_all$Title[cfr_long_min_all$Economic_setting=="Upper middle income"])[10:19],],
       aes(x=midpoint, y=logit(cfr))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(aes(x=midpoint, ymin=logit(cfr_lci), ymax=logit(cfr_uci)), width=.25) + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients (logit scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()

########################
## Plots with ribbons ------
########################

cfr_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(cfr_ribbons_global) = c("est", "lowci", "hici")
cfr_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
cfr_ribbons_global$mos = seq(0.1, 60, 0.1)

# cfr_ribbons$est[cfr_ribbons$est<0.001] = 0.001
# cfr_ribbons$mean[cfr_ribbons$mean<0.001] = 0.001
# cfr_ribbons$lowci[cfr_ribbons$lowci<0.001] = 0.001

cfr_ribbons_global$est[cfr_ribbons_global$est<0.001] = 0.001
cfr_ribbons_global$mean[cfr_ribbons_global$mean<0.001] = 0.001
cfr_ribbons_global$lowci[cfr_ribbons_global$lowci<0.001] = 0.001

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_ribbons.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=cfr_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Probability of Death among Hospitalized Patients") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=seq(0, 0.05, 0.01)) +
  coord_cartesian(ylim=c(0, 0.05))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode21_cfr/cfr_global_ribbons_logit.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=cfr_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  xlab("Age in months") + ylab("Probability of Death among\nHospitalized Patients (logit scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, seq(0.01, 0.05, 0.01))), 
                     labels = c("0.005", "0.01", "0.02", "0.03", "0.04", "0.05")) +
  coord_cartesian(ylim=c(-7,-3)) 
dev.off()
