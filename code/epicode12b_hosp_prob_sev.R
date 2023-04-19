#*******************************
## Splines of RSV incidence from community-based study data.-----
#*******************************
# note that not all high-income countries are "industrialized" and that one industrialized country is UMIC
# table(inc_shi$Economic_setting, inc_shi$Development_status)

#*******************************
## Make directories ------------ 
## to store output if it 
## doesn't already exist.
#*******************************

if (!dir.exists(file.path(paste(plotpre_fig, "epicode12b_hosp_prob_sev", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode12b_hosp_prob_sev", sep="")))
}

if (!dir.exists(file.path(paste(plotpre_out, "epicode12b_hosp_prob_sev", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode12b_hosp_prob_sev", sep="")))
}

#*******************************
## Read in new clean data
#*******************************

# For hospitalization
data_list = list()

for (i in 1:52){
  data_list[[i]] = read.xlsx("./data/HospInc_data_R_readable.xlsx", i)
}

# rbind them together
data_long = data_list[[41]]
data_long = data_long[is.na(data_long$Cases),]

for (i in c(1:52)){
  data_long = rbind.fill(data_long, data_list[[i]])
}

#*******************************
## Assign midpoints ------------
#*******************************

data_long$Age_Groups = c()
for (i in 1:dim(data_long)[1]){
  data_long$Age_Groups[i] = strsplit(as.character(data_long$Age_cat), "Ages_")[[i]][2]
}

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

data_long = left_join(data_long, agemdpt)
data_long = left_join(data_long, ageleft)
data_long = left_join(data_long, ageright)

## Titles for graphs
# data_long$Location = as.character(data_long$Location)
data_long$Title = paste(data_long$Location, "\n(", data_long$Author, ", ", data_long$Year, ")", 
                       "\n", data_long$Study_period, sep="")

#*******************************
# round all cases --------------
# to nearest whole (some are 
# X.9995 for some reason)
#*******************************
data_long$Cases = round(data_long$Cases)
data_long$Cases_sev = round(data_long$Cases_sev)
data_long$Cases_vsev = round(data_long$Cases_vsev)

data_long$Pop = round(data_long$Pop)
data_long$Pop_sev = round(data_long$Pop_sev)
data_long$Pop_vsev = round(data_long$Pop_vsev)

#*******************************
## calculate incidence and CIs --
#*******************************
data_long$Cases_sev[data_long$Cases<data_long$Cases_sev & !is.na(data_long$Cases_sev)] = data_long$Cases[data_long$Cases<data_long$Cases_sev & !is.na(data_long$Cases_sev)]
data_long$Cases_vsev[data_long$Cases<data_long$Cases_vsev & !is.na(data_long$Cases_vsev)] = data_long$Cases[data_long$Cases<data_long$Cases_vsev & !is.na(data_long$Cases_vsev)]

data_long$inc = data_long$Cases/data_long$Pop*1000
data_long$inc_lci = NaN
data_long$inc_hci = NaN
data_long[!is.na(!data_long$Cases) & !is.na(data_long$Pop), c("inc_lci", "inc_hci")] = 
  pois.exact(data_long$Cases[!is.na(data_long$Cases) & !is.na(data_long$Pop)], 
             data_long$Pop[!is.na(!data_long$Cases) & !is.na(data_long$Pop)], 0.95)[,c("lower", "upper")]*1000

data_long$inc_sev_prob = data_long$Cases_sev/data_long$Cases*100
data_long$inc_sev_lci = NaN
data_long$inc_sev_hci = NaN
data_long[!is.na(!data_long$Cases_sev) & !is.na(data_long$Cases) & data_long$Cases>0, c("inc_sev_lci", "inc_sev_hci")] = 
  binom.exact(data_long$Cases_sev[!is.na(data_long$Cases_sev) & !is.na(data_long$Cases) & data_long$Cases>0], 
             data_long$Cases[!is.na(!data_long$Cases_sev) & !is.na(data_long$Cases) & data_long$Cases>0], 0.95)[,c("lower", "upper")]*100

data_long$inc_vsev_prob = data_long$Cases_vsev/data_long$Cases*100
data_long$inc_vsev_lci = NaN
data_long$inc_vsev_hci = NaN
data_long[!is.na(!data_long$Cases_vsev) & !is.na(data_long$Cases) & data_long$Cases>0, c("inc_vsev_lci", "inc_vsev_hci")] = 
  binom.exact(data_long$Cases_vsev[!is.na(data_long$Cases_vsev) & !is.na(data_long$Cases) & data_long$Cases>0], 
              data_long$Cases[!is.na(!data_long$Cases_vsev) & !is.na(data_long$Cases) & data_long$Cases>0], 0.95)[,c("lower", "upper")]*100

data_long[!is.na(data_long$Cases_sev), c("Cases_sev", "Cases_vsev")]

#*******************************
## Logistic regression sev, global --
#*******************************

data_long_min = data_long[!is.na(data_long$Cases_sev) & !is.na(data_long$Cases) & data_long$Cases>0,]
# data_long_min = data_long_min[data_long_min$Author!="Howie",] # & data_long_min$Author!="Gessner"

data_long_min$study_no = as.numeric(as.factor(data_long_min$Title))
data_long_min_all = data_long_min # to graph the studies that were excluded from the analysis
data_long_min = data_long_min[data_long_min$study_no %in% as.numeric(names(table(data_long_min$study_no)))[table(data_long_min$study_no)>2],]
data_long_min_all = data_long_min_all[data_long_min_all$study_no %in% as.numeric(names(table(data_long_min_all$study_no)))[table(data_long_min_all$study_no)<3],]

write.csv(data_long_min, "./data_long_hospincsev_train.csv")
write.csv(data_long_min_all, "./data_long_hospincsev_val.csv")

# data_long_min$study_no = as.numeric(as.factor(data_long_min$study_no))
data_long_min$study_no = factor(as.numeric(as.factor(data_long_min$study_no)))

table(data_long_min$study_no) # make sure each study has at least 3 observations

# assign a value for midpoints
if (midpt_sensitivity == T){
  for (i in 1:length(unique(data_long_min$study_no))){
    data_long_min$midpoint[data_long_min$study_no == i & data_long_min$midpoint==min(data_long_min$midpoint[data_long_min$study_no == i])] = 
      0.5*(data_long_min$midpoint[data_long_min$study_no == i & data_long_min$midpoint==min(data_long_min$midpoint[data_long_min$study_no == i])] +
             data_long_min$right[data_long_min$study_no == i & data_long_min$midpoint==min(data_long_min$midpoint[data_long_min$study_no == i])])
    
    data_long_min$midpoint[data_long_min$study_no == i & data_long_min$midpoint==max(data_long_min$midpoint[data_long_min$study_no == i])] = 
      0.5*(data_long_min$midpoint[data_long_min$study_no == i & data_long_min$midpoint==max(data_long_min$midpoint[data_long_min$study_no == i])] +
             data_long_min$left[data_long_min$study_no == i & data_long_min$midpoint==max(data_long_min$midpoint[data_long_min$study_no == i])])
  }
}

data_long_min$Economic_Setting = factor(data_long_min$Economic_Setting)
data_long_min$dummy=1

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/forest_sev.jpeg", sep=""), 
     width = 10, height = 25, units = "in", quality = 100, bg = "white", res = 300)
forest(metaprop(Cases_sev, Cases, studlab=Age_cat, 
                byvar=paste(Author, ", ", Year, ", ", Study_period, sep=""),
                data=data_long_min, comb.random=T, print.byvar=F, method="inverse"))
dev.off()

b_gamm = gamm4(cbind(Cases_sev, Cases)~s(log(midpoint), k=3, bs=bs_type) + 
                 t2(log(midpoint), study_no, k=5, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=data_long_min, 
               family=binomial(link="logit"), REML=T)

# check_overdispersion(v_gamm$mer) # overdispersion not detected
# gam.check(v_gamm$gam)

v_gamm = gamm4(cbind(Cases_sev, Cases)~s(log(midpoint), k=3, bs=bs_type, by=Economic_Setting) + Economic_Setting + 
                 t2(log(midpoint), study_no, k=5, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=data_long_min, family=binomial(link="logit"))

# check_overdispersion(v_gamm$mer) # overdispersion not detected
# gam.check(v_gamm$gam)

v1_gamm = gamm4(cbind(Cases_sev, Cases)~s(log(midpoint), k=3, bs=bs_type, by=Economic_Setting) + Economic_Setting + 
                  t2(log(midpoint), study_no, k=5, bs=c(bs_type, 're'), by=Economic_Setting),
                random=~(Economic_Setting|study_no), data=data_long_min, family=binomial(link="logit"), REML=T,
                control=glmerControl(optCtrl = list(maxfun=2e4)))

# check_overdispersion(v1_gamm$mer) # overdispersion not detected
# gam.check(v1_gamm$gam)

modcomp = anova(b_gamm$mer, v_gamm$mer, v1_gamm$mer)
write.csv(modcomp, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/modcomp_sev.csv", sep=""))

newpred=data.frame(midpoint=seq(0.1, 60, 0.1), Cases=100, study_no=1, dummy=0)
bpred = data.frame(predict.gam(b_gamm$gam, newdata=newpred, se.fit=T))

bpred$lfit = bpred$fit-1.96*bpred$se.fit
bpred$ufit = bpred$fit+1.96*bpred$se.fit

bpred=cbind(newpred, bpred)

matplot(bpred$midpoint,logistic(as.matrix(bpred[,c("fit", "lfit", "ufit")])), type="l", lty=1, ylim=c(0,1))
points(data_long_min$midpoint, data_long_min$inc_sev_prob/100, pch=20)

# this predicts the basis at new values
tmp=predict.gam(b_gamm$gam, newdata=newpred, type="lpmatrix", se.fit=T) 
coef(b_gamm$gam)
tmp2 = b_gamm$gam$Vp # seems to be the same as vcov(b$gam,unconditional=TRUE)
# size: number of studies (res), number of knots, plus intercept
# gam.check(b_gamm$gam)
# uncertainty for smoothing parameter:
b_gamm$gam$Vc # only works for the gam function, but not the gamm
# According to Wood's book: Under ML or REML smoothing parameter estimation it is 
# possible to correct the covariance matrix Vp for smoothing 
# parameter uncertainty. This is the corrected version.

somebetas = rmvnorm(n=n_iter, coef(b_gamm$gam), b_gamm$gam$Vp)
someiterates = (tmp %*% t(somebetas))
allpred = logistic(someiterates)

matplot(seq(0.1, 60, 0.1), allpred, type="l", ylim=c(0, 1), lty=1, col=rgb(0,0,0,alpha=.1))
points(data_long_min$midpoint, data_long_min$inc_sev_prob/100, pch=20, col="red")

allpred_df_global = data.frame(pred=as.vector(allpred[seq(5, 595, 10),]),
                               mos=rep(seq(0.5, 59.5, 1), times=dim(allpred)[2]),
                               iter=rep(1:dim(allpred)[2], each=length(seq(0.5, 59.5, 1))))

write.csv(allpred_df_global, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_sev_global_predictions.csv", sep=""))
save(allpred_df_global, allpred, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_sev_global_predictions.Rdata", sep=""))

#*******************************
# Plot, Global estimates -------
#*******************************

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_sev_global_fit_obs_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[1:9],], aes(x=midpoint, y=inc_sev_prob/100)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_sev_lci/100, ymax=inc_sev_hci/100), width=.25) + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_sev_global_fit_obs_B.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[10:15],], aes(x=midpoint, y=inc_sev_prob/100)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_sev_lci/100, ymax=inc_sev_hci/100), width=.25) + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_sev_global_fit_obs_logit_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
print(ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[1:9],], aes(x=midpoint, y=logit(inc_sev_prob/100))) + themebar2 +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
                  aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        geom_errorbar(aes(x=midpoint, ymin=logit(inc_sev_lci/100), ymax=logit(inc_sev_hci/100)), width=.25) + 
        theme(panel.grid.minor =  element_blank()) +
        xlab("Age in months") + ylab("Probability of severity (logit scale)") + 
        scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
        scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                           labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                      "0.75", "0.95", "0.99", "0.995")) +
        coord_cartesian(ylim=c(-7, 7)))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_sev_global_fit_obs_logit_B.jpeg", sep=""), 
     width = 7, height = 5.5, units = 'in', res=600)
print(ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[10:15],], aes(x=midpoint, y=logit(inc_sev_prob/100))) + themebar2 +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
                  aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        geom_errorbar(aes(x=midpoint, ymin=logit(inc_sev_lci/100), ymax=logit(inc_sev_hci/100)), width=.25) + 
        theme(panel.grid.minor =  element_blank()) +
        xlab("Age in months") + ylab("Probability of severity (logit scale)") + 
        scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
        scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                           labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                      "0.75", "0.95", "0.99", "0.995")) +
        coord_cartesian(ylim=c(-7, 7)))
dev.off()

#*******************************
# Ribbon plots -----------------
#*******************************

load(file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_sev_global_predictions.Rdata", sep=""))
inc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(inc_ribbons_global) = c("est", "lowci", "hici")
inc_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
inc_ribbons_global$mos = seq(0.1, 60, 0.1)
inc_ribbons_global$est[inc_ribbons_global$est<0.001] = 0.001
inc_ribbons_global$mean[inc_ribbons_global$mean<0.001] = 0.001
inc_ribbons_global$lowci[inc_ribbons_global$lowci<0.001] = 0.001

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_sev_global_ribbons.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_sev_global_ribbons_logit.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=inc_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  xlab("Age in months") + ylab("Probability of severity (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()

#*******************************
# Logistic regression v. sev, global ----
#*******************************

data_long_min = data_long[!is.na(data_long$Cases_vsev) & !is.na(data_long$Cases) & data_long$Cases>0,]
data_long_min$study_no = as.numeric(as.factor(data_long_min$Title))
data_long_min_all = data_long_min # to graph the studies that were excluded from the analysis
data_long_min = data_long_min[data_long_min$study_no %in% as.numeric(names(table(data_long_min$study_no)))[table(data_long_min$study_no)>2],]
data_long_min_all = data_long_min_all[data_long_min_all$study_no %in% as.numeric(names(table(data_long_min_all$study_no)))[table(data_long_min_all$study_no)<3],]

# data_long_min$study_no = as.numeric(as.factor(data_long_min$study_no))
data_long_min$study_no = factor(as.numeric(as.factor(data_long_min$study_no)))

table(data_long_min$study_no) # make sure each study has at least 3 observations

write.csv(data_long_min, "./data_long_hospincVsev_train.csv")
write.csv(data_long_min_all, "./data_long_hospincVsev_val.csv")

# assign a value for midpoints
if (midpt_sensitivity == T){
  for (i in 1:length(unique(data_long_min$study_no))){
    data_long_min$midpoint[data_long_min$study_no == i & data_long_min$midpoint==min(data_long_min$midpoint[data_long_min$study_no == i])] = 
      0.5*(data_long_min$midpoint[data_long_min$study_no == i & data_long_min$midpoint==min(data_long_min$midpoint[data_long_min$study_no == i])] +
             data_long_min$right[data_long_min$study_no == i & data_long_min$midpoint==min(data_long_min$midpoint[data_long_min$study_no == i])])
    
    data_long_min$midpoint[data_long_min$study_no == i & data_long_min$midpoint==max(data_long_min$midpoint[data_long_min$study_no == i])] = 
      0.5*(data_long_min$midpoint[data_long_min$study_no == i & data_long_min$midpoint==max(data_long_min$midpoint[data_long_min$study_no == i])] +
             data_long_min$left[data_long_min$study_no == i & data_long_min$midpoint==max(data_long_min$midpoint[data_long_min$study_no == i])])
  }
}

forest(metaprop(Cases_vsev, Cases, studlab=midpoint, 
                byvar=paste(Author, ", ", Year, sep=""), data=data_long_min,
                comb.random=F, print.byvar=F), fontsize = 10)

data_long_min$Economic_Setting = factor(data_long_min$Economic_Setting)
data_long_min$dummy=1

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/forest_vsev.jpeg", sep=""), 
     width = 9, height = 28, units = "in", quality = 100, bg = "white", res = 300)
forest(metaprop(Cases_vsev, Cases, studlab=Age_cat, byvar=paste(Author, ", ", Year, ", ", Study_period, sep=""),
                data=data_long_min, comb.random=T, print.byvar=F))
dev.off()

data_long_min$Economic_Setting = factor(data_long_min$Economic_Setting)
data_long_min$dummy=1

b_gamm = gamm4(cbind(Cases_vsev, Cases)~s(log(midpoint), k=3, bs=bs_type) + 
                 t2(log(midpoint), study_no, k=5, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=data_long_min, family=binomial(link="logit"), REML=T)

as.numeric(logLik(b_gamm$mer))
check_overdispersion(b_gamm$mer) # overdispersion not detected
gam.check(b_gamm$gam)

v_gamm = gamm4(cbind(Cases_vsev, Cases)~s(log(midpoint), k=3, bs=bs_type, by=Economic_Setting) + Economic_Setting + 
                 t2(log(midpoint), study_no, k=5, bs=c(bs_type, 're'), by=dummy),
               random=~(1|study_no), data=data_long_min, family=binomial(link="logit"), REML=T)

v1_gamm = gamm4(cbind(Cases_vsev, Cases)~s(log(midpoint), k=3, bs=bs_type, by=Economic_Setting) + Economic_Setting + 
                  t2(log(midpoint), study_no, k=3, bs=c(bs_type, 're'), by=Economic_Setting),
                random=~(Economic_Setting|study_no), data=data_long_min, family=binomial(link="logit"), REML=T,
                control=glmerControl(optCtrl = list(maxfun=2e4)))

modcomp = anova(b_gamm$mer, v_gamm$mer, v1_gamm$mer)
write.csv(modcomp, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/modcomp_vsev.csv", sep=""))

newpred=data.frame(midpoint=seq(0.1, 60, 0.1), Cases=100, study_no=3, dummy=0)
bpred = data.frame(predict.gam(b_gamm$gam, newdata=newpred, se.fit=T))

bpred$lfit = bpred$fit-1.96*bpred$se.fit
bpred$ufit = bpred$fit+1.96*bpred$se.fit

bpred=cbind(newpred, bpred)

matplot(bpred$midpoint,logistic(as.matrix(bpred[,c("fit", "lfit", "ufit")])), type="l", lty=1, ylim=c(0,1))
points(data_long_min$midpoint, data_long_min$inc_vsev_prob/100, pch=20)

# this predicts the basis at new values
tmp=predict.gam(b_gamm$gam, newdata=newpred, type="lpmatrix", se.fit=T) 
coef(b_gamm$gam)
tmp2 = b_gamm$gam$Vp # seems to be the same as vcov(b$gam,unconditional=TRUE)
# size: number of studies (res), number of knots, plus intercept
# gam.check(b_gamm$gam)
# uncertainty for smoothing parameter:
b_gamm$gam$Vc # only works for the gam function, but not the gamm
# According to Wood's book: Under ML or REML smoothing parameter estimation it is 
# possible to correct the covariance matrix Vp for smoothing 
# parameter uncertainty. This is the corrected version.

somebetas = rmvnorm(n=n_iter, coef(b_gamm$gam), b_gamm$gam$Vp)
someiterates = (tmp %*% t(somebetas))
allpred = logistic(someiterates)

matplot(seq(0.1, 60, 0.1), allpred, type="l", ylim=c(0, 1), lty=1, col=rgb(0,0,0,alpha=.1))
points(data_long_min$midpoint, data_long_min$inc_vsev_prob/100, pch=20, col="red")

allpred_df_global = data.frame(pred=as.vector(allpred[seq(5, 595, 10),]),
                               mos=rep(seq(0.5, 59.5, 1), times=dim(allpred)[2]),
                               iter=rep(1:dim(allpred)[2], each=length(seq(0.5, 59.5, 1))))

write.csv(allpred_df_global, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_global_predictions.csv", sep=""))
save(allpred_df_global, allpred, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_global_predictions.Rdata", sep=""))

#*******************************
# Plot, Global estimates -------
#*******************************

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_global_fit_obs_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[1:9],], aes(x=midpoint, y=inc_vsev_prob/100)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_vsev_lci/100, ymax=inc_vsev_hci/100), width=.25) + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_global_fit_obs_B.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[10:13],], aes(x=midpoint, y=inc_vsev_prob/100)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_vsev_lci/100, ymax=inc_vsev_hci/100), width=.25) + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_y_continuous(limits = c(0, 1)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_global_fit_obs_logit_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = 'in', res=600)
print(ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[1:9],], aes(x=midpoint, y=logit(inc_vsev_prob/100))) + themebar2 +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
                  aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        geom_errorbar(aes(x=midpoint, ymin=logit(inc_vsev_lci/100), ymax=logit(inc_vsev_hci/100)), width=.25) + 
        theme(panel.grid.minor =  element_blank()) +
        xlab("Age in months") + ylab("Probability of severity (logit scale)") + 
        scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
        scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                           labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                      "0.75", "0.95", "0.99", "0.995")) +
        coord_cartesian(ylim=c(-7, 7)))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_global_fit_obs_logit_B.jpeg", sep=""), 
     width = 7, height = 5.5, units = 'in', res=600)
print(ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[10:13],], aes(x=midpoint, y=logit(inc_vsev_prob/100))) + themebar2 +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
                  aes(x=mos, y=logit(pred), group=iter), col="rosybrown2", alpha=0.15) +
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        geom_errorbar(aes(x=midpoint, ymin=logit(inc_vsev_lci/100), ymax=logit(inc_vsev_hci/100)), width=.25) + 
        theme(panel.grid.minor =  element_blank()) +
        xlab("Age in months") + ylab("Probability of severity (logit scale)") + 
        scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
        scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                           labels = c("0.005", "0.01", "0.05", "0.25", "0.50", 
                                      "0.75", "0.95", "0.99", "0.995")) +
        coord_cartesian(ylim=c(-7, 7)))
dev.off()

#*******************************
# Ribbon plots -----------------
#*******************************

load(file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_global_predictions.Rdata", sep=""))
inc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(inc_ribbons_global) = c("est", "lowci", "hici")
inc_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
inc_ribbons_global$mos = seq(0.1, 60, 0.1)
inc_ribbons_global$est[inc_ribbons_global$est<0.001] = 0.001
inc_ribbons_global$mean[inc_ribbons_global$mean<0.001] = 0.001
inc_ribbons_global$lowci[inc_ribbons_global$lowci<0.001] = 0.001

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_global_ribbons.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_global_ribbons_logit.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=inc_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  xlab("Age in months") + ylab("Probability of severity (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()


################################
# Analysis by Economic setting
################################

tmp = unique(data_long_min[,c("Economic_Setting")])
newdata = data.frame(midpoint=rep(seq(0.1, 60, 0.1), times=3), 
                     Cases=100,
                     Economic_Setting = rep(tmp, each=length(seq(0.1, 60, 0.1))),
                     study_no=2, dummy=0)

vpred = data.frame(predict.gam(v1_gamm$gam, newdata=newdata, se.fit=T))
vpred$lfit = vpred$fit-1.96*vpred$se.fit
vpred$ufit = vpred$fit+1.96*vpred$se.fit

vpred=cbind(newdata, vpred)

par(mfrow=c(1,3))
matplot(vpred$midpoint[vpred$Economic_Setting=="Lower income"],
        logistic(as.matrix(vpred[vpred$Economic_Setting=="Lower income",
                                 c("fit", "lfit", "ufit")])), 
        type="l", lty=1, ylim=c(0,1))
points(data_long_min$midpoint[data_long_min$Economic_Setting=="Lower income"], 
       data_long_min$inc_vsev_prob[data_long_min$Economic_Setting=="Lower income"]/100, pch=20)
matplot(vpred$midpoint[vpred$Economic_Setting=="Lower middle income"],
        logistic(as.matrix(vpred[vpred$Economic_Setting=="Lower middle income",
                                 c("fit", "lfit", "ufit")])), 
        type="l", lty=1, ylim=c(0,1))
points(data_long_min$midpoint[data_long_min$Economic_Setting=="Lower middle income"], 
       data_long_min$inc_vsev_prob[data_long_min$Economic_Setting=="Lower middle income"]/100, pch=20)
matplot(vpred$midpoint[vpred$Economic_Setting=="Upper middle income"],
        logistic(as.matrix(vpred[vpred$Economic_Setting=="Upper middle income",
                                 c("fit", "lfit", "ufit")])), 
        type="l", lty=1, ylim=c(0,1))
points(data_long_min$midpoint[data_long_min$Economic_Setting=="Upper middle income"], 
       data_long_min$inc_vsev_prob[data_long_min$Economic_Setting=="Upper middle income"]/100, pch=20)

# this predicts the basis at new values
tmp=predict.gam(v1_gamm$gam, newdata=newdata, type="lpmatrix", se.fit=T)
coef(v1_gamm$gam)
v1_gamm$gam$Vp # seems to be the same as vcov(b$gam,unconditional=TRUE)
# gam.check(v_gamm$gam)
# uncertainty for smoothing parameter:
vcov(v1_gamm$gam,unconditional=TRUE)

v1_gamm$gam$Vc 
# Under ML or REML smoothing parameter estimation it is 
# possible to correct the covariance matrix Vp for smoothing 
# parameter uncertainty. This is the corrected version.

somebetas = rmvnorm(n=n_iter, coef(v1_gamm$gam), v1_gamm$gam$Vp)
someiterates = (tmp %*% t(somebetas))
allpred_lic = logistic(someiterates[newdata$Economic_Setting=="Lower income",])
allpred_lmic = logistic(someiterates[newdata$Economic_Setting=="Lower middle income",])
allpred_umic = logistic(someiterates[newdata$Economic_Setting=="Upper middle income",])
matplot(allpred_lic, type="l", ylim=c(0, 1), lty=1, col=rgb(0,0,0, alpha=0.05))
matplot(allpred_lmic, type="l", ylim=c(0, 1), lty=1, col=rgb(0,0,0, alpha=0.05))
matplot(allpred_umic, type="l", ylim=c(0, 1), lty=1, col=rgb(0,0,0, alpha=0.05))

allpred_df_lic = data.frame(pred=as.vector(allpred_lic[seq(5, 595, 10),]), 
                             mos=rep(seq(0.5, 59.5, 1), times=dim(allpred_lic)[2]), 
                             iter=rep(1:dim(allpred_lic)[2], each=length(seq(0.5, 59.5, 1))))
allpred_df_lmic = data.frame(pred=as.vector(allpred_lmic[seq(5, 595, 10),]), 
                             mos=rep(seq(0.5, 59.5, 1), times=dim(allpred_lmic)[2]), 
                             iter=rep(1:dim(allpred_lmic)[2], each=length(seq(0.5, 59.5, 1))))
allpred_df_umic = data.frame(pred=as.vector(allpred_umic[seq(5, 595, 10),]), 
                             mos=rep(seq(0.5, 59.5, 1), times=dim(allpred_umic)[2]), 
                             iter=rep(1:dim(allpred_umic)[2], each=length(seq(0.5, 59.5, 1))))

save(allpred_df_lic, allpred_lic, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_lic_predictions.Rdata", sep=""))
save(allpred_df_lmic, allpred_lmic, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_lmic_predictions.Rdata", sep=""))
save(allpred_df_umic, allpred_umic, file=paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_vsev_umic_predictions.Rdata", sep=""))

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_fit_obs_lic.jpeg", sep=""), 
     width = 7, height = 3, units = 'in', res=600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower income",], aes(x=midpoint, y=inc_vsev_prob/100)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + 
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_line(data=allpred_df_lic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=data_long_min[data_long_min$Economic_Setting=="Lower income",], aes(x=midpoint, ymin=inc_vsev_lci/100, ymax=inc_vsev_hci/100), width=.25) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0, 60, 12)) + scale_y_continuous(limits = c(0, 1)) +  
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients")
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_fit_obs_lmic.jpeg", sep=""), 
     width = 7, height = 5.5, units = 'in', res=600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower middle income",], aes(x=midpoint, y=inc_vsev_prob/100)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + 
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=data_long_min[data_long_min$Economic_Setting=="Lower middle income",], aes(x=midpoint, ymin=inc_vsev_lci/100, ymax=inc_vsev_hci/100), width=.25) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0, 60, 12)) + scale_y_continuous(limits = c(0, 1)) +  
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients")
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_fit_obs_umic.jpeg", sep=""), 
     width = 7, height = 5.5, units = 'in', res=600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Upper middle income",], aes(x=midpoint, y=inc_vsev_prob/100)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + 
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=data_long_min[data_long_min$Economic_Setting=="Upper middle income",], aes(x=midpoint, ymin=inc_vsev_lci/100, ymax=inc_vsev_hci/100), width=.25) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0, 60, 12)) + scale_y_continuous(limits = c(0, 1)) +  
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients")
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_fit_obs_lic_logit.jpeg", sep=""), 
     width = 7, height = 3, units = 'in', res=600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower income",], aes(x=midpoint, y=logit(inc_vsev_prob/100+0.001))) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + 
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=logit(pred+0.001), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_line(data=allpred_df_lic[allpred_df_lic$iter %in% 1:300,], aes(x=mos, y=logit(pred+0.001), group=iter), col="lightskyblue", alpha=0.09) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=data_long_min[data_long_min$Economic_Setting=="Lower income",], aes(x=midpoint, ymin=logit(inc_vsev_lci/100+0.001), ymax=logit(inc_vsev_hci/100+0.001)), width=.25) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=logit(c(0.001, 0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995, 0.999)), 
                     labels = c("0.001", "0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995", "0.999"), limits = c(-7, 7)) +
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients (logit scale)")
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_fit_obs_lmic_logit.jpeg", sep=""), 
     width = 7, height = 5.5, units = 'in', res=600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower middle income",], aes(x=midpoint, y=logit(inc_vsev_prob/100+0.001))) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + 
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=logit(pred+0.001), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=logit(pred+0.001), group=iter), col="lightskyblue", alpha=0.09) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=data_long_min[data_long_min$Economic_Setting=="Lower middle income",], aes(x=midpoint, ymin=logit(inc_vsev_lci/100+0.001), ymax=logit(inc_vsev_hci/100+0.001)), width=.25) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=logit(c(0.001, 0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995, 0.999)), 
                     labels = c("0.001", "0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995", "0.999"), limits = c(-7, 7)) +
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients (logit scale)")
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_fit_obs_umic_logit.jpeg", sep=""), 
     width = 7, height = 5.5, units = 'in', res=600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Upper middle income",], aes(x=midpoint, y=logit(inc_vsev_prob/100+0.001))) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=logit(pred+0.001), group=iter), col="rosybrown2", alpha=0.15) +  
  geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=logit(pred+0.001), group=iter), col="lightskyblue", alpha=0.09) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  geom_errorbar(data=data_long_min[data_long_min$Economic_Setting=="Upper middle income",], aes(x=midpoint, ymin=logit(inc_vsev_lci/100+0.001), ymax=logit(inc_vsev_hci/100+0.001)), width=.25) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=logit(c(0.001, 0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995, 0.999)), 
                     labels = c("0.001", "0.005", "0.01", "0.05", "0.25", "0.50", 
                                "0.75", "0.95", "0.99", "0.995", "0.999"), limits = c(-7, 7)) +
  xlab("Age in months") + ylab("Probability of Hospitalization\nAmong All Patients (logit scale)")
dev.off()

# **************************************
# Ribbon plots, vsev economic group ----
# **************************************

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

inc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(inc_ribbons_global) = c("est", "lowci", "hici")
inc_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
inc_ribbons_global$mos = seq(0.1, 60, 0.1)
inc_ribbons_global$est[inc_ribbons_global$est<0.001] = 0.001
inc_ribbons_global$mean[inc_ribbons_global$mean<0.001] = 0.001
inc_ribbons_global$lowci[inc_ribbons_global$lowci<0.001] = 0.001

region_labels = c(`Lower income` = "LIC", 
                  `Lower middle income` = "LMIC",
                  `Upper middle income` = "UMIC")

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_ribbons.jpeg", sep=""), 
     width = 2.5, height = 7, units = 'in', res=600)
ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  geom_ribbon(data=inc_ribbons, aes(x=mos, ymin=lowci, ymax=hici), alpha=0.5, size=0, fill = "lightskyblue") +
  geom_line(data=inc_ribbons, aes(y=mean), color="mediumblue", linetype="dashed") + 
  geom_line(data=inc_ribbons, aes(y=est), color="mediumblue") +
  facet_wrap(~Economic_setting, ncol=1, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("Probability of severity") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12b_hosp_prob_sev/hospinc_vsev_ribbons_logit.jpeg", sep=""), 
     width = 2.5, height = 7, units = 'in', res=600)
ggplot(data=inc_ribbons_global, aes(x=mos, ymin=logit(lowci), ymax=logit(hici))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=logit(mean)), color="red4", linetype="dashed") + 
  geom_line(aes(y=logit(est)), color="red4") +
  geom_ribbon(data=inc_ribbons, aes(x=mos, ymin=logit(lowci), ymax=logit(hici)), alpha=0.5, size=0, fill = "lightskyblue") +
  geom_line(data=inc_ribbons, aes(y=logit(mean)), color="mediumblue", linetype="dashed") + 
  geom_line(data=inc_ribbons, aes(y=logit(est)), color="mediumblue") +
  facet_wrap(~Economic_setting, ncol=1, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("Probability of severity (log-odds scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=logit(c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)), 
                     labels = c("0.005", "0.01", "0.05", "0.25", "0.50", "0.75", "0.95", "0.99", "0.995")) +
  coord_cartesian(ylim=c(-7, 7)) 
dev.off()
