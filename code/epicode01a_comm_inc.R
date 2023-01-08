#*******************************
## Splines of RSV incidence from community-based study data.-----
#*******************************
# note that not all high-income countries are "industrialized" and that one industrialized country is UMIC
# table(inc_shi$Economic_setting, inc_shi$Development_status)

#*******************************
## Make directories ------------
## to store output if it 
## doesn't already exist
#*******************************

if (!dir.exists(file.path(paste(plotpre_fig, "epicode01a_comm_inc", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode01a_comm_inc", sep="")))
}

if (!dir.exists(file.path(paste(plotpre_out, "epicode01a_comm_inc", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode01a_comm_inc", sep="")))
}

#*******************************
## Read in new clean data -----
#*******************************

data_list = list()

for (i in 1:22){
  data_list[[i]] = read.xlsx("./data/Inc_data_R_readable.xlsx", i)
}

# rbind them together to make into a data.frame.

data_long = data_list[[7]]
data_long = data_long[is.na(data_long$Cases),]

for (i in 1:22){
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
data_long$Pop = round(data_long$Pop)

#*******************************
## calculate incidence and CIs --
#*******************************
data_long$inc = data_long$Cases/data_long$Pop*1000
data_long$inc_lci = NaN
data_long$inc_hci = NaN
data_long[!is.na(!data_long$Cases) & !is.na(data_long$Pop), c("inc_lci", "inc_hci")] = 
  pois.exact(data_long$Cases[!is.na(data_long$Cases) & !is.na(data_long$Pop)], 
             data_long$Pop[!is.na(!data_long$Cases) & !is.na(data_long$Pop)], 0.95)[,c("lower", "upper")]*1000

#*******************************
## Poisson regression, global --
#*******************************

data_long_min = data_long[!is.na(data_long$Cases),]
data_long_min$study_no = as.numeric(as.factor(data_long_min$Title))
data_long_min_all = data_long_min # to graph the studies that were excluded from the analysis
data_long_min = data_long_min[data_long_min$study_no %in% as.numeric(names(table(data_long_min$study_no)))[table(data_long_min$study_no)>2],]
data_long_min_all = data_long_min_all[data_long_min_all$study_no %in% as.numeric(names(table(data_long_min_all$study_no)))[table(data_long_min_all$study_no)<3],]

write.csv(data_long_min, paste(plotpre_out, "/data_long_inc_train.csv"))
write.csv(data_long_min_all, paste(plotpre_out, "./data_long_inc_val.csv"))

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

b_gamm = gamm4(Cases~s(log(midpoint), k=-1, m=2, bs=bs_type) + 
                 t2(log(midpoint), study_no, m=2, bs=c(bs_type, 're'), by=dummy)+offset(log(Pop)),
               random=~(1|study_no), data=data_long_min, family=poisson, REML=T)

# v_gamm = gamm4(Cases~s(log(midpoint), k=-1, bs=bs_type, m=2, by=Economic_Setting) + Economic_Setting + 
#                  t2(log(midpoint), study_no, m=2, bs=c(bs_type,"re"), by=dummy)+offset(log(Pop)),
#                random=~(1|study_no), data=data_long_min[data_long_min$right<25,], family=poisson)
# 
# v_gamm_nat= gamm4(Cases~s(midpoint, k=-1, bs=bs_type, m=2, by=Economic_Setting) + Economic_Setting +
#                  t2(midpoint, study_no, m=2, bs=c(bs_type,"re"), by=dummy)+offset(log(Pop)),
#                random=~(1|study_no), data=data_long_min[data_long_min$right<25,], family=poisson)

v_gamm = gamm4(Cases~s(log(midpoint), k=-1, bs=bs_type, m=2, by=Economic_Setting) + Economic_Setting +
                 t2(log(midpoint), study_no, m=2, bs=c(bs_type,"re"), by=dummy)+offset(log(Pop)),
               random=~(1|study_no), data=data_long_min, family=poisson)

v_gamm_nat= gamm4(Cases~s(midpoint, k=-1, bs=bs_type, m=2, by=Economic_Setting) + Economic_Setting +
                 t2(midpoint, study_no, m=2, bs=c(bs_type,"re"), by=dummy)+offset(log(Pop)),
               random=~(1|study_no), data=data_long_min, family=poisson)

v1_gamm = gamm4(Cases~s(log(midpoint), k=-1, bs=bs_type, m=2, by=Economic_Setting) + Economic_Setting + 
                 t2(log(midpoint), study_no, m=2, bs=c(bs_type,"re"), by=Economic_Setting)+offset(log(Pop)),
               random=~(Economic_Setting|study_no), data=data_long_min, family=poisson)

modcomp = anova(v_gamm$mer, b_gamm$mer, v1_gamm$mer)
write.csv(modcomp, file=paste(plotpre_out, "epicode01a_comm_inc/modcomp.csv", sep=""))

newpred=data.frame(midpoint=seq(0.1, 60, 0.1), Pop=rep(1000, length(seq(0.1, 60, 0.1))),
                   study_no=3, dummy=0)
bpred = data.frame(predict.gam(b_gamm$gam, newdata=newpred, se.fit=T))

bpred$lfit = bpred$fit-1.96*bpred$se.fit
bpred$ufit = bpred$fit+1.96*bpred$se.fit

bpred=cbind(newpred, bpred)

matplot(bpred$midpoint,exp(as.matrix(bpred[,c("fit", "lfit", "ufit")])), type="l", lty=1, ylim=c(0,200))
points(data_long_min$midpoint, data_long_min$inc, pch=20)

# this predicts the basis at new values
tmp=predict.gam(b_gamm$gam, newdata=newpred, type="lpmatrix", se.fit=T) 
coef(b_gamm$gam)
tmp2 = b_gamm$gam$Vp # seems to be the same as vcov(b$gam,unconditional=TRUE)
# size: number of studies (res), number of knots, plus intercept
# gam.check(b_gamm$gam)
# uncertainty for smoothing parameter:
# b_gamm$gam$Vc # only works for the gam function, but not the gamm
# According to Wood's book: Under ML or REML smoothing parameter estimation it is 
# possible to correct the covariance matrix Vp for smoothing 
# parameter uncertainty. This is the corrected version.

somebetas = rmvnorm(n=n_iter, coef(b_gamm$gam), b_gamm$gam$Vp)
someiterates = (tmp %*% t(somebetas))
allpred = exp(someiterates)*1000
matplot(allpred, type="l", col=rgb(0,0,0,alpha=.01), lty=1, ylim=c(0, 1000))
points(data_long_min$midpoint*10, data_long_min$inc, pch=20, col="red")

allpred_df_global = data.frame(pred=as.vector(allpred[seq(5, 595, 10),]),
                               mos=rep(seq(0.5, 59.5, 1), times=dim(allpred)[2]),
                               iter=rep(1:dim(allpred)[2], each=length(seq(0.5, 59.5, 1))))

write.csv(allpred_df_global, file=paste(plotpre_out, "epicode01a_comm_inc/inc_global_predictions.csv", sep=""))
save(allpred_df_global, allpred, file=paste(plotpre_out, "epicode01a_comm_inc/inc_global_predictions.Rdata", sep=""))

#*******************************
# Plot, Global estimates -------
#*******************************

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_global_fit_obs_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[1:9],], aes(x=midpoint, y=inc)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_global_fit_obs_B.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[10:17],], aes(x=midpoint, y=inc)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_global_fit_obs_log_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[1:9],], aes(x=midpoint, y=inc+0.1)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci+0.1, ymax=inc_hci+0.1), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_log10(breaks=10**c(-1:3), labels = c("0.1","1", "10", "100", "1000"), limits = c(0.1, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_global_fit_obs_log_B.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[10:17],], aes(x=midpoint, y=inc+0.1)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) + # , strip.text = element_blank()
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci+0.1, ymax=inc_hci+0.1), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_log10(breaks=10**c(-1:3), labels = c("0.1","1", "10", "100", "1000"), limits = c(0.1, 1000)) 
dev.off()

## Validation with the global splines

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_global_fit_obs_val.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", # pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all, 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_global_fit_obs_log10_val.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", # pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all, 
       aes(x=midpoint, y=inc)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], 
            aes(x=mos, y=pred+0.1, group=iter), col="rosybrown2", alpha=0.15) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci+0.1, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_log10(limits = c(0.1, 1000), 
                breaks=c(0.1, 1, 10, 100, 1000),
                labels=scales::comma) 
dev.off()

#*******************************
## By UMIC/LMIC ----------------
#*******************************

tmp_econ = unique(data_long_min[,c("Economic_Setting")])
newpred = data.frame(midpoint=rep(seq(0.1, 60, 0.1), 2), 
                     Pop=rep(1000, length(seq(0.1, 60, 0.1))*2),
                     Economic_Setting = rep(tmp_econ, each=length(seq(0.1, 60, 0.1))),
                     dummy=0, study_no=7)

vpred = data.frame(predict.gam(v_gamm$gam, newdata=newpred, se.fit=T))
vpred$lfit = vpred$fit-1.96*vpred$se.fit
vpred$ufit = vpred$fit+1.96*vpred$se.fit

vpred=cbind(newpred, vpred)

par(mfrow=c(1,2))
matplot(vpred$midpoint[vpred$Economic_Setting=="Lower middle income"], 
        exp(as.matrix(vpred[vpred$Economic_Setting=="Lower middle income",c("fit", "lfit", "ufit")])), 
        ylim=c(0, 300), type="l", lty=1)
# points(data_long_min$midpoint[data_long_min$Economic_Setting=="Lower middle income"], 
#        data_long_min$inc[data_long_min$Economic_Setting=="Lower middle income"], pch=20)

matplot(vpred$midpoint[vpred$Economic_Setting=="Upper middle income"], 
        exp(as.matrix(vpred[vpred$Economic_Setting=="Upper middle income", c("fit", "lfit", "ufit")])), 
        ylim=c(0, 300), type="l", lty=1)
# points(data_long_min$midpoint[data_long_min$Economic_Setting=="Upper middle income"], 
#        data_long_min$inc[data_long_min$Economic_Setting=="Upper middle income"], pch=20)

# this predicts the basis at new values
keepspline=!str_detect(names(coef(v_gamm$gam)), "study_no") 
  # keep splines of main effect+random effect at intercept.
tmp=predict.gam(v_gamm$gam, newdata=newpred, type="lpmatrix", se.fit=T)[,keepspline]
# coef(b$gam)
# b$gam$Vp

# Uncertainty in the coefficients?
somebetas = rmvnorm(n=n_iter, coef(v_gamm$gam), v_gamm$gam$Vp)[,keepspline]
# combine uncertainty in coefficients with uncertainty in TMP
someiterates = (tmp %*% t(somebetas))
allpred_lmic = exp(someiterates[newpred$Economic_Setting=="Lower middle income",])*1000
allpred_umic = exp(someiterates[newpred$Economic_Setting=="Upper middle income",])*1000

par(mfrow=c(1,2))
matplot(seq(0.1, 60, 0.1), allpred_lmic, type="l", lty=1, ylim=c(0, 1000), col=rgb(0,0,0,alpha=0.1))
points(data_long_min$midpoint[data_long_min$Economic_Setting=="Lower middle income"], 
       data_long_min$inc[data_long_min$Economic_Setting=="Lower middle income"], pch=20, col="red")
matplot(seq(0.1, 60, 0.1), allpred_umic, type="l", lty=1, ylim=c(0, 1000), col=rgb(0,0,0,alpha=0.1))
points(data_long_min$midpoint[data_long_min$Economic_Setting=="Upper middle income"], 
       data_long_min$inc[data_long_min$Economic_Setting=="Upper middle income"], pch=20, col="red")

# reformat to give to Lander + Xiao.
allpred_df_lmic = data.frame(pred=as.vector(allpred_lmic[seq(5, 595, 10),]), 
                             mos=rep(seq(0.5, 59.5, 1), times=dim(allpred_lmic)[2]), 
                             iter=rep(1:dim(allpred_lmic)[2], each=length(seq(0.5, 59.5, 1))))
allpred_df_umic = data.frame(pred=as.vector(allpred_umic[seq(5, 595, 10),]), 
                             mos=rep(seq(0.5, 59.5, 1), times=dim(allpred_umic)[2]), 
                             iter=rep(1:dim(allpred_umic)[2], each=length(seq(0.5, 59.5, 1))))

write.csv(allpred_df_lmic, file=paste(plotpre_out, "epicode01a_comm_inc/inc_lmic_predictions.csv", sep=""))
write.csv(allpred_df_umic, file=paste(plotpre_out, "epicode01a_comm_inc/inc_umic_predictions.csv", sep=""))
save(allpred_lmic, allpred_df_lmic, file=paste(plotpre_out, "epicode01a_comm_inc/inc_lmic_predictions.Rdata", sep=""))
save(allpred_umic, allpred_df_umic, file=paste(plotpre_out, "epicode01a_comm_inc/inc_umic_predictions.Rdata", sep=""))

#*******************************
# Fit vs. obs plots ------------
#*******************************

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_lmic_fit_obs.jpeg", sep=""),
     width = 7, height = 10, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower middle income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_lmic_fit_obs_log.jpeg", sep=""), 
     width = 7, height = 10, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower middle income",], 
       aes(x=midpoint, y=inc+0.1)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  theme(strip.text = element_text(size=6, face="bold")) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="rosybrown2", alpha=0.15) + 
  geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci+0.1, ymax=inc_hci+0.1), width=.25) +
  xlab("Age in months") + ylab("Cases per 1,000 person-years (log scale)") + 
  scale_y_log10(breaks=10**c(-1:3), labels = c("0.1","1", "10", "100", "1000"), limits = c(0.1, 1000)) 
dev.off()

allpred_df_global_short = allpred_df_global[allpred_df_global$mos %in% seq(0.5, 12.5, 0.5), ]
allpred_df_global_short$pred[allpred_df_global_short$pred<0.1] = 0.1

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_lmic_fit_obs_log_babies.jpeg", sep=""), 
     width = 7, height = 10, units = "in", # pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower middle income",], 
       aes(x=midpoint, y=inc+0.1)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  theme(strip.text = element_text(size=6, face="bold")) +
  geom_line(data=allpred_df_global_short[allpred_df_global_short$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="rosybrown2", alpha=0.15) + 
  geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 12.5), breaks=seq(0,12,1)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci+0.1, ymax=inc_hci+0.1), width=.25) +
  xlab("Age in months") + ylab("Cases per 1,000 person-years (log scale)") + 
  scale_y_log10(breaks=10**c(-1:3), labels = c("0.1","1", "10", "100", "1000"), limits = c(0.1, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_umic_fit_obs.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", # pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Upper middle income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  theme(strip.text = element_text(size=6, face="bold")) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000))  
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_umic_fit_obs_log.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", # pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Upper middle income",], 
       aes(x=midpoint, y=log10(inc+0.1))) + themebar2 +
  theme(panel.grid.minor =  element_blank()) + 
  theme(strip.text = element_text(size=6, face="bold")) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=log10(pred+0.1), group=iter), col="rosybrown2", alpha=0.15) + 
  geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=log10(pred+0.1), group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=log10(inc_lci+0.1), ymax=log10(inc_hci+0.1)), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(-1, 3), breaks=-1:3, labels = c(0.1, 1, 10, 100, 1000))  
dev.off()

#*******************************
# Validation -------------------
# from places where there are <3 age 
# groups worth of data
#*******************************

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_lmic_fit_obs_val.jpeg", sep=""), 
     width = 5, height = 2.5, units = "in", # pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Lower middle income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, y=inc, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_lmic_fit_obs_log10_val.jpeg", sep=""), 
     width = 5, height = 2.5, units = "in", # pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Lower middle income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="rosybrown2", alpha=0.15) + 
  geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, y=inc+0.1, ymin=inc_lci+0.1, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_log10(limits = c(0.1, 1000), 
                breaks=c(0.1, 1, 10, 100, 1000),
                labels=scales::comma) 
dev.off()

# jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_lic_fit_obs_val.jpeg", sep=""), 
#      width = 3, height = 2.5, units = "in", # pointsize = 12,
#      quality = 100, bg = "white", res = 600)
# ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Low income",], 
#        aes(x=midpoint, y=inc)) + themebar2 +
#   geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
#   geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
#   geom_point(size=1) + facet_wrap(~Title, ncol=4) + 
#   scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
#   geom_errorbar(aes(x=midpoint, y=inc, ymin=inc_lci, ymax=inc_hci), width=.25) + 
#   xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
#   scale_y_continuous(limits = c(0, 1000)) 
# dev.off()

# jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_lic_fit_obs_log10_val.jpeg", sep=""), 
#      width = 3, height = 2.5, units = "in", # pointsize = 12,
#      quality = 100, bg = "white", res = 600)
# ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Low income",], 
#        aes(x=midpoint, y=inc)) + themebar2 +
#   theme(panel.grid.minor =  element_blank()) +
#   geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="rosybrown2", alpha=0.15) + 
#   geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred+0.1, group=iter), col="lightskyblue", alpha=0.09) + 
#   geom_point(size=1) + facet_wrap(~Title, ncol=4) + 
#   scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
#   geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
#   xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
#   scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000),
#                      labels=scales::comma) 
# dev.off()

#*******************************
# Ribbon plots -----------------
#*******************************

load(file=paste(plotpre_out, "epicode01a_comm_inc/inc_global_predictions.Rdata", sep=""))
load(file=paste(plotpre_out, "epicode01a_comm_inc/inc_lmic_predictions.Rdata", sep=""))
load(file=paste(plotpre_out, "epicode01a_comm_inc/inc_umic_predictions.Rdata", sep=""))

# columns: lowci, hici, mos, econ
tmp1 = data.frame(t(apply(allpred_lmic, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp1) = c("est", "lowci", "hici")
tmp1$mean = apply(allpred_lmic, 1, mean)
tmp1$Economic_setting = "Low income"
tmp2 = data.frame(t(apply(allpred_lmic, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp2) = c("est", "lowci", "hici")
tmp2$mean = apply(allpred_lmic, 1, mean)
tmp2$Economic_setting = "Lower middle income"
tmp3 = data.frame(t(apply(allpred_umic, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(tmp3) = c("est", "lowci", "hici")
tmp3$mean = apply(allpred_umic, 1, mean)
tmp3$Economic_setting = "Upper middle income"
# tmp4 = data.frame(t(apply(allpred_hic, 1, quantile, c(0.5, 0.025, 0.975))))
# colnames(tmp4) = c("est", "lowci", "hici")
# tmp4$Economic_setting = "High income"

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

region_labels = c(`Low income` = "Low income countries\n(LIC)", 
                  `Lower middle income` = "Lower-middle income countries\n(LMIC)",
                  `Upper middle income` = "Upper-middle income countries\n(UMIC)")

region_labels = c(`Low income` = "LIC", 
                  `Lower middle income` = "LMIC",
                  `Upper middle income` = "UMIC")

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_ribbons.jpeg", sep=""), 
     width = 2.5, height = 7.5, units = 'in', res=600)
ggplot(data=inc_ribbons, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "lightskyblue") +
  geom_line(aes(y=mean), color="mediumblue", linetype="dashed") + 
  geom_line(aes(y=est), color="mediumblue") + 
  geom_ribbon(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici), 
              alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(data=inc_ribbons_global, aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(data=inc_ribbons_global, aes(y=est), color="red4") + 
  facet_wrap(Economic_setting~., labeller=labeller(Economic_setting=as_labeller(region_labels)), ncol=1) + # ncol=4
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_x_continuous(breaks=seq(0, 60, 12)) + 
  scale_y_continuous(breaks=seq(0, 1000, 200)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 1000))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_ribbons_log10.jpeg", sep=""), 
     width = 2.5, height = 7.5, units = 'in', res=600)
ggplot(data=inc_ribbons, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "lightskyblue") +
  geom_line(aes(y=mean), color="mediumblue", linetype="dashed") + 
  geom_line(aes(y=est), color="mediumblue") + 
  geom_ribbon(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici), 
              alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(data=inc_ribbons_global, aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(data=inc_ribbons_global, aes(y=est), color="red4") + 
  facet_wrap(Economic_setting~.,labeller=labeller(Economic_setting=as_labeller(region_labels)), ncol=1) + # ncol=4
  xlab("Age in months") + ylab("Cases per 1,000 person-years (log-scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_log10(breaks=c(0.1, 1, 10, 100, 1000), 
              labels = c("0.1", "1", "10", "100", "1,000")) + 
  coord_cartesian(ylim=c(0.1,1000))
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_global_ribbons.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(breaks=seq(0, 300, 100), limits = c(0, 300)) 
dev.off()

inc_ribbons_global$lowci[inc_ribbons_global$lowci<1] = 1
inc_ribbons_global$hici[inc_ribbons_global$hici<1] = 1

jpeg(filename = paste(plotpre_fig, "epicode01a_comm_inc/inc_global_ribbons_log10.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=inc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  theme(panel.grid.minor =  element_blank()) +
  geom_ribbon(alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(aes(y=est), color="red4") + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years (log-scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_log10(limits = c(1, 1000), breaks=c(1, 10, 100, 1000), 
                labels = c("1", "10", "100", "1000")) 
dev.off()
