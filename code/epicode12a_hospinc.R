# Incidence of HOSPITALIZED RSV cases

# note that not all high-income countries are "industrialized" and that one industrialized country is UMIC
# table(inc_shi$Economic_setting, inc_shi$Development_status)

######################
## Make directories to 
## store output if it 
## doesn't already exist.
######################

if (!dir.exists(file.path(paste(plotpre_fig, "epicode12a_hospinc", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode12a_hospinc", sep="")))
}

if (!dir.exists(file.path(paste(plotpre_out, "epicode12a_hospinc", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode12a_hospinc", sep="")))
}

# *********************
## Read in new clean data ----
# *********************

# For hospitalization
data_list = list()

for (i in 1:52){
  data_list[[i]] = read.xlsx("./data/HospInc_data_R_readable.xlsx", i)
}

# rbind them together
data_long = data_list[[41]]
data_long = data_long[is.na(data_long$Cases),]

for (i in 1:52){
  data_long = rbind.fill(data_long, data_list[[i]])
}

# *********************
# Assign midpoints ----
# *********************

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

data_long[is.na(data_long$midpoint),]

## Titles for graphs
# data_long$Location = as.character(data_long$Location)
data_long$Title = paste(data_long$Location, "\n(", data_long$Author, ", ", data_long$Year, ")", 
                       "\n", data_long$Study_period, sep="")

######################
# round all cases to 
# nearest whole (some are 
# X.9995 for some reason)
######################
data_long$Cases = round(data_long$Cases)
data_long$Pop = round(data_long$Pop)

data_long = data_long[data_long$Economic_Setting!="High income",]
data_long$Economic_Setting = factor(data_long$Economic_Setting)
# data_long$Economic_Setting = factor(as.character(data_long$Economic_Setting), levels(data_long$Economic_Setting)[c(3,1,2)])
levels(data_long$Economic_Setting)

######################
## calculate incidence and CIs
######################
data_long$inc = data_long$Cases/data_long$Pop*1000
data_long$inc = apply(cbind(data_long$inc, rep(0.1, length(data_long$inc))), 1, "max")

data_long$inc_lci = NaN
data_long$inc_hci = NaN
data_long[!is.na(!data_long$Cases) & !is.na(data_long$Pop), c("inc_lci", "inc_hci")] = 
  pois.exact(data_long$Cases[!is.na(data_long$Cases) & !is.na(data_long$Pop)], 
             data_long$Pop[!is.na(!data_long$Cases) & !is.na(data_long$Pop)], 0.95)[,c("lower", "upper")]*1000

data_long$inc_lci = apply(cbind(data_long$inc_lci, rep(0.1, length(data_long$inc_lci))), 1, "max")

# ********************
# Spline, global -----
# ********************

data_long_min = data_long[!is.na(data_long$Cases),]
data_long_min$study_no = as.numeric(as.factor(data_long_min$Title))
data_long_min_all = data_long_min # to graph the studies that were excluded from the analysis
data_long_min = data_long_min[data_long_min$study_no %in% as.numeric(names(table(data_long_min$study_no)))[table(data_long_min$study_no)>2],]
data_long_min_all = data_long_min_all[data_long_min_all$study_no %in% as.numeric(names(table(data_long_min_all$study_no)))[table(data_long_min_all$study_no)<3],]

write.csv(data_long_min, paste(plotpre_out, "/data_long_hospinc_train.csv"))
write.csv(data_long_min_all, paste(plotpre_out, "./data_long_hospinc_val.csv"))

data_long_min$study_no = factor(as.numeric(as.factor(data_long_min$study_no)))

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

tmp2 = table(data_long_min$study_no, data_long_min$Economic_Setting)
tmp2[tmp2==0] = NA 
median(tmp2[,1], na.rm=T) 
median(tmp2[,2], na.rm=T)
median(tmp2[,3], na.rm=T) 

# nknots = round(min(median(tmp2[,1], na.rm=T), median(tmp2[,2], na.rm=T), median(tmp2[,3], na.rm=T)))
data_long_min$dummy=1

b_gamm = gamm4(Cases~s(log(midpoint), k=-1, bs=bs_type) +
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=dummy)+offset(log(Pop)),
               random=~(1|study_no), data=data_long_min, family=poisson, REML=T)

v_gamm = gamm4(Cases~s(log(midpoint), k=-1, bs=bs_type, by=Economic_Setting) + Economic_Setting + 
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=dummy)+offset(log(Pop)),
               random=~(1|study_no), data=data_long_min, family=poisson, REML=T)

v1_gamm = gamm4(Cases~s(log(midpoint), k=-1, bs=bs_type, by=Economic_Setting) + Economic_Setting + 
                 t2(log(midpoint), study_no, bs=c(bs_type, 're'), by=Economic_Setting)+offset(log(Pop)),
               random=~(Economic_Setting|study_no), data=data_long_min, family=poisson, REML=T)

modcomp = anova(v_gamm$mer, b_gamm$mer, v1_gamm$mer)
write.csv(modcomp, file=paste(plotpre_out, "epicode12a_hospinc/modcomp.csv", sep=""))

newpred=data.frame(midpoint=seq(0.1, 60, 0.1), Pop=1000, study_no=1, dummy=0)
bpred = data.frame(predict.gam(b_gamm$gam, newdata=newpred, se.fit=T))
bpred$lfit = bpred$fit-1.96*bpred$se.fit
bpred$ufit = bpred$fit+1.96*bpred$se.fit

bpred=cbind(newpred, bpred)

matplot(bpred$midpoint,exp(as.matrix(bpred[,c("fit", "lfit", "ufit")])), 
        type="l", lty=1, ylim=c(0,100))
points(data_long_min$midpoint, data_long_min$inc, pch=20)

# this predicts the basis at new values
tmp=predict.gam(b_gamm$gam, newdata=newpred, type="lpmatrix", se.fit=T) 
coef(b_gamm$gam)
b_gamm$gam$Vp # seems to be the same as vcov(b$gam,unconditional=TRUE)
# gam.check(b_gamm$gam)
# uncertainty for smoothing parameter:
b_gamm$gam$Vc 
# Under ML or REML smoothing parameter estimation it is 
# possible to correct the covariance matrix Vp for smoothing 
# parameter uncertainty. This is the corrected version.

somebetas = rmvnorm(n=n_iter, coef(b_gamm$gam), b_gamm$gam$Vp)
someiterates = (tmp %*% t(somebetas))
allpred = exp(someiterates)*1000
matplot(seq(0.1, 60, 0.1), allpred, type="l", ylim=c(0, 1000), lty=1, col=rgb(0,0,0,alpha=0.01))
points(data_long_min$midpoint, data_long_min$inc, pch=20, col="red")

allpred_df_global = data.frame(pred=as.vector(allpred[seq(5, 595, 10),]), 
                               mos=rep(seq(0.5, 59.5, 1), times=dim(allpred)[2]), 
                               iter=rep(1:dim(allpred)[2], each=length(seq(0.5, 59.5, 1))))

write.csv(allpred_df_global, file=paste(plotpre_out, "epicode12a_hospinc/hospinc_global_predictions.csv", sep=""))
save(allpred_df_global, allpred, file=paste(plotpre_out, "epicode12a_hospinc/hospinc_global_predictions.Rdata", sep=""))

# **************************
# Plot, Global estimate ----
# **************************

for(i in 1:3){
jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_global_fit_obs", LETTERS[i], ".jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", 
     quality = 100, bg = "transparent", res = 600)
print(ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[((i-1)*9+1):(i*9)],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)))
dev.off()
}

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_global_fit_obs", LETTERS[4], ".jpeg", sep=""), 
     width = 7, height = 9.5, units = "in", 
     quality = 100, bg = "transparent", res = 600)
print(ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[28:37],], 
             aes(x=midpoint, y=inc)) + themebar2 +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
        geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
        xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
        scale_y_continuous(limits = c(0, 1000)))
dev.off()

for(i in 1:3){
  jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_global_fit_obs", LETTERS[i], "_log10.jpeg", sep=""), 
       width = 7, height = 7.5, units = "in", 
       quality = 100, bg = "transparent", res = 600)
  print(ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[((i-1)*9+1):(i*9)],], 
               aes(x=midpoint, y=inc)) + themebar2 +
          geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
          geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
          scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
          geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Log-cases per 1,000 person-years") + 
    scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                  labels = c("0.1", "1", "10", "100", "1,000")))
  dev.off()
}

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_global_fit_obs", LETTERS[4], "_log10.jpeg", sep=""), 
     width = 7, height = 9.5, units = "in", 
     quality = 100, bg = "transparent", res = 600)
print(ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title)[28:37],], 
             aes(x=midpoint, y=inc)) + themebar2 +
        geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
        geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
        scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
        geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
        xlab("Age in months") + ylab("Log-cases per 1,000 person-years") + 
        scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                      labels = c("0.1", "1", "10", "100", "1,000")))
dev.off()

# *************************
# By Economic Setting -----
# Final analysis showed econ setting was not a significant predictor
# *************************
# tmp = rev(unique(data_long_min[,c("Economic_Setting")]))
# newdata = data.frame(midpoint=rep(seq(0.1, 60, 0.1), 3), 
#                      Pop=rep(1000, length(seq(0.1, 60, 0.1))*3),
#                      Economic_Setting = rep(tmp, each=length(seq(0.1, 60, 0.1))),
#                      study_no=2, dummy=0)
# vpred = data.frame(predict.gam(v1_gamm$gam, newdata=newdata, se.fit=T))
# vpred$lfit = vpred$fit-1.96*vpred$se.fit
# vpred$ufit = vpred$fit+1.96*vpred$se.fit
# 
# vpred=cbind(newdata, vpred)
# 
# par(mfrow=c(1,3))
# matplot(vpred$midpoint[vpred$Economic_Setting=="Lower income"],
#         exp(as.matrix(vpred[vpred$Economic_Setting=="Lower income",c("fit", "lfit", "ufit")])),
#         ylim=c(0, 100), type="l", lty=1)
# # points(data_long_min$midpoint[data_long_min$Economic_Setting=="Lower income"],
# #        data_long_min$inc[data_long_min$Economic_Setting=="Lower income"], pch=20)
# 
# matplot(vpred$midpoint[vpred$Economic_Setting=="Lower middle income"],
#         exp(as.matrix(vpred[vpred$Economic_Setting=="Lower middle income", c("fit", "lfit", "ufit")])),
#         ylim=c(0, 100), type="l", lty=1)
# # points(data_long_min$midpoint[data_long_min$Economic_Setting=="Lower middle income"],
# #        data_long_min$inc[data_long_min$Economic_Setting=="Lower middle income"], pch=20)
# 
# matplot(vpred$midpoint[vpred$Economic_Setting=="Upper middle income"],
#         exp(as.matrix(vpred[vpred$Economic_Setting=="Upper middle income", c("fit", "lfit", "ufit")])),
#         ylim=c(0, 100), type="l", lty=1)
# # points(data_long_min$midpoint[data_long_min$Economic_Setting=="Upper middle income"],
# #        data_long_min$inc[data_long_min$Economic_Setting=="Upper middle income"], pch=20)
# 
# par(mfrow=c(1,1))
# 
# # this predicts the basis at new values
# keepspline=!str_detect(names(coef(v1_gamm$gam)), "study_no") # keep splines of main effect but not the study-specific effect
# tmp=predict.gam(v1_gamm$gam, newdata=newdata, type="lpmatrix")[,keepspline]
# somebetas = rmvnorm(n=n_iter, coef(v1_gamm$gam)[keepspline], v1_gamm$gam$Vp[keepspline,keepspline])
# someiterates = (tmp %*% t(somebetas))
# 
# allpred_lic = exp(someiterates[newdata$Economic_Setting=="Lower income",])*1000
# allpred_lmic = exp(someiterates[newdata$Economic_Setting=="Lower middle income",])*1000
# allpred_umic = exp(someiterates[newdata$Economic_Setting=="Upper middle income",])*1000
# 
# par(mfrow=c(1,3))
# matplot(seq(0.1, 60, 0.1), allpred_lic, type="l", lty = 1, col=rgb(0,0,0,alpha=.05), ylim=c(0, 500))
# matplot(seq(0.1, 60, 0.1), allpred_lmic, type="l", lty = 1, col=rgb(0,0,0,alpha=.05), ylim=c(0, 500))
# matplot(seq(0.1, 60, 0.1), allpred_umic, type="l", lty = 1, col=rgb(0,0,0,alpha=.05), ylim=c(0, 500))
# 
# par(mfrow=c(1,1))
# 
# allpred_df_lic = data.frame(pred=as.vector(allpred_lic[seq(5, 595, 10),]), 
#                              mos=rep(seq(0.5, 59.5, 1), times=dim(allpred_lic)[2]), 
#                              iter=rep(1:dim(allpred_lic)[2], each=length(seq(0.5, 59.5, 1))))
# allpred_df_lmic = data.frame(pred=as.vector(allpred_lmic[seq(5, 595, 10),]), 
#                              mos=rep(seq(0.5, 59.5, 1), times=dim(allpred_lmic)[2]), 
#                              iter=rep(1:dim(allpred_lmic)[2], each=length(seq(0.5, 59.5, 1))))
# allpred_df_umic = data.frame(pred=as.vector(allpred_umic[seq(5, 595, 10),]), 
#                              mos=rep(seq(0.5, 59.5, 1), times=dim(allpred_umic)[2]), 
#                              iter=rep(1:dim(allpred_umic)[2], each=length(seq(0.5, 59.5, 1))))
# 
# write.csv(allpred_df_lic, file=paste(plotpre_out, "epicode14_hospinc/hospinc_lic_predictions.csv", sep=""))
# write.csv(allpred_df_lmic, file=paste(plotpre_out, "epicode14_hospinc/hospinc_lmic_predictions.csv", sep=""))
# write.csv(allpred_df_umic, file=paste(plotpre_out, "epicode14_hospinc/hospinc_umic_predictions.csv", sep=""))
# 
# save(allpred_lic, allpred_df_lic, file=paste(plotpre_out, "epicode14_hospinc/hospinc_lic_predictions.Rdata", sep=""))
# save(allpred_lmic, allpred_df_lmic, file=paste(plotpre_out, "epicode14_hospinc/hospinc_lmic_predictions.Rdata", sep=""))
# save(allpred_umic, allpred_df_umic, file=paste(plotpre_out, "epicode14_hospinc/hospinc_umic_predictions.Rdata", sep=""))

# *************************
# Plots, fit vs obs ---
# *************************

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lic_fit_obs.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lic[allpred_df_lic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lic_fit_obs_log10.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Economic_Setting=="Lower income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lic[allpred_df_lic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100", "1,000")) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lmic_fit_obs_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title[data_long_min$Economic_Setting=="Lower middle income"])[1:9],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lmic_fit_obs_B.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title[data_long_min$Economic_Setting=="Lower middle income"])[10:14],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lmic_fit_obs_log10_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title[data_long_min$Economic_Setting=="Lower middle income"])[1:9],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Log-cases per 1,000 person-years") + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100", "1,000")) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lmic_fit_obs_log10_B.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title[data_long_min$Economic_Setting=="Lower middle income"])[10:14],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Log-cases per 1,000 person-years") + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100", "1,000")) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_umic_fit_obs_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title[data_long_min$Economic_Setting=="Upper middle income"])[1:9],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_umic_fit_obs_B.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title[data_long_min$Economic_Setting=="Upper middle income"])[10:18],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_umic_fit_obs_log10_A.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title[data_long_min$Economic_Setting=="Upper middle income"])[1:9],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  # geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, 
                    ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Log-cases per 1,000 person-years") + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100", "1,000")) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_umic_fit_obs_log10_B.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min[data_long_min$Title %in% unique(data_long_min$Title[data_long_min$Economic_Setting=="Upper middle income"])[10:18],], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) +  
  # geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) +  
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, 
                    ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Log-cases per 1,000 person-years") + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100", "1,000")) 
dev.off()

# *************************
# Validation --------------
# from places 
# where there are <3 age 
# groups worth of data
# Should these just be 
# scatter or bar graphs
# *************************

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lic_fit_obs_val.jpeg", sep=""), 
     width = 5, height = 3, units = "in", 
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Lower income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lic[allpred_df_lic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 100)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lic_fit_obs_val_log10.jpeg", sep=""), 
     width = 5, height = 3, units = "in",
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Lower income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lic[allpred_df_lic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years (log scale)") + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100", "1,000")) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lmic_fit_obs_val.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Lower middle income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 100)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_lmic_fit_obs_val_log10.jpeg", sep=""), 
     width = 7, height = 7.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Lower middle income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_lmic[allpred_df_lmic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100", "1,000")) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_umic_fit_obs_val.jpeg", sep=""), 
     width = 7, height = 5.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Upper middle income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_continuous(limits = c(0, 100)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_umic_fit_obs_val_log10.jpeg", sep=""),
     width = 7, height = 5.5, units = "in", pointsize = 12,
     quality = 100, bg = "white", res = 600)
ggplot(data=data_long_min_all[data_long_min_all$Economic_Setting=="Upper middle income",], 
       aes(x=midpoint, y=inc)) + themebar2 +
  geom_line(data=allpred_df_global[allpred_df_global$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="rosybrown2", alpha=0.15) + 
  # geom_line(data=allpred_df_umic[allpred_df_umic$iter %in% 1:300,], aes(x=mos, y=pred, group=iter), col="lightskyblue", alpha=0.09) + 
  geom_point(size=1) + facet_wrap(~Title, ncol=3) + 
  scale_x_continuous(limits = c(0, 60), breaks=seq(0,60,12)) +
  geom_errorbar(aes(x=midpoint, ymin=inc_lci, ymax=inc_hci), width=.25) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels=scales::comma) 
dev.off()

# *************************
## Ribbon plots --------
# *************************

# Final analysis showed econ setting was not a significant predictor
# columns: lowci, hici, mos, econ
# tmp1 = data.frame(t(apply(allpred_lic, 1, quantile, c(0.5, 0.025, 0.975))))
# colnames(tmp1) = c("est", "lowci", "hici")
# tmp1$mean = apply(allpred_lmic, 1, mean)
# tmp1$Economic_setting = "Lower income"
# tmp2 = data.frame(t(apply(allpred_lmic, 1, quantile, c(0.5, 0.025, 0.975))))
# colnames(tmp2) = c("est", "lowci", "hici")
# tmp2$mean = apply(allpred_lmic, 1, mean)
# tmp2$Economic_setting = "Lower middle income"
# tmp3 = data.frame(t(apply(allpred_umic, 1, quantile, c(0.5, 0.025, 0.975))))
# colnames(tmp3) = c("est", "lowci", "hici")
# tmp3$mean = apply(allpred_umic, 1, mean)
# tmp3$Economic_setting = "Upper middle income"

# hinc_ribbons = rbind(tmp1, tmp2, tmp3) # rbind(tmp1, tmp2, tmp3, tmp4)
# hinc_ribbons$mos = rep(seq(0.1, 60, 0.1), times=3)
# hinc_ribbons$Economic_setting = factor(hinc_ribbons$Economic_setting)
# hinc_ribbons$est[hinc_ribbons$est<0.1] = 0.1
# hinc_ribbons$mean[hinc_ribbons$mean<0.1] = 0.1
# hinc_ribbons$lowci[hinc_ribbons$lowci<0.1] = 0.1
# hinc_ribbons$est[hinc_ribbons$est>1000] = 1000
# hinc_ribbons$hici[hinc_ribbons$hici>1000] = 1000
# hinc_ribbons$mean[hinc_ribbons$mean>1000] = 1000

hinc_ribbons_global = data.frame(t(apply(allpred, 1, quantile, c(0.5, 0.025, 0.975))))
colnames(hinc_ribbons_global) = c("est", "lowci", "hici")
hinc_ribbons_global$mean = apply(allpred, 1, mean)
# cfr_ribbons_global$Economic_setting = "Global"
hinc_ribbons_global$mos = seq(0.1, 60, 0.1)
hinc_ribbons_global$est[hinc_ribbons_global$est<0.1] = 0.1
hinc_ribbons_global$mean[hinc_ribbons_global$mean<0.1] = 0.1
hinc_ribbons_global$lowci[hinc_ribbons_global$lowci<0.1] = 0.1
hinc_ribbons_global$est[hinc_ribbons_global$est>1000] = 1000
hinc_ribbons_global$hici[hinc_ribbons_global$hici>1000] = 1000
hinc_ribbons_global$mean[hinc_ribbons_global$mean>1000] = 1000

region_labels = c(`Lower income` = "LIC", 
                  `Lower middle income` = "LMIC",
                  `Upper middle income` = "UMIC")

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_ribbons.jpeg", sep=""), 
     width = 2.5, height =3, units = 'in', res=600)
ggplot(data=hinc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon(aes(x=mos, ymin=lowci, ymax=hici), 
              alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(data=hinc_ribbons_global, aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(data=hinc_ribbons_global, aes(y=est), color="red4") + 
  # geom_ribbon(data=hinc_ribbons, alpha=0.5, size=0, fill = "lightskyblue") +
  # geom_line(aes(y=mean), color="mediumblue", linetype="dashed") + 
  # geom_line(aes(y=est), color="mediumblue") +
  # facet_wrap(~Economic_setting, ncol=1, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_continuous(limits = c(0, 1000)) 
dev.off()

jpeg(filename = paste(plotpre_fig, "epicode12a_hospinc/hospinc_ribbons_log.jpeg", sep=""), 
     width = 2.5, height = 3, units = 'in', res=600)
ggplot(data=hinc_ribbons_global, aes(x=mos, ymin=lowci, ymax=hici)) + themebar2 +
  geom_ribbon( aes(x=mos, ymin=lowci, ymax=hici), 
              alpha=0.5, size=0, fill = "rosybrown1") +
  geom_line(data=hinc_ribbons_global, aes(y=mean), color="red4", linetype="dashed") + 
  geom_line(data=hinc_ribbons_global, aes(y=est), color="red4") + 
  # geom_ribbon(data=hinc_ribbons, alpha=0.5, size=0, fill = "lightskyblue") +
  # geom_line(aes(y=mean), color="mediumblue", linetype="dashed") + 
  # geom_line(aes(y=est), color="mediumblue") + 
  # facet_wrap(~Economic_setting, ncol=1, labeller=labeller(Economic_setting=as_labeller(region_labels))) + 
  xlab("Age in months") + ylab("Cases per 1,000 person-years (log-scale)") + 
  scale_x_continuous(breaks=seq(0, 60, 12), limits = c(0, 60)) + 
  scale_y_log10(limits = c(0.1, 1000), breaks=c(0.1, 1, 10, 100, 1000), 
                labels=scales::comma) 
dev.off()
