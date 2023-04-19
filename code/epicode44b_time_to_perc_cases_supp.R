#******************************************************************
# *Peak, mean, and median age of infection* -----------------------
#******************************************************************

## Calculate the time before 50% of cases happen, 70%, etc.
#### x-axis: percent, and y-axis is the months.
#### this is less simple because we should take into account background mortality...

#******************************************************************
# Make directories when necessary ---------------------------------
#******************************************************************

if (!dir.exists(file.path(paste(plotpre_fig, "epicode44b_time_to_perc_cases_supp", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode44b_time_to_perc_cases_supp", sep="")))
}

if (!dir.exists(file.path(paste(plotpre_out, "epicode44b_time_to_perc_cases_supp", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode44b_time_to_perc_cases_supp", sep="")))
}

#******************************************************************
# Bring in spline results -----------------------------------------
# This time for severity.
#******************************************************************

## Load the above into arrays to be able write succinct code
comm_sev_sp = array(NA, dim=c(600, n_iter, 4), 
                        dimnames=list(age=NULL, iterations=NULL, 
                        region=c("All", "LIC", "LMIC", "UMIC")))
comm_vsev_sp = hosp_sev_sp = hosp_vsev_sp = comm_sev_sp
inc_sp = hospinc_sp = comm_sev_sp
hosp_prob1_sp = hosp_prob2_sp = hosp_prob3_sp = comm_sev_sp

# severity among community cases
load(paste(plotpre_out, "epicode01b_comm_prob_sev/inc_sev_global_predictions.Rdata", sep=""))
comm_sev_sp[,,"All"] = comm_sev_sp[,,"LIC"] = comm_sev_sp[,,"LMIC"] = comm_sev_sp[,,"UMIC"] = allpred

load(paste(plotpre_out, "epicode01b_comm_prob_sev/inc_vsev_global_predictions.Rdata", sep=""))
comm_vsev_sp[,,"All"] = comm_vsev_sp[,,"LIC"] = comm_vsev_sp[,,"LMIC"] = comm_vsev_sp[,,"UMIC"] = allpred

# severity among hospital cases
load(paste(plotpre_out, "epicode12b_hosp_prob_sev/hospinc_sev_global_predictions.Rdata", sep=""))
hosp_sev_sp[,,"All"] = hosp_sev_sp[,,"LIC"] = hosp_sev_sp[,,"LMIC"] = hosp_sev_sp[,,"UMIC"] = allpred

load(paste(plotpre_out_hinc, "epicode12b_hosp_prob_sev/hospinc_vsev_global_predictions.Rdata", sep=""))
load(paste(plotpre_out_hinc, "epicode12b_hosp_prob_sev/hospinc_vsev_lic_predictions.Rdata", sep=""))
load(paste(plotpre_out_hinc, "epicode12b_hosp_prob_sev/hospinc_vsev_lmic_predictions.RData", sep=""))
load(paste(plotpre_out_hinc, "epicode12b_hosp_prob_sev/hospinc_vsev_umic_predictions.RData", sep=""))
hosp_vsev_sp[,,"All"] = allpred
hosp_vsev_sp[,,"LIC"] = allpred_lic
hosp_vsev_sp[,,"LMIC"] = allpred_lmic
hosp_vsev_sp[,,"UMIC"] = allpred_umic

# baseline incidence; divide inc by 1000
load(paste(plotpre_out, "epicode01a_comm_inc/inc_global_predictions.RData", sep=""))
inc_sp[,,"All"] = allpred/1000
load(paste(plotpre_out, "epicode01a_comm_inc/inc_lmic_predictions.RData", sep=""))
inc_sp[,,"LIC"] = inc_sp[,,"LMIC"] = allpred_lmic/1000
load(paste(plotpre_out, "epicode01a_comm_inc/inc_umic_predictions.RData", sep=""))
inc_sp[,,"UMIC"] = allpred_umic/1000

# hospital probability
# the one without the Homaira data
load(paste(plotpre_out, "epicode11_hosp_prob/hosp_global_predictions.Rdata", sep=""))
hosp_prob2_sp[,,"All"] = hosp_prob2_sp[,,"LIC"] = hosp_prob2_sp[,,"LMIC"] = hosp_prob2_sp[,,"UMIC"] = allpred

# hospital incidence; divide hosp inc by 1000
load(paste(plotpre_out_hinc, "epicode12a_hospinc/hospinc_global_predictions.RData", sep=""))
load(paste(plotpre_out_hinc, "epicode12a_hospinc/hospinc_lic_predictions.RData", sep=""))
load(paste(plotpre_out_hinc, "epicode12a_hospinc/hospinc_lmic_predictions.RData", sep=""))
load(paste(plotpre_out_hinc, "epicode12a_hospinc/hospinc_umic_predictions.RData", sep=""))
hospinc_sp[,,"All"] = hospinc_sp[,,"LIC"] = hospinc_sp[,,"LMIC"] = hospinc_sp[,,"UMIC"] = allpred/1000


# hospinc_sp[,,"All"] = allpred/1000
# hospinc_sp[,,"LIC"] = allpred_lic/1000
# hospinc_sp[,,"LMIC"] = allpred_lmic/1000
# hospinc_sp[,,"UMIC"] = allpred_umic/1000

# rm(list = ls(, pattern = "all"))

#******************************************************************
# Initialize arrays for calculations ------------------------------
#******************************************************************

# Load community incidence and hospital incidence, because those are nec for 
# the number of severe cases. 
# We could do up to 2x4 of these: 2 est of comm_sev, 2 comm_vsev, 2 hosp_sev, 2 hosp_vsev
# We could do up to 4x4 of these: BM I & II each in comb with OM I & II, 4 outcomes...

peak = array(NA, dim=c(iterations = n_iter, outcome=4, model=2, region=4),
            dimnames=list(iterations=NULL, outcome=c("comm_sev", "comm_vsev", "hosp_sev", "hosp_vsev"),
                          model=c("com_inc", "hosp_inc"), 
                          region=c("All", "LIC", "LMIC", "UMIC")))

meanage = age50p = peak

# for burden, we need to leave in a dimension for age.
burden = array(NA, dim=c(age=600, iterations=n_iter, outcome=4, model=2, region=4),
             dimnames=list(age = 0:599/10,
                           iterations=NULL, outcome=c("comm_sev", "comm_vsev", "hosp_sev", "hosp_vsev"),
                           model=c("com_inc", "hosp_inc"), 
                           region=c("All", "LIC", "LMIC", "UMIC")))

#******************************************************************
# Calculate summaries (mean, median, peak age of infection) -------
#******************************************************************

for (i in 1:4){ # the region 
#  for (j in 1:3){ # assumptions about hospitalization

   if (i==1){
      # For global estimates
      agebinperc = rep(lxt$ALL$pop[1:60], each=10)/sum(rep(lxt$ALL$pop[1:60], each=10))
      pop = pop_all
    } else if (i==2) {
      # LIC
      agebinperc = rep(lxt$LIC$pop[1:60], each=10)/sum(rep(lxt$LIC$pop[1:60], each=10))
      pop = pop_lic
    } else if (i==3){
      # LMIC
      agebinperc = rep(lxt$LMIC$pop[1:60], each=10)/sum(rep(lxt$LMIC$pop[1:60], each=10))
      pop = pop_lmic
    } else {
      # UMIC
      agebinperc = rep(lxt$UMIC$pop[1:60], each=10)/sum(rep(lxt$UMIC$pop[1:60], each=10))
      pop = pop_umic
    }
   
     hosp_probx_sp = hosp_prob2_sp
   
  # community severe
  outcome = inc_sp[,,i]*comm_sev_sp[,,i]
  peak[,"comm_sev", "com_inc", i] = apply(outcome, 2, "which.max")/10
  age50p[,"comm_sev", "com_inc", i] = apply(abs(cumsum(data.frame(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/")))-0.5), 2, "which.min")/10
  burden[,,"comm_sev", "com_inc", i] = sweep(outcome, 1, agebinperc, "*")*pop
  meanage[,"comm_sev", "com_inc", i] = apply(sweep(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/"), 1, seq(0.1, 60, 0.1), "*"), 2, "sum")
  
  # community very severe
  outcome = inc_sp[,,i]*comm_vsev_sp[,,i]
  peak[,"comm_vsev", "com_inc", i] = apply(outcome, 2, "which.max")/10
  age50p[,"comm_vsev", "com_inc", i] = apply(abs(cumsum(data.frame(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/")))-0.5), 2, "which.min")/10
  burden[,,"comm_vsev", "com_inc", i] = sweep(outcome, 1, agebinperc, "*")*pop
  meanage[,"comm_vsev", "com_inc", i] = apply(sweep(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/"), 1, seq(0.1, 60, 0.1), "*"), 2, "sum")
  
  outcome = inc_sp[,,i]*hosp_probx_sp[,,i]*hosp_sev_sp[,,i]
  peak[,"hosp_sev", "com_inc", i] = apply(outcome, 2, "which.max")/10
  age50p[,"hosp_sev", "com_inc", i] = apply(abs(cumsum(data.frame(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/")))-0.5), 2, "which.min")/10
  burden[,,"hosp_sev", "com_inc", i] = sweep(outcome, 1, agebinperc, "*")*pop
  meanage[,"hosp_sev", "com_inc", i] = apply(sweep(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/"), 1, seq(0.1, 60, 0.1), "*"), 2, "sum")
  
  outcome = inc_sp[,,i]*hosp_probx_sp[,,i]*hosp_vsev_sp[,,i]
  peak[,"hosp_vsev", "com_inc", i] = apply(outcome, 2, "which.max")/10
  age50p[,"hosp_vsev", "com_inc", i] = apply(abs(cumsum(data.frame(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/")))-0.5), 2, "which.min")/10
  burden[,,"hosp_vsev", "com_inc", i] = sweep(outcome, 1, agebinperc, "*")*pop
  meanage[,"hosp_vsev", "com_inc", i] = apply(sweep(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/"), 1, seq(0.1, 60, 0.1), "*"), 2, "sum")
  
  # hosp_inc model...
  outcome = hospinc_sp[,,i]/hosp_probx_sp[,,i]*comm_sev_sp[,,i]
  peak[,"comm_sev", "hosp_inc", i] = apply(outcome, 2, "which.max")/10
  age50p[,"comm_sev", "hosp_inc", i] = apply(abs(cumsum(data.frame(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/")))-0.5), 2, "which.min")/10
  burden[,,"comm_sev", "hosp_inc", i] = sweep(outcome, 1, agebinperc, "*")*pop
  meanage[,"comm_sev", "hosp_inc", i] = apply(sweep(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/"), 1, seq(0.1, 60, 0.1), "*"), 2, "sum")

  outcome = hospinc_sp[,,i]/hosp_probx_sp[,,i]*comm_vsev_sp[,,i]
  peak[,"comm_vsev", "hosp_inc", i] = apply(outcome, 2, "which.max")/10
  age50p[,"comm_vsev", "hosp_inc", i] = apply(abs(cumsum(data.frame(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/")))-0.5), 2, "which.min")/10
  burden[,,"comm_vsev", "hosp_inc", i] = sweep(outcome, 1, agebinperc, "*")*pop
  meanage[,"comm_vsev", "hosp_inc", i] = apply(sweep(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/"), 1, seq(0.1, 60, 0.1), "*"), 2, "sum")
  
  outcome = hospinc_sp[,,i]*hosp_sev_sp[,,i]
  peak[,"hosp_sev", "hosp_inc", i] = apply(outcome, 2, "which.max")/10
  age50p[,"hosp_sev", "hosp_inc", i] = apply(abs(cumsum(data.frame(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/")))-0.5), 2, "which.min")/10
  burden[,,"hosp_sev", "hosp_inc", i] = sweep(outcome, 1, agebinperc, "*")*pop
  meanage[,"hosp_sev", "hosp_inc", i] = apply(sweep(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/"), 1, seq(0.1, 60, 0.1), "*"), 2, "sum")
  
  outcome = hospinc_sp[,,i]*hosp_vsev_sp[,,i]
  peak[,"hosp_vsev", "hosp_inc", i] = apply(outcome, 2, "which.max")/10
  age50p[,"hosp_vsev", "hosp_inc", i] = apply(abs(cumsum(data.frame(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/")))-0.5), 2, "which.min")/10
  burden[,,"hosp_vsev", "hosp_inc", i] = sweep(outcome, 1, agebinperc, "*")*pop
  meanage[,"hosp_vsev", "hosp_inc", i] = apply(sweep(sweep(sweep(outcome, 1, agebinperc, "*"), 2, apply(sweep(outcome, 1, agebinperc, "*"), 2, "sum"),"/"), 1, seq(0.1, 60, 0.1), "*"), 2, "sum")

#  } # cycle through hospitalization probability assumptions
} # cycle through Economic_settings


save(burden, file = paste(plotpre_out, "epicode44b_time_to_perc_cases_supp/burden_sev_results1.Rdata", sep=""))

#******************************************************************
# Final summaries: global - level analyses ------------------------
#******************************************************************

# # sanity check - burden, global
# quantile(apply(burden[,,"comm_sev", "com_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"comm_sev", "hosp_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"comm_sev", "hosp_inc", "All", "Nokes only"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"comm_sev", "hosp_inc", "All", "Nokes, Zur"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# 
# quantile(apply(burden[,,"comm_vsev", "com_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"comm_vsev", "hosp_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"comm_vsev", "hosp_inc", "All", "Nokes only"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"comm_vsev", "hosp_inc", "All", "Nokes, Zur"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# 
# quantile(apply(burden[,,"hosp_sev", "com_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"hosp_sev", "com_inc", "All", "Nokes, Zur"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"hosp_sev", "com_inc", "All", "Nokes only"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"hosp_sev", "hosp_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# 
# quantile(apply(burden[,,"hosp_vsev", "com_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"hosp_vsev", "com_inc", "All", "Nokes, Zur"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"hosp_vsev", "com_inc", "All", "Nokes only"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# quantile(apply(burden[,,"hosp_vsev", "hosp_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# 
# # sanity check: peak incidence - lic
# quantile(peak[,"comm_sev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"comm_sev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"comm_vsev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"comm_vsev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"hosp_sev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"hosp_sev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"hosp_vsev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"hosp_vsev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# # LMIC, peak
# quantile(peak[,"comm_sev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"comm_sev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"comm_vsev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"comm_vsev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"hosp_sev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"hosp_sev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"hosp_vsev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"hosp_vsev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# # UMIC, peak age
# quantile(peak[,"comm_sev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"comm_sev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"comm_vsev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"comm_vsev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"hosp_sev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"hosp_sev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(peak[,"hosp_vsev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(peak[,"hosp_vsev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# ## sanity check: age to 50% of cases - LIC
# quantile(age50p[,"comm_sev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"comm_sev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"comm_vsev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"comm_vsev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"hosp_sev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"hosp_sev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"hosp_vsev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"hosp_vsev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# # sanity check: age to 50% of cases - LMIC
# quantile(age50p[,"comm_sev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"comm_sev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"comm_vsev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"comm_vsev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"hosp_sev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"hosp_sev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"hosp_vsev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"hosp_vsev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# # sanity check: age to 50% of cases - UMIC
# quantile(age50p[,"comm_sev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"comm_sev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"comm_vsev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"comm_vsev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"hosp_sev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"hosp_sev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(age50p[,"hosp_vsev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(age50p[,"hosp_vsev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# # LIC
# ## mean age of infection, hospitalization, deaths
# quantile(meanage[,"comm_sev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"comm_sev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"comm_vsev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"comm_vsev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"hosp_sev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"hosp_sev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"hosp_vsev", "hosp_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"hosp_vsev", "com_inc", "LIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# # LMIC
# ## mean age of infection, hospitalization, deaths
# quantile(meanage[,"comm_sev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"comm_sev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"comm_vsev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"comm_vsev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"hosp_sev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"hosp_sev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"hosp_vsev", "hosp_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"hosp_vsev", "com_inc", "LMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# # UMIC
# ## mean age of infection, hospitalization, deaths
# quantile(meanage[,"comm_sev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"comm_sev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"comm_vsev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"comm_vsev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"hosp_sev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"hosp_sev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# 
# quantile(meanage[,"hosp_vsev", "hosp_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))
# quantile(meanage[,"hosp_vsev", "com_inc", "UMIC", "All studies"], c(0.5, 0.025, 0.975))

#******************************************************************
# Make summaries --------------------------------------------------
#******************************************************************

# easier to make this super table(s), 1 for econ and one for global splines) 
# and then to make the summary.

peak_summary = apply(peak, 2:4, quantile, c(0.5, 0.025, 0.975))
names(dimnames(peak_summary))[1] = "measure"
peak_summary_df = peak_summary %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=measure, values_from = value) 
peak_summary_df$measure = "Peak age"

# sanity check:
unique(peak_summary_df[,2:4])

## mean
mean_summary = apply(meanage, 2:4, quantile, c(0.5, 0.025, 0.975))
names(dimnames(mean_summary))[1] = "measure"
mean_summary_df = mean_summary %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=measure, values_from = value) 
mean_summary_df$measure = "Mean age"

# sanity check:
unique(mean_summary_df[,2:4])

# Median age
median_summary = apply(age50p, 2:4, quantile, c(0.5, 0.025, 0.975))
names(dimnames(median_summary))[1] = "measure"
median_summary_df = median_summary %>% as.tbl_cube(met_name = "value") %>% as_tibble %>% 
  pivot_wider(names_from=measure, values_from = value) 
median_summary_df$measure = "Median age"

# sanity check:
unique(median_summary_df[,2:4])

pmm_df = rbind(peak_summary_df, mean_summary_df, median_summary_df)
pmm_df$outcome = factor(pmm_df$outcome, levels=c("comm_sev", "comm_vsev", "hosp_sev", "hosp_vsev"))
levels(pmm_df$outcome) = c("Community severe", "Community very severe", "Hospital severe", "Hospital very severe")
levels(pmm_df$model) = c("Outcome model (OM) I",
                         "Outcome model (OM) II")
colnames(pmm_df)[4:6] = c("est", "lci", "hci")

# Now plot them all
pmm_df$model2 = factor(pmm_df$model)
pmm_df$model2 = factor(pmm_df$model2, levels = rev(levels(pmm_df$model2)),
                                      labels = rev(levels(pmm_df$model)))

pmm_df$lbl = apply(pmm_df[,c("est", "lci", "hci")], 1, ci_string_dec, 1)
pmm_df$textpos = 0.5*(pmm_df$lci + pmm_df$hci)
pmm_df$textpos[pmm_df$textpos<6] = 6

p = ggplot(data=pmm_df, 
           aes(x=outcome, y=est, colour=model2)) +
  themebar2 +
  theme(axis.text = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white")) + 
  geom_point(position = position_dodge(width = 1), size=1) + 
  geom_errorbar(aes(ymin = lci, ymax=hci), width=.2, position = position_dodge(width = 1)) + 
  facet_grid(region~measure) + 
  geom_text(aes(y=textpos, label=lbl), position = position_dodge(width = 1), vjust=-0.65, hjust=0.5, size=2.8, show.legend = FALSE, fontface="bold") +
  scale_colour_manual(values=c("darkorange2", "forestgreen")) + 
  xlab("") + ylab("Age in months") + coord_flip(clip="on") + 
  scale_x_discrete(limits = rev(levels(pmm_df$outcome))) +
  scale_y_continuous(breaks=seq(0, 36, 12), limits = c(0, 36),
                     minor_breaks = seq(6, 30, 12)) + 
  guides(colour=guide_legend(title = "", title.position="left", reverse=T)) 

ggsave(paste(plotpre_fig, "epicode44b_time_to_perc_cases_supp/peak_mean_median_restricted.eps", sep=""), 
       plot = grid.draw(p), device="eps", width=7, height=8.5, units="in")


#******************************************************************
# Combination of the global analyses and country-level model ------
#******************************************************************

# For the countries missing country-level data, 
# just assum it is equal to the global or to the economic_setting.
country_global = array(NA, dim=c(age = 60, iterations = n_iter, outcome = 4, 
                                 model = 2, ISO3 = length(final_ISO3$ISO3)), 
                       dimnames=list(age = 0:59, iterations = NULL, 
                                     outcome = c("comm_sev", "comm_vsev", "hosp_sev", "hosp_vsev"), 
                                     model = c("com_inc", "hosp_inc"), ISO3 = final_ISO3$ISO3))
# country_incomegroup = country_global

# c("Community severe", "Community very severe", "Hospital severe", "Hospital very severe")

cmodel = matrix(NA, length(final_ISO3$ISO3), n_iter)

# from Li's country-by-country model
for(i in 1:length(final_ISO3$ISO3)){
  # if (!is.na(countryBOD$lci[countryBOD$ISO3==final_ISO3[i]])){
    tmp_country = countryBOD[countryBOD$ISO3==final_ISO3$ISO3[i], c("ISO3", "est", "lci", "hci", "Year2020")]
    cmodel[i,] = rlnorm(n_iter, log(tmp_country$est), (log(tmp_country$est)-log(tmp_country$lci))/1.96)
  # } else {
  #   tmp_country = countryBOD[countryBOD$ISO3=="AFG", c("ISO3", "est", "lci", "hci", "Year2020")]
  #   cmodel[i,] = rlnorm(n_iter, log(tmp_country$est), (log(tmp_country$est)-log(tmp_country$lci))/1.96)
  # }
}

# calculate using the global splines for both models
rm("burden") # opening up space in the RAM

for(i in 1:length(final_ISO3$ISO3)){
#  for(j in 1:3){ # hospitalization probability data assumptions
    tmp_pop = as.numeric(lxt_df[lxt_df$ISO3==final_ISO3$ISO3[i],2:61])/sum(as.numeric(lxt_df[lxt_df$ISO3==final_ISO3$ISO3[i],2:61]))*popA$pop_2020[popA$ISO3==final_ISO3$ISO3[i]]
    
      hosp_probx_sp = hosp_prob2_sp
    
    # if (!is.na(countryBOD$lci[countryBOD$ISO3==final_ISO3[i]])){
    
    ### com_inc: Incidence from the community-based studies.
    # Cases if the country had the same incidence as the spline
    # cases = incidence per 1000 * population by age
    tmp_rsv_cases = sweep(inc_sp[seq(1, 599, 10),,"All"], 1, tmp_pop, "*")
    # percent of cases that fall in each month of age = cases / total cases (per column)
    tmp_rsv_percent_agemonth = sweep(tmp_rsv_cases, 2, colSums(tmp_rsv_cases), "/")
    # now the cases in each month of age assuming the incidence predicted in Shi's country-level model
    country_global[,,"comm_sev", "com_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*comm_sev_sp[seq(1, 599, 10),,"All"]
    country_global[,,"comm_vsev", "com_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*comm_vsev_sp[seq(1, 599, 10),,"All"]
    country_global[,,"hosp_sev", "com_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*hosp_probx_sp[seq(1, 599, 10),,"All"]*hosp_sev_sp[seq(1, 599, 10),,"All"]
    country_global[,,"hosp_vsev", "com_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*hosp_probx_sp[seq(1, 599, 10),,"All"]*hosp_vsev_sp[seq(1, 599, 10),,"All"]
    
    rm("tmp_rsv_cases", "tmp_rsv_percent_agemonth")
    ### hosp_inc: Incidence from the hospital-based studies.
    # Cases if the country had the same incidence as the spline
    # cases = incidence per 1000 * population by age
    tmp_rsv_cases = sweep(hospinc_sp[seq(1, 599, 10),,"All"]/hosp_probx_sp[seq(1, 599, 10),,"All"], 1, tmp_pop, "*")
    # percent of cases that fall in each month of age = cases / total cases (per column)
    tmp_rsv_percent_agemonth = sweep(tmp_rsv_cases, 2, colSums(tmp_rsv_cases), "/")
    # now the cases in each month of age assuming the incidence predicted in Shi's country-level model
    country_global[,,"comm_sev", "hosp_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*comm_sev_sp[seq(1, 599, 10),,"All"]
    country_global[,,"comm_vsev", "hosp_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*comm_vsev_sp[seq(1, 599, 10),,"All"]
    country_global[,,"hosp_sev", "hosp_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*hosp_probx_sp[seq(1, 599, 10),,"All"]*hosp_sev_sp[seq(1, 599, 10),,"All"]
    country_global[,,"hosp_vsev", "hosp_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*hosp_probx_sp[seq(1, 599, 10),,"All"]*hosp_vsev_sp[seq(1, 599, 10),,"All"]
    
    # rsv rate = rsv cases / population # for pure incidence
    # country_rsv_pred_rate = sweep(country_rsv_pred_cases, 1, tmp_pop, "/")
    # } else { # for countries with no incidence data, assume the spline incidence...
    #   country_global[,,"disease", "com_inc", final_ISO3[i],j] = sweep(inc_sp[seq(1, 599, 10),,"All"], 1, tmp_pop, "*")
    #   country_global[,,"hosp", "com_inc", final_ISO3[i],j] = country_global[,,"disease", "com_inc", final_ISO3[i],j]*hosp_probx_sp[seq(1, 599, 10),,"All"]
    #   country_global[,,"deaths", "com_inc", final_ISO3[i],j] = country_global[,,"hosp", "com_inc", final_ISO3[i],j]*cfr_sp[seq(1, 599, 10),,"All"]
    #   
    #   country_global[,,"disease", "hosp_inc", final_ISO3[i],j] = sweep(hospinc_sp[seq(1, 599, 10),,"All"]/hosp_probx_sp[seq(1, 599, 10),,"All"], 1, tmp_pop, "*")
    #   country_global[,,"hosp", "hosp_inc", final_ISO3[i],j] = country_global[,,"disease", "hosp_inc", final_ISO3[i],j]*hosp_probx_sp[seq(1, 599, 10),,"All"]
    #   country_global[,,"deaths", "hosp_inc", final_ISO3[i],j] = country_global[,,"hosp", "hosp_inc", final_ISO3[i],j]*cfr_sp[seq(1, 599, 10),,"All"]
    # }
#  }
}

save(country_global, 
     file = paste(plotpre_out, "epicode44b_time_to_perc_cases_supp/burden_results_country_global.Rdata", sep=""))
country_incomegroup = country_global
rm("country_global")

gc()

# Now use the spline from the country's economic development stratum
for(i in 1:length(final_ISO3$ISO3)){
  
  if (countryBOD$Year2020[countryBOD$ISO3==final_ISO3$ISO3[i]]=="L"){
    region = "LIC"
  } else {if (countryBOD$Year2015[countryBOD$ISO3==final_ISO3$ISO3[i]]=="LM"){
    region = "LMIC"
  } else {
    region = "UMIC"
  }}
  
  for(j in 1:3){ # hospitalization probability data assumptions
    tmp_pop = as.numeric(lxt_df[lxt_df$ISO3==final_ISO3$ISO3[i],2:61])/sum(as.numeric(lxt_df[lxt_df$ISO3==final_ISO3$ISO3[i],2:61]))*popA$pop_2020[popA$ISO3==final_ISO3$ISO3[i]]
    
    hosp_probx_sp = hosp_prob2_sp
    ### com_inc: Incidence from the community-based studies.
    # Cases if the country had the same incidence as the spline
    # cases = incidence per 1000 * population by age
    tmp_rsv_cases = sweep(inc_sp[seq(1, 599, 10),,region], 1, tmp_pop, "*")
    # percent of cases that fall in each month of age = cases / total cases (per column)
    tmp_rsv_percent_agemonth = sweep(tmp_rsv_cases, 2, colSums(tmp_rsv_cases), "/")
    # now the cases in each month of age assuming the incidence predicted in Shi's country-level model
    country_incomegroup[,,"comm_sev", "com_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*comm_sev_sp[seq(1, 599, 10),,region]
    country_incomegroup[,,"comm_vsev", "com_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*comm_vsev_sp[seq(1, 599, 10),,region]
    country_incomegroup[,,"hosp_sev", "com_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*hosp_probx_sp[seq(1, 599, 10),,region]*hosp_sev_sp[seq(1, 599, 10),,region]
    country_incomegroup[,,"hosp_vsev", "com_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*hosp_probx_sp[seq(1, 599, 10),,region]*hosp_vsev_sp[seq(1, 599, 10),,region]
    
    ### hosp_inc: Incidence from the hospital-based studies.
    # Cases if the country had the same incidence as the spline
    # cases = incidence per 1000 * population by age
    tmp_rsv_cases = sweep(hospinc_sp[seq(1, 599, 10),,region]/hosp_probx_sp[seq(1, 599, 10),,region], 1, tmp_pop, "*")
    # percent of cases that fall in each month of age = cases / total cases (per column)
    tmp_rsv_percent_agemonth = sweep(tmp_rsv_cases, 2, colSums(tmp_rsv_cases), "/")
    # now the cases in each month of age assuming the incidence predicted in Shi's country-level model
    country_incomegroup[,,"comm_sev", "hosp_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*comm_sev_sp[seq(1, 599, 10),,region]
    country_incomegroup[,,"comm_vsev", "hosp_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*comm_vsev_sp[seq(1, 599, 10),,region]
    country_incomegroup[,,"hosp_sev", "hosp_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*hosp_probx_sp[seq(1, 599, 10),,region]*hosp_sev_sp[seq(1, 599, 10),,region]
    country_incomegroup[,,"hosp_vsev", "hosp_inc", final_ISO3$ISO3[i]] = sweep(tmp_rsv_percent_agemonth, 2, cmodel[i,]/1000*sum(tmp_pop), "*")*hosp_probx_sp[seq(1, 599, 10),,region]*hosp_vsev_sp[seq(1, 599, 10),,region]
    
  }
}

save(country_incomegroup, 
     file = paste(plotpre_out, "epicode44b_time_to_perc_cases_supp/burden_results_country_incomegroup.Rdata", sep=""))
rm("country_incomegroup")

# Some sanity checks. Totals should look similar but not the same, as Shi gave us country inc estimates only.
# quantile(apply(country_incomegroup[,,"comm_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[,,"comm_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# quantile(apply(country_incomegroup[,,"comm_sev", "com_inc", final_ISO3$Year2015=="L","All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[,,"comm_sev", "com_inc", final_ISO3$Year2015=="L","All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# quantile(apply(country_incomegroup[,,"comm_sev", "com_inc", final_ISO3$Year2015=="LM","All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[,,"comm_sev", "com_inc", final_ISO3$Year2015=="LM","All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# quantile(apply(country_incomegroup[,,"comm_sev", "com_inc", final_ISO3$Year2015=="UM","All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[,,"comm_sev", "com_inc", final_ISO3$Year2015=="UM","All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# 
# quantile(apply(country_incomegroup[,,"hosp_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[,,"hosp_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# quantile(apply(country_incomegroup[,,"hosp_sev", "com_inc",,"Nokes, Zur"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[,,"hosp_sev", "com_inc",,"Nokes, Zur"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6

#******************************************************************
# Sanity check by age group (3 ages) ------------------------------
# for both sets of splines and 
# both kinds of models.
# HERE: any more sanity checks?
#******************************************************************

# Some sanity checks. Totals should look similar, but not 
# necessarily the same, since both models allocate cases 
# across age groups differently
# quantile(apply(country_incomegroup[1:6,,"comm_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[1:6,,"comm_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# quantile(apply(country_incomegroup[7:12,,"comm_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[7:12,,"comm_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# quantile(apply(country_incomegroup[13:60,,"comm_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[13:60,,"comm_sev", "com_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# # now with hosp_inc # some whacky things in the younger age group
# quantile(apply(country_incomegroup[1:6,,"comm_sev", "hosp_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[1:6,,"comm_sev", "hosp_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# quantile(apply(country_incomegroup[7:12,,"comm_sev", "hosp_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[7:12,,"comm_sev", "hosp_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# 
# quantile(apply(country_incomegroup[13:60,,"comm_sev", "hosp_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[13:60,,"comm_sev", "hosp_inc",,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6

#******************************************************************
