# RSV in LIC, LMIC, and UMICs. -----
### Created by Marina Antillon, December 2022
### University of Antwerp

#************************************
## 00 - set working directory ----
#************************************
setwd("/Users/Marina/My Drive/Gavi RSV/code/epicode_OSF")

#************************************
## Make directories --------------
## Making directories to store output if they doesn't already exist.
#************************************

if (!dir.exists(file.path("out_figs"))){
  dir.create(file.path("out_figs"))
}

if (!dir.exists(file.path("out_sims"))){
  dir.create(file.path("out_sims"))
}

#************************************
## 01 - incidence splines ---------
#************************************

rm(list=ls())
midpt_sensitivity = F
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
source("./code/epicode00b_config.R")
bs_type = "ts"
n_iter = 5000
set.seed(20190118)
source("./code/epicode01a_comm_inc.R", print.eval = T)

#************************************
## 01B - severity in community studies ---
# as probability of all cases -- severe and very severe
#************************************
rm(list=ls())
midpt_sensitivity = F
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
source("./code/epicode00b_config.R")
bs_type = "ts"
n_iter = 5000
set.seed(20190118)
source("./code/epicode01b_comm_prob_sev.R", print.eval = T)

#************************************
## 11 - hospitalization probability ---
# as probability that a community case is hospitalized
#************************************
rm(list=ls())
midpt_sensitivity = F
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
source("./code/epicode00b_config.R")
bs_type = "ts"
n_iter = 5000
set.seed(20190118)
source("./code/epicode11_hosp_prob.R", print.eval = T)

#************************************
## 12a - hospitalized incidence ---
#************************************
rm(list=ls())
midpt_sensitivity = F
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
source("./code/epicode00b_config.R")
bs_type = "ts"
n_iter = 5000
set.seed(20190118)
source("./code/epicode12a_hospinc.R", print.eval = T)

#************************************
## 12b - severity in hospital studies ---
# as probability that hospitalized cases are severe or very severe
#************************************
rm(list=ls())
midpt_sensitivity = F
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
source("./code/epicode00b_config.R")
bs_type = "ts"
n_iter = 5000
set.seed(20190118)
source("./code/epicode12b_hospinc_sev.R", print.eval = T)

#*********************************************************************
## 20s - CFR among hospitalized cases ---------------------------------
#*********************************************************************
rm(list=ls())
midpt_sensitivity = F
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
source("./code/epicode00b_config.R")
bs_type = "ts"
n_iter = 5000
set.seed(20190118)
source("./code/epicode21_cfr.R", print.eval = T)

#******************************************************************
## 30s Study summaries --------------------------------------------
#******************************************************************
rm(list=ls())
midpt_sensitivity = F
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")

# To make Table 1
source("./code/epicode31_study_summaries.R")
# I got the summaries that R would give me 
# and entered them into the latex table by hand.

# To make Table A in Appendix S1.1
source("./code/epicode32a_study_summaries_supp.R")
source("./code/epicode32b_study_summaries_supp.R")

#******************************************************************
## 42 Results: Model summaries -----------------------------------
#******************************************************************

rm(list=ls())
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")

if (!dir.exists(file.path(paste(plotpre_out, "epicode42_modelsummaries", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode42_modelsummaries", sep="")))}

source("./code/epicode42a_modelsummaries.R", print.eval = T)
source("./code/epicode42b_modelsummaries_supp.R", print.eval = T)

write.csv(chi_table, file=paste0(plotpre_out, "epicode42_modelsummaries/model_summaries_chi2.csv"))
write.csv(AIC_table, file=paste0(plotpre_out, "epicode42_modelsummaries/model_summaries_AIC.csv"))
write.csv(chi_table_sev, file=paste0(plotpre_out, "epicode42_modelsummaries/model_summaries_chi2_sev.csv"))
write.csv(AIC_table_sev, file=paste0(plotpre_out, "epicode42_modelsummaries/model_summaries_AIC_sev.csv"))

#******************************************************************
## 41 Results: Splines of each outcome -------------------------------
#******************************************************************

rm(list=ls())
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")
source("./code/epicode41a_base_ribbons_together.R", print.eval = T) 
source("./code/epicode41b_base_ribbons_together_supp.R", print.eval = T) 

#******************************************************************
## 44 Results: Critical windows of infection -------------------------
#******************************************************************

rm(list=ls())
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")
n_iter = 5000
set.seed(20190118)
a=Sys.time()
source("./code/epicode43_bring_in_pop.R")
source("./code/epicode44a_time_to_perc_cases.R", print.eval = T) 
  # because of large arrays being saved, it takes more than 2.973 minutes to run
b=Sys.time()

save(peak, meanage, age50p, # burden, 
     pop_all, pop_lic, pop_lmic, pop_umic,
     file = paste(plotpre_out, "epicode44a_time_to_perc_cases/burden_results2.Rdata", sep=""))
# these three are now saved (and promptly removed) within the file 
# because they are too big: 
# country_incomegroup, country_global, burden. That resolves some RAM issues.

# For severity (sev and v.sev among comm and hosp cases)
rm(list=ls())
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")
n_iter = 5000
set.seed(20190118)
a=Sys.time()
source("./code/epicode43_bring_in_pop.R")
source("./code/epicode44b_time_to_perc_cases_supp.R", print.eval = T)
b=Sys.time() #~4.095 minutes on the apple air with m1 chip

save(peak, meanage, age50p, 
     pop_all, pop_lic, pop_lmic, pop_umic, 
     file = paste(plotpre_out, "epicode44b_time_to_perc_cases/burden_sev_results2.Rdata", sep=""))

# these three are now saved (and promptly removed) within the file 
# because they are too big: 
# country_incomegroup, country_global, burden That resolves some RAM issues.

#******************************************************************
## 45 Results: Burden in UMICs, LMICs, and LICs ----------------------
#******************************************************************

rm(list=ls())
gc()
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")
load(paste0(plotpre_out, "epicode44a_time_to_perc_cases/burden_results.Rdata"))
load(paste0(plotpre_out, "epicode44a_time_to_perc_cases/burden_results2.Rdata"))
n_iter = 5000
set.seed(20190118)
source("./code/epicode42_bring_in_pop.R")

if (!dir.exists(file.path(paste(plotpre_out, "epicode45_burden_calculations", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode45_burden_calculations", sep="")))}

source("./code/epicode45a_burden_calculations.R", print.eval = T)
write.csv(td_bu1_fmt_wide, file=paste(plotpre_out, "epicode45_burden_calculations/All_models_totals_global_splines.csv", sep=""))
write.csv(td_bu2_fmt_wide, file=paste(plotpre_out, "epicode45_burden_calculations/All_models_totals_econ_splines.csv", sep=""))
write.csv(td_bu3_fmt_wide, file=paste(plotpre_out, "epicode45_burden_calculations/All_models_totals_econ_splines_byage.csv", sep=""))

rm(list=ls())
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")
n_iter = 5000
set.seed(20190118)
load(paste(plotpre_out, "epicode45_burden_calculations/td_bu1.Rdata", sep=""))
load(paste(plotpre_out, "epicode45_burden_calculations/td_bu2.Rdata", sep=""))
load(paste(plotpre_out, "epicode45_burden_calculations/td_bu3.Rdata", sep=""))

if (!dir.exists(file.path(paste(plotpre_fig, "epicode45_burden_figures", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode45_burden_figures", sep="")))}

source("./code/epicode45b_burden_figures.R", print.eval = T)

#******************************************************************
# Make ribbon plots of global burden (agg and disagg) -------------
#******************************************************************

rm(list=ls())
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")
n_iter = 5000
set.seed(20190118)
source("./code/epicode43_bring_in_pop.R")
load(paste(plotpre_out, "epicode44a_time_to_perc_cases/burden_results.Rdata", sep=""))
load(paste(plotpre_out, "epicode44a_time_to_perc_cases/burden_results2.Rdata", sep=""))
source("./code/epicode46a_ribbons.R", print.eval = T)

# Make ribbon plots of burden for severe disease ------ 
rm(list=ls())
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")
n_iter = 5000
set.seed(20190118)
source("./code/epicode42_bring_in_pop.R")
load(paste(plotpre_out, "epicode44b_time_to_perc_cases_supp/burden_sev_results1.Rdata", sep=""))
load(paste(plotpre_out, "epicode44b_time_to_perc_cases_supp/burden_sev_results2.Rdata", sep=""))
source("./code/epicode46b_ribbons_supp.R", print.eval = T)

#******************************************************************
## Results: make an excel sheet for each country. -----------------
#******************************************************************

# For main analysis
rm(list=ls())
gc()
plotpre_out = "./out_sims/"
plotpre_fig = "./out_figs/"
plotpre_out_hinc = "./out_sims/"
source("./code/epicode00b_config.R")
load(paste0(plotpre_out, "epicode44a_time_to_perc_cases/burden_results.Rdata"))
load(paste0(plotpre_out, "epicode44a_time_to_perc_cases/burden_results2.Rdata"))
n_iter = 5000
set.seed(20190118)
source("./code/epicode43_bring_in_pop.R")
source("./code/epicode47_country_specific_estimates.R")
