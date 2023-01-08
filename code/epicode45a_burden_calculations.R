#******************************************************************
# *Burden calculations* -------------------------------------------
#******************************************************************

#******************************************************************
# Make directories when necessary ---------------------------------
#******************************************************************

if (!dir.exists(file.path(paste(plotpre_fig, "epicode45_burden_calculations", sep="")))){
  dir.create(file.path(paste(plotpre_fig, "epicode45_burden_calculations", sep="")))
}

if (!dir.exists(file.path(paste(plotpre_out, "epicode45_burden_calculations", sep="")))){
  dir.create(file.path(paste(plotpre_out, "epicode45_burden_calculations", sep="")))
}

#******************************************************************
# TABLE (splines WITHOUT income group stratification, td_bu1) -----
#******************************************************************

# Top-down total cases model without income group specification
# a1=Sys.time()
td_all_ages_gl = apply(burden, 2:5, "sum") # this is just to get the dimensions I want. I am going to replace it all
# b1=Sys.time()

# a2=Sys.time()
td_all_ages_gl = Reduce(`+`,lapply(dimnames(burden)$age,function(name){burden[name,,,,]}))
# b2=Sys.time()

td_all_ages_gl[,,,"All"] = apply(burden[,,,,"All"], 2:4, "sum")
td_all_ages_gl[,,,"LIC"] = apply(sweep(burden[,,,,"All"], 1, rep(lxt$LIC$pop[1:60], each=10)/rep(lxt$ALL$pop[1:60], each=10)*pop_lic/pop_all, "*"), 2:4, "sum")
td_all_ages_gl[,,,"LMIC"] = apply(sweep(burden[,,,,"All"], 1, rep(lxt$LMIC$pop[1:60], each=10)/rep(lxt$ALL$pop[1:60], each=10)*pop_lmic/pop_all, "*"), 2:4, "sum")
td_all_ages_gl[,,,"UMIC"] = apply(sweep(burden[,,,,"All"], 1, rep(lxt$UMIC$pop[1:60], each=10)/rep(lxt$ALL$pop[1:60], each=10)*pop_umic/pop_all, "*"), 2:4, "sum")
gc()

# sanity check:
# quantile(td_all_ages_gl[,1,1,"All",1], c(.5, .025, .975))/1e6
# quantile(td_all_ages_gl[,1,1,"LIC",1], c(.5, .025, .975))/1e6
# quantile(td_all_ages_gl[,1,1,"LMIC",1], c(.5, .025, .975))/1e6
# quantile(td_all_ages_gl[,1,1,"UMIC",1], c(.5, .025, .975))/1e6
# quantile(rowSums(td_all_ages_gl[,1,1,2:4,1]), c(.5, .025, .975))/1e6
# compare with:
# quantile(apply(burden[,,"disease", "com_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# ok, that was correct.

# Bottom-up total cases model without income group specification

load(paste0(plotpre_out, "epicode44a_time_to_perc_cases/burden_results_country_global.Rdata"))

bu_all_ages_gl = td_all_ages_gl

bu_all_ages_gl[,,,"All"] = apply(country_global, c(2:4), "sum")
bu_all_ages_gl[,,,"LIC"] = apply(country_global[,,,,final_ISO3$Year2015=="L"], c(2:4), "sum")
bu_all_ages_gl[,,,"LMIC"] = apply(country_global[,,,,final_ISO3$Year2015=="LM"], c(2:4), "sum")
bu_all_ages_gl[,,,"UMIC"] = apply(country_global[,,,,final_ISO3$Year2015=="UM"], c(2:4), "sum")

td_all_ages_gl_summary_df = apply(td_all_ages_gl, 2:4, meanpi) %>% 
  cubelyr::as.tbl_cube(met_name="Val") %>% as_tibble %>% 
  pivot_wider(id_cols = c("outcome", "model", "region"),
              names_from = Var1, values_from = Val) 
  
bu_all_ages_gl_summary_df = apply(bu_all_ages_gl, 2:4, meanpi) %>% 
              cubelyr::as.tbl_cube(met_name="Val") %>% as_tibble %>% 
  pivot_wider(id_cols = c("outcome", "model", "region"),
              names_from = Var1, values_from = Val)
gc()

# td_all_ages_gl_summary_df$hosp_assume = as.character(td_all_ages_gl_summary_df$hosp_assume)
# td_all_ages_gl_summary_df$hosp_assume[(td_all_ages_gl_summary_df$outcome=="disease" & td_all_ages_gl_summary_df$model=="com_inc" & td_all_ages_gl_summary_df$hosp_assume=="All studies")|
#                                 (td_all_ages_gl_summary_df$outcome=="hosp" & td_all_ages_gl_summary_df$model=="hosp_inc" & td_all_ages_gl_summary_df$hosp_assume=="All studies")|
#                                 (td_all_ages_gl_summary_df$outcome=="deaths" & td_all_ages_gl_summary_df$model=="hosp_inc" & td_all_ages_gl_summary_df$hosp_assume=="All studies")] = "None"
# 
# td_all_ages_gl_summary_df$est[td_all_ages_gl_summary_df$outcome=="disease" & td_all_ages_gl_summary_df$model=="com_inc" & td_all_ages_gl_summary_df$hosp_assume!="None"] = NA
# td_all_ages_gl_summary_df$est[td_all_ages_gl_summary_df$outcome=="hosp" & td_all_ages_gl_summary_df$model=="hosp_inc" & td_all_ages_gl_summary_df$hosp_assume!="None"] = NA
# td_all_ages_gl_summary_df$est[td_all_ages_gl_summary_df$outcome=="deaths" & td_all_ages_gl_summary_df$model=="hosp_inc" & td_all_ages_gl_summary_df$hosp_assume!="None"] = NA

td_all_ages_gl_summary_df = td_all_ages_gl_summary_df[!is.na(td_all_ages_gl_summary_df$est),]

td_all_ages_gl_summary_df$lbl = apply(td_all_ages_gl_summary_df[,4:6], 1, "ci_string_dec_1M")
# td_all_ages_gl_summary_df$lbl[td_all_ages_gl_summary_df$outcome!="disease"] = apply(td_all_ages_gl_summary_df[td_all_ages_gl_summary_df$outcome!="disease",5:7]/1e3, 1, "ci_string_dec", 1)

td_all_ages_gl_summary_df$country_total = "Burden Model (BM) I"

# bu_all_ages_gl_summary_df$hosp_assume = as.character(bu_all_ages_gl_summary_df$hosp_assume)
# bu_all_ages_gl_summary_df$hosp_assume[(bu_all_ages_gl_summary_df$outcome=="disease" & bu_all_ages_gl_summary_df$model=="com_inc" & bu_all_ages_gl_summary_df$hosp_assume=="All studies")|
#                                         (bu_all_ages_gl_summary_df$outcome=="hosp" & bu_all_ages_gl_summary_df$model=="hosp_inc" & bu_all_ages_gl_summary_df$hosp_assume=="All studies")|
#                                         (bu_all_ages_gl_summary_df$outcome=="deaths" & bu_all_ages_gl_summary_df$model=="hosp_inc" & bu_all_ages_gl_summary_df$hosp_assume=="All studies")] = "None"
# 
# bu_all_ages_gl_summary_df$est[bu_all_ages_gl_summary_df$outcome=="disease" & bu_all_ages_gl_summary_df$model=="com_inc" & bu_all_ages_gl_summary_df$hosp_assume!="None"] = NA
# bu_all_ages_gl_summary_df$est[bu_all_ages_gl_summary_df$outcome=="hosp" & bu_all_ages_gl_summary_df$model=="hosp_inc" & bu_all_ages_gl_summary_df$hosp_assume!="None"] = NA
# bu_all_ages_gl_summary_df$est[bu_all_ages_gl_summary_df$outcome=="deaths" & bu_all_ages_gl_summary_df$model=="hosp_inc" & bu_all_ages_gl_summary_df$hosp_assume!="None"] = NA

bu_all_ages_gl_summary_df = bu_all_ages_gl_summary_df[!is.na(bu_all_ages_gl_summary_df$est),]

bu_all_ages_gl_summary_df$lbl = apply(bu_all_ages_gl_summary_df[,4:6], 1, "ci_string_dec_1M")
# bu_all_ages_gl_summary_df$lbl[bu_all_ages_gl_summary_df$outcome!="disease"] = apply(bu_all_ages_gl_summary_df[bu_all_ages_gl_summary_df$outcome!="disease",5:7]/1e3, 1, "ci_string_dec", 1)
gc()
bu_all_ages_gl_summary_df$country_total = "Burden Model (BM) II"

td_bu1 = rbind(td_all_ages_gl_summary_df, bu_all_ages_gl_summary_df) %>% 
  mutate(outcome = factor(outcome, levels=c("disease", "hosp", "deaths"), 
                          labels=c("Cases", # \n(labels in millions)", 
                                   "Hospitalizations", #\n(labels in thousands)",
                                   "In-hospital deaths"))) # \n(labels in thousands)")
td_bu1$outcome2 = factor(td_bu1$outcome, levels=rev(levels(td_bu1$outcome)))

td_bu1$model = factor(td_bu1$model)
levels(td_bu1$model) = c("Outcome Model (OM) I",
                          "Outcome Model (OM) II")

td_bu1$country_total = factor(td_bu1$country_total)
levels(td_bu1$country_total) = c("Burden Model (BM) I",
                                 "Burden Model (BM) II")

save(td_bu1, file=paste(plotpre_out, "epicode45_burden_calculations/td_bu1.Rdata", sep=""))

#******************************************************************
# TABLE (splines WITH income group specification, td_bu2) ---------
#******************************************************************

# Top-down total cases model with income group specification
td_all_ages_es = apply(burden, 2:5, "sum") 
# this is just to get the dimensions I want. I am going to replace some of it.

# make the "all category" the sum of the income groups
td_all_ages_es[,,,"All"] = apply(td_all_ages_es[,,,2:4], c(1:3), "sum") 

# # sanity check:
# quantile(td_all_ages_es[,1,1,"LIC",1], c(.5, .025, .975))/1e6
# quantile(td_all_ages_es[,1,1,"LMIC",1], c(.5, .025, .975))/1e6
# quantile(td_all_ages_es[,1,1,"UMIC",1], c(.5, .025, .975))/1e6
# # compare with:
# quantile(apply(td_all_ages_es[,"disease","com_inc",c("LIC", "LMIC", "UMIC"), "All studies"], 1, "sum"), c(.5, .025, .975))/1e6
# quantile(td_all_ages_es[,"disease","com_inc","All", "All studies"], c(.5, .025, .975))/1e6
# # ok, that was correct.
# # this doesn't have to be exact, but close is reassuring:
# quantile(apply(burden[,,"disease", "com_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# 
# quantile(td_all_ages_es[,1,2,"LIC",1], c(.5, .025, .975))/1e6
# quantile(td_all_ages_es[,1,2,"LMIC",1], c(.5, .025, .975))/1e6
# quantile(td_all_ages_es[,1,2,"UMIC",1], c(.5, .025, .975))/1e6
# # compare with:
# quantile(apply(td_all_ages_es[,"disease","hosp_inc",c("LIC", "LMIC", "UMIC"), "All studies"], 1, "sum"), c(.5, .025, .975))/1e6
# quantile(td_all_ages_es[,"disease","hosp_inc","All", "All studies"], c(.5, .025, .975))/1e6
# # this doesn't have to be exact, but close is reassuring:
# quantile(apply(burden[,,"disease", "hosp_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6

# Bottom-up total cases model with income group specification

rm(country_global)
load(paste(plotpre_out, "epicode44a_time_to_perc_cases/burden_results_country_incomegroup.Rdata", sep=""))

bu_all_ages_es = td_all_ages_es

bu_all_ages_es[,,,"All"] = apply(country_incomegroup, c(2:4), "sum")
bu_all_ages_es[,,,"LIC"] = apply(country_incomegroup[,,,,final_ISO3$Year2020=="L"], c(2:4), "sum")
bu_all_ages_es[,,,"LMIC"] = apply(country_incomegroup[,,,,final_ISO3$Year2020=="LM"], c(2:4), "sum")
bu_all_ages_es[,,,"UMIC"] = apply(country_incomegroup[,,,,final_ISO3$Year2020=="UM"], c(2:4), "sum")

td_all_ages_es_summary_df = apply(td_all_ages_es, 2:4, meanpi) %>% 
  cubelyr::as.tbl_cube(met_name="Val") %>% as_tibble %>% 
  pivot_wider(id_cols = c("outcome", "model", "region"),
              names_from = Var1, values_from = Val) 

bu_all_ages_es_summary_df = apply(bu_all_ages_es, 2:4, meanpi) %>% 
  cubelyr::as.tbl_cube(met_name="Val") %>% as_tibble %>% 
  pivot_wider(id_cols = c("outcome", "model", "region"),
              names_from = Var1, values_from = Val) 
gc()

# td_all_ages_es_summary_df$hosp_assume = as.character(td_all_ages_es_summary_df$hosp_assume)
# td_all_ages_es_summary_df$hosp_assume[(td_all_ages_es_summary_df$outcome=="disease" & td_all_ages_es_summary_df$model=="com_inc" & td_all_ages_es_summary_df$hosp_assume=="All studies")|
#                                         (td_all_ages_es_summary_df$outcome=="hosp" & td_all_ages_es_summary_df$model=="hosp_inc" & td_all_ages_es_summary_df$hosp_assume=="All studies")|
#                                         (td_all_ages_es_summary_df$outcome=="deaths" & td_all_ages_es_summary_df$model=="hosp_inc" & td_all_ages_es_summary_df$hosp_assume=="All studies")] = "None"
# 
# td_all_ages_es_summary_df$est[td_all_ages_es_summary_df$outcome=="disease" & td_all_ages_es_summary_df$model=="com_inc" & td_all_ages_es_summary_df$hosp_assume!="None"] = NA
# td_all_ages_es_summary_df$est[td_all_ages_es_summary_df$outcome=="hosp" & td_all_ages_es_summary_df$model=="hosp_inc" & td_all_ages_es_summary_df$hosp_assume!="None"] = NA
# td_all_ages_es_summary_df$est[td_all_ages_es_summary_df$outcome=="deaths" & td_all_ages_es_summary_df$model=="hosp_inc" & td_all_ages_es_summary_df$hosp_assume!="None"] = NA

td_all_ages_es_summary_df = td_all_ages_es_summary_df[!is.na(td_all_ages_es_summary_df$est),]
td_all_ages_es_summary_df$lbl = apply(td_all_ages_es_summary_df[,4:6], 1, "ci_string_dec_1M")

td_all_ages_es_summary_df$country_total = "Burden Model (BM) I"

# bu_all_ages_es_summary_df$hosp_assume[(bu_all_ages_es_summary_df$outcome=="disease" & bu_all_ages_es_summary_df$model=="com_inc" & bu_all_ages_es_summary_df$hosp_assume=="All studies")|
#                                         (bu_all_ages_es_summary_df$outcome=="hosp" & bu_all_ages_es_summary_df$model=="hosp_inc" & bu_all_ages_es_summary_df$hosp_assume=="All studies")|
#                                         (bu_all_ages_es_summary_df$outcome=="deaths" & bu_all_ages_es_summary_df$model=="hosp_inc" & bu_all_ages_es_summary_df$hosp_assume=="All studies")] = "None"
# 
# bu_all_ages_es_summary_df$est[bu_all_ages_es_summary_df$outcome=="disease" & bu_all_ages_es_summary_df$model=="com_inc" & bu_all_ages_es_summary_df$hosp_assume!="None"] = NA
# bu_all_ages_es_summary_df$est[bu_all_ages_es_summary_df$outcome=="hosp" & bu_all_ages_es_summary_df$model=="hosp_inc" & bu_all_ages_es_summary_df$hosp_assume!="None"] = NA
# bu_all_ages_es_summary_df$est[bu_all_ages_es_summary_df$outcome=="deaths" & bu_all_ages_es_summary_df$model=="hosp_inc" & bu_all_ages_es_summary_df$hosp_assume!="None"] = NA

bu_all_ages_es_summary_df = bu_all_ages_es_summary_df[!is.na(bu_all_ages_es_summary_df$est),]
bu_all_ages_es_summary_df$lbl = apply(bu_all_ages_es_summary_df[,4:6], 1, "ci_string_dec_1M")

bu_all_ages_es_summary_df$country_total = "Burden Model (BM) II"

td_bu2 = rbind(td_all_ages_es_summary_df, bu_all_ages_es_summary_df) %>% 
              mutate(outcome = factor(outcome, levels=c("disease", "hosp", "deaths"), 
                          labels=c("Cases", # \n(labels in millions)", 
                                   "Hospitalizations", #\n(labels in thousands)",
                                   "In-hospital deaths"))) # \n(labels in thousands)")
td_bu2$outcome2 = factor(td_bu2$outcome, levels=rev(levels(td_bu2$outcome)))
  
td_bu2$model = factor(td_bu2$model)
levels(td_bu2$model) = c("Outcome Model (OM) I",
                         "Outcome Model (OM) II")

td_bu2$country_total = factor(td_bu2$country_total)

td_bu2$model2 = paste(td_bu2$model, "; ", td_bu2$country_total, sep="")
td_bu2$model2 = factor(td_bu2$model2)
td_bu2$model2 = factor(td_bu2$model2, levels = rev(levels(td_bu2$model2)))

# line below doesn't work; must make factor first
td_bu2$region = factor(td_bu2$region)
td_bu2$region2 = factor(td_bu2$region, levels=rev(levels(td_bu2$region)))
td_bu2$outcome = factor(td_bu2$outcome)
td_bu2$outcome2 = factor(td_bu2$outcome, levels=rev(levels(td_bu2$outcome)))

td_bu2$textpos = 10^(0.5*(log10(td_bu2$lci) + log10(td_bu2$hci)))

save(td_bu2, file=paste(plotpre_out, "epicode45_burden_calculations/td_bu2.Rdata", sep=""))

#******************************************************************
# TABLE (splines WITH income group  AND age, td_bu3) --------------
#******************************************************************

# Top-down total cases model with income group specification
td_by_ages_es = abind(apply(burden[1:600,,,,], 2:5, "sum"),
                      apply(burden[1:59,,,,], 2:5, "sum"), 
                      apply(burden[59:119,,,,], 2:5, "sum"),
                      apply(burden[120:600,,,,], 2:5, "sum"), along=0)

td_by_ages_es[,,,,"All"] = apply(td_by_ages_es[,,,,2:4], c(1:4), "sum")

names(dim(td_by_ages_es)) = names(dim(burden))
dimnames(td_by_ages_es)[1] = list(ages = c("Ages: All", "Ages:\n0m-5.9m", "Ages:\n6m-11.9m", "Ages:\n12m-59m"))
dimnames(td_by_ages_es)[2:5] = dimnames(burden)[2:5]
names(dimnames(td_by_ages_es)) = names(dim(td_by_ages_es))

# sanity check:
# quantile(td_by_ages_es[1,,"disease", "com_inc","LIC",1], c(.5, .025, .975))/1e6
# quantile(td_by_ages_es[1,,"disease", "com_inc","LMIC",1], c(.5, .025, .975))/1e6
# quantile(td_by_ages_es[1,,"disease", "com_inc","UMIC",1], c(.5, .025, .975))/1e6
# quantile(rowSums(td_by_ages_es[1,,"disease", "com_inc",2:4,"All studies"]), c(.5, .025, .975))/1e6
# quantile(td_by_ages_es[1,,"disease", "com_inc","All",1], c(.5, .025, .975))/1e6
# # compare with:
# quantile(apply(burden[1:59,,"disease", "com_inc", "All", "All studies"], 2, "sum"), c(0.5, 0.025, 0.975))/1e6
# # there's a bit of a difference.

# Bottom-up total cases model with income group specification
bu_by_ages_es = td_by_ages_es
gc()
bu_by_ages_es[2:4,,,,"All"] = abind(apply(country_incomegroup[1:6,,,,], c(2:4), "sum"),
                                  apply(country_incomegroup[7:12,,,,], c(2:4), "sum"),
                                  apply(country_incomegroup[13:60,,,,], c(2:4), "sum"), along=0)
bu_by_ages_es[1,,,,"All"] = bu_by_ages_es[2,,,,"All"]+bu_by_ages_es[3,,,,"All"]+bu_by_ages_es[4,,,,"All"]

bu_by_ages_es[2:4,,,,"LIC"] = abind(apply(country_incomegroup[1:6,,,,final_ISO3$Year2020=="L"], c(2:4), "sum"),
                              apply(country_incomegroup[7:12,,,,final_ISO3$Year2020=="L"], c(2:4), "sum"),
                              apply(country_incomegroup[13:60,,,,final_ISO3$Year2020=="L"], c(2:4), "sum"), along=0)
bu_by_ages_es[1,,,,"LIC"] = bu_by_ages_es[2,,,,"LIC"]+bu_by_ages_es[3,,,,"LIC"]+bu_by_ages_es[4,,,,"LIC"]

bu_by_ages_es[2:4,,,,"LMIC"] = abind(apply(country_incomegroup[1:6,,,,final_ISO3$Year2020=="LM"], c(2:4), "sum"),
                              apply(country_incomegroup[7:12,,,,final_ISO3$Year2020=="LM"], c(2:4), "sum"),
                              apply(country_incomegroup[13:60,,,,final_ISO3$Year2020=="LM"], c(2:4), "sum"), along=0)
bu_by_ages_es[1,,,,"LMIC"] = bu_by_ages_es[2,,,,"LMIC"]+bu_by_ages_es[3,,,,"LMIC"]+bu_by_ages_es[4,,,,"LMIC"]

bu_by_ages_es[2:4,,,,"UMIC"] = abind(apply(country_incomegroup[1:6,,,,final_ISO3$Year2020=="UM"], c(2:4), "sum"),
                              apply(country_incomegroup[7:12,,,,final_ISO3$Year2020=="UM"], c(2:4), "sum"),
                              apply(country_incomegroup[13:60,,,,final_ISO3$Year2020=="UM"], c(2:4), "sum"), along=0)
bu_by_ages_es[1,,,,"UMIC"] = bu_by_ages_es[2,,,,"UMIC"]+bu_by_ages_es[3,,,,"UMIC"]+bu_by_ages_es[4,,,,"UMIC"]

names(dim(bu_by_ages_es)) = names(dim(burden))
dimnames(bu_by_ages_es)[1] = list(ages = c("Ages: All", "Ages:\n0m-5.9m", "Ages:\n6m-11.9m", "Ages:\n12m-59m"))
dimnames(bu_by_ages_es)[2:5] = dimnames(burden)[2:5]
names(dimnames(bu_by_ages_es)) = names(dim(bu_by_ages_es))

td_by_ages_es_summary_df = apply(td_by_ages_es, c(1, 3:5), meanpi) %>% 
  cubelyr::as.tbl_cube(met_name="Val") %>% as_tibble %>% 
  pivot_wider(id_cols = c("outcome", "model", "region", "age"),
              names_from = Var1, values_from = Val) 

bu_by_ages_es_summary_df = apply(bu_by_ages_es, c(1, 3:5), meanpi) %>% 
  cubelyr::as.tbl_cube(met_name="Val") %>% as_tibble %>% 
  pivot_wider(id_cols = c("outcome", "model", "region", "age"),
              names_from = Var1, values_from = Val)
gc()

# td_by_ages_es_summary_df$hosp_assume = as.character(td_by_ages_es_summary_df$hosp_assume)
# td_by_ages_es_summary_df$hosp_assume[(td_by_ages_es_summary_df$outcome=="disease" & td_by_ages_es_summary_df$model=="com_inc" & td_by_ages_es_summary_df$hosp_assume=="All studies")|
#                                         (td_by_ages_es_summary_df$outcome=="hosp" & td_by_ages_es_summary_df$model=="hosp_inc" & td_by_ages_es_summary_df$hosp_assume=="All studies")|
#                                         (td_by_ages_es_summary_df$outcome=="deaths" & td_by_ages_es_summary_df$model=="hosp_inc" & td_by_ages_es_summary_df$hosp_assume=="All studies")] = "None"
# 
# td_by_ages_es_summary_df$est[td_by_ages_es_summary_df$outcome=="disease" & td_by_ages_es_summary_df$model=="com_inc" & td_by_ages_es_summary_df$hosp_assume!="None"] = NA
# td_by_ages_es_summary_df$est[td_by_ages_es_summary_df$outcome=="hosp" & td_by_ages_es_summary_df$model=="hosp_inc" & td_by_ages_es_summary_df$hosp_assume!="None"] = NA
# td_by_ages_es_summary_df$est[td_by_ages_es_summary_df$outcome=="deaths" & td_by_ages_es_summary_df$model=="hosp_inc" & td_by_ages_es_summary_df$hosp_assume!="None"] = NA

td_by_ages_es_summary_df = td_by_ages_es_summary_df[!is.na(td_by_ages_es_summary_df$est),]
td_by_ages_es_summary_df$lbl = apply(td_by_ages_es_summary_df[,5:7], 1, "ci_string_dec_1M")

td_by_ages_es_summary_df$country_total = "Burden Model (BM) I"

# bu_by_ages_es_summary_df$hosp_assume = as.character(bu_by_ages_es_summary_df$hosp_assume)
# bu_by_ages_es_summary_df$hosp_assume[(bu_by_ages_es_summary_df$outcome=="disease" & bu_by_ages_es_summary_df$model=="com_inc" & bu_by_ages_es_summary_df$hosp_assume=="All studies")|
#                                         (bu_by_ages_es_summary_df$outcome=="hosp" & bu_by_ages_es_summary_df$model=="hosp_inc" & bu_by_ages_es_summary_df$hosp_assume=="All studies")|
#                                         (bu_by_ages_es_summary_df$outcome=="deaths" & bu_by_ages_es_summary_df$model=="hosp_inc" & bu_by_ages_es_summary_df$hosp_assume=="All studies")] = "None"
# 
# bu_by_ages_es_summary_df$est[bu_by_ages_es_summary_df$outcome=="disease" & bu_by_ages_es_summary_df$model=="com_inc" & bu_by_ages_es_summary_df$hosp_assume!="None"] = NA
# bu_by_ages_es_summary_df$est[bu_by_ages_es_summary_df$outcome=="hosp" & bu_by_ages_es_summary_df$model=="hosp_inc" & bu_by_ages_es_summary_df$hosp_assume!="None"] = NA
# bu_by_ages_es_summary_df$est[bu_by_ages_es_summary_df$outcome=="deaths" & bu_by_ages_es_summary_df$model=="hosp_inc" & bu_by_ages_es_summary_df$hosp_assume!="None"] = NA

bu_by_ages_es_summary_df = bu_by_ages_es_summary_df[!is.na(bu_by_ages_es_summary_df$est),]
bu_by_ages_es_summary_df$lbl = apply(bu_by_ages_es_summary_df[,5:7], 1, "ci_string_dec_1M")

bu_by_ages_es_summary_df$country_total = "Burden Model (BM) II"

td_bu3 = rbind(td_by_ages_es_summary_df, bu_by_ages_es_summary_df) %>% 
        mutate(outcome = factor(outcome, levels=c("disease", "hosp", "deaths"), 
                          labels=c("Cases", # \n(labels in millions)", 
                                    "Hospitalizations", #\n(labels in thousands)",
                                    "In-hospital deaths"))) # \n(labels in thousands)")
td_bu3$outcome2 = factor(td_bu3$outcome, levels=rev(levels(td_bu3$outcome)))

td_bu3$model = factor(td_bu3$model)
levels(td_bu3$model) = c("Outcome Model (OM) I", 
                         "Outcome Model (OM) II")
td_bu3$country_total = factor(td_bu3$country_total)

levels(td_bu1$country_total) = c("Burden Model (BM) I",
                                 "Burden Model (BM) II")

td_bu3$model2 = paste(td_bu3$model, "; ", td_bu3$country_total, sep="")
td_bu3$model2 = factor(td_bu3$model2)

td_bu3$textpos = 10^(0.5*(log10(td_bu3$lci) + log10(td_bu3$hci)))
# td_bu3$textpos[td_bu3$textpos<15e6 & td_bu3$outcome=="All cases"] = 15e6
td_bu3$model2b = factor(td_bu3$model2, levels=rev(levels(td_bu3$model2)))
td_bu3$ageb = factor(td_bu3$age, levels=rev(levels(td_bu3$age)))

save(td_bu3, file=paste(plotpre_out, "epicode45_burden_calculations/td_bu3.Rdata", sep=""))

##******************************************************************
## Save: now format to save in a csv file. -------------------------
##******************************************************************

td_bu3_fmt = td_bu3
# td_bu3_fmt$lbl = apply(td_bu3_fmt[,5:7], 1, ci_string_dec, 0)
td_bu3_fmt = td_bu3_fmt[,c("outcome", "model", "region", "age", "country_total", "lbl")]

td_bu3_fmt_wide = pivot_wider(td_bu3_fmt, 
                              id_cols = c("outcome", "model", "country_total"), 
                              names_from = c("region", "age"), names_sep = "; ",
                              values_from = "lbl")

load(paste(plotpre_out, "epicode45_burden_calculations/td_bu2.Rdata", sep=""))
td_bu2_fmt = td_bu2
# td_bu2_fmt$lbl =apply(td_bu2_fmt[,5:7], 1, ci_string_dec, 0)
td_bu2_fmt = td_bu2_fmt[,c("outcome", "model", "region", "country_total", "lbl")]

td_bu2_fmt_wide = pivot_wider(td_bu2_fmt, 
                              id_cols = c("outcome", "model", "country_total"), 
                              names_from = "region",
                              values_from = "lbl")

load(paste(plotpre_out, "epicode45_burden_calculations/td_bu1.Rdata", sep=""))
td_bu1_fmt = td_bu1
# td_bu1_fmt$lbl =apply(td_bu1_fmt[,5:7], 1, ci_string_dec, 0)
td_bu1_fmt = td_bu1_fmt[,c("outcome", "model", "region", "country_total", "lbl")]

td_bu1_fmt_wide = pivot_wider(td_bu1_fmt, 
                              id_cols = c("outcome", "model", "country_total"), 
                              names_from = "region",
                              values_from = "lbl")

