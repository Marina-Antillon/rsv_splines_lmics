# *****************************************************************
# Country-level estimates -----------------------------------------
# *****************************************************************

if (!dir.exists(file.path(paste(plotpre_out, "country_estimates", sep="")))){
  dir.create(file.path(paste(plotpre_out, "country_estimates", sep="")))
}

# Top-down total cases model with income group specification
burden_age_redux = abind(lapply(1:60, function(i){apply(burden[((i-1)*10+1):(i*10),,,,], 2:5, "sum")}), along=0)
names(dim(burden_age_redux)) = names(dim(burden))[1:5]
dimnames(burden_age_redux) = append(list(age=as.character(0:59)), dimnames(burden)[2:5])

td_country = array(NA,
                   dim=c(dim(burden_age_redux)[1:4], ISO3=length(final_ISO3$ISO3)),
                   dimnames = append(dimnames(burden_age_redux)[1:4], list(ISO3=final_ISO3$ISO3)))

for(i in 1:length(final_ISO3$ISO3)){
  if (final_ISO3$Year2020[i]=="L"){ # LIC
    agebinperc = lxt$LIC$pop[1:60]/sum(lxt$LIC$pop[1:60])
    pop = pop_lic
  } else if (final_ISO3$Year2020[i]=="LM") { # LMIC
    agebinperc = lxt$LMIC$pop[1:60]/sum(lxt$LMIC$pop[1:60])
    pop = pop_lmic
  } else { # UMIC
    agebinperc = lxt$UMIC$pop[1:60]/sum(lxt$UMIC$pop[1:60])
    pop = pop_umic
  }
  tmp_pop = as.numeric(lxt_df[lxt_df$ISO3==final_ISO3$ISO3[i],2:61])/sum(as.numeric(lxt_df[lxt_df$ISO3==final_ISO3$ISO3[i],2:61]))*popA$pop_2020[popA$ISO3==final_ISO3$ISO3[i]]
  tmp_burden = sweep(burden_age_redux[,,,,paste0(final_ISO3$Year2020[i], "IC")], 1, 1/(agebinperc*pop), "*")
  td_country[,,,,final_ISO3$ISO3[i]] = sweep(tmp_burden, 1, tmp_pop, "*")
}

# *****************************************************************
# burden per age group --------------------------------------------
# *****************************************************************

td_country_df = td_country %>% apply(c(1,3:5), meanmedianpi) %>% 
  as.tbl_cube(met_name = "values") %>% as_tibble() %>% 
  pivot_wider(values_from = "values",
              id_cols = c("age", "outcome", "model", "ISO3"),
              names_from = "Var1") %>% 
  mutate(burden_model = "Burden Model (BM) I",
         model = factor(model, level=c("com_inc", "hosp_inc"), 
                        labels=c("Outcome Model (OM) I", "Outcome Model (OM) II")),
         outcome = factor(outcome, level = c("disease", "hosp", "deaths"), 
                          labels=c("Cases", "Hospitalizations", "In-hospital deaths"))) %>%
         left_join((countryBOD %>% dplyr::select("location_name", "ISO3"))) %>% 
         dplyr::rename(country=location_name) %>% 
         dplyr::rename(outcome_model=model) %>% 
         dplyr::rename(age_months=age) %>% 
         relocate(outcome) %>% relocate(burden_model) %>% relocate(outcome_model) %>% 
         relocate(country) %>% relocate(ISO3)

# burden for all <1m, <6m, <12m, <5y year olds # this one...
td_grp_country_df = abind((td_country[1,,,,] %>% apply(c(2:4), meanmedianpi)), 
  (td_country[1:6,,,,] %>% apply(c(2:5), sum) %>% apply(c(2:4), meanmedianpi)), 
  (td_country[1:12,,,,] %>% apply(c(2:5), sum) %>% apply(c(2:4), meanmedianpi)), 
  (td_country %>% apply(c(2:5), sum) %>% apply(c(2:4), meanmedianpi)), along = 0) 

names(dim(td_grp_country_df)) = c("age_grp", "Var1", "outcome", "outcome_model", "ISO3")
dimnames(td_grp_country_df) = list(age_grp = c("<1m", "<6m", "<12m", "<5y"), 
                                   Var1 = c("mean_est", "median_est", "lci", "hci"),
                                   outcome = c("Cases", "Hospitalizations", "In-hospital deaths"),
                                   outcome_model = c("Outcome Model (OM) I", "Outcome Model (OM) II"),
                                   ISO3=final_ISO3$ISO3)

td_grp_country_df = td_grp_country_df %>% as.tbl_cube(met_name = "values") %>% as_tibble() %>% 
  pivot_wider(values_from = "values",
              id_cols = c("age_grp", "outcome", "outcome_model", "ISO3"),
              names_from = "Var1") %>% 
  mutate(burden_model = "Burden Model (BM) I") %>%
  left_join((countryBOD %>% dplyr::select(ISO3, location_name))) %>% 
  dplyr::rename(country=location_name) %>% 
  relocate(outcome) %>% relocate(burden_model) %>% 
  relocate(outcome_model) %>% relocate(country) %>% relocate(ISO3)

#******************************************************************
## mean, median, peak ages - same for top down bottom up -----------
#******************************************************************

meanage_country = peak_country = age50p_country = 
                  array(NA,
                   dim=c(dim(meanage)[1:3], ISO3=length(final_ISO3$ISO3)),
                   dimnames = append(dimnames(meanage)[1:3], list(ISO3=final_ISO3$ISO3)))

for(i in 1:length(final_ISO3$ISO3)){
  meanage_country[,,,final_ISO3$ISO3[i]] = meanage[,,,paste0(final_ISO3$Year2020[i], "IC")]
  peak_country[,,,final_ISO3$ISO3[i]] = peak[,,,paste0(final_ISO3$Year2020[i], "IC")]
  age50p_country[,,,final_ISO3$ISO3[i]] = age50p[,,,paste0(final_ISO3$Year2020[i], "IC")]
}

meanage_country_df = meanage_country %>% apply(2:4, meanmedianpi) %>% 
  as.tbl_cube(met_name = "values") %>% as_tibble() %>% 
  pivot_wider(values_from = "values",
              id_cols = c("outcome", "model", "ISO3"),
              names_from = "Var1") %>% 
  mutate(model = factor(model, level=c("com_inc", "hosp_inc"), labels=c("Outcome Model (OM) I", "Outcome Model (OM) II")),
         outcome = factor(outcome, level = c("disease", "hosp", "deaths"), 
                          labels=c("Cases", "Hospitalizations", "In-hospital deaths"))) %>%
  left_join((countryBOD %>% dplyr::select(ISO3, location_name))) %>% 
  dplyr::rename(country=location_name) %>% 
  dplyr::rename(outcome_model=model) %>% 
  relocate(outcome) %>% relocate(outcome_model) %>% relocate(country) %>% relocate(ISO3)

peak_country_df = peak_country %>% apply(2:4, meanmedianpi) %>% 
  as.tbl_cube(met_name = "values") %>% as_tibble() %>% 
  pivot_wider(values_from = "values",
              id_cols = c("outcome", "model", "ISO3"),
              names_from = "Var1") %>% 
  mutate(model = factor(model, level=c("com_inc", "hosp_inc"), labels=c("Outcome Model (OM) I", "Outcome Model (OM) II")),
         outcome = factor(outcome, level = c("disease", "hosp", "deaths"), labels=c("Cases", "Hospitalizations", "In-hospital deaths"))) %>%
  left_join((countryBOD %>% dplyr::select(ISO3, location_name))) %>% 
  dplyr::rename(country=location_name) %>% 
  dplyr::rename(outcome_model=model) %>% 
  relocate(outcome) %>% relocate(outcome_model) %>% relocate(country) %>% relocate(ISO3)

age50p_country_df = age50p_country %>% apply(2:4, meanmedianpi) %>% 
  as.tbl_cube(met_name = "values") %>% as_tibble() %>% 
  pivot_wider(values_from = "values",
              id_cols = c("outcome", "model", "ISO3"),
              names_from = "Var1") %>% 
  mutate(model = factor(model, level=c("com_inc", "hosp_inc"), labels=c("Outcome Model (OM) I", "Outcome Model (OM) II")),
         outcome = factor(outcome, level = c("disease", "hosp", "deaths"), labels=c("Cases", "Hospitalizations", "In-hospital deaths"))) %>%
  left_join((countryBOD %>% dplyr::select(ISO3, location_name))) %>% 
  dplyr::rename(country=location_name) %>% 
  dplyr::rename(outcome_model=model) %>% 
  relocate(outcome) %>% relocate(outcome_model) %>% relocate(country) %>% relocate(ISO3)

#******************************************************************
## bottom-up ------------------------------------------------------
#******************************************************************

load(paste0(plotpre_out, "epicode44a_time_to_perc_cases/burden_results_country_incomegroup.Rdata"))
bu_country_df = country_incomegroup[,,,,] %>% apply(c(1,3:5), meanmedianpi) %>% 
  as.tbl_cube(met_name = "values") %>% as_tibble %>% 
  pivot_wider(values_from = "values",
              id_cols = c("age", "outcome", "model", "ISO3"),
              names_from = "Var1") %>% 
  mutate(burden_model = "Burden Model (BM) II",
         model = factor(model, level=c("com_inc", "hosp_inc"), labels=c("Outcome Model (OM) I", "Outcome Model (OM) II")),
         outcome = factor(outcome, level = c("disease", "hosp", "deaths"), labels=c("Cases", "Hospitalizations", "In-hospital deaths"))) %>%
  left_join((countryBOD %>% dplyr::select(ISO3, location_name))) %>% 
  dplyr::rename(country=location_name) %>% 
  dplyr::rename(outcome_model=model) %>% 
  dplyr::rename(age_months=age) %>% 
  relocate(outcome) %>% relocate(burden_model) %>% relocate(outcome_model) %>% 
  relocate(country) %>% relocate(ISO3)

# burden for all <1m, <6m, <12m, <5y year olds # this one...
bu_grp_country_df = abind((country_incomegroup[1,,,,] %>% apply(c(2:4), meanmedianpi)), 
                          (country_incomegroup[1:6,,,,] %>% apply(c(2:5), sum) %>% apply(c(2:4), meanmedianpi)), 
                          (country_incomegroup[1:12,,,,] %>% apply(c(2:5), sum) %>% apply(c(2:4), meanmedianpi)), 
                          (country_incomegroup[,,,,] %>% apply(c(2:5), sum) %>% apply(c(2:4), meanmedianpi)), along = 0) 
names(dim(bu_grp_country_df)) = c("age_grp", "Var1", "outcome", "outcome_model", "ISO3")
dimnames(bu_grp_country_df) = list(age_grp = c("<1m", "<6m", "<12m", "<5y"), 
                                   Var1 = c("mean_est", "median_est", "lci", "hci"),
                                   outcome = c("Cases", "Hospitalizations", "In-hospital deaths"),
                                   outcome_model = c("Outcome Model (OM) I", "Outcome Model (OM) II"),
                                   ISO3 = final_ISO3$ISO3)

bu_grp_country_df = bu_grp_country_df %>% as.tbl_cube(met_name = "values") %>% as_tibble() %>% 
  pivot_wider(values_from = "values",
              id_cols = c("age_grp", "outcome", "outcome_model", "ISO3"),
              names_from = "Var1") %>% 
  mutate(burden_model = "Burden Model (BM) II") %>%
  left_join((countryBOD %>% dplyr::select(ISO3, location_name))) %>% 
  dplyr::rename(country=location_name) %>% 
  relocate(outcome) %>% relocate(burden_model) %>% relocate(outcome_model) %>% 
  relocate(country) %>% relocate(ISO3)

# Combine the td and bu estimates
country_burden = bind_rows(td_country_df, bu_country_df)
country_burden_age_grp = bind_rows(td_grp_country_df, bu_grp_country_df)

#******************************************************************
## Community-deaths -----------------------------------------------
#******************************************************************

inf_fac = runif(5000, 1.5, 2.9)*runif(5000, 0.9, 1)-1

comm_deaths = abind((td_country[,,"deaths",,] %>% apply(c(2:4), sum) %>% sweep(1,inf_fac, "*")),
  (country_incomegroup[,,"deaths",,] %>% apply(c(2:4), sum) %>% sweep(1,inf_fac, "*")), rev.along=0)
#  Uniform distribution is assumed for the overall death adjustment factor 2.2 (1.5–2.9) - 1 and influenza adjustment factor (0.9–1)

hosp_deaths = abind((td_country[,,"deaths",,] %>% apply(c(2:4), sum)),
                    (country_incomegroup[,,"deaths",,] %>% apply(c(2:4), sum)), rev.along=0)

names(dim(comm_deaths)) = c("iterations", "outcome_model", "ISO3", "burden_model")
dimnames(comm_deaths) = list(iterations = 1:5000,
                                   outcome_model = c("Outcome Model (OM) I", "Outcome Model (OM) II"),
                                   ISO3 = final_ISO3$ISO3,
                                   burden_model = c("Burden Model (BM) I",  "Burden Model (BM) II"))

tmp = abind(list(`Community deaths` = comm_deaths, 
                             `Hospital deaths` = hosp_deaths, 
                             `All deaths` = comm_deaths+hosp_deaths), along=0)
dimnames(tmp) = list(Category = c("Community deaths", "Hospital deaths", "All deaths"),
                     Iterations = 1:5000,
                     `Outcome Model` = c("OM I", "OM II"),
                     ISO3 = final_ISO3$ISO3,
                     `Burden Model` = c("BM I", "BM II"))

all_country_deaths_df = tmp %>% apply(c(1, 3:5), meanmedianpi) %>% 
  as.tbl_cube(met_name = "values") %>% as_tibble() %>% 
  pivot_wider(values_from = "values",
              id_cols = c("Burden Model", "Outcome Model", "ISO3", "Category"),
              names_from = "Var1") %>% 
  left_join((countryBOD %>% dplyr::select(ISO3, location_name))) %>% 
  dplyr::rename(country=location_name) %>% 
  mutate(Category = factor(Category, levels=c("Community deaths", "Hospital deaths", "All deaths"))) %>% 
  relocate("Burden Model") %>% relocate("Outcome Model") %>% relocate(country) %>% relocate(ISO3) %>% 
  arrange(ISO3, Category, `Outcome Model`, `Burden Model`)


## Deaths for the main paper

tmp_total_commdeaths = lapply(c("L", "LM", "UM"), 
                      function(i){apply(comm_deaths[,,final_ISO3$Year2020==i,], c(1,2,4), "sum")})
names(tmp_total_commdeaths) = c("LIC", "LMIC", "UMIC")
tmp_total_commdeaths$ALL=apply(comm_deaths, c(1,2,4), "sum")
tmp_total_commdeaths = abind(tmp_total_commdeaths, rev.along=0)
names(dimnames(tmp_total_commdeaths)) = c('iterations', 'outcome_model', "burden_model", "region")

inhosp_deaths = abind((td_country[,,"deaths",,] %>% apply(c(2:4), sum)),
                      (country_incomegroup[,,"deaths",,] %>% apply(c(2:4), sum)), rev.along=0)
tmp_total_hospdeaths = lapply(c("L", "LM", "UM"),
                              function(i){apply(inhosp_deaths[,,final_ISO3$Year2020==i,], c(1,2,4), "sum")})
names(tmp_total_hospdeaths) = c("LIC", "LMIC", "UMIC")
tmp_total_hospdeaths$ALL=apply(inhosp_deaths, c(1,2,4), "sum")
tmp_total_hospdeaths = abind(tmp_total_hospdeaths, rev.along=0)
dimnames(tmp_total_hospdeaths) = dimnames(tmp_total_commdeaths)

tmp_total_commdeaths_df = tmp_total_commdeaths %>% apply(c(2:4), meanpi) %>%
  apply(2:4, ci_string_comma) %>% as.tbl_cube(met_name = "est_ci") %>% as_tibble() %>%
  mutate(subgroup = "Community deaths")

tmp_total_hospdeaths_df = tmp_total_hospdeaths %>% apply(c(2:4), meanpi) %>%
  apply(2:4, ci_string_comma) %>% as.tbl_cube(met_name = "est_ci") %>% as_tibble()  %>%
  mutate(subgroup = "In-hospital deaths")

tmp_total_anydeaths_df = (tmp_total_commdeaths+tmp_total_hospdeaths) %>% apply(c(2:4), meanpi) %>%
  apply(2:4, ci_string_comma) %>% as.tbl_cube(met_name = "est_ci") %>% as_tibble()  %>%
  mutate(subgroup = "All deaths")

# **
# tmp_total_commdeaths_df = tmp_total_commdeaths %>% apply(c(2:4), meanpi) %>%
#   as.tbl_cube(met_name = "value") %>% as_tibble() %>% 
#   pivot_wider(id_cols = c(outcome_model, burden_model, region), names_from = Var1, values_from = value) %>%
#   mutate(subgroup = "Community deaths")
# 
# tmp_total_hospdeaths_df = tmp_total_hospdeaths %>% apply(c(2:4), meanpi) %>%
#   as.tbl_cube(met_name = "value") %>% as_tibble() %>% 
#   pivot_wider(id_cols = c(outcome_model, burden_model, region), names_from = Var1, values_from = value) %>%
#   mutate(subgroup = "In-hospital deaths")
# 
# tmp_total_anydeaths_df = (tmp_total_commdeaths+tmp_total_hospdeaths) %>% apply(c(2:4), meanpi) %>%
#   as.tbl_cube(met_name = "value") %>% as_tibble() %>% 
#   pivot_wider(id_cols = c(outcome_model, burden_model, region), names_from = Var1, values_from = value) %>%
#   mutate(subgroup = "All deaths")
# 
# write.csv(tmp_total_commdeaths_df, file="/Users/Marina/switchdrive/TB-lifetime-costs/Example_Rcode/tmp_total_commdeaths_df.csv")
# write.csv(tmp_total_hospdeaths_df, file="/Users/Marina/switchdrive/TB-lifetime-costs/Example_Rcode/tmp_total_hospdeaths_df.csv")
# write.csv(tmp_total_anydeaths_df, file="/Users/Marina/switchdrive/TB-lifetime-costs/Example_Rcode/tmp_total_anydeaths_df.csv")

all_deaths_df = bind_rows(bind_rows(tmp_total_commdeaths_df, tmp_total_hospdeaths_df), tmp_total_anydeaths_df) %>% 
  pivot_wider(id_cols = c(outcome_model, burden_model, subgroup), names_from = region, values_from = est_ci) %>% 
  dplyr::rename(`All regions` = ALL) %>% dplyr::rename(`Outcome Model` = outcome_model) %>% dplyr::rename(`Burden Model` = burden_model) %>% 
  relocate("Burden Model")

# Community, in-hospital, and total deaths. -----
tb_kb = knitr::kable((all_deaths_df %>% dplyr::select(-"subgroup")), 
                     format="latex", escape=T, row.names=F, 
                     caption = "All deaths: deaths in the community (which did not reach a health facility),
                     in-hospital deaths, and total deaths. See section \\ref{subsubsec:supp_methods_comm_deaths}
                     for the methods of calculation of community-based deaths.",
                     label="alldeaths", 
                     align = c(rep("C{1cm}",2) ,rep("R{3cm}",4)))

newtb = kableExtra::group_rows(tb_kb, "Community deaths", 1, 4)
newtb = kableExtra::group_rows(newtb, "In-hospital deaths", 5, 8)
newtb = kableExtra::group_rows(newtb, "All deaths", 9, 12)

newtb = gsub("\\begin{table}", "\\begin{table}[h!]", newtb, fixed=T)

newtb = gsub("\\hspace{1em}Burden Model (BM) I", "\\hspace{1em}I", newtb, fixed=T)
newtb = gsub("\\hspace{1em}Burden Model (BM) II", "\\hspace{1em}II", newtb, fixed=T)
newtb = gsub("Outcome Model (OM) I", "I", newtb, fixed=T)
newtb = gsub("Outcome Model (OM) II", "II", newtb, fixed=T)

write(newtb, file=paste0(plotpre_out, "/all_deaths.tex"))

# ********************************************************
## Assemble in excel sheets for each country -------------
# First sheet should be some definitions + notes (only hosp-assume of Nokes+Zur together)
# ********************************************************

Notes_definitions = 
  data.frame(Term = c("Outcome Model (OM) I", "Outcome Model (OM) II", 
                      "Burden Model (BM) I", "Burden Model (BM) II", 
                      "Cases", "Hospitalizations", "In-hospital deaths",
                      "Community deaths",
                      "Mean age", "Median age", "Peak age",
                      "Note: Hospital probability assumption",
                      "Note: No estimate of peak, median, and mean age for community deaths. "),
             Definition=c("Model where incidence is based on splines estimates from community-based incidence studies. See figure 1A.",
              "Model where incidence is based on splines estimates from hospital-based incidence studies. See figure 1A.",
              "Model where burden is based on splines estimates. See figure 1B.",
              "Model where burden is based on the risk-factor model by Shi et al. See figure 1B.", 
              "Estimate of cases of RSV in the community, whether or not they seek care.",
              "Estimate of RSV hospitalized cases.",
              "Estimate of RSV deaths within hospitals.",
              "Estimate of RSV deaths outside the purview of health facilities. See supplemental section 1.3.5 for further elaboration.",
              "Average age of cases, hospitalizations, or in-hospital deaths according to that outcome model in that country",
              "Median age of cases, hospitalizations, or in-hospital deaths (age at which 50% of cases, hospitalization, or in-hospital deaths have occurred) according to that outcome model in that country",
              "Peak age of cases, hospitalizations, or in-hospital deaths. Age at which these outcome peak.",
              "The probability of hospitalization is the spline estimated from using the Nokes et al 2008 data and the Zur unpublished data.",
              "There was no data to calculate splines of community-based deaths. Community-based deaths were calculated as a factor of total in-hospital deaths, so therefore only totals are available."))

out_country=list()

final_ISO3 = left_join(final_ISO3, (countryBOD %>% dplyr::select(ISO3, location_name)))

for(i in 1:length(final_ISO3$ISO3)){
  out_country$README = Notes_definitions
  out_country$burden_by_age_group = country_burden_age_grp %>% filter(ISO3==final_ISO3$ISO3[i])
  out_country$all_deaths = all_country_deaths_df %>% filter(ISO3==final_ISO3$ISO3[i])
  out_country$mean_age = meanage_country_df %>% filter(ISO3==final_ISO3$ISO3[i])
  out_country$median_age = age50p_country_df %>% filter(ISO3==final_ISO3$ISO3[i])
  out_country$peak_age = peak_country_df %>% filter(ISO3==final_ISO3$ISO3[i])
  out_country$burden_1m_age_bracket = country_burden %>% filter(ISO3==final_ISO3$ISO3[i])
    
  write.xlsx(out_country, file = paste0(plotpre_out, "country_estimates/", final_ISO3$ISO3[i], "-" , 
                                        final_ISO3$location_name[i], ".xlsx"), overwrite=T)
}

# ********************************************************
## RSV deaths in the first 6 months of life -------------
# (only hosp-assume of Nokes+Zur together)
# ********************************************************

dimnames(inhosp_deaths)
inhosp_deaths %>% apply(c(1,2,4), sum) %>% apply(2:3, mean)

tmp_deaths_6m = abind((td_country[1:6,,"deaths",,] %>% apply(c(2:3), sum)),
                    (country_incomegroup[1:6,,"deaths",,] %>% apply(c(2:3), sum)), rev.along=0)

tmp_deaths = abind((td_country[,,"deaths",,] %>% apply(c(2:3), sum)),
                    (country_incomegroup[,,"deaths",,] %>% apply(c(2:3), sum)), rev.along=0)

tmp_ratio = tmp_deaths_6m/tmp_deaths
tmp_ratio %>% apply(c(2:3), mean)
tmp_ratio %>% apply(c(2:3), meanpi)

