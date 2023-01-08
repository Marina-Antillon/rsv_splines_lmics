#******************************************************************
# BOD, pop structure, and final_ISO3 ------------------------------
#******************************************************************


#******************************************************************
## Bring in burden of disease -------------------------------------
#******************************************************************

countryBOD = read.csv("./data/RSV_burden_of_disease_v2.csv", sep=";")

# find median incidence that I had imputed to countries missing incidence:
# countrymodISO3 = countryBOD$ISO3[(countryBOD$ISO3 %in% final_ISO3$ISO3) & !is.na(countryBOD$lci)]
# quantile(apply(country_incomegroup[,,"disease", "com_inc",countrymodISO3,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6
# quantile(apply(country_global[,,"disease", "com_inc",countrymodISO3,"All studies"], 2, "sum"), c(0.025, 0.5, 0.975))/1e6

countryBOD$country_iso = as.character(countryBOD$country_iso)

colnames(countryBOD)[colnames(countryBOD)=="country_iso"] = "ISO3"
colnames(countryBOD)[colnames(countryBOD)=="incidence_RSV_associated_ALRI"] = "est"
colnames(countryBOD)[colnames(countryBOD)=="lower_Incidence"] = "lci"
colnames(countryBOD)[colnames(countryBOD)=="upper_Incidence"] = "hci"

# bring in table that puts each country in it's economic setting
income_cat = read.csv("./data/historical_WB_income_group_V2.csv", sep=";")

countryBOD$ISO3[!(countryBOD$ISO3 %in% income_cat$ISO3)]

countryBOD = left_join(countryBOD, income_cat)

# How many of Li's "developing countries" are HIC in 2020?
countryBOD[countryBOD$Year2020=="H" & !is.na(countryBOD$nrb_episodes), c("ISO3", "location_name")]
# ISO3        location_name
# 4    ATG  Antigua and Barbuda
# 8    BHS         Bahamas, The
# 9    BHR              Bahrain
# 11   BRB             Barbados
# 18   BRN    Brunei Darussalam
# 26   CHL                Chile
# 66   KOR          Korea, Rep.
# 67   KWT               Kuwait
# 92   OMN                 Oman
# 99   QAT                Qatar
# 103  SAU         Saudi Arabia
# 105  SYC           Seychelles
# 107  SGP            Singapore
# 124  TTO  Trinidad and Tobago
# 129  ARE United Arab Emirates
# 130  URY              Uruguay

#*********************************************************************
## Bring in population by age-month -----------------------------------
#*********************************************************************

# Mortality (stratified by gender)
data(mxM, package = "wpp2019", envir = environment()) # Male
data(mxF, package = "wpp2019", envir = environment()) # Female
mxM = mxM[mxM$age %in% c(0, 1, 5), c("age", "name", "2010-2015", "2015-2020", "2020-2025")]
colnames(mxM) = c("age", "name", "male_10_15", "male_15_20", "male_20_25")
mxF = mxF[mxF$age %in% c(0, 1, 5), c("age", "name", "2010-2015", "2015-2020", "2020-2025")]
colnames(mxF) = c("age", "name", "female_10_15", "female_15_20", "female_20_25")

# Population
data(popM, package = "wpp2019", envir = environment()) # Male
popM=popM[popM$age=="0-4",c("name", "age", "2015", "2020")] # leave only what we need.
colnames(popM)[3:4]= c("popM_2015", "popM_2020")
popM$popM_2015=popM$popM_2015*1000
popM$popM_2020=popM$popM_2020*1000
data(popF, package = "wpp2019", envir = environment()) # Female
popF=popF[popF$age=="0-4",c("name", "age", "2015", "2020")] # leave only what we need.
colnames(popF)[3:4]= c("popF_2015", "popF_2020")
popF$popF_2015=popF$popF_2015*1000
popF$popF_2020=popF$popF_2020*1000

# sex ratio by country
data(sexRatio, package = "wpp2019", envir = environment())
sexRatio = sexRatio[, c("name", "2015-2020", "2020-2025")] # "2010-2015", 
# sexRatio$prop_female_10_15 = sexRatio$`2010-2015`/2
sexRatio$prop_female_15_20 = sexRatio$`2015-2020`/2
sexRatio$prop_female_20_25 = sexRatio$`2020-2025`/2

# combine mortality and population with sex ratio to get statistics for the whole population
mxA = full_join(mxM, mxF)
mxA = full_join(mxA, sexRatio[,c("name", 
                                 # "prop_female_10_15", 
                                 "prop_female_15_20", 
                                 "prop_female_20_25")])

popA = full_join(popM, popF)
popA$pop_2015 = popA$popM_2015 + popA$popF_2015
popA$pop_2020 = popA$popM_2020 + popA$popF_2020
popA$iso3 = countrycode::countrycode(popA$name,'country.name','iso3c',warn=F)
popA$iso3[popA$name=="Less developed regions, excluding China"] = NA

mxA$iso3 = countrycode::countrycode(mxA$name,'country.name','iso3c',warn=F)
mxA$iso3[mxA$name=="Less developed regions, excluding China"] = NA
mxA$iso3[mxA$name=="Low-income countries"] = "LIC"
mxA$iso3[mxA$name=="Lower-middle-income countries"] = "LMIC"
mxA$iso3[mxA$name=="Upper-middle-income countries"] = "UMIC"
mxA$iso3[mxA$name=="High-income countries"] = "HIC"
mxA = mxA[!is.na(mxA$iso3),]

popA$iso3[popA$name=="Low-income countries"] = "LIC"
popA$iso3[popA$name=="Lower-middle-income countries"] = "LMIC"
popA$iso3[popA$name=="Upper-middle-income countries"] = "UMIC"
popA$iso3[popA$name=="High-income countries"] = "HIC"
# perhaps look at WHO region later too
popA = popA[!is.na(popA$iso3),]
colnames(popA)[colnames(popA)=="iso3"] = "ISO3"

# How much population in the high-income developing countries? -----
# countryBODtmp = left_join(countryBOD, (popA %>% dplyr::select(ISO3, pop_2020)))
# 
# tmp = countryBODtmp[countryBODtmp$Year2020=="H" & !is.na(countryBODtmp$nrb_episodes), c("ISO3", "location_name", "pop_2020", "nrb_episodes")]
# tmp = rbind(tmp, data.frame(ISO3="", location_name="Total", pop_2020=sum(tmp$pop_2020, na.rm=T), nrb_episodes=sum(tmp$nrb_episodes, na.rm=T)))
# tmp$pop_2020 = format(tmp$pop_2020, nsmall=0, big.mark=",")
# tmp$nrb_episodes = format(tmp$nrb_episodes, nsmall=0, big.mark=",")
# 
# tb_kb = knitr::kable(tmp,
#                      format="latex", escape=T, row.names=F,
#                      col.names = c("ISO3", "Country", "Population 2020", "RSV cases"),
#                      caption = "Population in countries that were `developing' but high-income countries.
#                      These were included in Li et al's analysis but excluded in our analysis.",
#                      label="pop_dif_li_us",
#                      align = c(rep("C{1cm}",1) ,rep("R{3cm}",3)))
# 
# tb_kb = kableExtra::row_spec(tb_kb, 0, color="white", background = "black", bold=T)
# tb_kb = kableExtra::row_spec(tb_kb, 18, background = "lightgray", bold=T)
# 
# newtb = gsub("\\begin{table}", "\\begin{table}[h!]", tb_kb, fixed=T)
# newtb = gsub("{C{1cm}", "{|C{1cm}", newtb, fixed=T)
# newtb = gsub("R{3cm}}", "R{3cm}|}", newtb, fixed=T)
# 
# write(newtb, file=paste0(plotpre_out, "/epicode43/pop_dif_Li_us.tex"))

popA = popA[popA$ISO3 %in% c(countryBOD$ISO3, "LIC", "LMIC", "UMIC"),]
popA = popA[order(popA$ISO3),]

countryBOD = countryBOD %>% filter(Year2020!="H") 
countryBOD = left_join(countryBOD, (popA %>% dplyr::select(ISO3, pop_2020)))

# Countries without good population information
# tmp = countryBOD[is.na(countryBOD$pop_2020), c("ISO3", "location_name", "nrb_episodes")]
# # "COK" "DMA" "MHL" "PLW" "TUV"
# # Cook Islands, Dominica, Marshall Islands, Palau, Tuvalu
# tmp = rbind(tmp, data.frame(ISO3="", location_name="Total", nrb_episodes=sum(tmp$nrb_episodes, na.rm=T)))
# tb_kb = knitr::kable(tmp,
#                      format="latex", escape=T, row.names=F,
#                      col.names = c("ISO3", "Country", "RSV cases"),
#                      caption = "Case estimates in risk-factor model by Li et al in LIC/LMIC/UMIC countries that have no estimates of population in the World Population Prospects.",
#                      label="no_pop_countries",
#                      align = c(rep("C{1cm}",1) ,rep("R{3cm}",2)))
# 
# tb_kb = kableExtra::row_spec(tb_kb, 0, color="white", background = "black", bold=T)
# tb_kb = kableExtra::row_spec(tb_kb, 6, background = "lightgray", bold=T)
# 
# newtb = gsub("\\begin{table}", "\\begin{table}[h!]", tb_kb, fixed=T)
# newtb = gsub("{C{1cm}", "{|C{1cm}", newtb, fixed=T)
# newtb = gsub("R{3cm}}", "R{3cm}|}", newtb, fixed=T)
# 
# write(newtb, file=paste0(plotpre_out, "/epicode43/no_pop_countries.tex")

# just to check
unique(mxA$name[is.na(mxA$iso3)]) 
# No longer missing any names as of 2022.

# Make life table
lxt = list()

for (i in countryBOD$ISO3[(countryBOD$ISO3 %in% popA$ISO3)]){
  tmp = mxA[mxA$iso3==i,]
  if(length(tmp$name)==0){
    tmp = mxA[mxA$iso3==paste0(countryBOD$Year2020[countryBOD$ISO3==i], "IC"),]
  }
  tmp$mort_monthly = (tmp$female_20_25*tmp$prop_female_20_25 + tmp$male_20_25*(1-tmp$prop_female_20_25))/12
  tmp$age_midpoint_m = c(0.5, 3, 7.5)
  
  tmp_approx = approx(tmp$age_midpoint_m*12,tmp$mort_monthly,xout=0:59, method="linear", rule=2)
  # plot(tmp_approx)
  eval(parse(text=paste("lxt$", i,
                        " = data.frame(age = 0:60, pop = c(1e5, exp(-cumsum(tmp_approx$y))*1e5))", sep="")))
}

pop_lic = sum(popA$pop_2020[popA$ISO3 %in% countryBOD$ISO3[countryBOD$Year2020=="L"]]) # popA$pop_2015[popA$ISO3=="LIC"]
pop_lmic = sum(popA$pop_2020[popA$ISO3 %in% countryBOD$ISO3[countryBOD$Year2020=="LM"]]) # popA$pop_2015[popA$ISO3=="LMIC"]
pop_umic = sum(popA$pop_2020[popA$ISO3 %in% countryBOD$ISO3[countryBOD$Year2020=="UM"]]) # popA$pop_2015[popA$ISO3=="UMIC"]
pop_all = pop_lic + pop_lmic + pop_umic

# > popA$pop_2015[popA$ISO3=="UMIC"] - pop_umic
# [1] 16024654
# > popA$pop_2015[popA$ISO3=="LMIC"] - pop_lmic
# [1] 4858852
# > popA$pop_2015[popA$ISO3=="LIC"] - pop_lic
# [1] 8974447
# > 29857953/pop_all
# [1] 0.0523
  
# in order to get a sense of the population by monthly age-group for all of the 
# LIC, LMIC, and UMIC countries.
# why does COD not have lxt?
lxt$LIC$pop = sapply(countryBOD$ISO3[countryBOD$Year2020=="L"], function(i){lxt[[i]]$pop/sum(lxt[[i]]$pop)*popA$pop_2020[popA$ISO3==i]}) %>% 
                rowSums()
lxt$LIC$pop = lxt$LIC$pop/lxt$LIC$pop[1]*1e5

lxt$LMIC$pop = sapply(countryBOD$ISO3[countryBOD$Year2020=="LM"], function(i){lxt[[i]]$pop/sum(lxt[[i]]$pop)*popA$pop_2020[popA$ISO3==i]}) %>% 
  rowSums()
lxt$LMIC$pop = lxt$LMIC$pop/lxt$LMIC$pop[1]*1e5

lxt$UMIC$pop = sapply(countryBOD$ISO3[countryBOD$Year2020=="UM"], function(i){lxt[[i]]$pop/sum(lxt[[i]]$pop)*popA$pop_2020[popA$ISO3==i]}) %>% 
  rowSums()
lxt$UMIC$pop = lxt$UMIC$pop/lxt$UMIC$pop[1]*1e5

lxt$ALL$pop = (lxt$LIC$pop*pop_lic + lxt$LMIC$pop*pop_lmic + lxt$UMIC$pop*pop_umic)/pop_all

# make life table as one table for each country...
lxt_df = data.frame(ISO3 = names(lxt), matrix(NA, length(names(lxt)), 61, dimnames = list(NULL, sprintf("age_month_%02d", 0:60))))

for (i in 1:dim(lxt_df)[1]){
  eval(parse(text = paste("lxt_df[i,2:62] = lxt$", as.character(lxt_df$ISO3[i]), "$pop", sep="")))
}

lxt_df = lxt_df[lxt_df$ISO3 %in% c(countryBOD$ISO3, "LIC", "LMIC", "UMIC"),]
lxt_df = lxt_df[order(lxt_df$ISO3),]

countryBOD$ISO3[!(countryBOD$ISO3 %in% lxt_df$ISO3)]
# none as of 2022

#******************************************************************
## Make dataset final_ISO3 ----------------------------------------
#******************************************************************

final_ISO3 = countryBOD[,c("ISO3", "Year2020")]
final_ISO3 = final_ISO3[(final_ISO3$ISO3 %in% popA$ISO3),]

#******************************************************************
# Summary: countries and populations in income groups -------------
#******************************************************************

# tmp = countryBOD %>% filter(!is.na(pop_2020)) %>% 
#   group_by(Year2020) %>% 
#   dplyr::summarize(Countries = n(), Population = sum(pop_2020)) %>% 
#   bind_rows(data.frame(Year2020 = "Total", 
#                        Countries = sum(!is.na(countryBOD$pop_2020)), 
#                        Population=sum(countryBOD$pop_2020, na.rm=T))) %>% 
#   mutate(Population = format(Population, nsmall=0, big.mark=",")) %>% 
#   mutate(Year2020 = factor(Year2020, levels=c("L", "LM", "UM", "Total"), 
#                            labels=c("LIC", "LMIC", "UMIC", "Total"))) %>% 
#   rename(`Income Group` = Year2020)
# 
# tmp$`Age 0-5m` = c(sum(pop_lic*lxt$LIC$pop[1:6]/sum(lxt$LIC$pop)), 
#   sum(pop_lmic*lxt$LMIC$pop[1:6]/sum(lxt$LMIC$pop)), 
#   sum(pop_umic*lxt$UMIC$pop[1:6]/sum(lxt$UMIC$pop)), 
#   sum(pop_all*lxt$ALL$pop[1:6]/sum(lxt$ALL$pop)))
# 
# tmp$`Age 6-11m` = c(sum(pop_lic*lxt$LIC$pop[7:12]/sum(lxt$LIC$pop)), 
#   sum(pop_lmic*lxt$LMIC$pop[7:12]/sum(lxt$LMIC$pop)), 
#   sum(pop_umic*lxt$UMIC$pop[7:12]/sum(lxt$UMIC$pop)), 
#   sum(pop_all*lxt$ALL$pop[7:12]/sum(lxt$ALL$pop)))
# 
# tmp$`Age 12-59m` = c(sum(pop_lic*lxt$LIC$pop[13:60]/sum(lxt$LIC$pop)), 
#   sum(pop_lmic*lxt$LMIC$pop[13:60]/sum(lxt$LMIC$pop)), 
#   sum(pop_umic*lxt$UMIC$pop[13:60]/sum(lxt$UMIC$pop)), 
#   sum(pop_all*lxt$ALL$pop[13:60]/sum(lxt$ALL$pop)))
# 
# tmp$`Age 0-5m` = format(tmp$`Age 0-5m`, nsmall=0, big.mark=",")
# tmp$`Age 6-11m` = format(tmp$`Age 6-11m`, nsmall=0, big.mark=",")
# tmp$`Age 12-59m` = format(tmp$`Age 12-59m`, nsmall=0, big.mark=",")
# 
# tb_kb = knitr::kable(tmp[,c(1:2,4:6,3)],
#                      format="latex", escape=T, row.names=F,
#                      caption = "Country income groups and population age <5 in 2020.",
#                      label="wb_groups",
#                      align = c(rep("C{1.5cm}",2) ,rep("R{2cm}",4)))
# 
# tb_kb = kableExtra::row_spec(tb_kb, 0, color="white", background = "black", bold=T)
# tb_kb = kableExtra::row_spec(tb_kb, 4, background = "lightgray", bold=T)
# 
# newtb = gsub("\\begin{table}", "\\begin{table}[h!]", tb_kb, fixed=T)
# newtb = gsub("{C{2cm}", "{|C{2cm}", newtb, fixed=T)
# newtb = gsub("R{2cm}}", "R{2cm}|}", newtb, fixed=T)
# 
# write(newtb, file=paste0(plotpre_out, "/epicode43/wb_groups.tex")

# 121 instead of 137

#******************************************************************
# Summary: countries missing --------------------------------------
#******************************************************************

# tmp_popA = full_join(popM, popF)
# tmp_popA$pop_2015 = tmp_popA$popM_2015 + tmp_popA$popF_2015
# tmp_popA$pop_2020 = tmp_popA$popM_2020 + tmp_popA$popF_2020
# tmp_popA$iso3 = countrycode::countrycode(tmp_popA$name,'country.name','iso3c',warn=F)
# tmp_popA$iso3[popA$name=="Less developed regions, excluding China"] = NA
# 
# tmp_popA$iso3[tmp_popA$name=="Low-income countries"] = "LIC"
# tmp_popA$iso3[tmp_popA$name=="Lower-middle-income countries"] = "LMIC"
# tmp_popA$iso3[tmp_popA$name=="Upper-middle-income countries"] = "UMIC"
# tmp_popA$iso3[tmp_popA$name=="High-income countries"] = "HIC"
# # perhaps look at WHO region later too
# tmp_popA = tmp_popA[!is.na(tmp_popA$iso3),]
# colnames(tmp_popA)[colnames(tmp_popA)=="iso3"] = "ISO3"
# 
# income_cat = income_cat %>% left_join(tmp_popA[,c("ISO3", "pop_2020")]) %>%
#   filter(Year2020!="H") %>% filter(!is.na(pop_2020))
# 
# tmp = income_cat %>% anti_join(data.frame(ISO3 = countryBOD$ISO3))
# tmp$name = countrycode::countrycode(tmp$ISO3, 'iso3c', 'country.name', warn = TRUE)
# tmp = bind_rows(tmp, data.frame(ISO3="", Year2020="Total", pop_2020=sum(tmp$pop_2020, na.rm=T), name=""))
# tmp = mutate(tmp, Year2015 = factor(Year2020, levels = c("L", "LM", "UM", "Total"),
#                              labels = c("LIC", "LMIC", "UMIC", "Total")))
# tmp$pop_2020 = format(tmp$pop_2020, nsmall=0, big.mark=",")
# 
# tb_kb = knitr::kable(tmp[,c("ISO3", "name", "Year2020", "pop_2020")],
#                      format="latex", escape=T, row.names=F,
#                      col.names = c("ISO3", "Country name", "Income Group", "Population, 2020"),
#                      caption = "Countries missing from the risk-factor model by Li et al that were LIC/LMIC/UMIC countries in 2020",
#                      label="no_est_countries",
#                      align = c("C{1cm}", "R{3cm}", "R{1cm}", "R{3cm}"))
# 
# tb_kb = kableExtra::row_spec(tb_kb, 0, color="white", background = "black", bold=T)
# tb_kb = kableExtra::row_spec(tb_kb, 24, background = "lightgray", bold=T)
# 
# newtb = gsub("\\begin{table}", "\\begin{table}[h!]", tb_kb, fixed=T)
# newtb = gsub("{C{1cm}", "{|C{1cm}", newtb, fixed=T)
# newtb = gsub("R{3cm}}", "R{3cm}|}", newtb, fixed=T)
# 
# write(newtb, file=paste0(plotpre_out, "/epicode43/no_est_countries.tex")

#******************************************************************
# Clean up environment --------------------------------------------
#******************************************************************

rm(mxM, mxF, mxA, popM, popF, sexRatio, tmp, tmp_approx)
