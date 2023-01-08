
#**********************************************
## Incidence from community-based study data.--
#**********************************************

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

data_long_min = data_long[!is.na(data_long$Cases),]

## Titles for graphs
# data_long$Location = as.character(data_long$Location)
data_long_min$Title = paste(data_long_min$Location, "\n(", data_long_min$Author, ", ", data_long_min$Year, ")", 
                        "\n", data_long_min$Study_period, sep="")

inc_summary = aggregate(data_long_min[,c("Title","Economic_Setting")], data_long_min[,c("Title","Economic_Setting")],  "length")[,1:3]
inc_summary = inc_summary[inc_summary$Economic_Setting!="High income",]
inc_summary$Training = inc_summary$Title.1>2
inc_summary$Title[inc_summary$Title.1>2]
inc_summary$Title[inc_summary$Title.1<3]
table(inc_summary$Economic_Setting, inc_summary$Training)

inc_sev=unique(data_long_min$Title[!is.na(data_long_min$Cases_sev)])
inc_vsev=unique(data_long_min$Title[!is.na(data_long_min$Cases_vsev)])

inc_summary$Title[inc_summary$Title.1>2 & inc_summary$Title %in% inc_sev]
inc_summary$Title[inc_summary$Title.1<3 & inc_summary$Title %in% inc_sev]

table(inc_summary$Economic_Setting[inc_summary$Title %in% inc_sev], inc_summary$Training[inc_summary$Title %in% inc_sev])

inc_summary$Title[inc_summary$Title.1>2 & inc_summary$Title %in% inc_vsev]
inc_summary$Title[inc_summary$Title.1<3 & inc_summary$Title %in% inc_vsev]

table(inc_summary$Economic_Setting[inc_summary$Title %in% inc_vsev], inc_summary$Training[inc_summary$Title %in% inc_vsev])

#**********************************************
## Incidence from hospital-based study data.---
#**********************************************

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

data_long_min = data_long[!is.na(data_long$Cases),]

## Titles for graphs
# data_long$Location = as.character(data_long$Location)
data_long_min$Title = paste(data_long_min$Location, "\n(", data_long_min$Author, ", ", data_long_min$Year, ")", 
                            "\n", data_long_min$Study_period, sep="")

hinc_summary = aggregate(data_long_min[,c("Title","Economic_Setting")], data_long_min[,c("Title","Economic_Setting")],  "length")[,1:3]
hinc_summary = hinc_summary[hinc_summary$Economic_Setting!="High income",]
hinc_summary$Training = hinc_summary$Title.1>2
hinc_summary$Title[hinc_summary$Title.1>2]
hinc_summary$Title[hinc_summary$Title.1<3]
table(hinc_summary$Economic_Setting, hinc_summary$Training)

hinc_sev=unique(data_long_min$Title[!is.na(data_long_min$Cases_sev) & data_long_min$Economic_Setting!="High income"])
hinc_vsev=unique(data_long_min$Title[!is.na(data_long_min$Cases_vsev) & data_long_min$Economic_Setting!="High income"])

hinc_summary$Title[hinc_summary$Title.1>2 & hinc_summary$Title %in% hinc_sev]
hinc_summary$Title[hinc_summary$Title.1<3 & hinc_summary$Title %in% hinc_sev]

table(hinc_summary$Economic_Setting[hinc_summary$Title %in% hinc_sev], hinc_summary$Training[hinc_summary$Title %in% hinc_sev])

hinc_summary$Title[hinc_summary$Title.1>2 & hinc_summary$Title %in% hinc_vsev]
hinc_summary$Title[hinc_summary$Title.1<3 & hinc_summary$Title %in% hinc_vsev]

table(hinc_summary$Economic_Setting[hinc_summary$Title %in% hinc_vsev], hinc_summary$Training[hinc_summary$Title %in% hinc_vsev])

#**********************************************
## Probability of hospitalization -------------
#**********************************************

hosp = read.csv("./data/hospitalizations.csv")

hosp$est = hosp$hosp/hosp$rsv_cases
hosp$lci = binconf(hosp$hosp, hosp$rsv_cases, method="exact")[,"Lower"]
hosp$uci = binconf(hosp$hosp, hosp$rsv_cases, method="exact")[,"Upper"]

hosp$study_no = as.factor(as.numeric(factor(hosp$citation)))

hosp$Title = paste(hosp$Location, "\n(", hosp$Author, ", ", hosp$Year, ")", 
                   "\n", hosp$Study_period, sep="")

# take out Wu 2015 and Sutmoller 1995.
# take out Zur for the published vs unpublished analysis
hosp_val = hosp[hosp$citation %in% c("Wu 2015", "Bigogo 2013", "Homaira 2012"),]
hosp = hosp[!(hosp$citation %in% c("Sutmoller 1995", "Wu 2015", "Bigogo 2013")),]
hosp$study_no = as.factor(as.numeric(factor(hosp$citation)))

hosp_summary = aggregate(hosp[,c("Title","Economic_setting")], 
                         hosp[,c("Title","Economic_setting")],  "length")[,1:3]
table(hosp_summary$Economic_setting)

hosp_val_summary = aggregate(hosp_val[,c("Title","Economic_setting")], 
                             hosp_val[,c("Title","Economic_setting")],  "length")[,1:3]
table(hosp_val_summary$Economic_setting)

#**********************************************
## (hospital) Case fatality rate (hCFR, from hospital-based study data) -----
#**********************************************

load("./data/CFR_workingfile.Rdata")
cfr_all$study_no = 1:dim(cfr_all)[1]

cfr_all = cfr_all[substrRight(cfr_all$Study_period, 4)>1999,]

cases_vars = which(substr(colnames(cfr_all), 1, 5) == "Cases")
deaths_vars = which(substr(colnames(cfr_all), 1, 6) == "Deaths")
redundant_vars = which(substr(colnames(cfr_all), 1, 9) == "Redundant")
id_vars = which(colnames(cfr_all) %in% c("study_no", "Author", "Year", "Study_period", "Location"))

cfr_all$Location = trimws(cfr_all$Location, "right")

table(cfr_all$Economic_setting)

# cases
agegroups=c()
for (i in 1:length(colnames(cfr_all)[cases_vars])){
  agegroups[i] = paste(strsplit(colnames(cfr_all)[cases_vars], "_")[[i]][2], "_", 
                       strsplit(colnames(cfr_all)[cases_vars], "_")[[i]][3], sep="")
}

tmp_cases = reshape(cfr_all[,-c(deaths_vars, redundant_vars)], 
                    idvar = c("study_no", "Author", "Year", "Study_period", "Location"), 
                    varying = colnames(cfr_all)[cases_vars],
                    v.names="Cases", 
                    timevar="Age_Groups", times=agegroups, 
                    direction="long")
tmp_cases = tmp_cases[!is.na(tmp_cases$Cases),]
rownames(tmp_cases) = NULL

# deaths
agegroups=c()
for (i in 1:length(colnames(cfr_all)[deaths_vars])){
  agegroups[i] = paste(strsplit(colnames(cfr_all)[deaths_vars], "_")[[i]][2], "_", 
                       strsplit(colnames(cfr_all)[deaths_vars], "_")[[i]][3], sep="")
}

tmp_deaths = reshape(cfr_all[,c(id_vars, deaths_vars)], 
                     idvar = c("study_no", "Author", "Year", "Study_period", "Location"), 
                     varying = colnames(cfr_all)[deaths_vars],
                     v.names="Deaths", 
                     timevar="Age_Groups", times=agegroups, 
                     direction="long")
tmp_deaths = tmp_deaths[!is.na(tmp_deaths$Deaths),]
rownames(tmp_deaths) = NULL

# redundancy vars
agegroups=c()
for (i in 1:length(colnames(cfr_all)[redundant_vars])){
  agegroups[i] = paste(strsplit(colnames(cfr_all)[redundant_vars], "_")[[i]][2], "_", 
                       strsplit(colnames(cfr_all)[redundant_vars], "_")[[i]][3], sep="")
}

tmp_red = reshape(cfr_all[,c(id_vars, redundant_vars)], 
                  idvar = c("study_no", "Author", "Year", "Study_period", "Location"), 
                  varying = colnames(cfr_all)[redundant_vars],
                  v.names="Redundant", 
                  timevar="Age_Groups", times=agegroups, 
                  direction="long")
tmp_red = tmp_red[!is.na(tmp_red$Redundant),]
rownames(tmp_red) = NULL

# join it all together
cfr_long = full_join(tmp_cases, tmp_deaths)
cfr_long = left_join(cfr_long, tmp_red)

sum(cfr_long$Redundant==0, na.rm=T)
sum(tmp_red$Redundant==0, na.rm=T)

cfr_long = cfr_long[!is.na(cfr_long$Cases),]
cfr_long = cfr_long[cfr_long$Economic_setting!="High income",]
cfr_long = cfr_long[cfr_long$Age_Groups!="0m_47m",] # "0m_35m",
cfr_long = cfr_long[cfr_long$Redundant==F | is.na(cfr_long$Redundant),]

## Titles for graphs
cfr_long$Title = paste(cfr_long$Location, "\n(", cfr_long$Author, ", ", cfr_long$Year, ")", 
                       "\n", cfr_long$Study_period, sep="")
cfr_long$study_no = factor(as.numeric(as.factor(cfr_long$Title)))

hcfr_summary = aggregate(cfr_long[,c("Title","Economic_setting")], cfr_long[,c("Title","Economic_setting")],  "length")[,1:3]
hcfr_summary$Training = hcfr_summary$Title.1>2
hcfr_summary$Title[hcfr_summary$Title.1>2]
hcfr_summary$Title[hcfr_summary$Title.1<3]
table(hcfr_summary$Economic_setting, hcfr_summary$Training)

