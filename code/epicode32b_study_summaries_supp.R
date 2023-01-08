# ****************************************
# Summarizing the studies ---------
# To make Table A in Appendix S1.1
# ****************************************

# ****************************************
# Bring packages needed for Rmarkdown ----------
# ****************************************

library(knitr)
library(kableExtra)
library(magrittr)
library(tidyr)

# ****************************************
# Bring summaries in ----------
# ****************************************

study_summaries = read.csv("./out_sims/summarizing_studies/study_summaries_7dec.csv")
# From Phil's summaries

colnames(study_summaries)[grep("sample_size", colnames(study_summaries))] = gsub("sample_size_", "samplesize_", colnames(study_summaries)[grep("sample_size", colnames(study_summaries))])
  
study_summaries_long = study_summaries %>% 
  pivot_longer(samplesize_total:Events_max, 
               names_to = c("characteristic", "stat"),
               names_sep="_",
               values_to = "value") %>% 
  mutate(stat = ifelse(stat=="totals", "total", stat)) %>% 
  mutate(characteristic = ifelse(characteristic=="Events", "events", characteristic)) %>% 
  pivot_wider(id_col = c(file, characteristic),
              names_from = stat,
              values_from = value)

study_summaries_wide = study_summaries_long %>% 
            mutate(range = paste0("(", min, "-", max, ")")) %>% 
            pivot_wider(id_cols=file, 
                        names_from=characteristic,
                        values_from=c("total", "mean", "median", "range")) %>% 
  mutate(mean_samplesize = round(mean_samplesize), median_samplesize = round(median_samplesize), 
         mean_events = round(mean_events), median_events = round(median_events),
         mean_agegroup = round(mean_agegroup, 1), median_agegroup = round(median_agegroup, 1),
         total_samplesize = round(total_samplesize)) %>% 
  relocate(`mean_samplesize`, `median_samplesize`, `range_samplesize`, .after=`total_samplesize`) %>% 
  relocate(`total_events`, `mean_events`, `median_events`, `range_events`, .after=`range_samplesize`) %>% 
  relocate(`total_agegroup`, `mean_agegroup`, `median_agegroup`, `range_agegroup`, .after=`range_events`)

study_summaries_wide2 = study_summaries_wide[c(11,12,5,6,1:4,13:16,7:10),]
study_summaries_wide2$file= rep(c("Estimation", "Validation"), 8)
tb_kb = knitr::kable(study_summaries_wide2, 
                     format="latex", escape=T, row.names=F, 
                     col.names = c("Spline", rep(c("Total", "Mean", "Median", "Range"), times=3)),
                     caption = "Characteristics of the studies included for the main splines and the supplemental severity splines. A summary of the study sizes, the observed events of interest (cases, hospitalizations, deaths) and the number of data points (mutually exclusive age groups) present in the data.", 
                     align = c(rep("L{1.5cm}", 1), rep(c(rep("R{1cm}", 3), "R{1.7cm}"), times=3))) %>% 
                     add_header_above(c(" " = 1, "Study sample" = 4, "Events" = 4, 
                                        "Data points (age groups)"= 4),
                                      color="white", background = "black", bold=T)

newtb = kableExtra::group_rows(tb_kb, "Community-based incidence", 1, 2)
newtb = kableExtra::group_rows(newtb, "Hospital-based incidence", 3, 4)
newtb = kableExtra::group_rows(newtb, "Probability of hospitalization (among cases in the community)", 5, 6)
newtb = kableExtra::group_rows(newtb, "In-hospital deaths (hCFR)", 7, 8)
newtb = kableExtra::group_rows(newtb, "Probability of severity (among cases in the community)", 9, 10)
newtb = kableExtra::group_rows(newtb, "Probability of very severe cases (among cases in the community)", 11, 12)
newtb = kableExtra::group_rows(newtb, "Probability of severity (among cases in the hospital)", 13, 14)
newtb = kableExtra::group_rows(newtb, "Probability of very severe cases (among cases in the hospital)", 15, 16)

newtb = row_spec(newtb, 0, color="white", background = "black", bold=T)
newtb = gsub("\\begin{table}", "\\begin{table}[h!] \n\\small", newtb, fixed=T)
newtb = gsub("\\multicolumn{5}{l}", "\\rowcolor{lightgray}  \\multicolumn{13}{l}", newtb, fixed=T)

write(newtb, file="./out_sims/supp_study_summaries_dec7.tex")
