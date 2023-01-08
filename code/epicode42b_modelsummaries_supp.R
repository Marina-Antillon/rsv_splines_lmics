#******************************
### Summaries of the models with severity ---
#******************************
# one set of tables for the chisq and one for the AIC.

## Community 

inc_sev = read.csv(paste(plotpre_out, "epicode01b_comm_prob_sev/modcomp_sev.csv", sep=""))

inc_sev$chi_string = NA
inc_sev$chi_string[2] = paste("P=", ifelse(inc_sev$Pr..Chisq.[2]>0.01, 
                                           as.character(round(inc_sev$Pr..Chisq.[2], digits=2)),"<0.01"),
                              " (DF=", as.character(inc_sev$Df[2]), ", Chi2=", 
                              as.character(round(inc_sev$Chisq[2], digits=2)), ")", sep="")
inc_sev$chi_string[3] = paste("P=", ifelse(inc_sev$Pr..Chisq.[3]>0.01, 
                                           as.character(round(inc_sev$Pr..Chisq.[3], digits=2)),"<0.01"),
                              " (DF=", as.character(inc_sev$Df[3]), ", Chi2=", 
                              as.character(round(inc_sev$Chisq[3], digits=2)), ")", sep="")

# inc_vsev = read.csv(paste(plotpre_out, "epicode03_sev_inc/modcomp_vsev.csv", sep=""))
# 
# inc_vsev$chi_string = NA
# inc_vsev$chi_string[2] = paste("P=", ifelse(inc_vsev$Pr..Chisq.[2]>0.01, 
#                                            as.character(round(inc_vsev$Pr..Chisq.[2], digits=2)),"<0.01"),
#                               " (DF=", as.character(inc_vsev$Chi.Df[2]), ", Chi2=", 
#                               as.character(round(inc_vsev$Chisq[2], digits=2)), ")", sep="")
# inc_vsev$chi_string[3] = paste("P=", ifelse(inc_vsev$Pr..Chisq.[3]>0.01, 
#                                            as.character(round(inc_vsev$Pr..Chisq.[3], digits=2)),"<0.01"),
#                               " (DF=", as.character(inc_vsev$Chi.Df[3]), ", Chi2=", 
#                               as.character(round(inc_vsev$Chisq[3], digits=2)), ")", sep="")

hospinc_sev = read.csv(paste(plotpre_out, "epicode12b_hosp_prob_sev/modcomp_sev.csv", sep=""))
hospinc_sev$chi_string = NA
hospinc_sev$chi_string[2] = paste("P=", ifelse(hospinc_sev$Pr..Chisq.[2]>0.01, 
                                               as.character(round(hospinc_sev$Pr..Chisq.[2], digits=2)),"<0.01"),
                                  " (DF=", as.character(hospinc_sev$Df[2]), ", Chi2=", 
                                  as.character(round(hospinc_sev$Chisq[2], digits=2)), ")", sep="")

hospinc_sev$chi_string[3] = paste("P=", ifelse(hospinc_sev$Pr..Chisq.[3]>0.01, 
                                               as.character(round(hospinc_sev$Pr..Chisq.[3], digits=2)),"<0.01"),
                                  " (DF=", as.character(hospinc_sev$Df[3]), ", Chi2=", 
                                  as.character(round(hospinc_sev$Chisq[3], digits=2)), ")", sep="")

hospinc_vsev = read.csv(paste(plotpre_out, "epicode12b_hosp_prob_sev/modcomp_vsev.csv", sep=""))
hospinc_vsev$chi_string = NA
hospinc_vsev$chi_string[2] = paste("P=", ifelse(hospinc_vsev$Pr..Chisq.[2]>0.01, 
                                                as.character(round(hospinc_vsev$Pr..Chisq.[2], digits=2)),"<0.01"),
                                   " (DF=", as.character(hospinc_vsev$Df[2]), ", Chi2=", 
                                   as.character(round(hospinc_vsev$Chisq[2], digits=2)), ")", sep="")

hospinc_vsev$chi_string[3] = paste("P=", ifelse(hospinc_vsev$Pr..Chisq.[3]>0.01, 
                                                as.character(round(hospinc_vsev$Pr..Chisq.[3], digits=2)),"<0.01"),
                                   " (DF=", as.character(hospinc_vsev$Df[3]), ", Chi2=", 
                                   as.character(round(hospinc_vsev$Chisq[3], digits=2)), ")", sep="")

### Full tables (supplement) ----

chi_table_sev = data.frame(`Income Group FE`=c(inc_sev$chi_string[2], #inc_vsev$chi_string[2],
                                               hospinc_sev$chi_string[2], hospinc_vsev$chi_string[2]),
                           `Income Group FE and RE`=c(inc_sev$chi_string[3], #inc_vsev$chi_string[3], 
                                                      hospinc_sev$chi_string[3], hospinc_vsev$chi_string[3]), check.names=F)

row.names(chi_table_sev) = c("Spline I.S: Probability of severity among cases in the community", 
                             # "Spline I.VS: Probability of extreme severity among severe cases in the community",
                             "Spline II.S: Probability of chest indrawing among hospitalizations", 
                             "Spline II.VS: Probability of extreme severity among all hospitalizations")

AIC_table_sev = data.frame(Global=c(inc_sev$AIC[1], hospinc_sev$AIC[1], hospinc_vsev$AIC[1]), # inc_vsev$AIC[1],
                           `Income Group in FE`=c(inc_sev$AIC[2],  hospinc_sev$AIC[2], hospinc_vsev$AIC[2]), # inc_vsev$AIC[2],
                           `Income Group in FE and RE`=c(inc_sev$AIC[3], hospinc_sev$AIC[3], hospinc_vsev$AIC[3]), # inc_vsev$AIC[3], 
                           check.names=F)
row.names(AIC_table_sev) = row.names(chi_table_sev)


### chi_table table as a latex table ----
tb_kb = knitr::kable(chi_table_sev, format="latex", escape=T,
                     row.names=T, caption = "Model selection via the generalized likelihood ratio test.
                     FE: fixed-effects. RE: random-effects. DF: degrees of freedom.",
                     label="modelchi2sev", 
                     align = c("R{3cm}", "R{3cm}"))
newtb = gsub("\\begin{table}", "\\begin{table}\begin{table}[h!]", tb_kb, fixed=T)
newtb = gsub("Chi2", "$\\chi^2$", tb_kb, fixed=T)
newtb = gsub("{l|", "{L{6cm}|", tb_kb, fixed=T)

write(newtb, file=paste0(plotpre_out, "epicode42/model_summaries_chi2_sev.tex"))

