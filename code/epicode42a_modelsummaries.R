#******************************
### Summaries of the models ---
#******************************
# one set of tables for the chisq and one for the AIC.

### Incidence ----
inc = read.csv(paste(plotpre_out, "epicode01a_comm_inc/modcomp.csv", sep=""))

inc$chi_string = NA
inc$chi_string[2] = paste("P=", ifelse(inc$Pr..Chisq.[2]>0.01, 
                                       as.character(round(inc$Pr..Chisq.[2], digits=2)),"<0.01"),
                          " (DF=", as.character(inc$Df[2]), ", Chi2=", 
                          as.character(round(inc$Chisq[2], digits=2)), ")", sep="")
inc$chi_string[3] = paste("P=", ifelse(inc$Pr..Chisq.[3]>0.01, 
                                       as.character(round(inc$Pr..Chisq.[3], digits=2)),"<0.01"),
                          " (DF=", as.character(inc$Df[3]), ", Chi2=", 
                          as.character(round(inc$Chisq[3], digits=2)), ")", sep="")

### Hospital-based incidence ----
hospinc = read.csv(paste(plotpre_out_hinc, "epicode12a_hospinc/modcomp.csv", sep=""))
hospinc$chi_string = NA
hospinc$chi_string[2] = paste("P=", ifelse(hospinc$Pr..Chisq.[2]>0.01, 
                                           as.character(round(hospinc$Pr..Chisq.[2], digits=2)),"<0.01"),
                                 " (DF=", as.character(hospinc$Df[2]), ", Chi2=", 
                                 as.character(round(hospinc$Chisq[2], digits=2)), ")", sep="")

hospinc$chi_string[3] = paste("P=", ifelse(hospinc$Pr..Chisq.[3]>0.01, 
                                           as.character(round(hospinc$Pr..Chisq.[3], digits=2)),"<0.01"),
                              " (DF=", as.character(hospinc$Df[3]), ", Chi2=", 
                              as.character(round(hospinc$Chisq[3], digits=2)), ")", sep="")

### Probability of hospitalization ----
hosp = read.csv(paste(plotpre_out, "epicode11_hosp_prob/modcomp_hospassume1.csv", sep=""))
hosp$chi_string = NA
hosp$chi_string[2] = paste("P=", ifelse(hosp$Pr..Chisq.[2]>0.01, 
                                        as.character(round(hosp$Pr..Chisq.[2], digits=2)),"<0.01"),
                                     " (DF=", as.character(hosp$Df[2]), ", Chi2=", 
                                     as.character(round(hosp$Chisq[2], digits=2)), ")", sep="")

hosp$chi_string[3] = paste("P=", ifelse(hosp$Pr..Chisq.[3]>0.01, 
                                        as.character(round(hosp$Pr..Chisq.[3], digits=2)),"<0.01"),
                           " (DF=", as.character(hosp$Df[3]), ", Chi2=", 
                           as.character(round(hosp$Chisq[3], digits=2)), ")", sep="")

### Probability of death among hospitalized individuals
cfr = read.csv(paste(plotpre_out, "/epicode21_cfr/modcomp.csv", sep=""))
cfr$chi_string = NA
cfr$chi_string[2] = paste("P=", ifelse(cfr$Pr..Chisq.[2]>0.01, 
                                       as.character(round(cfr$Pr..Chisq.[2], digits=2)),"<0.01"),
                              " (DF=", as.character(cfr$Df[2]), ", Chi2=", 
                              as.character(round(cfr$Chisq[2], digits=2)), ")", sep="")
cfr$chi_string[3] = paste("P=", ifelse(cfr$Pr..Chisq.[3]>0.01, 
                                       as.character(round(cfr$Pr..Chisq.[3], digits=2)),"<0.01"),
                          " (DF=", as.character(cfr$Df[3]), ", Chi2=", 
                          as.character(round(cfr$Chisq[3], digits=2)), ")", sep="")

### Full tables (main body) ----
chi_table = data.frame(`Income Group FE`=c(inc$chi_string[2], hospinc$chi_string[2],
                               hosp$chi_string[2], cfr$chi_string[2]),
                       `Income Group FE and RE`=c(inc$chi_string[3], hospinc$chi_string[3], 
                               hosp$chi_string[3], cfr$chi_string[3]), check.names=F)
  
row.names(chi_table) = c("Spline I: Community-based incidence", "Spline II: Hospital-based incidence",
                         "Spline III: Probability of hospitalization", "Spline IV: Probability of death among hospitalized cases")

AIC_table = data.frame(Global=c(inc$AIC[1], hospinc$AIC[1], hosp$AIC[1], cfr$AIC[1]),
                       `Income Group in FE`=c(inc$AIC[2], hospinc$AIC[2], hosp$AIC[2], cfr$AIC[2]),
                       `Income Group in FE and RE`=c(inc$AIC[3], hospinc$AIC[3], hosp$AIC[3], cfr$AIC[3]),
                       check.names=F)
row.names(AIC_table) = row.names(chi_table)

### chi_table table as a latex table ----
tb_kb = knitr::kable(chi_table, format="latex", escape=T,
                     row.names=T, 
                     caption = "Model selection via the generalized likelihood ratio test. 
                     FE: fixed-effects. RE: random-effects. DF: degrees of freedom.",
                     label="modelchi2", 
                     align = c("R{3cm}", "R{3cm}"))
newtb = gsub("\\begin{table}", "\\begin{table}\begin{table}[h!]", tb_kb, fixed=T)
newtb = gsub("Chi2", "$\\chi^2$", tb_kb, fixed=T)
newtb = gsub("{l|", "{L{4cm}|", tb_kb, fixed=T)
write(newtb, file=paste0(plotpre_out, "epicode42/model_summaries_chi2.tex"))

