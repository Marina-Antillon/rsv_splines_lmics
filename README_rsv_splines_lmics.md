The burden of respiratory syncytial virus in infants of low- and middle-income countries: A semi-parametric, meta-regression approach
=======================

On behalf of the Respiratory Syncytial Virus in Europe ([RESC-EU](https://resc-eu.org))  

Administered by Marina Antillon, PhD.  

Collaborating centers:

Center for Health Economics Research and Modeling of Infectious Diseases (CHERMID) 
University of Antwerp
2610 Antwerp, Belgium

Department of Epidemiology and Public Health 
Swiss Tropical and Public Health (Swiss TPH) Institute  
(An affiliated institute of the University of Basel)  
4123 Allschwil, Switzerland

COPYRIGHT 2022, Swiss TPH and University of Antwerp

---

# Project Objective 

In the current study, we leverage the RSV data collected on cases, hospitalizations, and deaths in a systematic review in combination with flexible generalized additive mixed models (GAMM) to characterize the age burden of RSV incidence, hospitalization, and hospital-based case fatality rate (hCFR) across different settings. We aim to address the following open issues in the field:
- We re-estimate the peak, median, and mean age of infection, therefore informing discussions on the ideal "vaccine window" of a prophylactic drug with a defined duration of protection. 
- We re-estimate and reconsider the burden of RSV across settings. 

## Analysis with generalized semi-parametric mixed effects models

** Link to the package descriptions ** mgcv and any other
https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/random.effects.html

---

# Brief description

The analysis is broadly defined in four parts, each of these parts is executed in various R code files:  
I. Master file and config file that are useful for all parts of the analysis (prefix: epicode00a and epicode00b). 
II. Estimating the splines for each outcome: community-based incidence, hospitalization probability, hospital-based incidence, hCFR. 
III. Summarizing the splines and determining the window of protection needed.
IV. Burden re-estimations and summaries.

## File structure, output, and dependencies

**epicode00: MASTER. Run the simulations**
code00a_master: runs the code overall. Calls all other code in the code directory to make the results in out_figs and out_sims. 
code00b_config: functions and libraries necessary.

**epicode01 community-based incidence**
epicode01a_comm_inc: runs spline model of community-based incidence. Necessary for later projections of incidence in countries. Makes modcomp.csv which is a comparison of the models with and without country income group as a predictor, as well as three .Rdata files that contain the splines for the projections later on (one global spline from the model without income group as predictor, and two splines for LMIC and UMIC countries from the model with income group as a predictor). These files are stored in ./out_figs/epicode01a_comm_inc/. It also makes figures of splines for different income groups that are not shown in the paper, which are stored in ./out_figs/epicode01a_comm_inc/, as well as validation figures which are shown in Supplementary Section S2.
epicode01b_comm_prob_sev: runs spline model of community-based cases are severe or very severe. Necessary for later projections of incidence in countries. Makes modcomp_sev.csv which is a comparison of the models with and without country income group as a predictor, as well as .Rdata files that contain the splines for the projections later on on severe and very severe cases in the community. These files are stored in ./out_figs/epicode01b_comm_prob_sev/. It also makes figures of splines that are not shown in the paper, and some forest plots of the probability of severity (also not shown in the paper), which are stored in ./out_figs/epicode01b_comm_prob_sev/, as well as validation figures which are shown in Supplementary Section S2.

**epicode11 and epicode12 - hospital-based incidence and probability of hospitalization**

epicode11_hosp_prob: runs spline model of probability of hospitalization among community cases. Necessary for later projections of hospitalizations in countries. Makes modcomp.csv which is a comparison of the models with and without country income group as a predictor, as well as an .Rdata file that contains the spline for the projections later on (one global spline from the model without income group as predictor, and two splines for LMIC and UMIC countries from the model with income group as a predictor). These files are stored in ./out_figs/epicode11_hosp_prob/. It also makes figures of splines for different income groups that are not shown in the paper, which are stored in ./out_figs/epicode11_hosp_prob/, as well as validation figures which are shown in Supplementary Section S2.

epicode12a_hospinc: runs spline model of probability of hospitalization among community cases. Necessary for later projections of hospitalizations in countries. Makes modcomp.csv which is a comparison of the models with and without country income group as a predictor, as well as an .Rdata file that contains the spline for the projections later on. These files are stored in ./out_figs/epicode12a_hospinc/. It also makes figures of splines for different income groups that are not shown in the paper, which are stored in ./out_figs/epicode12a_hospinc/, as well as validation figures which are shown in Supplementary Section S2.

epicode12b_hosp_prob_sev: runs spline model of probability of hospitalization among community cases. Necessary for later projections of hospitalizations in countries. Makes modcomp.csv which is a comparison of the models with and without country income group as a predictor, as well as an .Rdata files that contains the splines for the projections later on of severe and very severe cases in hospitals. These files are stored in ./out_figs/epicode12b_hosp_prob_sev/. It also makes figures of splines for different income groups that are not shown in the paper, which are stored in ./out_figs/epicode12b_hosp_prob_sev/, as well as validation figures which are shown in Supplementary Section S2.

**epicode21 - hospital-based case-fatality-rate (CFR)**
epicode21_cfr: runs spline model of probability of death among hospitalized cases. Necessary for later projections of deaths in countries. Makes modcomp.csv which is a comparison of the models with and without country income group as a predictor, as well as an .Rdata files that contains the spline for the projections later on. These files are stored in ./out_sims/epicode21_cfr/. It also makes figures of splines for different income groups that are not shown in the paper, which are stored in ./out_figs/epicode21_cfr/, as well as validation figures which are shown in Supplementary Section S2.

**epicode31, epicode32a, and epicode32b: study summaries**
These files allow us to describe the number of studies that are used for each spline model, the number of studies that are used for validation, and the number of studies used for 

These files produce Table 1 in the main paper and Table A in Supplementary Material 1.

**epicode41-epicode47: summary of all results and most tables and figures**
epicode41a_base_ribbons_together and epicode41b_base_ribbons_together_supp: take the spline files stored in ./out_sims/ and produce a multi-panel figure for the paper that show the splines for each outcome. These files make Figure 2 and S3 Text, Figure A (Supplementary Section S3.1.1). 

epicode42a_modelsummaries and epicode42b_modelsummaries_supp: these files summarize whether the variable country income group was a significant predictor for the splines. These files depend on the "modcomp.csv" files produced by code epicode01-epicode21. The results of these files are shown in Table 2 of the paper and S3 Text, Table A in (Supplementary Section S3.1.1).

epicode43_bring_in_pop: calculates the population size in each age_group of 1 month in order to make projections about the numbers of cases in different age groups in each country. The population data was pulled using the wpp2019 R package version 1.1-1. The populations for the analyses are shown in Table C of Supplementary Section 1.8. These files also make Tables D and E of Supplementary Section 1.8 which describe the countries that were included either in our analysis or in Li's analysis but not both. 

epicode44a_time_to_perc_cases and epicode44a_time_to_perc_cases_supp: these files calculate the critical windows of infection. These files depend on the splines produced by epicode01-epicode21 and the population size estimates produced by epicode43_bring_in_pop. These files make Figure 4 and Figure C of S3 Text (Supplementary Section S3.1.1) as well as burden_results_country_global.Rdata and burden_results_country_incomegroup.Rdata which are saved in "out_sims/epicode44a_time_to_perc_cases/" and "out_sims/epicode44b_time_to_perc_cases/". These .Rdata files are necessary for epicode45_burden_calculations, epicode46a_burden_ribbons, and epicode46b_ribbons_supp. 

epicode45a_burden_calculations and epicode45b_burden_figures: these files calculate the burden of disease in each country with each configuration of outcome and burden model (OMII/BMI, OMII/BMI, OMII/BMII, OMII/BMII) and make Figures 5 and 6 and Figures D-F in S3 Text, Supplementary Section 3.1.2.

epicode46a_burden_ribbons and epicode46b_ribbons_supp: these files draw the incidence of cases, hospitalizations, and deaths in each income group which each of the outcome models, OM I and OM II. These files result in Figure 3 and Figure B in S3 Text, Supplementary Section 3.1.1.

epicode47_country_specific_estimates: This makes country-specific estimates of cases, hospitalizations, and deaths under each model configuration (OMII/BMI, OMII/BMI, OMII/BMII, OMII/BMII). The country-specific estimates are given in Supplementary Zip File. Lastly, this code estimates both in-hospital and community-based deaths, which are shown in Table 3.

---
# Computational considerations

The tools for the analysis are coded in R. While we would highly recommend using the code within RStudio environment (in part because of it's features to manage the project with .RProj and packrat) this is not strictly necessary and the benefits of git and packrat are available from a classic R interface and a shell command line.

Some of the results tables for the project are produced automatically with the code in the project. This is done via the knitr and kableExtra packages. See the help links later in this document for more information.

A single run of the code takes about 60 minutes in a MacBook Pro (Mid-2014 model) with a 3 GHz Intel Core i7 processor and 16 GB of RAM. 

# Installation to-do list (all free)
- [R](https://www.r-project.org) (required)
- [renv](https://rstudio.github.io/renv/index.html) (packages installed for R; required)
- [RStudio](https://www.rstudio.com/) (highly recommended though not required; it integrates well with the other management tools used in the project. The free version is more than enough.)
- Latex engine (not required, though some documentation created automatically won't run). See [here](https://support.rstudio.com/hc/en-us/articles/200532257-Customizing-LaTeX-Options) for the recommended engines.

---

# Troubleshooting

*Issues with the renv repository:* If there is an issue with the repository, try typing renv:: rinit() and when prompted type the number 1, for Restored the project from lockfile.
*Adding a package to the renv library:* just use the command install.packages(). Note that doing this won't install the package for use with other projects. Then update the lockfile by typing renv::snapshot().
