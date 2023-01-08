## Configuration

library(ggplot2)
# library(reshape2)
# library(scales)
library(grid)
# library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(cubelyr)
library(stringr)

library(mgcv) # you want function gam. THERE's an issue with the R 4.1.1
# here's the error: *** recursive gc invocation
library(splines)
library(MASS) # for fitdistr()
library(mvtnorm)
library(meta)
library(metafor)
library(Hmisc)
library(lme4)
library(gamm4)
library(epitools)
library(RColorBrewer)
library(abind)
library(openxlsx)

# to center titles among plots in ggplot.
library(magrittr)
library(gtable)

# related to country populations
library(countrycode)
library(wpp2017)

################################
# USEFUL FUNCTIONS
################################

meanpi = function(x){return(c(est = mean(x), lci = unname(quantile(x, 0.025)), hci = unname(quantile(x, 0.975))))}
meanmedianpi = function(x){return(c(mean_est = mean(x), median_est = median(x), lci = unname(quantile(x, 0.025)), hci = unname(quantile(x, 0.975))))}

logit = function(x){
  x[x<0.001] = 0.001
  x[x>0.999] = 0.999
  log(x/(1-x))
}
logit(c(0, 0.5, 1)) # sanity check
logistic = function(x){exp(x)/(1+exp(x))}
substrRight = function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
trim = function (x) gsub("^\\s+|\\s+$", "", x)

if (!dir.exists(file.path("./", "out_raw_data"))){
  dir.create(file.path("./", "out_raw_data"))
}
if (!dir.exists(file.path("./", "fig_raw_data"))){
  dir.create(file.path("./", "fig_raw_data"))
}
if (!dir.exists(file.path("./", "out_raw_data_sensitivity"))){
  dir.create(file.path("./", "out_raw_data_sensitivity"))
}
if (!dir.exists(file.path("./", "fig_raw_data_sensitivity"))){
  dir.create(file.path("./", "fig_raw_data_sensitivity"))
}


wrapper = function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}

ci_string = function(myvector){
  result = paste(round(sort(myvector)[2]), 
                 " (", round(min(myvector)), "-", 
                 round(max(myvector)), ")", sep="")
  return(result)
}

mean_se_string = function(myvector){
  result = paste("Mean: ", round(sort(myvector)[2])/100, 
                 ", SE: ", round(max(myvector)-min(myvector))/4/100, sep="")
  return(result)
}

ci_string_1000 = function(myvector){
  if(min(myvector)>1000){
    result = paste(format(round(sort(myvector)[2]/1000, 0), nsmall=0, big.mark=","), " (", 
                   format(round(min(myvector)/1000, 0), nsmall=0, big.mark=","), "-", 
                   format(round(max(myvector)/1000, 0), nsmall=0, big.mark=","), ")", sep="")
  }else{
    result = paste(format(round(sort(myvector)[2]/1000, 3), nsmall=0, big.mark=","), " (", 
                   format(round(min(myvector)/1000, 3), nsmall=0, big.mark=","), "-", 
                   format(round(max(myvector)/1000, 3), nsmall=0, big.mark=","), ")", sep="")
  }
  return(result)
}

ci_string_comma = function(myvector){
  result = paste(format(round(sort(myvector)[2], 0), nsmall=0, big.mark=","), " (", 
                 format(round(min(myvector), 0), nsmall=0, big.mark=","), "-", 
                 format(round(max(myvector), 0), nsmall=0, big.mark=","), ")", sep="")
  return(result)
}

options(scipen = 4)
options(digits = 4)

ci_string_dec = function(myvector, dec){
  result = paste(format(round(sort(myvector)[2], dec), nsmall=dec, big.mark=","), " (", 
                 format(round(min(myvector), dec), nsmall=dec, big.mark=","), "-", 
                 format(round(max(myvector), dec), nsmall=dec, big.mark=","), ")", sep="")
  return(result)
}

ci_string_dec_1M = function(myvector){
  myv2 = sort(myvector)
  ind = ifelse(all(myv2<1e6), 0, min(which(myv2>=1e6)))
  
  result = switch(ind+1,
                  paste(format(round(myv2[2], 0), nsmall=0, big.mark=","), " (", 
                        format(round(myv2[1], 0), nsmall=0, big.mark=","), "-", 
                        format(round(myv2[3], 0), nsmall=0, big.mark=","), ")", sep=""),
                  paste(format(round(myv2[2]/1e6, 2), nsmall=2, big.mark=","), "M (", 
                        format(round(myv2[1]/1e6, 2), nsmall=2, big.mark=","), "M-", 
                        format(round(myv2[3]/1e6, 2), nsmall=2, big.mark=","), "M)", sep=""), 
                  paste(format(round(myv2[2]/1e6, 2), nsmall=2, big.mark=","), "M (", 
                        format(round(myv2[1], 0), nsmall=0, big.mark=","), "-", 
                        format(round(myv2[3]/1e6, 2), nsmall=2, big.mark=","), "M)", sep=""),                  
                  paste(format(round(myv2[2], 0), nsmall=0, big.mark=","), " (", 
                        format(round(myv2[1], 0), nsmall=0, big.mark=","), "-", 
                        format(round(myv2[3]/1e6, 2), nsmall=2, big.mark=","), "M)", sep=""))

  return(result)
}

############################
# Useful ggplot stuff
############################

themebar = theme(axis.text.x = element_text(face="bold", color="black", size=rel(.5), angle=0),
                 axis.title.x = element_text(size = rel(.5), angle = 0, face="bold"),
                 axis.text.y = element_text(face="bold", color="black", size=rel(.5), angle=0), 
                 axis.title.y = element_text(size = rel(.5), angle = 90, face="bold"),
                 panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
                 legend.text = element_text(size = rel(0.7), face = "bold", lineheight=0.8),
                 legend.position = "bottom",
                 legend.box = "vertical",
                 legend.background = element_rect(fill=NA, size=0.25, linetype="solid", colour ="black"),
                 legend.key = element_rect(fill = "white"),
                 # legend.title = element_blank(),
                 panel.grid.major = element_line(colour="gray", linetype = "dotted"),
                 panel.background = element_rect(fill = NA),
                 strip.background = element_rect(fill = NA),
                 strip.text = element_text(size=rel(.5), face="bold")) # , strip.text = element_blank()

themebar2 = theme(axis.text.x = element_text(color="black", size=8, angle=0),
                 axis.title.x = element_text(size = 8, angle = 0, face="bold"),
                 axis.text.y = element_text(color="black", size=8, angle=0), 
                 axis.title.y = element_text(size = 8, angle = 90, face="bold"),
                 panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
                 legend.text = element_text(size = 8, face = "bold", lineheight=0.8),
                 legend.position = "bottom",
                 legend.box = "vertical",
                 legend.background = element_rect(fill=NA, size=0.25, linetype="solid", colour ="black"),
                 legend.key = element_rect(fill = "white"),
                 # legend.title = element_blank(),
                 panel.grid.major = element_line(colour="gray70", linetype = "dotted"),
                 panel.grid.minor = element_line(colour="gray60", linetype = "dotted"),
                 panel.background = element_rect(fill = NA),
                 strip.background = element_rect(fill = NA),
                 strip.text = element_text(size=8, face="bold")) # , strip.text = element_blank()

# display.brewer.pal(n = 9, name = 'Set1') # Only if I want to visualize them
Set0 = brewer.pal(n = 9, name = "Set1") # Include do-nothing option, which will be delineated in gray.
Set0 = c(Set0[c(3:5)], "#404040")

