#coded by Philipp Tellenbach

library(dplyr)

#declare all file names
file_name <- c("data_hospitalizations",
               "data_long_cfr_train",
               "data_long_cfr_val",
               "data_long_hospinc_train",
               "data_long_hospinc_val",
               "data_long_hospincsev_train",
               "data_long_hospincsev_val",
               "data_long_hospincVsev_train",
               "data_long_hospincvsev_val",
               "data_long_inc_train",
               "data_long_inc_val",
               "data_long_incsev_train",
               "data_long_incsev_val",
               "data_long_incVsev_train",
               "data_long_incVsev_val")

#declare use category for hospitalisation
usecat <- c("train",
            "val")

#declare sample size variable
size_lookup <- c("rsv_cases", #data_hospitalizations.csv
                 "Cases",     #data_long_cfr_train.csv
                 "Cases",     #data_long_cfr_val.csv
                 "Pop",       #data_long_hospinc_train.csv
                 "Pop",       #data_long_hospinc_val.csv
                 "Pop",       #data_long_hospincsev_train.csv
                 "Pop",       #data_long_hospincsev_val.csv
                 "Pop",       #data_long_hospincVsev_train.csv
                 "Pop",       #data_long_hospincvsev_val.csv
                 "Pop",       #data_long_inc_train.csv
                 "Pop",       #data_long_inc_val.csv
                 "Pop_sev",   #data_long_incsev_train.csv
                 "Pop_sev",   #data_long_incsev_val.csv
                 "Pop_vsev",  #data_long_incVsev_train.csv
                 "Pop_vsev"   #data_long_incVsev_val.csv
                 )

#declare age group variable
agegroup_lookup <- c("Age_Groups", #data_hospitalizations.csv
                 "Age_Groups", #data_long_cfr_train.csv
                 "Age_Groups", #data_long_cfr_val.csv
                 "Age_Groups", #data_long_hospinc_train.csv
                 "Age_Groups", #data_long_hospinc_val.csv
                 "Age_Groups", #data_long_hospincsev_train.csv
                 "Age_Groups", #data_long_hospincsev_val.csv
                 "Age_Groups", #data_long_hospincVsev_train.csv
                 "Age_Groups", #data_long_hospincvsev_val.csv
                 "Age_Groups", #data_long_inc_train.csv
                 "Age_Groups", #data_long_inc_val.csv
                 "Age_Groups", #data_long_incsev_train.csv
                 "Age_Groups", #data_long_incsev_val.csv
                 "Age_Groups", #data_long_incVsev_train.csv
                 "Age_Groups"  #data_long_incVsev_val.csv
)

#declare event variable
event_lookup <- c("hosp",       #data_hospitalizations.csv
                 "Deaths",     #data_long_cfr_train.csv
                 "Deaths",     #data_long_cfr_val.csv
                 "Cases",      #data_long_hospinc_train.csv
                 "Cases",      #data_long_hospinc_val.csv
                 "Cases",      #data_long_hospincsev_train.csv
                 "Cases",      #data_long_hospincsev_val.csv
                 "Cases",      #data_long_hospincVsev_train.csv
                 "Cases",      #data_long_hospincvsev_val.csv
                 "Cases",      #data_long_inc_train.csv
                 "Cases",      #data_long_inc_val.csv
                 "Cases_sev",  #data_long_incsev_train.csv
                 "Cases_sev",  #data_long_incsev_val.csv
                 "Cases_vsev", #data_long_incVsev_train.csv
                 "Cases_vsev"  #data_long_incVsev_val.csv
)

#summary by study
#for each defined file
#open the file
for (i in 1:length(file_name)){
  #if the file is hospitalizations split it into 2 files first: 
  #train and val
  #by use train and val
  if(file_name[i] == "data_hospitalizations"){
    
    for (usecat in usecat){
      # file_data <- read.csv(paste(file_name[i],"csv",sep=".")) #file_name[1])
      file_data <- read.csv("./data/hospitalizations.csv")
      data_hospitalizations_train <- file_data[file_data$use=="train",]
      data_hospitalizations_val <- file_data[file_data$use=="val",]
      
      #train sample size by study
      samplesize_bystudy <- aggregate(eval(parse(text=size_lookup[i])) ~ citation, data = eval(parse(text=paste(file_name[i],usecat,sep="_"))), FUN=sum)
      colnames(samplesize_bystudy)  = c("Citation", "Sample_size")
      samplesize_bystudy$file = paste(file_name[i],usecat,sep="_")
       
      #train age group count by study
      agegroupcount_bystudy <- aggregate(eval(parse(text=agegroup_lookup[i])) ~ citation, data = eval(parse(text=paste(file_name[i],usecat,sep="_"))), FUN=length)
      colnames(agegroupcount_bystudy)  = c("Citation", "Age_groups")
      agegroupcount_bystudy$file = paste(file_name[i],usecat,sep="_")
      
      #train events by study
      event_bystudy <- aggregate(eval(parse(text=event_lookup[i])) ~ citation, data = eval(parse(text=paste(file_name[i],usecat,sep="_"))), FUN=sum)
      colnames(event_bystudy)  = c("Citation", "Events")
      event_bystudy$file = paste(file_name[i],usecat,sep="_")
      
      if (usecat == "train"){
        summary = full_join(samplesize_bystudy, agegroupcount_bystudy)
        summary = full_join(summary, event_bystudy)
        summary = relocate(summary, file)
        summary_bystudy = summary
      } else{
        summary = full_join(samplesize_bystudy, agegroupcount_bystudy)
        summary = full_join(summary, event_bystudy)
        summary = relocate(summary, file)
        summary_bystudy = full_join(summary_bystudy, summary)
      }
    } 

  } else {
    file_data <- read.csv(paste(file_name[i],"csv",sep=".")) #file_name[1])
    file_name[i]
    #train sample size by study
    samplesize_bystudy <- aggregate(eval(parse(text=size_lookup[i])) ~ Title, data = file_data, FUN=sum)
    colnames(samplesize_bystudy)  = c("Citation", "Sample_size")
    samplesize_bystudy$file = file_name[i]
    
    #train age group count by study
    agegroupcount_bystudy <- aggregate(eval(parse(text=agegroup_lookup[i])) ~ Title, data = file_data, FUN=length)
    colnames(agegroupcount_bystudy)  = c("Citation", "Age_groups")
    agegroupcount_bystudy$file = file_name[i]
     
    #train events by study
    event_bystudy <- aggregate(eval(parse(text=event_lookup[i])) ~ Title, data = file_data, FUN=sum)
    colnames(event_bystudy)  = c("Citation", "Events")
    event_bystudy$file = file_name[i]
     
    summary = full_join(samplesize_bystudy, agegroupcount_bystudy)
    summary = full_join(summary, event_bystudy)
    summary = relocate(summary, file)
    summary_bystudy = full_join(summary_bystudy, summary)
    
  }
}

#summary by dataset ----
#totals
summary <- aggregate(Sample_size ~ file, data = summary_bystudy, FUN=sum)
colnames(summary)  = c("file", "sample_size_total")
summary_bydataset <- summary

#means
summary <- aggregate(Sample_size ~ file, data = summary_bystudy, FUN=mean)
colnames(summary)  = c("file", "sample_size_mean")
summary_bydataset <- full_join(summary_bydataset, summary)

summary = aggregate(Sample_size ~ file, data = summary_bystudy, FUN=median)
colnames(summary)  = c("file", "sample_size_median")
summary_bydataset <- full_join(summary_bydataset, summary)

summary = aggregate(Sample_size ~ file, data = summary_bystudy, FUN=min)
colnames(summary)  = c("file", "sample_size_min")
summary_bydataset <- full_join(summary_bydataset, summary)

#median
summary = aggregate(Sample_size ~ file, data = summary_bystudy, FUN=max)
colnames(summary)  = c("file", "sample_size_max")
summary_bydataset <- full_join(summary_bydataset, summary)

#age groups -----
#totals
summary = aggregate(Age_groups ~ file, data = summary_bystudy, FUN=sum)
colnames(summary)  = c("file", "agegroup_totals")
summary_bydataset <- full_join(summary_bydataset, summary)

#mean
summary = aggregate(Age_groups ~ file, data = summary_bystudy, FUN=mean)
colnames(summary)  = c("file", "agegroup_mean")
summary_bydataset <- full_join(summary_bydataset, summary)

#median
summary = aggregate(Age_groups ~ file, data = summary_bystudy, FUN=median)
colnames(summary)  = c("file", "agegroup_median")
summary_bydataset <- full_join(summary_bydataset, summary)

#min
summary = aggregate(Age_groups ~ file, data = summary_bystudy, FUN=min)
colnames(summary)  = c("file", "agegroup_min")
summary_bydataset <- full_join(summary_bydataset, summary)

#max
summary = aggregate(Age_groups ~ file, data = summary_bystudy, FUN=max)
colnames(summary)  = c("file", "agegroup_max")
summary_bydataset <- full_join(summary_bydataset, summary)

#events ----
#totals
summary = aggregate(Events ~ file, data = summary_bystudy, FUN=sum)
colnames(summary)  = c("file", "Events_totals")
summary_bydataset <- full_join(summary_bydataset, summary)

# mean
summary = aggregate(Events ~ file, data = summary_bystudy, FUN=mean)
colnames(summary)  = c("file", "Events_mean")
summary_bydataset <- full_join(summary_bydataset, summary)

# median 
summary = aggregate(Events ~ file, data = summary_bystudy, FUN=median)
colnames(summary)  = c("file", "Events_median")
summary_bydataset <- full_join(summary_bydataset, summary)

# min 
summary = aggregate(Events ~ file, data = summary_bystudy, FUN=min)
colnames(summary)  = c("file", "Events_min")
summary_bydataset <- full_join(summary_bydataset, summary)

# max 
summary = aggregate(Events ~ file, data = summary_bystudy, FUN=max)
colnames(summary)  = c("file", "Events_max")
summary_bydataset <- full_join(summary_bydataset, summary)

write.csv(summary_bydataset, "./summarizing_studies/study_summaries_7dec.csv",
          row.names = F)

