###############################################################
# Citation link:
# https://dataverse.harvard.edu/file.xhtml?fileId=5370285&version=1.2
# Codes are obtained from the above link, in 02-meta-analysis.R, with adjusment
###############################################################

#### Preamble ####
# Purpose: Pre-processing of meta-analysis data
# Author: Sinan Ma, Yuean Wang, Yang Zhou
# Date: 15 Feb 2024
# Contact: cocoyang.zhou@mail.utoronto.ca
# License: MIT

library(tidyverse)
library(readxl) # reading excel files
library(ggplot2)
library(ggpubr) # for gg rearrange
library(dplyr)
options(warn=-1)

PATH = "inputs/data/02-metaanalysis-data.xlsx"

dat <- read_excel(PATH, sheet = "Data",  col_names = TRUE, skip = 3)
dat_studies <- dat[c(1:87)] |> 
  group_by(ID_RS) |>
  slice(1)

### dataframe of reports
dat_reports <- dat[c(1:87)]  |> 
  group_by(ID_R) |>
  slice(1)


# wrangling
dat$TerrorMeasure[dat$IV_Code==1] <- "Acts of violence"
dat$TerrorMeasure[dat$IV_Code==2] <- "Acts of violence"
dat$TerrorMeasure[dat$IV_Code==3] <- "Threat"
dat$TerrorMeasure[dat$IV_Code==4] <- "Threat"
dat$TerrorMeasure[dat$IV_Code==5] <- "Other"
dat$TerrorMeasure[dat$IV_Code==6] <- "Reported exposure"
dat$TerrorMeasure[dat$IV_Code==7] <- "Reported exposure"
dat$TerrorMeasure[dat$IV_Code==8] <- "Reported exposure"
dat$TerrorMeasure[dat$IV_Code==9] <- "Threat"
dat$TerrorMeasure[dat$IV_Code==10] <- "Threat"
dat$TerrorMeasure[dat$IV_Code==11] <- "Fear"
dat$TerrorMeasure[dat$IV_Code==12] <- "Anger"
dat$TerrorMeasure[dat$IV_Code==13] <- "Other"
dat$TerrorMeasure[dat$IV_Code==14] <- "Other"
dat$TerrorMeasure[dat$IV_Code==15] <- "Other"
dat$TerrorMeasure[dat$IV_Code==16] <- "Other"
dat$TerrorMeasure <- as.factor(dat$TerrorMeasure)



#wrangling
dat$TerrorType[dat$Type==0] <- "No ideology"
dat$TerrorType[dat$Type==1] <- "Islamist"
dat$TerrorType[dat$Type==2] <- "Extreme right"
dat$TerrorType[dat$Type==3] <- "Other"
dat$TerrorType[dat$Type==4] <- "Other"
dat$TerrorType[dat$Type==5] <- "State terror"
dat$TerrorType[dat$Type==6] <- "Other"
dat$TerrorType <- as.factor(dat$TerrorType)


write_csv(dat, "outputs/data/dat.csv")
write_csv(dat_studies, "outputs/data/dat_studies.csv")
write_csv(dat_reports, "outputs/data/dat_reports.csv")


# 1) outgroup dataset
dat_outgroup <- dat[dat$PA_Category == 10 |
                      dat$PA_Category == 99, ]
# 2) conservative shift dataset
dat_conservatism <- dat[dat$PA_Category == 5 |
                          dat$PA_Category == 6 |
                          dat$PA_Category == 7 |
                          dat$PA_Category == 8 |
                          dat$PA_Category == 9 |
                          dat$PA_Category == 11, ]
# 3) rally-around-the-flag dataset
dat_rally <- dat[dat$PA_Category == 1 |
                   dat$PA_Category == 2 |
                   dat$PA_Category == 3 |
                   dat$PA_Category == 4, ]


process<- function(df){
  ##########################################
  df$TerrorType2[df$Type==0] <- "No ideology"
  df$TerrorType2[df$Type==1] <- "Islamist"
  df$TerrorType2[is.na(df$TerrorType2)] <- "Other"
  df$TerrorType2 <- as.factor(df$TerrorType2)
  
  df$Islam <- ifelse(df$TerrorType2=="Islamist", yes=1, no=0)
  df$No_Ideology <- ifelse(df$TerrorType2=="No ideology", yes=1, no=0)
  df$Other_Ideology <- ifelse(df$TerrorType2=="Other", yes=1, no=0)
  
  ##########################################
  df$Design[df$TypeStudy==1] <- "Experiments"
  df$Design[df$TypeStudy==2] <- "Quasi-Long"
  df$Design[df$TypeStudy==3] <- "Correlations"
  df$Design[df$TypeStudy==4] <- "Quasi-Long"
  df$Design <- as.factor(df$Design)
  #convert characters into dummy variables
  df$Exp <- ifelse(df$TypeStudy==1, yes=1, no=0)
  df$QuasiLong <- ifelse(df$TypeStudy==2 | df$TypeStudy==4, yes=1, no=0)
  df$Corr <- ifelse(df$TypeStudy==3, yes=1, no=0)
  
  ##########################################
  
  df$Sample[df$GeneralPop==1] <- "General"
  df$Sample[df$StudentPop==1] <- "Student"
  df$Sample[is.na(df$Sample)] <- "Convenience"
  df$Sample <- as.factor(df$Sample)
  
  df$General <- ifelse(df$Sample=="General", yes=1, no=0)
  df$Student <- ifelse(df$Sample=="Student", yes=1, no=0)
  df$Convenience <- ifelse(df$Sample=="Convenience", yes=1, no=0)
  ##########################################
  
  df$TargetSim[df$OA_Similarity==2] <- "A lot"
  df$TargetSim[df$OA_Similarity==1] <- "A bit"
  df$TargetSim[df$OA_Similarity==0] <- "No"
  df$TargetSim <- as.factor(df$TargetSim)
  
  df$Lot <- ifelse(df$TargetSim=="A lot", yes=1, no=0)
  df$Bit <- ifelse(df$TargetSim=="A bit", yes=1, no=0)
  df$No <- ifelse(df$TargetSim=="No", yes=1, no=0)
  
  ##########################################
  
  df$Exposure <- ifelse(df$TerrorMeasure=="Acts of violence", yes=1, no=0)
  df$R_Exposure <- ifelse(df$TerrorMeasure=="Reported exposure", yes=1, no=0)
  df$Threat <- ifelse(df$TerrorMeasure=="Threat", yes=1, no=0)
  df$Emotions <- ifelse(df$TerrorMeasure=="Fear" | df$TerrorMeasure=="Anger", yes=1, no=0)
  df$Fear <- ifelse(df$TerrorMeasure=="Fear", yes=1, no=0)
  df$Anger <- ifelse(df$TerrorMeasure=="Anger", yes=1, no=0)
  df$Other_IV <- ifelse(df$TerrorMeasure=="Other", yes=1, no=0)
  
  df$Country2[df$Country=="US"] <- "US"
  df$Country2[df$Country=="Israel"] <- "Israel"
  df$Country2[is.na(df$Country2)] <- "Other"
  df$Country2 <- as.factor(df$Country2)
  #convert characters into dummy variables
  df$US <- ifelse(df$Country=="US", yes=1, no=0)
  df$Israel <- ifelse(df$Country=="Israel", yes=1, no=0)
  df$Other_c <- ifelse(df$US==0 & df$Israel==0, yes=1, no=0)
  return(df)
}
dat_outgroup <- process(dat_outgroup)
dat_conservatism <- process(dat_conservatism)
dat_rally <- process(dat_rally)

write_csv(dat_outgroup, "outputs/data/dat_outgroup.csv")
write_csv(dat_conservatism, "outputs/data/dat_conservatism.csv")
write_csv(dat_rally, "outputs/data/dat_rally.csv")





