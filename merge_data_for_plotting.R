#############################################################################
# use coordinates from Andy's project to extract data
# ouput csv with data in common format
# Sept. 2017: updates to file paths due to file reorganization
#############################################################################
rm(list=ls())
library(dplyr)
library(tidyr)

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Indiv_model_output/"

# load plot list w/ clim data + site list with Katie's odd coords
clim_dat <- read.csv(paste(dpath, "../focal_sites_for_comparison.csv", sep=""))
ksites <- read.csv(paste(dpath, "../KMR_sitelist2.csv", sep=""))

# load data from each model, mutate to make formats consistent
andy <- read.csv(paste(dpath, "AK_cover_predictions.csv",sep="")) %>%
  na.omit() %>%
  filter(baseline<6.8 & baseline>6) %>%
  distinct()
katie <- read.csv(paste(dpath, "KMR_cover_data-comp.csv", sep="")) %>%
  mutate(longitude=round(longitude,3), latitude=round(latitude,3)) %>%
  mutate(baseline=baseline*100,predicted=predicted*100)
caroline <- read.csv(paste(dpath, "focal_sites_for_comparison_CCrandfor_extract_new7-18.csv", sep=""))
daniel <- read.csv(paste(dpath, "DRS_726focusSites_recruitment_predictions_DaymetCorrected.csv", sep="")) %>%
  mutate(baseline=baseline*100,predicted=predicted*100)

# combine all into one data frame 
# FIRST: get all disparate data in the same format
c2 <- select(clim_dat,longitude:site) 
car2 <- select(caroline,longitude:site,model:projected) %>%
  rename(predicted=projected)
k2 <- merge(katie, ksites, by=c("longitude","latitude")) %>%
  select(longitude,latitude,site,model, var,mag,baseline,predicted)
d2 <- merge(daniel,c2, by="site",all.x=T, all.y=F) %>%
  select(longitude,latitude,site,model, var,mag,baseline,predicted)

# SECOND: rbind to combine them all
all <- rbind(d2,andy,k2,car2)

# THIRD: fix inconsistencies in var/mag coding
all2 <- mutate(all, var=ifelse(var=="tmax"|var=="tmean", "temp","ppt")) %>%
  mutate(mag=ifelse(mag==-.1,.9,mag)) %>%
  mutate(mag=ifelse(mag==.1,1.1,mag)) %>%
  mutate(mag=ifelse(mag==.2&var=="ppt",1.2,mag)) %>%
  na.omit()
# check- should have same # in each category
table(all2$var, all2$mag, all2$model)

# merge in the climate covariates
all3 <- merge(all2,clim_dat, by="site")
table(all3$var, all3$mag, all3$model)

# scale MAT so is degrees (original data lacked decimal)
all4 <- mutate(all3, MAT=MAT/10)

# output merged data
write.csv(all4, (paste(dpath, "merged_data-co2.csv", sep="")), row.names=F)

