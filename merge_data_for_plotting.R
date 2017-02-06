#############################################################################
# use coordinates from Andy's project to extract data
# ouput csv with data in common format
# Sept. 2017: updates to file paths due to file reorganization
#############################################################################
rm(list=ls())
library(dplyr)
library(tidyr)

# List of "bad" sites
bad <- c(495,496,497,498,580,581,583,632,633,634,668,62)

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
setwd("/Users/poulterlab1/version-control/sageseer/")

# folder path:
dpath <- "data/indiv_model_output/"

# load plot list w/ clim data
clim_dat <- read.csv(paste(dpath, "../focal_sites_for_comparison.csv", sep=""))
ksites <- read.csv(paste(dpath, "../KMR_sitelist2.csv", sep=""))

# load data from each model, mutate to make formats consistent
andy <- read.csv(paste(dpath, "AK_cover_predictions.csv",sep="")) %>%
  na.omit() %>%
  group_by(model,site,var,mag) %>%
  summarise_each(funs(mean)) %>%
  select(longitude,latitude,site,model,var,mag,baseline,predicted)
katie <- read.csv(paste(dpath, "KMR_cover-fullmodel_noSSM_400ppm.csv", sep="")) %>%
  mutate(longitude=round(longitude,3), latitude=round(latitude,3)) %>%
  mutate(baseline=baseline*100,predicted=predicted*100)
caroline <- read.csv(paste(dpath, "RF_perturb_rawoutput2.csv", sep=""))
daniel <- read.csv(paste(dpath, "DRS_GISSM-output_726AKpg_Exp1-Sensitivity_DaymetCorrected.csv", sep="")) %>%
  mutate(baseline=baseline*100,predicted=predicted*100)

# combine all into one data frame 
# FIRST: get all disparate data in the same format
car2 <- select(caroline,lon,lat,site,model,scenario,baseline,predicted) %>%
  rename(longitude=lon,latitude=lat) %>%
  mutate(scenario2=ifelse(scenario=="02inctemp","tmean_0.2","fix")) %>%
  mutate(scenario2=ifelse(scenario=="2inctemp","tmean_2.0",scenario2)) %>%
  mutate(scenario2=ifelse(scenario=="4inctemp","tmean_4.0",scenario2)) %>%
  mutate(scenario2=ifelse(scenario=="ppt10dec","ppt_0.9",scenario2)) %>%
  mutate(scenario2=ifelse(scenario=="ppt10inc","ppt_1.1",scenario2)) %>%
  mutate(scenario2=ifelse(scenario=="ppt20inc","ppt_1.2",scenario2)) %>%
  separate(scenario2,c("var","mag"),sep="_") %>%
  select(-scenario) %>%
  select(longitude:model,var,mag,baseline,predicted)
k2 <- merge(katie, ksites, by=c("longitude","latitude")) %>%
  select(longitude,latitude,site,model, var,mag,baseline,predicted)
d2 <- daniel %>% select(-X) %>%
  select(longitude,latitude,site,model, var,mag,baseline,predicted) %>%
  mutate(model="DRS")

# SECOND: rbind to combine them all
all <- rbind.data.frame(d2,andy,k2,car2)

# THIRD: fix inconsistencies in var/mag coding
all2 <- mutate(all, var=ifelse(var=="tmax"|var=="tmean", "temp","ppt")) %>%
  mutate(mag=ifelse(mag==-.1,.9,mag)) %>%
  mutate(mag=ifelse(mag==.1,1.1,mag)) %>%
  mutate(mag=ifelse(mag==.2&var=="ppt",1.2,mag)) %>%
  na.omit()
# check- should have same # in each category
#table(all2$var, all2$mag, all2$model)

# merge in the climate covariates
all3 <- merge(all2,clim_dat, by="site")

# Eliminate "bad" sites, including ND point Katie doesn't have
all4 <- filter(all3, site %in% bad==FALSE)
table(all4$var, all4$mag, all4$model) # Check- should be 714

# output merged data
write.csv(all4, (paste(dpath, "../merged_data_perturb.csv", sep="")), row.names=F)

