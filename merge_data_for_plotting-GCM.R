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
c2 <- select(clim_dat,longitude:site) # need for merging

# FIRST: load data from each model, mutate to make formats consistent
andy <- read.csv(paste(dpath, "AK_cover_GCM_predictions_with_sites.csv",
                       sep="")) %>%
  select(-X) %>%
  group_by(model,site,scenario,GCM) %>%
  summarise_each(funs(mean)) %>%
  select(model,site,longitude,latitude,baseline,predicted,scenario,GCM) 
a2 <- data.frame(andy)
katie <- read.csv(paste(dpath, "KMR_cover_GCM-fullmodel.csv", sep="")) %>%
  mutate(baseline=baseline*100,predicted=predicted*100)
RF <- read.csv(paste(dpath, "focal_sites_for_comparison_CAC_RandforCorrectedoutput.csv", sep="")) %>%
  rename(longitude=lon,latitude=lat)
Maxraw <- read.csv(paste(dpath, "focal_sites_for_comparison_CAC_maxentRawoutput.csv", sep="")) %>%
  rename(longitude=lon,latitude=lat)
Maxbin <- read.csv(paste(dpath, "focal_sites_for_comparison_CAC_maxentBinaryoutput.csv", sep="")) %>%
  mutate(model="MaxEntBin") %>%
  rename(longitude=lon,latitude=lat)
daniel <- read.csv(paste(dpath, "DRS_GISSM-output_726AKpg_Exp2-GCM-Scenarios_DaymetCorrected.csv", sep="")) %>%
  mutate(baseline=baseline*100,predicted=predicted*100) %>%
  select(-X) %>%
  mutate(scenario=gsub("RCP", "rcp",scenario)) 

# SECOND: rbind to combine them all
all <- rbind(katie,a2,RF,Maxraw,Maxbin,daniel)
tail(all)

# check- should have same # in each category
table(all$scenario, all$GCM, all$model)

# merge in the climate covariates
all3 <- merge(all,clim_dat, by="site")
table(all3$scenario, all3$GCM, all3$model)

# scale MAT so is degrees (original data lacked decimal)
all4 <- mutate(all3, MAT=bio1/10)

# output merged data
write.csv(all4, (paste(dpath, "../merged_data_GCM.csv", sep="")), row.names=F)

