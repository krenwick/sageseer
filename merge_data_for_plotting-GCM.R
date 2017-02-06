#############################################################################
# use coordinates from Andy's project to extract data
# ouput csv with data in common format
# Sept. 2016: updates to file paths due to file reorganization
# Jan 2017: more minor updates to read in new RF
#############################################################################
rm(list=ls())
library(dplyr)
library(tidyr)

# List of "bad" sites
bad <- c(495,496,497,498,580,581,583,632,633,634,668,62)

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
setwd("/Users/poulterlab1/version-control/sageseer/")

# folder path:
dpath <- "data/Indiv_model_output/"

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
RF <- read.csv(paste(dpath, "RF_allmodels.csv", sep="")) %>%
  mutate(GCM2=ifelse(GCM=="ccsm","CCSM4","fix")) %>%
  mutate(GCM2=ifelse(GCM=="cesm","CESM1-CAM5",GCM2)) %>%
  mutate(GCM2=ifelse(GCM=="giss","GISS-E2-H-CC",GCM2)) %>%
  mutate(GCM2=ifelse(GCM=="hgem","HadGEM2-AO",GCM2)) %>%
  mutate(GCM2=ifelse(GCM=="mpi","MPI-ESM-LR",GCM2)) %>%
  select(-GCM) %>%
  rename(GCM=GCM2)
#Maxraw <- read.csv(paste(dpath, "focal_sites_for_comparison_CAC_maxentRawoutput.csv", sep="")) %>%
 # rename(longitude=lon,latitude=lat)
#Maxbin <- read.csv(paste(dpath, "focal_sites_for_comparison_CAC_maxentBinaryoutput.csv", sep="")) %>%
 # mutate(model="MaxEntBin") %>%
  #rename(longitude=lon,latitude=lat)
daniel <- read.csv(paste(dpath, "DRS_GISSM-output_726AKpg_Exp2-GCM-Scenarios_DaymetCorrected.csv", sep="")) %>%
  mutate(baseline=baseline*100,predicted=predicted*100) %>%
  select(-X) %>%
  mutate(scenario=gsub("RCP", "rcp",scenario)) 

# SECOND: rbind to combine them all
k_RF <- katie %>% select(site,longitude,latitude) %>%
  group_by(site) %>% summarise_each(funs(mean)) %>%
   merge(RF, by="site") %>%
  select(model, site:GCM)
nrow(k_RF)
all <- rbind(katie,a2,k_RF,daniel) #,Maxraw,Maxbin
tail(all)

# Eliminate "bad" sites 
# (problem in initial presence data from which focal sites chosen)
all2 <- filter(all, site %in% bad==FALSE)

# check- should have 714 in each category
table(all2$scenario, all2$GCM, all2$model)

# merge in the climate covariates
# load plot list w/ clim data
clim_dat <- read.csv(paste(dpath, "../focal_sites_for_comparison.csv", sep=""))
all3 <- merge(all2,clim_dat, by="site")
#table(all3$scenario, all3$GCM, all3$model)

# output merged data
write.csv(all3, (paste(dpath, "../merged_data_GCM.csv", sep="")), row.names=F)

