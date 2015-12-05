#############################################################################
# use coordinates from Andy's project to extract data
# ouput csv with data in common format
#############################################################################
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"

# load data from each model
clim_dat <- read.csv(paste(dpath, "plot_group_mean_climate.csv", sep=""))
andy <- read.csv(paste(dpath, "", sep="AK_cover_predictions.csv"))
katie <- read.csv(paste(dpath, "katie_sample_output.csv", sep=""))
caroline <- read.csv(paste(dpath, "plot_group_locations_CC_randfor_extract.csv", sep=""))
daniel <- read.csv(paste(dpath, "", sep=""))

# combine all into one data frame (if column names the same, should work!)
#andy <- dplyr::select(andy,plot_group:predicted)
all1 <- rbind(andy,katie,caroline) # daniel

# merge in the climate covariates
all2 <- merge(all1,clim_dat, by="plot_group")
head(all2)
dim(all2)

# calculate % change
all3 <- mutate(all2, change=(predicted-baseline)/baseline)

# output merged data
write.csv(all3, (paste(dpath, "merged_data.csv", sep="")), row.names=F)

# make graphs of preliminary data
merged <- read.csv(paste(dpath, "merged_data.csv", sep=""))
head(merged)
m2 <- merged %>%
  na.omit()

m3 <- dplyr::select(m2, -var, -baseline, -predicted) %>%
  spread(model, change) 
head(m3)

ggplot(m3, aes(x=AK, y=KR, color=PRISM_avg_ann_tmax)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_abline(intercept=0, slope=1)

ggplot(data=m2, aes(x=PRISM_avg_ann_tmax, y=change, color=model)) +
  geom_smooth(method=lm) +
  geom_point(aes(x=PRISM_avg_ann_tmax, y=change, color=model)) +
  geom_hline(y=0) +
  xlab(expression("Mean Annual Temperature ("*~degree*"C)")) +
  ylab("Percent Change") +
  ggtitle(expression("4"*~degree*"C Temperature Increase"))
  
  
  
  
