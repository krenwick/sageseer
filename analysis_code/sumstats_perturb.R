#############################################################################
# Summary stats: GCM
# Code created Feb. 2017
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=11)) # sized for ppt
library(Hmisc) # rcorr function for spearman's w/ p-val

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
setwd("/Users/poulterlab1/version-control/sageseer/")

# folder path:
dpath <- "data/"
opath <- "figures/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_perturb.csv", sep="")) %>%
  mutate(change=(predicted-baseline))
unit <- read.csv(paste(dpath, "focal_sites_by_zone.csv", sep="")) %>%
  dplyr::select(site,elev:NA_L1NAME)

# Calculate change and direction:
m4 <- merged %>% 
  mutate(cat=ifelse(change>0, "increase", "decrease"))

################################################################################
# Test: lmer
library(lme4)
mer2 <- merged 
mer2$mag <- as.factor(mer2$mag)
mer2$change2 <- abs(mer2$change)

# start one model at a time for ease interpreting
m <- lmer(data=mer2[mer2$model=="AK",], formula=change2~mag + (1|site))
summary(m) # temp4 has biggest effect

m <- lmer(data=mer2[mer2$model=="DRS",], formula=change2~mag + (1|site))
summary(m) # temp4 again

m <- lmer(data=mer2[mer2$model=="DGVM-full-400ppm",], formula=change2~mag + (1|site))
summary(m) # temp4 again

m <- lmer(data=mer2[mer2$model=="randfor",], formula=change2~mag + (1|site))
summary(m) # temp4 again... really??

mm <- mer2 %>% group_by(model,mag) %>% summarise(c=mean(change2))
# yes, really!

# for randfor, look just at split between variables (unfare bc of .2)
mer3 <- filter(mer2, mag!=.2)
m <- lmer(data=mer3[mer3$model=="randfor",], formula=change2~var + (1|site))
summary(m)

