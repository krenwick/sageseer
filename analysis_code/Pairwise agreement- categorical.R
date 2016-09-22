#############################################################################
# Look at pairwise agreement- categorical (increase/decrease)
# Code created Aug 2016
#############################################################################
rm(list=ls())
library(tidyverse)

# path to data
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data-co2.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() 
m3 %>% group_by(model) %>% summarise(mbase=mean(baseline),mpred=mean(predicted))
unit <- read.csv(paste(dpath, "focal_sites_by_zone.csv", sep="")) %>%
  select(site,elev:Name)
################################################################################
# New, super-simple analysis of ratio:
m4 <- m3 %>% 
  mutate(perc_change=(predicted/baseline)) %>%
  mutate(cat=ifelse(perc_change>1, "increase", "decrease")) %>%
  mutate(cat=ifelse(perc_change==1, "nochange", cat)) %>%
  dplyr::select(latitude.x,longitude.x,site, model:mag,cat)

############## Function to categorize PAIRWISE uncertainty
get_uncert <- function(data,model1,model2) {
  d2 <- filter(data, model==model1|model==model2) %>%
    dplyr::select(-latitude.x,-longitude.x) %>%
    tidyr::spread(model, cat)
  names(d2)[4] <- "m1"
  names(d2)[5] <- "m2"
  d3 <- d2 %>%
    mutate(uncert=ifelse(m1=="decrease"&m2=="decrease",-2,0)) %>%
    mutate(uncert=ifelse(m1=="decrease"&m2=="nochange",-1,uncert)) %>%
    mutate(uncert=ifelse(m1=="decrease"&m2=="increase",0,uncert)) %>%
    mutate(uncert=ifelse(m1=="increase"&m2=="increase",2,uncert)) %>%
    mutate(uncert=ifelse(m1=="increase"&m2=="nochange",1,uncert)) %>%
    mutate(uncert=ifelse(m1=="increase"&m2=="decrease",0,uncert)) %>%
    mutate(uncert=ifelse(m1=="nochange"&m2=="decrease",-1,uncert)) %>%
    mutate(uncert=ifelse(m1=="nochange"&m2=="increase",-1,uncert)) %>%
    mutate(uncert=ifelse(m1=="nochange"&m2=="nochange",4,uncert)) %>%
    group_by(var,mag,uncert) %>%
    summarise(n=n()) %>%
    mutate(total=sum(n)) %>%
    filter(uncert==0) %>%
    mutate(perc=n/total) %>%
    na.omit()
  return(d3)
}

##################################################
# Get pairwise uncert for each variable x mag x model combo
# This will show % of sites (perc) where models disagree on direction
CCDVM <- get_uncert(m4, "CC","DGVM")
TSRF <- get_uncert(m4, "AK","CC")
TSGISS <- get_uncert(m4, "AK","DRS") 
TSDVM <- get_uncert(m4, "AK","DGVM") 
RFGISS <- get_uncert(m4, "CC","DRS")
DG <- get_uncert(m4, "DGVM","DRS")


