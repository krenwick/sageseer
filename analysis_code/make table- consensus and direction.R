#############################################################################
# Make table: consensus and direction
# Code created Sep 2016
#############################################################################
rm(list=ls())
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt
library(dplyr) # must load ggplot first!
library(tidyr)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Documents/sageseer/Figures/"
opath <- "/Users/poulterlab1/Documents/sageseer/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data-co2.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() 
m3 %>% group_by(model) %>% summarise(mbase=mean(baseline),mpred=mean(predicted))
unit <- read.csv(paste(dpath, "focal_sites_by_zone.csv", sep="")) %>%
  select(site,elev:Name)

m4 <- m3 %>% 
  mutate(perc_change=(predicted-baseline)) %>%
  mutate(cat=ifelse(perc_change>0, "increase", "decrease")) %>%
  mutate(cat=ifelse(perc_change==0, "nochange", cat)) 

# Function to get % of sites with increase vs. decrease + confidence
# for testing:
data=m4
magn=4
varn="temp"
direction_change <- function(data,magn,varn) {
  d2 <- data %>%
    dplyr::select(site, model:mag,perc_change) %>%
    #PROBLEM: 0/0 = NaN, so sites with no cover that still have no cover are cut
    #PROBLEM: sites with no cover in baseline get ration of Inf
    # To leave these in, uncomment following lines (BAD SOLUTION)
    #mutate(perc_change=ifelse(perc_change==Inf,999,perc_change)) %>%
    #mutate(perc_change=ifelse(is.na(perc_change),-999,perc_change)) %>%
    filter(mag==magn&var==varn) %>%
    #filter(perc_change!=Inf) %>% # get rid of sites with zero as baseline
    group_by(site) %>%
    summarise(n=n(),n.increase=sum(perc_change>0), n.decrease=sum(perc_change<0)) %>%
    mutate(conf2=n.increase-n.decrease) %>%
    mutate(consensus=ifelse(conf2>0,"increase","nada")) %>%
    mutate(consensus=ifelse(conf2<0,"decrease",consensus)) %>%
    mutate(consensus=ifelse(conf2==0,"unsure",consensus)) %>%
    mutate(confidence=ifelse(n==n.increase|n==n.decrease,"high","medium")) %>%
    mutate(confidence=ifelse(consensus=="unsure","low",confidence)) %>%
    # remove sites with < 3 models (cuts 1 site)
    filter(n>2)
  
  conf <- table(d2$confidence)
  table(d2$consensus)
  table(d2$conf2)
  table(d2$n) # down to just 619 sites
  # categorize levels of agreement
  full_agreement <- table(d2$confidence)[1]/nrow(d2)
  some_agreement <- table(d2$confidence)[3]/nrow(d2)
  consensus <- full_agreement+some_agreement
  
  #among sites with consensus, what is the direction of change?
  perc_inc <- nrow(d2 %>% filter(consensus=="increase"))/nrow(d2)
  perc_dec <- nrow(d2 %>% filter(consensus=="decrease"))/nrow(d2)
  unsure <- nrow(d2 %>% filter(consensus=="unsure"))/nrow(d2)
  numbs <- cbind(full_agreement,some_agreement,consensus,perc_inc,perc_dec,unsure)
  return(numbs)
}

# set up df for results
results <- cbind.data.frame(var="temp",mag=4,direction_change(m4,4,"temp"))
results <- rbind(results,cbind.data.frame(var="temp",mag=2,direction_change(m4,2,"temp")))
results <- rbind(results,cbind(var="temp",mag=.2,direction_change(m4,.2,"temp")))
results <- rbind(results,cbind(var="ppt",mag=.9,direction_change(m4,.9,"ppt")))
results <- rbind(results,cbind(var="ppt",mag=1.1,direction_change(m4,1.1,"ppt")))
results <- rbind(results,cbind(var="ppt",mag=1.2,direction_change(m4,1.2,"ppt")))

write.csv(results,paste(fpath,"consensus_table.csv",sep=""),row.names=F)



