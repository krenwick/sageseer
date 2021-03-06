#############################################################################
# Make table: consensus and direction
# Code created Sep 2016, update Feb. 2017
#############################################################################
rm(list=ls())
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt
library(dplyr) # must load ggplot first!
library(tidyr)

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
setwd("/Users/poulterlab1/version-control/sageseer/")

# folder path:
dpath <- "data/"
opath <- "figures/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_perturb.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() 

# look at mean baseline and mean predicted for each model
m3 %>% group_by(model) %>% summarise(mbase=mean(baseline),mpred=mean(predicted))

# calculate change
m4 <- m3 %>% mutate(change=(predicted-baseline)) 

# Function to get % of sites with increase vs. decrease + confidence
direction_change <- function(data,magn,varn) {
  d2 <- data %>%
    dplyr::select(site, model:mag,change) %>%
    filter(mag==magn&var==varn) %>%
    #filter(change!=Inf) %>% # get rid of sites with zero as baseline
    group_by(site) %>%
    summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<0)) %>%
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

write.csv(results,paste(opath,"consensus_table.csv",sep=""),row.names=F)



