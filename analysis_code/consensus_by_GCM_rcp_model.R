#############################################################################
# Look at consensus btwn GCMs vs. rcp vs. models
# Code created Oct 2016
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Documents/sageseer/figures/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_GCM.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() 

# Color Palette for GCMs (color-blind friendly)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

################################################################################
# agreement btwn gcms
m4 <- m3 %>% group_by(model,site,scenario) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(consensus=ifelse(conf2>0,"increase","nada")) %>%
  mutate(consensus=ifelse(conf2<0,"decrease",consensus)) %>%
  mutate(consensus=ifelse(conf2==0,"unsure",consensus)) %>%
  mutate(confidence=ifelse(n==n.increase|n==n.decrease,"high","nada")) %>%
  mutate(confidence=ifelse(n==n.increase+1|n==n.decrease+1,"medium",confidence)) %>%
  mutate(confidence=ifelse(n==n.increase+2|n==n.decrease+2,"low",confidence)) %>%
  group_by(model,scenario) %>%
  summarise(tot.high=sum(confidence=="high"), tot.med=sum(confidence=="medium"),
            tot.low=sum(confidence=="low")) %>%
  mutate(sum=tot.high+tot.med+tot.low) %>%
  mutate(perc=tot.high/sum)
m4 %>% filter(model!="MaxEntBin" & model!="MaxEntRaw")
mean(m4$perc) # agree on average 77%  of time

# agreement btwn rcps
m4 <- m3 %>% group_by(model,site,GCM) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(consensus=ifelse(conf2>0,"increase","nada")) %>%
  mutate(consensus=ifelse(conf2<0,"decrease",consensus)) %>%
  mutate(consensus=ifelse(conf2==0,"unsure",consensus)) %>%
  mutate(confidence=ifelse(n==n.increase|n==n.decrease,"high","low")) %>%
  group_by(model,GCM) %>%
  summarise(tot.high=sum(confidence=="high"), 
            tot.low=sum(confidence=="low")) %>%
  mutate(sum=tot.high+tot.low) %>%
  mutate(perc=tot.high/sum)
m4 %>% filter(model!="MaxEntBin" & model!="MaxEntRaw")
mean(m4$perc) # agree on average 80%  of time

# agreement btwn models
m4 <- m3 %>% 
  filter(model!="MaxEntBin" & model!="MaxEntRaw") %>%
  group_by(scenario,site,GCM) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(consensus=ifelse(conf2>0,"increase","nada")) %>%
  mutate(consensus=ifelse(conf2<0,"decrease",consensus)) %>%
  mutate(consensus=ifelse(conf2==0,"unsure",consensus)) %>%
  mutate(confidence=ifelse(n==n.increase|n==n.decrease,"high","nada")) %>%
  mutate(confidence=ifelse(n==n.increase+1|n==n.decrease+1,"medium",confidence)) %>%
  mutate(confidence=ifelse(n==n.increase+2|n==n.decrease+2,"low",confidence)) %>%
  mutate(confidence=ifelse(consensus=="unsure","none",confidence)) %>%
  group_by(GCM,scenario) %>%
  summarise(tot.high=sum(confidence=="high"), tot.med=sum(confidence=="medium"),
            tot.low=sum(confidence=="low"), issues=sum(confidence=="nada")) %>%
  mutate(sum=tot.high+tot.med+tot.low) %>%
  mutate(perc=tot.high/sum)
m4 
mean(m4$perc) # agree on average 43%  of time... not too bad :)
  


