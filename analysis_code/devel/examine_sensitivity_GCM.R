#############################################################################
# Plots exploring response to climate change for each model 
# try histogram or violin plot
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
setwd("/Users/poulterlab1/version-control/sageseer/")

# folder path:
dpath <- "data/"
opath <- "figures/"

# Color Palette for GCMs (color-blind friendly)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_GCM.csv", sep="")) %>%
  mutate(change=(predicted-baseline))
unit <- read.csv(paste(dpath, "focal_sites_by_zone.csv", sep="")) %>%
  dplyr::select(site,elev:NA_L1NAME)

################################################################################
# Calculate change and direction:
m4 <- merged %>% 
  mutate(cat=ifelse(change>0, "increase", "decrease")) %>%
  # uncomment next line to add "nochange" back in
  #mutate(cat=ifelse(change==0, "nochange", cat)) %>%
  dplyr::select(site:GCM,change,cat) %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") 

#write.csv(m4, "~/Documents/sagemodeldata.csv")

d2 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") %>%
  filter(scenario=="rcp85") %>% #exclude RCP4.5 output
  group_by(site) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf_cat=pmax(n.increase,n.decrease)) %>%
  mutate(rel_conf_cat=conf_cat/n) %>%
  mutate(consensus=ifelse(conf_cat==n.increase,"increase","decrease")) %>%
  mutate(consensus=ifelse(n.increase==n.decrease&n.increase==conf_cat,"unsure",consensus)) %>%
  filter(n==max(n))
dim(d2)

################################################################################
# Plot histogram of absolute change, in original scale, 4-plot grid
################################################################################
d2 <- m4 %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") %>%
  filter(scenario=="rcp85")  #exclude RCP4.5 output

# Try histogram
ggplot(data=d2, aes(x=change, fill=GCM)) +
  geom_histogram() +
  geom_vline(xintercept=0) +
  facet_wrap(~model, scales="free")

# Try frequency polygon
ggplot(data=d2, aes(x=change, color=GCM)) +
  geom_freqpoly() +
  geom_vline(xintercept=0) +
  facet_wrap(~model, scales="free")

# Try Violinplot
ggplot(data=d2, aes(x=model, y=change, fill=GCM)) +
  geom_violin() +
  geom_hline(yintercept=0) +
  facet_wrap(~model, scales="free")

# Back to the bar chart
m5 <- m4 %>% mutate(abschange=abs(change)) %>%
  mutate(direction=ifelse(change>0,"increase","decrease")) %>%
  group_by(model,GCM,scenario, direction) %>%
  summarise(meanchange=mean(change), lower=meanchange-sd(change)/sqrt(length(change)),
            upper=meanchange+sd(change)/sqrt(length(change))) 
head(m5)
ggplot(data=m5, aes(x=as.factor(GCM), y=meanchange, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00"), 
                    name="Variable",
                    breaks=c("ppt", "temp"),
                    labels=c("Precip", "Temp")) +
  xlab("GCM") +
  ylab("Mean Change in Model Response") +
  facet_wrap(~model,scale="free")

# Try color by model instead of RCP (first option better for b/w)
ggplot(data=m5, aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=cbPalette, name="GCM") +
  xlab("GCM") +
  ylab("Mean Change in Model Response") +
  facet_wrap(~model,scale="free_y")
  #facet_wrap(~model) +
  opts(strip.background=theme_blank())

# Try boxplot instead of bar chart- show full distribution (NOPE! bar is best)
m5 <- m4 %>% mutate(abschange=abs(change)) %>%
  mutate(direction=ifelse(change>0,"increase","decrease")) 
ggplot(data=m5, aes(x=scenario, y=change, fill=GCM)) +
  #geom_boxplot(position=position_dodge(width=0.9), notch=T) +
  geom_violin() +
  geom_hline(yintercept=0) +
  #geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=cbPalette, name="GCM") +
  xlab("GCM") +
  ylab("Mean Change in Model Response") +
  facet_wrap(~model,scale="free_y")

# Is there a way to show how gcm rank order plays out spatially?

################################################################################
# Try a scatteplot showing baseline vs. mean predicted for each model
m6 <- m4 %>% group_by(site, model) %>%
  summarise_each(funs(mean))

ggplot(data=m6, aes(x=baseline, y=predicted)) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope=1,intercept=0, linetype="dashed") +
  facet_wrap(~model)

# RandFor predicted is a narrow range- look at distributions
m7 <- m6 %>% gather(time, value, baseline:predicted)
ggplot(data=m7, aes(x=model,y=value, fill=time)) +
  geom_boxplot() +
  ylim(c(0,75))

# Can it be due to averaging the gcm predictions?
rcp85 <- filter(m4, scenario=="rcp85")
ggplot(data=rcp85, aes(x=baseline, y=predicted, color=GCM)) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope=1,intercept=0, linetype="dashed") +
  #scale_x_continuous(limits=c(0,100)) +
  #scale_y_continuous(limits=c(0,100)) +
  facet_wrap(~model, scale="free")

m7 <- rcp85 %>% gather(time, value, baseline:predicted)
ggplot(data=m7, aes(x=GCM,y=value, fill=time)) +
  geom_boxplot() +
  facet_wrap(~model, scale="free")

# Focus just on randfor
randfor <- filter(rcp85, model=="RandFor")
ggplot(data=randfor, aes(x=baseline, y=predicted, color=GCM)) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope=1,intercept=0, linetype="dashed") +
  scale_x_continuous(limits=c(0,75)) +
  scale_y_continuous(limits=c(0,75)) 

##################################################
# How often does choice of GCM cause a change in the direction of response?
m5 <- m4 %>% 
  group_by(site,model,scenario) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(model,scenario) %>%
  summarise(nochange=sum(n.increase==5|n.decrease==5), n=n()) %>%
  mutate(perc=(n-nochange)/n)
(m5) # WOW! For SS and rcp45, it flips 73% of time! WHY?
mean(m5$perc) # 17%

# How often does RCP cause a flip?
m5 <- m4 %>% 
  group_by(site,model,GCM) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(model,GCM) %>%
  summarise(nochange=sum(n.increase==2|n.decrease==2), n=n()) %>%
  mutate(perc=(n-nochange)/n)
m5 # Again- what's up with GISSM?!
mean(m5$perc) # 8%

# How often does the ecological model cause a flip?
m5 <- m4 %>% 
  group_by(site,scenario,GCM) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(scenario,GCM) %>%
  summarise(nochange=sum(n.increase==2|n.decrease==2), n=n()) %>%
  mutate(perc=(n-nochange)/n)
m5 # All the time... rare for all 4 to agree
mean(m5$perc) # 82%

# For paper, more clear to say how often is the direction of change consistent?

# 1. Look at effect of RCP:--------------------------------------
# Split into model x GCM combos (20)
m5 <- m4 %>% 
  group_by(site,model,GCM) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(model,GCM) %>%
  summarise(nochange=sum(n.increase==2|n.decrease==2), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
m5 
mean(m5$perc) 

# Split by model (4): GISSM has lower agreement
m5 <- m4 %>% 
  group_by(site,model,GCM) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(model) %>%
  summarise(nochange=sum(n.increase==2|n.decrease==2), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
m5 
mean(m5$perc) 

# Split by GCM (5): (pretty similar, CCSM4 maybe a bit different)
m5 <- m4 %>% 
  group_by(site,model,GCM) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(GCM) %>%
  summarise(nochange=sum(n.increase==2|n.decrease==2), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
m5 

# Not split at all:
mean(m5$perc) #can do this cause all have same sample size

# 2. Look at effect of GCM:-----------------------------------------------------
# Make a table- for each model, consistent across: GCMs? RCPs?
m5 <- m4 %>% 
  group_by(site,model,scenario) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(model,scenario) %>%
  summarise(nochange=sum(n.increase==5|n.decrease==5), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
(m5) # Keep RCPs separate
mean(m5$perc) # overall is 83%

# Combine RCPs
m5 <- m4 %>% 
  group_by(site,model,scenario) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(model) %>%
  summarise(nochange=sum(n.increase==5|n.decrease==5), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
(m5) 

# Combine models
m5 <- m4 %>% 
  group_by(site,model,scenario) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(scenario) %>%
  summarise(nochange=sum(n.increase==5|n.decrease==5), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
(m5) 

# 2. Look at effect of veg model:-----------------------------------------------
m5 <- m4 %>% 
  group_by(site,GCM,scenario) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(GCM,scenario) %>%
  summarise(nochange=sum(n.increase==4|n.decrease==4), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
(m5) # Keep RCPs separate
mean(m5$perc) # overall is 83%

# Lower the standard- have just 3 models agree:
m5 <- m4 %>% 
  group_by(site,GCM,scenario) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(GCM,scenario) %>%
  summarise(nochange=sum(n.increase>=3|n.decrease>=3), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
(m5) 
mean(m5$perc) 

# Look at importance of RCP:
m5 <- m4 %>% 
  group_by(site,GCM,scenario) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(scenario) %>%
  summarise(nochange=sum(n.increase==4|n.decrease==4), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
(m5) 

# Look at importance of GCM:
m5 <- m4 %>% 
  group_by(site,GCM,scenario) %>%
  summarise(n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  group_by(GCM) %>%
  summarise(nochange=sum(n.increase==4|n.decrease==4), n=n()) %>%
  mutate(perc=(nochange)/n) %>%
  mutate(perc=round(perc,2))
(m5) # CCSM4 is odd

# All together now!-------------------
library(MASS)
m5 <- mutate(m4, direction=ifelse(cat=="decrease", 0, 1))
m1 <- stepAIC(lm(data=m5, direction~model*GCM*scenario))
anova(m1) #everything matters. For primary effects, model > site > GCM > scenario
library(lme4)
m1 <- lmer(data=m5, direction~model*GCM*scenario + (1|site))
summary(m1)
anova(m1)



# I don't much care about interactions (SS skews it I suspect)
# Try random effects for all but var of interest:


# Does scenario affect direction, holding other variables constant?
m2 <- lmer(data=m5, direction~scenario + (1|model) + (1|GCM) + (1|site))
summary(m2) # YES- significant. But minimal: .047
anova(m2)

# Does model affect direction, holding other variables constant?
m3 <- lmer(data=m5, direction~model + (1|scenario) + (1|GCM) + (1|site))
anova(m3) # Yes.
summary(m3) # DGVM more positive
anova(m3)

################################################################################
# Next: Look at uncertainty in the magnitude of change
################################################################################
# Problem: obs are correlated (any output from same GCM but diff model, for example)
# Also, some sites similar to other sites.
# Solution? Multiple mixed effects models, with stuff not focused on as random effects
# Site: random on intercept, model/GCM/rcp: random on interaction too
# OR? Separate model? Nah... loose power
# This is basically a full factorial design. Which factor has biggest effect?
# I need to do this separately for inc vs. decrease. Could do absolute value, but
# Results will differ, and that is interesting! Problem: then I get diff sample sizes

# I think what I really want is to do 2 separate 3-factor anovas
anova(lm(data=m4, change~model*GCM*scenario*cat)) # too complicated

# First: For sites where we expect an increase
m5 <- filter(m4, cat=="increase")
table(m5$model, m5$GCM, m5$scenario)

# First look at impact of RCP
m1 <- lmer(data=m5, change~scenario + (1|site) + (1|GCM) + (1|model))
anova(m1) # all groups differ
summary(m1) # Estimate is 

# Second: For sites with a decrease
library(lme4)
m1 <- lmer(data=m4, change~model*GCM*scenario*cat))
summary(m1)

# Look at difference between RCP: basically, what is the effect size?
m1 <- lm(data=m4, change~scenario*cat)
summary(m1)

# Look at difference between GCMs:
m1 <- lm(data=m4, change~scenario*cat)
summary(m1)
