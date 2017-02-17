#############################################################################
# Why does model x disagree?
# Pull out sites where each model disagrees and look for patterns
# Use data from GCM + sensitivity analysis. If they disagree, is it because precip
# change is high and disagree on response to precip, for example?
# Or: is there something odd about that site?
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt

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
  dplyr::select(site:GCM,change,cat)

d2 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") %>%
  group_by(site,GCM,scenario) %>%
  mutate(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf_cat=pmax(n.increase,n.decrease)) %>%
  mutate(rel_conf_cat=conf_cat/n) %>%
  mutate(consensus=ifelse(conf_cat==n.increase,"increase","decrease")) %>%
  mutate(consensus=ifelse(n.increase==n.decrease&n.increase==conf_cat,"unsure",consensus))

############################## Which model disagrees?
d3 <- d2 %>% dplyr::select(-change,-n,-conf_cat,-rel_conf_cat) %>%
  spread(model, cat) %>%
  #filter(scenario!="rcp45") %>%
  mutate(issue=ifelse(AK==GISSM_v1.6.3&AK==randfor&AK==DGVM,"none","split")) %>%
  mutate(issue=ifelse(AK==GISSM_v1.6.3&AK==randfor&AK!=DGVM,"DGVM",issue)) %>%
  mutate(issue=ifelse(AK==GISSM_v1.6.3&AK!=randfor&AK==DGVM,"RF",issue)) %>%
  mutate(issue=ifelse(AK!=GISSM_v1.6.3&AK==randfor&AK==DGVM,"GISSM",issue)) %>%
  mutate(issue=ifelse(randfor==GISSM_v1.6.3&randfor==DGVM&randfor!=AK,"TS",issue))
head(d3)

# Disagreement Table
table(d3$issue,d3$GCM)
table(d3$issue,d3$GCM,d3$scenario)
table(d3$issue, d3$GCM, d3$consensus)
table(d3$issue, d3$scenario)

# Across all scenarios: how often does each model disagree?
round(table(d3$issue)/7140,2)

# What about direction?
table(d3$issue,d3$conf2)
table(d3$issue,d3$consensus)
# convert to percentage
round(table(d3$issue,d3$consensus)/3570, 2)

################################################################################
# Look for climatic cause of differences
################################################################################
clim <- merged %>% group_by(site) %>% summarise_each(funs(mean))

##############################################
# Look at disagreement along PCA axes
###########################################
d4 <- filter(d3, scenario=="rcp85", GCM=="CCSM4")
p1 <- merge(d4,clim, by=c("site"), all.y=F)
ggplot(data=p1, aes(x=Comp.1*-1,y=Comp.2, color=issue, shape=consensus)) +
  geom_point()



c1 <- filter(d3, consensus=="increase")
c2 <- merge(c1,clim, by=c("site"), all.y=F)

ggplot(data=c2, aes(x=issue, y=bio1)) +
  geom_boxplot(notch=T)
# Time series disagrees on hot sites.
# DGVM disagrees on sites with less seasonality (bio3, bio7)
# winter and summer have similar temp. Why? Competition? check.
# also more winter precip. Sites cool, wet, strong seasonality of precip, weak
# seasonality of temp.

# Flip this and look at exceptions in the other direction
do1 <- filter(d3, consensus=="decrease")
do2 <- merge(do1,clim, by=c("site"), all.y=F)

ggplot(data=do2, aes(x=issue, y=bio3)) +
  geom_boxplot(notch=T)

# DGVM again differs on diurnal temp range (bio2)
# DGVM lower bio3- opposite of what we saw with reverse difference

################################################################################
# Look for magnitude cause: does RCP or GCM matter?
################################################################################
table(d3$issue,d3$consensus)
# TS pretty much always differs on consensus: increase. Previous analysis shows
# it is mostly the very hot sites where TS more pessimistic.
table(d3$issue,d3$consensus, d3$scenario)
ts <- filter(d3, issue=="TS")
table(ts$consensus, ts$GCM,ts$scenario)
table(ts$site, ts$consensus)

################################################################################
# I'm surprised that there are sites where DGVM more negative. Why?
################################################################################
DGVMneg <- filter(d3, issue=="DGVM", consensus=="increase")
DGVMneg2 <- merge(DGVMneg,merged, by=c("site", "scenario","GCM"), all.y=T) %>%
  filter(model=="DGVM")
head(DGVMneg2)
ggplot(data=DGVMneg2, aes(x=bio7, y=change, color=issue)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept=0)
#BIO4: temperature seasonality. Good!
#BIO5-6 ok too
#BIO7- really good. Temp range (bio5-bio6). Issues all at low range (less seasonal)

ggplot(data=DGVMneg2, aes(x=issue, y=bio1, color=issue)) +
  geom_boxplot(notch=T) 
#133 instances where DGVM decreases against the trend. How many total negs?
t1 <- filter(merged, model=="DGVM", change<=0)
nrow(t1) #828 negs + zeros
t1 <- filter(merged, model=="DGVM", change<0)
nrow(t1) #434 negs
t1 <- filter(merged, model=="DGVM", change<=-1)
nrow(t1) #101 where drops by > -1


# 1. Is the difference due to threshold location? Evidence: model agrees for 8.5,
# not for 4.5. Or agrees for extreme GCM, not mild. Basically, disagrees on the threshold
# where response switches from 1 to the other

# 2. Is the difference due to response to a specific variable?
# Evidence: for that site, agree on temp impact but not precip.

# 3. Is the difference due to CO2?
# Evidence: GCM agrees when run with constant CO2 (or agrees on decrease for simple 
#temp manipulation, but shows increase with GCM runs)
