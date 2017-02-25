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

################################################################################
# DIRECTION OF CHANGE
# Calculate % of sites with pos vs. neg response for each model
m5 <- m4 %>% group_by(model) %>%
  summarise(n.inc=sum(cat=="increase"), n.dec=sum(cat=="decrease"), 
            perc=round(n.dec/(n.inc+n.dec),2))
m5 #12-28

# Break down by RCP and GCM
m6 <- m4 %>% group_by(model, scenario) %>%
  summarise(n.inc=sum(cat=="increase"), n.dec=sum(cat=="decrease"), 
            perc=round(n.dec/(n.inc+n.dec),3))
m6

# what is the temp range of pos + negative changes for each model?
m7 <- m4 %>% group_by(model, cat) %>%
  summarise(m.change=round(mean(change),2), min=round(min(change),2),
            max=round(max(change),2), sd=round(sd(change),2))
m7 #12-28

# Calculate percent absolute agreement vs. consensus
d2 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") %>%
  group_by(site,GCM,scenario) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf_cat=pmax(n.increase,n.decrease)) %>%
  mutate(rel_conf_cat=conf_cat/n) %>%
  mutate(consensus=ifelse(conf_cat==n.increase,"increase","decrease")) %>%
  mutate(consensus=ifelse(n.increase==n.decrease&n.increase==conf_cat,"unsure",consensus)) %>%
  mutate(rel_cat2=ifelse(consensus=="decrease", rel_conf_cat*-1, rel_conf_cat)) %>%
  filter(n==max(n))
dim(d2)
head(d2)
round(table(d2$conf2)/7140,2)
# perfectly agree 44% of time
# consensus 83% of time

########### Look at similarity in direction using Cohen's Kappa
kap1 <- m4 %>% dplyr::select(site,model,scenario,GCM,cat) %>%
  spread(model,cat)
kap2 <- as.matrix(kap1[,4:7])

library(irr)

# Can't find an organizd way to loop through models- calc. pairwise
head(kap2)
AK.KR <- kappa2(kap2[,1:2])
AK.DS <- kappa2(kap2[,c(1,3)])
AK.CC <- kappa2(kap2[,c(1,4)])
KR.DS <- kappa2(kap2[,2:3])
KR.CC <- kappa2(kap2[,c(2,4)])
DS.CC <- kappa2(kap2[,3:4])

ps <- c(AK.KR$p.value,AK.DS$p.value,AK.CC$p.value,KR.DS$p.value,KR.CC$p.value,
        DS.CC$p.value)
# Correct p-values for familywise error rate
# can choose several methods, holm perhaps better than bonferonni
newp <- p.adjust(ps, method="holm")
round(newp,2) # significance isn't terribly useful here

# look at actual kappa values. Higher -> more agreement
kaps <- c(AK.KR$value,AK.DS$value,AK.CC$value,KR.DS$value,KR.CC$value,
          DS.CC$value)
kaps #
############################
# re-do kaps for GCMs
kapGCM1 <- m4 %>% dplyr::select(site,model,scenario,GCM,cat) %>%
  spread(GCM,cat)
kap2 <- as.matrix(kapGCM1[,4:8])

p1 <- kappa2(kap2[,1:2])
p2 <- kappa2(kap2[,c(1,3)])
p3 <- kappa2(kap2[,c(1,4)])
p4 <- kappa2(kap2[,c(1,5)])
p5 <- kappa2(kap2[,c(2,3)])
p6 <- kappa2(kap2[,c(2,4)])
p7 <- kappa2(kap2[,c(2,5)])
p8 <- kappa2(kap2[,c(3,4)])
p9 <- kappa2(kap2[,c(3,5)])
p10 <- kappa2(kap2[,c(4,5)])

kapsGCM <- c(p1$value,p2$value,p3$value,p4$value,p5$value,p6$value,p7$value,
             p8$value,p8$value,p10$value)
range(kapsGCM)
mean(kapsGCM)

############
# kappa for emissions scenario
kapRCP1 <- m4 %>% dplyr::select(site,model,scenario,GCM,cat) %>%
  spread(scenario,cat)
kap2 <- as.matrix(kapRCP1[,4:5])
kappa2(kap2)

################################################################################
# MAGNITUDE OF CHANGE
################################################################################
# Look at correlations in predicted change between models
# Use spearmans rank correlation, since can't assume parametric
# for each model, take mean response across RCP x GCM combos
m8 <- m4 %>% group_by(site, model) %>%
  dplyr::select(site,model,change) %>%
    summarise_each(funs(mean)) %>%
  spread(model,change)
m9 <- as.matrix(m8[,2:5])  

cors <- rcorr(m9, type="spearman")
p.corrs <- as.numeric(cors$P)
round(p.adjust(p.corrs, method="holm"),2) # lose .0561, rest still <.05.


################################################################################
# Aggregate response: vulnerability scale
################################################################################
d2 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") %>%
  #filter(scenario=="rcp85") %>% #exclude RCP4.5 output
  group_by(site) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf_cat=pmax(n.increase,n.decrease)) %>%
  mutate(rel_conf_cat=conf_cat/n) %>%
  mutate(consensus=ifelse(conf_cat==n.increase,"increase","decrease")) %>%
  mutate(consensus=ifelse(n.increase==n.decrease&n.increase==conf_cat,"unsure",consensus)) %>%
  mutate(rel_cat2=ifelse(consensus=="decrease", rel_conf_cat*-1, rel_conf_cat)) %>%
  filter(n==max(n))
dim(d2)
head(d2)
hist(d2$n.decrease)

# Split by RCP
d3 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  group_by(site, scenario) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  mutate(vuln=n.decrease/n) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf3=conf2/n)
head(d3)
table(d3$scenario,d3$vuln)

# Average vulnerability score:
d3 %>% group_by(scenario) %>%
  summarise(vuln=mean(vuln), cat=mean(conf2),cat2=mean(conf3))

# % of sites with increase
d3 %>% group_by(scenario) %>%
  summarise(perc.inc=round(sum(conf2>0)/714,2), per.dec=round(sum(conf2<0)/714,2), 
            uns=round(sum(conf2==0)/714,2))

# % increase by GCM, both emissions scenarios
# split by GCM
GCM1 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  group_by(site, GCM) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  mutate(vuln=n.decrease/n) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf3=conf2/n)

GCM1 %>%
  group_by(GCM) %>%
  summarise(perc.inc=round(sum(conf2>0)/714,2), per.dec=round(sum(conf2<0)/714,2), 
            uns=round(sum(conf2==0)/714,2))

###############
# mean absolute value of agreement for each rcp
d3 %>% group_by(scenario) %>%
  summarise(agree=mean(abs(conf2)))
# mean value for each direction
d3 %>% group_by(scenario) %>%
  summarise(pos=mean(conf2[conf2>0]), neg=mean(conf2[conf2<0]))
hist(d3$conf2)
table(d3$scenario, d3$conf2)

# Split into categories: high vs. low confidence, each direction
d4 <- d3 %>% group_by(scenario) %>%
  summarise(pos.high=sum(conf2>11), pos.low=sum(conf2<11&conf2>0),
            neg.high=sum(conf2<=-11), neg.low=sum(conf2>=-11&conf2<0))


######################################################
# Look at min and max MAT for these categories
d5 <- merged %>% 
  mutate(cat=ifelse(change>0, "increase", "decrease")) %>%
  dplyr::select(site:GCM,change,cat, bio1) %>% 
  group_by(site, scenario) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0),MAT=mean(bio1)) %>%
  mutate(vuln=n.decrease/n) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(cat2=ifelse(conf2>11, "pos.h", "fix")) %>%
  mutate(cat2=ifelse(conf2<11&conf2>0,"pos.l",cat2)) %>%
  mutate(cat2=ifelse(conf2<=-11,"neg.h",cat2)) %>%
  mutate(cat2=ifelse(conf2>=-11&conf2<0,"neg.l",cat2)) %>%
  mutate(cat2=ifelse(conf2==0,"unsure",cat2))
table   (d5$cat2)     
d5 %>% group_by(scenario,cat2) %>%
  summarise(min=min(MAT/10), max=max(MAT/10))
#neg.h: 9.1-17.7, 9.1-17
#pos.h:-1.9 - 9.5, -1.9 - 12.3


