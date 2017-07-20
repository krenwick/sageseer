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
  mutate(cat=ifelse(change==0&baseline>0,"increase",cat)) %>%
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
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
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
# perfectly agree 48% of time
# consensus 85% of time

##################################################
# How often does choice of GCM cause a change in the direction of response?
# OR: how often are GCMs in perfect agreement?
m5 <- m4 %>% 
  group_by(site,model,scenario) %>%
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
  group_by(model,scenario) %>%
  summarise(nochange=sum(n.increase==5|n.decrease==5), n=n()) %>%
  mutate(perc=(n-nochange)/n)
(m5) # WOW! For SS and rcp45, it flips 73% of time! WHY?
mean(m5$perc) # 18% flip, 82% consistent
# 82% = where ALL 5 agree. Higher if look at 4/5.
m5 %>% group_by(model) %>% summarise(mean=mean(perc))

# Vary by RCP?
m4 %>% 
  group_by(site,model,scenario) %>%
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
  group_by(model,scenario) %>%
  summarise(nochange=sum(n.increase==5|n.decrease==5), n=n()) %>%
  mutate(perc=(n-nochange)/n) %>%
  group_by(scenario) %>%
  summarise(mean(perc))

# Look at 5 vs. 4/5 GCMs
d2 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  group_by(site,model,scenario) %>%
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
  mutate(conf2=n.increase-n.decrease) 
dim(d2)
head(d2)
round(table(d2$conf2)/5712,3) # at least 4/5 agree 92% of the time

# How often does RCP cause a flip?
m5 <- m4 %>% 
  group_by(site,model,GCM) %>%
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
  group_by(model,GCM) %>%
  summarise(nochange=sum(n.increase==2|n.decrease==2), n=n()) %>%
  mutate(perc=(n-nochange)/n)
m5 # Again- what's up with GISSM?!
mean(m5$perc) # 7%

# How often does the ecological model cause a flip?
m5 <- m4 %>% 
  group_by(site,scenario,GCM) %>%
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
  group_by(scenario,GCM) %>%
  summarise(nochange=sum(n.increase==4|n.decrease==4), n=n()) %>%
  mutate(perc=(n-nochange)/n)
m5 # All the time... rare for all 4 to agree
mean(m5$perc) 

# Does agreement among models vary by RCP?
m4 %>% 
  group_by(site,scenario,GCM) %>%
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
  group_by(scenario,GCM) %>%
  summarise(nochange=sum(n.increase==4|n.decrease==4), n=n()) %>%
  mutate(perc=(n-nochange)/n) %>%
  group_by(scenario) %>%
  summarise(mean(perc))
#
# For paper, more clear to say how often is the direction of change consistent?


########### Look at similarity in direction using Cohen's Kappa
kap1 <- m4 %>% dplyr::select(site,model,scenario,GCM,cat) %>%
  spread(model,cat)
kap2 <- as.matrix(kap1[,4:7])

# Look into why kappas so low
nrow(kap1[kap1$GISSM_v1.6.3==kap1$DGVM,])/nrow(kap1) # agree for 74% of cases

# number of "increase" for each model:
with(kap1, table(AK,DGVM))
# Kappa value is .147
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
mean(kaps) #.117

# Calculate Fleiss' Kappa, for agreement across mult models
kappam.fleiss(kap2) # value is .115 ("slight agreement")
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

# Fleiss across all models
kappam.fleiss(kap2) # .737, "substantial agreement"


############
# kappa for emissions scenario
kapRCP1 <- m4 %>% dplyr::select(site,model,scenario,GCM,cat) %>%
  spread(scenario,cat)
kap2 <- as.matrix(kapRCP1[,4:5])
kappa2(kap2) #.767
kappam.fleiss(kap2) #.766 (reassuringly similar!)

################################################################################
# MAGNITUDE OF CHANGE
################################################################################
# Look at correlations in predicted change between models
# Use spearmans rank correlation, since can't assume parametric

# first: eco models
m8 <- m4 %>% group_by(site, model) %>%
  dplyr::select(site,model,GCM,scenario,change) %>%
  spread(model,change)
m9 <- as.matrix(m8[,4:7])  

cors <- rcorr(m9, type="spearman") # moderately laughable
cs <- as.matrix(cors$r,2)
all6 <- c(cs[2:4,1],cs[3:4,2],cs[4,3])
round(range(all6),2)
round(mean(all6),2)

p.corrs <- as.numeric(cors$P)
round(p.adjust(p.corrs, method="holm"),2) # all except GCM:randfor significant

# look at correlation mong GCMs
m10 <- m4 %>% group_by(site, GCM) %>%
  dplyr::select(site,model,GCM,scenario,change) %>%
  spread(GCM,change)
m11 <- as.matrix(m10[,4:8])  
cors <- rcorr(m11, type="spearman") # GCM pairs tend to agree- more than models!
p.corrs <- as.numeric(cors$P)
round(p.adjust(p.corrs, method="holm"),2) # all significant
cors

cs <- as.matrix(cors$r,2)
all10 <- c(cs[2:5,1],cs[3:5,2],cs[4:5,3],cs[5,4])
round(range(all10),2)
round(mean(all10),2)

# look at correlation among rcps
m12 <- m4 %>% group_by(site, scenario) %>%
  dplyr::select(site,model,GCM,scenario,change) %>%
  spread(scenario,change)
m13 <- as.matrix(m10[,4:5])  
cors <- rcorr(m13, type="spearman") # .67, significant
p.corrs <- as.numeric(cors$P)
round(p.adjust(p.corrs, method="holm"),2) # all significant
cors

# calculate mean up and down for each. How does it change when switch?
# Ask: is the mean (in each direction) significantly different?
# OR: absolute value?
# First, RCP:
m4 %>% group_by(scenario) %>%
  summarise(mean(abs(change)))
a1 <- lmer(data=m4, formula=change~scenario + (scenario|site))
anova(a1) #significant


# Next, GCM:
m4 %>% group_by(GCM) %>%
  summarise(mean(abs(change)))
a2 <- aov(data=m4, formula=change~GCM)
summary(a2) # significant

# Next, model:
m4 %>% group_by(model) %>%
  summarise(mean(abs(change)))
a3 <- aov(data=m4, formula=change~model)
summary(a3) # yep.



################################################################################
# Aggregate response: vulnerability scale
################################################################################
d2 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") %>%
  #filter(scenario=="rcp85") %>% #exclude RCP4.5 output
  group_by(site) %>%
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
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
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
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
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease")) %>%
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
  summarise(n=n(),n.increase=sum(cat=="increase"), n.decrease=sum(cat=="decrease"),MAT=mean(bio1)) %>%
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

################################################################################
# Output matrix with pairwise % agreement and kappa values


