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
# Calculate: which perturbation has biggest impact according to each model?
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
mm
# yes, really! Shows that abs(change) highest for 4c in all models

# for randfor, look just at split between variables (unfare bc of .2)
mer3 <- filter(mer2, mag!=.2)
m <- lmer(data=mer3[mer3$model=="randfor",], formula=change2~var + (1|site))
summary(m)

################################################################################
# Calculate pairwise correlations between models.
# Method 1: spearmans rank correlation, since can't assume parametric
# for each model, take mean response across RCP x GCM combos
library(Hmisc)
spe <- merged %>%
  dplyr::select(site,model,var,mag,change) %>%
  spread(model,change) 
spe2 <- as.matrix(spe[,4:7])

cors <- rcorr(spe2, type="spearman")
p.corrs <- as.numeric(cors$P)
p.adjust(p.corrs, method="holm") # Still just the zeros are significant.

# Method 2 (for categorical): kappa statistic
# prep matrix of observations:
kap1 <- m4 %>% dplyr::select(site,model,mag,var,cat) %>%
  spread(model,cat)
kap2 <- as.matrix(kap1[,4:7])

library(irr)
#k1 <- kappam.fleiss(kap2, detail=T) 
#k1 # overall- not significantly correlated
# NOTE: not valid when small number of categories (here: 2, inc/dec)

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
kaps # these are uniformly terrible
#Landis and Koch: values < 0 indicate no agreement, 0–0.20 slight, 
#0.21–0.40 fair, 0.41–0.60 moderate, 0.61–0.80 substantial, and 0.81–1 as almost perfect agreement

# Most agreement: Daniel and Caroline. But I don't think can really compare vals
# that are this close. Except AK.KR- really bad.

################################################################################
# Try a different way to look at agreement on direction across perturbs
# Test via Fleiss' Kappa

# temp .2
kap1 <- m4 %>% dplyr::select(site,model,mag,var,cat) %>%
  filter(mag==.2) %>%
  spread(model,cat)
kap2 <- as.matrix(kap1[,4:7])
kappam.fleiss(kap2) # .002, basically random

# temp 2
kap1b <- m4 %>% dplyr::select(site,model,mag,var,cat) %>%
  filter(mag==2) %>%
  spread(model,cat)
kap2b <- as.matrix(kap1b[,4:7])
kappam.fleiss(kap2b) #.06

# temp 4
kap1c <- m4 %>% dplyr::select(site,model,mag,var,cat) %>%
  filter(mag==4) %>%
  spread(model,cat)
kap2c <- as.matrix(kap1c[,4:7])
kappam.fleiss(kap2c) # .07
kappam.fleiss(kap2c, exact=T) #.127

# precip .9
kap1 <- m4 %>% dplyr::select(site,model,mag,var,cat) %>%
  filter(mag==.9) %>%
  spread(model,cat)
kap2 <- as.matrix(kap1[,4:7])
kappam.fleiss(kap2) # -0.108... worse than random??

# precip 1.1
kap1 <- m4 %>% dplyr::select(site,model,mag,var,cat) %>%
  filter(mag==1.1) %>%
  spread(model,cat)
kap2 <- as.matrix(kap1[,4:7])
kappam.fleiss(kap2) # -0.121

# precip 1.2
kap1 <- m4 %>% dplyr::select(site,model,mag,var,cat) %>%
  filter(mag==1.2) %>%
  spread(model,cat)
kap2 <- as.matrix(kap1[,4:7])
kappam.fleiss(kap2) # -0.145
kappam.fleiss(kap2, exact=T) # -.104

# Basically the same story that percent agreement tells
# Overall: agreement only marginally better than random

