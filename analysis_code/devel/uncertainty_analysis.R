#############################################################################
# What is the greatest source of uncertainty in projections?
# Code created Feb. 2017
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=11)) # sized for print
library(lme4) # mixed-effects models
library(MASS) #stepAIC

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
setwd("/Users/poulterlab1/version-control/sageseer/")

# folder paths:
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
# Calculate: where does most uncertainty come from?
# First, magnitude (easier model). Second, direction (binomial)
################################################################################

# just for kicks: standard model
m <- lm(data=merged, formula=change~model*scenario+model*GCM+GCM*scenario)
summary(stepAIC(m)) # keeps all...

# simplest possible model with random site effect
m0 <- lmer(data=merged, formula=change~model+scenario+GCM + (1|site))
summary(m0)

# are coefficients different in standard model?
m0b <- lm(data=merged, formula=change~model+scenario+GCM)
summary(m0b) # not perceptibly

#with interactios
m1 <- lmer(data=merged, formula=change~model*scenario+model*GCM+GCM*scenario+ (1|site))
summary(m1)

# simple model with nesting

# replicate model structure from Andrea:
# add input data grouping factor:
dat <- merged %>% mutate(clim=paste(scenario,GCM, sep="."))
#m2 <- lmer(data=dat, formula=change~scenario+GCM+scenario*GCM+model|clim)
#summary(m2)

#m2 <- lmer(data=dat, formula=change~scenario + GCM + scenario*GCM + model + (model|clim))
#summary(m2)

# Stop attempting andrea's fix and write how i think this should go:
# I only care about fixed effects. Interactions can be wrapped into random effects
m3 <- lmer(data=dat, formula=change~scenario + GCM + model + (0+GCM|scenario)+(0+scenario|GCM))
summary(m3) # didn't explode!
# With above structure: coefs same as m0 except for scenario.

#m4 <- lmer(data=dat, formula=change~scenario + GCM + model + (scenario|GCM/model))
#m5 <- lmer(data=dat, formula=change~scenario + GCM + model + 
            # (scenario|GCM/model/site)) # takes too long
#m6 <- lmer(data=dat, formula=change~scenario + GCM + model + 
            # (scenario|GCM/model)+(GCM|model/site)) # takes too long
#m7 <- lmer(data=dat, formula=change~scenario + GCM + model + (scenario|GCM/model) 
           #+ (GCM|model/site) + (model|site) + (1|site)) # taking forever...

# Try cutting site from nesting- just intercept-leve
#m8 <- lmer(data=dat, formula=change~scenario + GCM + model + (scenario|GCM/model) 
           #+ (GCM|scenario/model) + (model|scenario/GCM) + (1|site)) # taking forever

# Try straight-up interaction model with stepwise selection
m9 <- stepAIC(lm(data=dat, formula=change~scenario*GCM*model))
summary(m9) # leaves everything in... completely uninterperatable
summary(m4) # model as a fixed effect doesn't work- gissm on a diff scale!

# is site effect the problem with convergence? NOPE- still takes forever
#m10 <- lmer(data=dat, formula=change~scenario + GCM + model + (scenario|GCM/model) 
           #+ (GCM|scenario/model) + (model|scenario/GCM))

# do away with nesting and group by category?
dat2 <- dat %>% mutate(scen.mod=paste(scenario,model,sep="")) %>%
  mutate(GCM.mod=paste(GCM,model,sep=""))
m11 <- lmer(data=dat2, formula=change~scenario + GCM + model + (scenario|GCM.mod) 
            + (GCM|scen.mod) + (model|clim))
summary(m11, corr=F) # failed to converge but fits it anyway- suggests rescaling variables
# Scale of GISSM still leads to wonky estimate of difference
# random effects are cool.

# Re-scale response by model:
dat3 <- dat2 %>% group_by(model) %>%
  mutate(change.mean=mean(change)) %>%
  mutate(change.sd=sd(change)) %>%
  mutate(change.scale=(change-change.mean)/change.sd) %>%
  dplyr::select(site,model,scenario,GCM,change:change.scale) %>%
  ungroup() # scale function was messing things up somehow, did manually
head(dat3)

m12 <- lmer(data=dat3, formula=change.scale ~scenario + GCM + model + 
              (scenario|GCM.mod) + (GCM|scen.mod) + (model|clim))
summary(m12, corr=F) # converges! no error messages
# not really sure how helpful this is...
# not sure about random effects structure

# If I want to stretch computational luck: add site effect back in (not run yet)
m13 <- lmer(data=dat3, formula=change.scale ~scenario + GCM + model + 
              (scenario|GCM.mod) + (GCM|scen.mod) + (model|clim) + (1|site))
summary(m13, corr=F) # won't converge but still gave output
anova(m13)

# Actually, I don't want the random affects acting on the intercept (just site)
m14 <- lmer(data=dat3, formula=change.scale ~scenario + GCM + model + 
              (0+scenario|GCM.mod) + (0+GCM|scen.mod) + (0+model|clim) + (1|site))
summary(m14, corr=F) # failed to converge

# Does scaling actually put change on same scale?
dat3a <- dat3 %>%
  group_by(model) %>%
  summarise(mean=mean(change.scale),max=max(change.scale), min=min(change.scale),
            u75=quantile(change.scale,.75),l25=quantile(change.scale,.25))
dat3a # Andy's is actually the odd one 

# Maybe the thing to do is fit the super complex model with all interactions,
# then change one at a time and look at how it affects the average response
# across all sites.

# Maybe do like bayesian: make each level a variable and code 0-1 response 
# goal: data frame with 12 columns. So NOT tidy!
# pointless- this is exactly what lmer does.
dat4 <- dat3 %>%
  mutate(present=1) %>%
  spread(GCM,present, fill=0) %>%
  mutate(present=1) %>%
  spread(scenario,present,fill=0) %>%
  mutate(present=1) %>%
  spread(model,present,fill=0) %>%
  dplyr::select(-change.mean, -change.sd)
head(dat4)
dim(dat4)
dim(dat3) # same # rows- success!

# start simple(ish)
dat5 <- dat4 %>% dplyr::select(change.scale:randfor)
m14 <- lm(data=dat5, formula=change.scale~.)
summary(m14)

#############################################################
# this is actually all looking at change
dat6 <- dat3 %>% mutate(cat=ifelse(change>0, 1, 0))

# test the importance of nesting vs. method I'm using
t1 <- lmer(data=dat6, formula=change.scale~model + (0+model|scenario/GCM))
t2 <- lmer(data=dat6, formula=change.scale~model + (0+model|clim))
summary(t1, corr=F)
summary(t2, corr=F)
coef(t1)
coef(t2)
# well shit... matters, but coefs all basically zero
# looking at coef though- reminds me "clim" isn't independent.
# 2 models per rcp. maybe less concenerning when included as covariates. maybe.
# in terms of AIC, t1 wins by just 2 points
t3 <- lmer(data=dat6, formula=change.scale~model + GCM + scenario + 
             (0+model|scenario/GCM))
t4 <- lmer(data=dat6, formula=change.scale~model + GCM + scenario + 
             (0+model|scenario/GCM) + (0+GCM|scenario)) # didn't converge
summary(t3, corr=F)
summary(t4, corr=F) # totall different

t5 <- stepAIC(lm(data=dat6, formula=change.scale~model*GCM*scenario))
summary(t5) #still keep all? Yep!
t6 <- lmer(data=dat6, formula=change.scale~model*GCM*scenario + (1|site))
t7 <- lmer(data=dat6, formula=change.scale~model*GCM*scenario + (model|site))
t8 <- lmer(data=dat6,formula=change.scale~model*GCM*scenario + (model|scenario/GCM)) # no converge
summary(t6)
summary(t5) # coefs from 5 and 6 identical
summary(t7) # coefs still identical!
summary(t8) # coefs still the same... so random effect means nothing if interactions.
# I think given this, t6 is the model I want!
anova(t6,t7) # refits with ML not REML. Why?
# huh. coefs the same but t7 WAY better fit.
anova(t7,t8) #not significantly better, but AIC way better for 7
# not too surprising given that it's less complex
anova(t5)
AIC(t5,t7) # t7 is better. Ben says not valid if used REML
t7ML <- lmer(data=dat6, formula=change.scale~model*GCM*scenario + (model|site), REML=F)
AIC(t5,t7ML) # AIC slightly different, t7 still better
AIC(t6,t7)

# do I need all interactions?
t9 <- lmer(data=dat6, formula=change.scale~model*GCM*scenario-
             model:GCM:scenario + (model|site)) #t7 marginally better

# do I need a site effect for the effect of GCM and scenario?
t10 <- lmer(data=dat6, formula=change.scale~model*GCM*scenario
            + (model|site) + (GCM|site) + (scenario|site)) # didn't converge

# compare them all, ignoring REML issue
library(bbmle)
bbmle::AICtab(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) #shit- t10 is best
bbmle::AICtab(m12,m13,m14) # the re-scaled ones pre-test
# m13 best, m14 only 5 higher
bbmle::AICtab(m13,m14,t10) #t10 wins

t11 <- lmer(data=dat6, formula=change.scale~model*GCM*scenario
            + (model|site) + (0+GCM|site) + (0+scenario|site)) #didn't converge
#t12 <- lmer(data=dat6, formula=change.scale~model*GCM*scenario
# + (model*GCM*scenario|site)) # not even possible
bbmle::AICtab(t10,t11) #t11 wins- makes sense
# For chosen model, look at coefficients
c <- coef(summary(t11))[ , "Estimate"]
c2 <- as.data.frame(c) 
c2$name <- rownames(c2)
c3 <- c2 %>%
  mutate(abs=abs(c)) %>%
  filter(grepl('DGVM', name))

summary(t11)
anova(t11)

#If reference GCM and RCP, impact of switching model= its individual effect.
# all other coefficients will be zero
# say switch to rcp8.5. Add it's indiv. effect. then switch model- effect size is
# the indiv effect + coef of interaction with rcp8.5.


# Actually... I might be more interested in the variance than the fixed effects
anova(t11)
anova(t5)
c3 <- 
  

################################################################################
# look into glmer for binomial or probit

# start with binomal, could also try probit
b1 <- glmer(data=dat6, formula=cat ~scenario + GCM + model + 
              (0+scenario|GCM.mod) + (0+GCM|scen.mod) + (0+model|clim) + 
              (1|site), family=binomial) # Didn't converge
summary(b1)

# try cutting the site effect
b2 <- glmer(data=dat6, formula=cat ~scenario + GCM + model + 
              (0+scenario|GCM.mod) + (0+GCM|scen.mod) + (0+model|clim),
            family=binomial) 
summary(b2) # Didn't converge
# cut site effect -> coefs do differ, but may be convergence issue

# since t11 best for magnitude, try that formula
#b11 <- glmer(data=dat6, formula=cat~model*GCM*scenario
            #+ (model|site) + (0+GCM|site) + (0+scenario|site), family=binomial) # wouldn't run
# same but probit (shouldn't make a diff but interp easier)
#b12 <- glmer(data=dat6, formula=cat~model*GCM*scenario
           #  + (model|site) + (0+GCM|site) + (0+scenario|site), 
            # family=binomial(link=probit)) # not running
b13 <- glmer(data=dat6, formula=cat~model*GCM*scenario
             + (model|site), family=binomial) # took forever, didn't converge

# just site effect
b3 <- glmer(data=dat6, formula=cat~model*GCM*scenario
             + (1|site), family=binomial) # didn't converge
# no random effect
b4 <- glm(data=dat6, formula=cat~model*GCM*scenario, family=binomial)
summary(stepAIC(b4)) # keeps all

bbmle::AICtab(b1,b2,b3,b4,b13) # b13 is best by a mile. Then b3,1,4,2

save.image("uncert_models.RData")
source("uncert_models.RData")

#####################################################

# try a probit link
b3 <- glmer(data=dat6, formula=cat ~scenario + GCM + model + 
              (0+scenario|GCM.mod) + (0+GCM|scen.mod) + (0+model|clim) + 
              (1|site), family=binomial(link=probit)) 
summary(b3)

# apparently the wald test can test the group as a whole
library(aod)
# terms=order listed in model output, so 7:9 is the eco models
# will test if the overall effect of model is significant
# doesn't seem that useful since I'm interested in effect size not significance
wald.test(b = coef(b3), Sigma = vcov(b3), Terms = 7:9)
