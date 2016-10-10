#############################################################################
# Look at 4-way model agreement
# What can we learn from where the models disagree?
#############################################################################
rm(list=ls())
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)
library(colorspace)
library(rgdal)
library(dplyr) # must load ggplot first!
library(tidyr)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Documents/sageseer/Figures/"
opath <- "/Users/poulterlab1/Documents/sageseer/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_GCM.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) 
m3 %>% group_by(model) %>% summarise(mbase=mean(baseline),mpred=mean(predicted))
unit <- read.csv(paste(dpath, "focal_sites_by_zone.csv", sep="")) %>%
  dplyr::select(site,elev:NA_L1NAME)

du <- merge(m3,unit, by="site") 

################################################################################
# Calculate change and direction:
m4 <- m3 %>% 
  mutate(change=(predicted-baseline)) %>%
  mutate(cat=ifelse(change>0, "increase", "decrease")) %>%
  mutate(cat=ifelse(change==0, "nochange", cat)) %>%
  dplyr::select(site:GCM,change,cat)

d2 <- m4 %>%
  dplyr::select(site, model,scenario:GCM,change:cat) %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") %>%
  group_by(site,scenario,GCM) %>%
  mutate(n=n(),n.increase=sum(change>0), n.decrease=sum(change<0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(consensus=ifelse(conf2>0,"increase","nada")) %>%
  mutate(consensus=ifelse(conf2<0,"decrease",consensus)) %>%
  mutate(consensus=ifelse(conf2==0,"unsure",consensus)) %>%
  mutate(confidence=ifelse(n==n.increase|n==n.decrease,"high","medium")) %>%
  mutate(confidence=ifelse(consensus=="unsure","low",confidence)) #%>%
# remove sites with < 3 models (cuts 1 site)
# doing this screws up colors in PCA farther down
#filter(n>2)
table(d2$conf2)
table(d2$consensus)
############################## Which model disagrees?
d3 <- d2 %>% select(-change) %>%
  spread(model, cat) %>%
  filter(scenario!="rcp45") %>%
  mutate(issue=ifelse(AK==GISSM_v1.6.3&AK==RandFor&AK==DGVM,"none","split")) %>%
  mutate(issue=ifelse(AK==GISSM_v1.6.3&AK==RandFor&AK!=DGVM,"DGVM",issue)) %>%
  mutate(issue=ifelse(AK==GISSM_v1.6.3&AK!=RandFor&AK==DGVM,"RF",issue)) %>%
  mutate(issue=ifelse(AK!=GISSM_v1.6.3&AK==RandFor&AK==DGVM,"GISSM",issue)) %>%
  mutate(issue=ifelse(RandFor==GISSM_v1.6.3&RandFor==DGVM&RandFor!=AK,"TS",issue)) #%>%
  # get rid of no change sites
 # filter(DGVM!="nochange")

# Disagreement Table
# Note: only includes siets for which Andy made prediction
table(d3$issue,d3$GCM)
table(d3$consensus)

# What about direction?
table(d3$issue,d3$conf2)
sum(table(d3$conf2))

##########################################################
# MAP which model disagrees- THIS WAS NOT USEFUL
# get shapefiles
states <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers", "states")
wus <- states[states$STATE_ABBR=="WA"|states$STATE_ABBR=="OR"|states$STATE_ABBR=="CA"
              |states$STATE_ABBR=="ID"|states$STATE_ABBR=="NV"|states$STATE_ABBR=="MT"
              |states$STATE_ABBR=="UT"|states$STATE_ABBR=="NM"|states$STATE_ABBR=="WY"
              |states$STATE_ABBR=="CO"|states$STATE_ABBR=="AZ",]

m6 <- merged %>% group_by(site) %>%
  summarise_each(funs(mean)) %>%
  select(site:latitude.x,MAT:bio19)

mapdat <- merge(d3,m6,by="site",all.y=F) 
ggplot(data=mapdat, aes(y=latitude.x, x=longitude.x,color=issue)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point() 
# Grey points are NAs (no predictions from Andy)

##########################################################
# Can I figure out where in climate space specific models disagree?
# Plot along temp or seasonality gradient
d5 <- mapdat %>% select(-model) %>% na.omit()
d4 <- gather(mapdat,model2,direction, AK:GISSM_v1.6.3)
ggplot(data=d5,aes(x=consensus,y=MAT,fill=issue)) +
  geom_boxplot()

ggplot(data=d5,aes(x=MAT,fill=consensus)) +
  geom_histogram(position="dodge") 
# bad because dont have even sample across MAT, so misleading

test <- filter(d5, DGVM!="nochange")
d6 <- filter(d5, consensus!="unsure")
ggplot(data=d5,aes(x=MAT,y=conf2,color=issue)) +
  geom_point()

# show conseunsus
ggplot(data=d5,aes(y=MAT,x=consensus)) +
  geom_boxplot(notch=T) +
  #geom_violin() +
  xlab("Consensus on Change")

ggplot(data=d5,aes(y=MAP,x=consensus)) +
  geom_boxplot(notch=T) +
  #geom_violin() +
  xlab("Consensus on Change")

# Mean temperature coldest quarter
ggplot(data=d5,aes(y=bio11,x=consensus)) +
  geom_boxplot(notch=T) +
  #geom_violin() +
  xlab("Consensus on Change")

# Mean temperature wettest quarter (precip in winter vs. summer?)
ggplot(data=d5,aes(y=bio8,x=consensus)) +
  geom_boxplot(notch=T) +
  #geom_violin() +
  xlab("Consensus on Change")

table(d5$consensus,d5$issue)



# do a pca -------------------------------------------------
# rename BIOCLIM variables
bioclimNames <- c("MAT", # BIO1 = Annual Mean Temperature
                  "TrangeD",# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
                  "Tiso",# BIO3 = Isothermality (BIO2/BIO7) (* 100)
                  "Tseas",# BIO4 = Temperature Seasonality (standard deviation *100)
                  "Tmaxwarm",# BIO5 = Max Temperature of Warmest Month
                  "Tmincold",# BIO6 = Min Temperature of Coldest Month
                  "TrangeY",# BIO7 = Temperature Annual Range (BIO5-BIO6)
                  "Twet",# BIO8 = Mean Temperature of Wettest Quarter
                  "Tdry",# BIO9 = Mean Temperature of Driest Quarter
                  "Tmeanwarm",# BIO10 = Mean Temperature of Warmest Quarter
                  "Tmeancold",# BIO11 = Mean Temperature of Coldest Quarter
                  "MAP",# BIO12 = Annual Precipitation
                  "PwetM",# BIO13 = Precipitation of Wettest Month
                  "PdryM",# BIO14 = Precipitation of Driest Month
                  "Pseas",# BIO15 = Precipitation Seasonality (Coefficient of Variation)
                  "PwetQ",# BIO16 = Precipitation of Wettest Quarter
                  "PdryQ",# BIO17 = Precipitation of Driest Quarter
                  "PwarmQ",# BIO18 = Precipitation of Warmest Quarter
                  "PcoldQ") # BIO19 = Precipitation of Coldest Quarter

# ----------------------------------------------------------  
# bio clim column names  
# ----------------------------------------------------------
m7 <- m6 %>%
  select(MAT:bio19)
names(m7) <- bioclimNames

# Which bioclim varaibles are highly correlated?
envcor<-as.data.frame(cor(m7))
e2 <- envcor %>% mutate(var1=rownames(envcor)) %>% 
  gather(var2,cors, -var1) %>% 
  filter(abs(cors)>.8 & abs(cors)<1) %>%
  arrange(var1)
# cut: PwetM,PwetQ,PdryQ,PcoldQ,Tmaxwarm,Tmincold,Tmeanwarm,Tmeancold,TrangeY
m8 <- select(m7, -PwetM, -PwetQ, -PdryQ, -PcoldQ, -Tmaxwarm, -Tmincold, 
             -Tmeanwarm, -Tmeancold, -TrangeY)

out.pca <- prcomp(m8,scale.=T)
summary(out.pca)

out.pca$rotation
source("/Users/poulterlab1/Documents/code/R_functions/ggbiplot_kr.r")
ggbiplot_k(out.pca)

ggbiplot_k(out.pca, choices=c(1,2),obs.scale = 1, var.scale = 1, 
           # Next line not working b/c not 726 rows in g
           #groups = as.factor(mapdat$consensus), ellipse = TRUE, varname.size=6,
           circle = F, alpha=.5, shape=1) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'top') +
  theme(text = element_text(size=50)) +
  scale_color_manual(values=c("#7CAE00", "#00BFC4", "#C77CFF")) +
  theme_bw()

head(mapdat)
