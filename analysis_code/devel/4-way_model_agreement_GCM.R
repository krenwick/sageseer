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
library(mapdata)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Figures/"
#fpath <- "/Users/poulterlab1/Documents/sageseer/Figures/"
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
  mutate(n=n(),n.increase=sum(change>0), n.decrease=sum(change<0),n.same=sum(change==0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf_cat=max(n.increase,n.decrease,n.same)) %>%
  mutate(consensus=ifelse(conf_cat==n.increase,"increase","decrease")) %>%
  mutate(consensus=ifelse(conf_cat==n.same,"nochange",consensus)) %>%
  mutate(consensus=ifelse(n.increase==n.decrease&n.increase==conf_cat,"unsure",consensus))
# remove sites with < 3 models (cuts 1 site)
# doing this screws up colors in PCA farther down
#filter(n>2)
table(d2$conf2)
table(d2$consensus)
write.csv(d2,paste(dpath,"agreement_output_env.csv"))

# Get agreement for 1 model to export
mod4 <- filter(d2,scenario=="rcp85", GCM=="CESM1-CAM5",model=="RandFor") %>%
  select(site,consensus)

dim(mod4)
table(mod4$consensus)
write.csv(mod4,paste(dpath,"mod4_consensus.csv"))
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

#######################################################
# Map agreement just on sites Andy has predictions for
mapdat <- merge(d2,m6,by="site",all.y=F) %>%
  filter(n==4)
md2 <- mapdat %>% group_by(site) %>%
  summarise(conf2=mean(conf2), lat=mean(latitude.x), lon=mean(longitude.x))
table(md2$conf2)

jpeg(paste(fpath, "map_agreement_437sites.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggplot(data=md2, aes(y=lat, x=lon,color=conf2)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(size=1) +
  scale_color_gradient2(name="Model\nAgreement") +
  ylab("Longitude") +
  xlab("Latitude") +
  coord_fixed(1.3)
dev.off()

# Map agreement across all sites
mapdat <- merge(d2,m6,by="site",all.y=F) 
md2 <- mapdat %>% group_by(site) %>%
  summarise(conf2=mean(conf2), lat=mean(latitude.x), lon=mean(longitude.x))
table(md2$conf2)
jpeg(paste(fpath, "map_agreement_726sites.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggplot(data=md2, aes(y=lat, x=lon,color=conf2)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(size=1) +
  scale_color_gradient2(name="Model\nAgreement") +
  ylab("Longitude") +
  xlab("Latitude") +
  coord_fixed(1.3)
dev.off()

# Map agreement across all sites, just 3 categories
mapdat <- merge(d2,m6,by="site",all.y=F)
table(mapdat$consensus)
jpeg(paste(fpath, "map_agreement_726sites_3categories.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggplot(data=mapdat, aes(y=latitude.x, x=longitude.x,color=consensus)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(size=1) +
  scale_color_manual(values=c("#d7191c","#2c7bb6","yellow","grey"),name="Model\nConsensus") +
  ylab("Longitude") +
  xlab("Latitude") +
  coord_fixed(1.3)
dev.off()


# Map agreement across all sites, just 3 categories
# overlay on Sagebrush distribution map
artrdist <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers/","bigsagebrush")
artrdist<-spTransform(artrdist, CRS(proj4string(states)))

mapdat <- merge(d2,m6,by="site",all.y=F) 

jpeg(paste(fpath, "map_agreement_726sites_3categories.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggplot(data=mapdat, aes(y=latitude.x, x=longitude.x,color=consensus)) +
  geom_polygon(data=artrdist, aes(long,lat,group), fill="gray", color=NA) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(size=1) +
  scale_color_discrete(name="Model\nConsensus") +
  ylab("Longitude") +
  xlab("Latitude") +
  coord_fixed(1.3)
dev.off()

## Map acreement but focus on CO
CO <- states[states$STATE_ABBR=="CO",]
mapdat <- merge(d2,m6,by="site",all.y=F) %>%
  filter(n==4)
md2 <- mapdat %>% group_by(site) %>%
  summarise(conf2=mean(conf2), lat=mean(latitude.x), lon=mean(longitude.x))
table(md2$conf2)
jpeg(paste(fpath, "map_agreement_437sites_COzoom.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggplot(data=md2, aes(y=lat, x=lon,color=conf2)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(size=1) +
  scale_color_gradient2(name="Model\nAgreement") +
  ylab("Longitude") +
  xlab("Latitude") +
  coord_fixed(xlim=c(-111,-106),ylim=c(37,43),1.3) 
dev.off()

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
ggplot(data=d5,aes(y=MAT,x=as.factor(conf2))) +
  geom_boxplot(notch=T) +
  #geom_violin() +
  xlab("Consensus on Change")
table(d5$conf2)/nrow(d5)

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

########################## Cut all areas where unsure to focus on certain areas
md2 <- filter(mapdat, conf2<=-3|conf2>=3)
ggplot(data=md2, aes(y=latitude.x, x=longitude.x,color=conf2)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(size=2) +
  scale_color_gradient2()

table(md2$conf2)
ggplot(data=md2,aes(y=MAT,x=as.factor(conf2))) +
  geom_boxplot(notch=T, aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

##########################################################################
# Look at characteristics of sites that will increase/decrease
names(md2)[18:36] <- bioclimNames

jpeg(paste(fpath, "MAT_by_consensus_group.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggplot(data=md2,aes(y=MAT,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")
dev.off()

jpeg(paste(fpath, "MAP_by_consensus_group.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggplot(data=md2,aes(y=MAP,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")
dev.off()

ggplot(data=md2,aes(y=TrangeD,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

ggplot(data=md2,aes(y=Tiso,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

ggplot(data=md2,aes(y=Tseas,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

ggplot(data=md2,aes(y=Tmaxwarm,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

ggplot(data=md2,aes(y=TrangeY,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

ggplot(data=md2,aes(y=Twet,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

ggplot(data=md2,aes(y=Pseas,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

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
