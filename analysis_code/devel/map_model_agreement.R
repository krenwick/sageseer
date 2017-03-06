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

# paths to data and folder for figures- USER MUST CHANGE
dpath <- "~/version-control/sageseer/"
fpath <- "~/version-control/sageseer/figures/"

#----------------------------------------
# Journal Specifications for figure size
# Global Change Bio:
col1 <- 80 # 1 column width = 80 mm
col2 <- 169 # 2 column width = 169 mm
#--------------------------------------

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "data/merged_data_GCM.csv", sep="")) %>%
  mutate(change=(predicted-baseline))
unit <- read.csv(paste(dpath, "data/focal_sites_by_zone.csv", sep="")) %>%
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
table(d2$conf2)
table(d2$consensus)

##########################################################
# get shapefiles
states <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers", "states")
wus <- states[states$STATE_ABBR=="WA"|states$STATE_ABBR=="OR"|states$STATE_ABBR=="CA"
              |states$STATE_ABBR=="ID"|states$STATE_ABBR=="NV"|states$STATE_ABBR=="MT"
              |states$STATE_ABBR=="UT"|states$STATE_ABBR=="NM"|states$STATE_ABBR=="WY"
              |states$STATE_ABBR=="CO"|states$STATE_ABBR=="AZ",]

m6 <- merged %>% group_by(site) %>%
  summarise_each(funs(mean)) %>%
  dplyr::select(site:latitude.x,bio1:bio19)

#######################################################
mapdat <- merge(d2,m6,by="site",all.y=F) %>%
  filter(n==max(n))
md2 <- mapdat %>% group_by(site) %>%
  summarise(conf2=mean(conf2), lat=mean(latitude.x), lon=mean(longitude.x))
table(md2$conf2)

png(paste(fpath, "map_agreement_714sites_poster.png", sep=""),
     width = col1, height = col1, units = 'mm', res = 450)
ggplot(data=md2, aes(y=lat, x=lon,fill=conf2)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point( size = 2.5,colour="black",pch=21) + 
  #scale_fill_gradient2(name="Model\nAgreement\n") +
  scale_fill_gradient2(low="red", high="blue",
                       name="Model\nAgreement\n",
    breaks=c(-20,0,20),labels=c(-20,0,20),
    limits=c(-20,20)) +
  ylab("Longitude") +
  xlab("Latitude") +
  coord_fixed(1.3) +
  #theme(legend.justification=c(0,0), legend.position=c(0,0)) +
  theme(legend.position="left") +
  theme(legend.margin=unit(0, "cm"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
dev.off()


# Map agreement across all sites, just 3 categories
png(paste(fpath, "map_agreement_725sites_3categories.png", sep=""),
     width = 320, height = 220, units = 'mm', res = 450)
ggplot(data=mapdat, aes(y=latitude.x, x=longitude.x)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(aes(fill=consensus),size = 2.5,colour="black",pch=21) +
  scale_fill_manual(labels=c("Decrease","Increase", "Unsure"),
                    values=c("#d7191c","#2c7bb6","grey"),name="Model\nConsensus") +
  ylab("Longitude") +
  xlab("Latitude") +
  coord_fixed(1.3) +
  theme(legend.justification=c(0,0), legend.position=c(0,0))
dev.off()

# Map just for Utah to zoom in on problem sites
UT <- states[states$STATE_ABBR=="UT",]
ggplot(data=mapdat, aes(y=latitude.x, x=longitude.x)) +
  #geom_polygon(data=UT, aes(long,lat, group), fill=NA,color="black") +
  geom_point(aes(color=consensus),size = 2) +
  geom_text(aes(label=site), nudge_y = 0.002) +
  scale_color_manual(labels=c("Decrease","Increase", "Unsure"),
                    values=c("#d7191c","#2c7bb6","grey"),name="Model\nConsensus") +
  #scale_fill_gradient2(low="red", high="blue",
                      # name="Model\nAgreement\n",
                       #breaks=c(-20,0,20),labels=c(-20,0,20),
                       #limits=c(-20,20)) +
  ylab("Longitude") +
  xlab("Latitude") +
  #coord_fixed(1.3) +
  xlim(c(-112.14,-111.8)) +
  ylim(c(40.38,40.46)) 

##########################################################
# Try plotting points on base map from google
library(ggmap)
myLoc <- c(-120,38, -119, 40) # define bounding box
myMap <- get_map(location=myLoc, source="google", maptype="terrain", crop=FALSE)
# Terrain
ggmap(myMap) +
  #geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(data=mapdat,aes(y=latitude.x, x=longitude.x,color=consensus)) +
  scale_color_manual(labels=c("Decrease","Increase", "Unsure"),
          values=c("#d7191c","#2c7bb6","grey"),name="Model\nConsensus")

# Satellite
sat <- get_map(location=myLoc, source="google", maptype="satellite", crop=FALSE)
jpeg(paste(fpath, "satellite_map_NV_cluster.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggmap(sat) +
  #geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(data=mapdat,aes(y=latitude.x, x=longitude.x,color=consensus)) +
  scale_color_manual(labels=c("Decrease","Increase","No Change", "Unsure"),
                     values=c("#d7191c","#2c7bb6","yellow","grey"),name="Model\nConsensus")
dev.off()

# Satellite- zoom to ridge above carson city
myLoc <- c(-120,39, -119.75, 39.3) # define bounding box
sat <- get_map(location=myLoc, source="google", maptype="satellite", crop=FALSE)
jpeg(paste(fpath, "satellite_map_NV_cluster_zoom1.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggmap(sat) +
  #geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(data=mapdat,aes(y=latitude.x, x=longitude.x,color=consensus)) +
  scale_color_manual(labels=c("Decrease","Increase","No Change", "Unsure"),
                     values=c("#d7191c","#2c7bb6","yellow","grey"),name="Model\nConsensus")
dev.off()

myLoc <- c(-119.9,39.145, -119.85, 39.2) # define bounding box
sat <- get_map(location=myLoc, source="google", maptype="satellite", crop=FALSE)
jpeg(paste(fpath, "satellite_map_NV_cluster_zoom2.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggmap(sat) +
  #geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(data=mapdat,aes(y=latitude.x, x=longitude.x,color=consensus)) +
  scale_color_manual(labels=c("Decrease","Increase","No Change", "Unsure"),
                     values=c("#d7191c","#2c7bb6","yellow","grey"),name="Model\nConsensus")
dev.off()

myLoc <- c(-119.9,39.145, -119.85, 39.2) # define bounding box
sat <- get_map(location=myLoc, source="google", maptype="hybrid", crop=FALSE)
jpeg(paste(fpath, "satellite_map_NV_cluster_zoom2.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggmap(sat) +
  #geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(data=mapdat,aes(y=latitude.x, x=longitude.x,color=consensus)) +
  scale_color_manual(labels=c("Decrease","Increase","No Change", "Unsure"),
                     values=c("#d7191c","#2c7bb6","yellow","grey"),name="Model\nConsensus")
dev.off()

######### CHack out the area in S. Arizona
myLoc <- c(-114, 33.9, -111.4, 34.2) # define bounding box
sat <- get_map(location=myLoc, source="google", maptype="hybrid", crop=FALSE)
jpeg(paste(fpath, "satellite_map_AZ_cluster_zoom2.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggmap(sat) +
  #geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point(data=mapdat,aes(y=latitude.x, x=longitude.x,color=consensus)) +
  scale_color_manual(labels=c("Decrease","Increase","No Change", "Unsure"),
                     values=c("#d7191c","#2c7bb6","yellow","grey"),name="Model\nConsensus")
dev.off()

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
ggplot(data=md2,aes(y=bio1/10,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")
dev.off()

jpeg(paste(fpath, "MAP_by_consensus_group.jpeg", sep=""),
     width = 320, height = 220, units = 'mm', res = 300)
ggplot(data=md2,aes(y=bio12,x=as.factor(conf2))) +
  geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")
dev.off()

ggplot(data=md2,aes(y=bio12,x=bio1/10)) +
  #geom_boxplot(aes(color=conf2)) +
  geom_point(aes(color=conf2)) +
  scale_color_gradient2(name="Model\nAgreement") +
  xlab("Consensus on Change")

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
  dplyr::select(bio1:bio19)
names(m7) <- bioclimNames

# Which bioclim varaibles are highly correlated?
envcor<-as.data.frame(cor(m7))
e2 <- envcor %>% mutate(var1=rownames(envcor)) %>% 
  gather(var2,cors, -var1) %>% 
  filter(abs(cors)>.8 & abs(cors)<1) %>%
  arrange(var1)
# cut: PwetM,PwetQ,PdryQ,PcoldQ,Tmaxwarm,Tmincold,Tmeanwarm,Tmeancold,TrangeY
m8 <- dplyr::select(m7, -PwetM, -PwetQ, -PdryQ, -PcoldQ, -Tmaxwarm, -Tmincold, 
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

