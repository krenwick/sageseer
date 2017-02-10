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

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
setwd("/Users/poulterlab1/version-control/sageseer/")

# folder path:
dpath <- "data/"
fpath <- "figures/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_perturb.csv", sep=""))
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
  dplyr::select(latitude.x,longitude.x,site, model:mag,change,cat)

d2 <- m4 %>%
  dplyr::select(site, model:mag,change:cat) %>%
  filter(mag==4&var=="temp") %>%
  group_by(site) %>%
  mutate(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(consensus=ifelse(conf2>0,"increase","nada")) %>%
  mutate(consensus=ifelse(conf2<0,"decrease",consensus)) %>%
  mutate(consensus=ifelse(conf2==0,"unsure",consensus)) %>%
  mutate(confidence=ifelse(n==n.increase|n==n.decrease,"high","medium")) %>%
  mutate(confidence=ifelse(consensus=="unsure","low",confidence)) #%>%
  # remove sites with < 3 models (cuts 1 site)
  # doing this screws up colors in PCA farther down
  #filter(n>2)
  
############################## Which model disagrees?
d3 <- d2 %>% select(-change) %>%
  spread(model, cat) %>%
  rename(CC=randfor, DGVM=`DGVM-full-400ppm`) %>%
  mutate(issue=ifelse(AK==DRS&AK==CC&AK==DGVM,"none","split")) %>%
  mutate(issue=ifelse(AK==DRS&AK==CC&AK!=DGVM,"DGVM",issue)) %>%
  mutate(issue=ifelse(AK==DRS&AK!=CC&AK==DGVM,"RF",issue)) %>%
  mutate(issue=ifelse(AK!=DRS&AK==CC&AK==DGVM,"GISSM",issue)) %>%
  mutate(issue=ifelse(CC==DRS&CC==DGVM&CC!=AK,"TS",issue))

# Disagreement Table: which model disagrees?
table(d3$issue)

# What about direction?
table(d3$issue,d3$conf2)

##########################################################
# Plot which model disagrees- THIS WAS NOT USEFUL
# get shapefiles
states <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers", "states")
wus <- states[states$STATE_ABBR=="WA"|states$STATE_ABBR=="OR"|states$STATE_ABBR=="CA"
              |states$STATE_ABBR=="ID"|states$STATE_ABBR=="NV"|states$STATE_ABBR=="MT"
              |states$STATE_ABBR=="UT"|states$STATE_ABBR=="NM"|states$STATE_ABBR=="WY"
              |states$STATE_ABBR=="CO"|states$STATE_ABBR=="AZ",]

m6 <- merged %>% group_by(site) %>%
  summarise_each(funs(mean))

mapdat <- merge(d3,m6,by="site",all.y=F) 
ggplot(data=mapdat, aes(y=latitude.x, x=longitude.x,color=issue)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point() 
# Grey points are NAs (no predictions from Andy)

##########################################################
# Can I figure out where in climate space specific models disagree?
# Plot along temp or seasonality gradient
d5 <- mapdat 
d4 <- gather(mapdat,model2,direction, AK:DRS)
ggplot(data=d5,aes(x=consensus,y=bio1/10,color=issue)) +
  geom_boxplot(notch=T) 

table(d5$consensus,d5$issue)
# TS seems to disagree only at hotter sites... how odd! Which direction?
d3 %>% filter(issue=="TS")
# In ALL 45 cases, TS predicts decrease where other 3 models predict increase!
d4 %>% filter(issue=="GISSM")
# In ALL 37 cases, GISSM predicts decrease/no change where others predict increase
d4 %>% filter(issue=="RF")
# RF issues: decrease in 87/89 issues
# DGVM: relatively even split


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
  select(bio1:bio19)
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
           groups = as.factor(mapdat$consensus), ellipse = TRUE, varname.size=6,
           circle = F, alpha=.5, shape=1) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'top') +
  theme(text = element_text(size=50)) +
  scale_color_manual(values=c("#7CAE00", "#00BFC4", "#C77CFF")) +
  theme_bw()

