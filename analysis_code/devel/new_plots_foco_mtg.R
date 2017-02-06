#############################################################################
# Make more polished graphs for a few of the grouping options
# These are for GCM projections
#############################################################################

# intro code to read in and organize data
rm(list=ls())
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)
library(colorspace)
library(rgdal)
library(dplyr) # must load ggplot first!
library(tidyr)
library(knitr)
library(xtable)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Figures/"
fpath <- "/Users/poulterlab1/Documents/sageseer/Figures/"
opath <- "/Users/poulterlab1/Documents/sageseer/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_GCM.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() %>%
  dplyr::select(site,model:GCM,change,direction)
unit <- read.csv(paste(dpath, "focal_sites_by_zone.csv", sep="")) 

du <- merge(m3,unit, by="site") %>%
  filter(model!="MaxEntBin", model!="MaxEntRaw")

# Function to add sample size to boxplots:
give.n <- function(x){
  return(c(y = median(x)*3, label = length(x))) 
  # experiment with the multiplier (3) to find the perfect position
}

# Manipulate du to get consensus
du2 <- du %>%
  group_by(site,scenario,GCM) %>%
  dplyr::summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf3=(n.increase)/n) %>%
  mutate(consensus=ifelse(conf2>0,"increase","nada")) %>%
  mutate(consensus=ifelse(conf2<0,"decrease",consensus)) %>%
  mutate(consensus=ifelse(conf2==0,"unsure",consensus)) %>%
  merge(unit, by="site") 


# Color Palette for GCMs (color-blind friendly)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

################################################################################
## 1. Sagegrouse management units
# Make bar plots showing change within each Mgmt. unit
################################################################################
# can I scale the change somehow?
du.n <- du %>% 
  mutate(model=gsub("AK","TS",model)) %>%
  mutate(model=gsub("GISSM_v1.6.3","GISSM",model)) %>%
  mutate(model=gsub("RandFor","RF",model))
table(du.n$model)

jpeg(paste(fpath, "change_by_mgmt_zone.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du.n, aes(x=Mgmt_zone,y=change, fill=model)) +
  geom_boxplot(notch=F) +
  ylab("Change in Response") +
  theme(panel.background=element_blank(),plot.background=element_blank(),
  legend.text.align = 0) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scales="free") +
  theme(legend.position="none")
dev.off()

################################################################################
## 1. R & R classes
# Make bar plots showing change within each Mgmt. unit
################################################################################
# can I scale the change somehow?
du.n2 <- filter(du.n, RR_class_name!=0) %>% na.omit()
jpeg(paste(fpath, "change_by_RR.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du.n2, aes(x=RR_class_name,y=change, fill=model)) +
  geom_boxplot(notch=F) +
  ylab("Change in Response") +
  theme(panel.background=element_blank(),plot.background=element_blank(),
        legend.text.align = 0) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scales="free") +
  theme(legend.position="none")
dev.off()

jpeg(paste(fpath, "change_by_RR_elev.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du.n2, aes(x=MAT,y=change, color=model)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab("Change in Response") +
  #theme(panel.background=element_blank(),plot.background=element_blank(),
        #legend.text.align = 0) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~RR_class_name, scales="free") 
dev.off()

################################################################################
## 1. Elevation
# try scatter plot
################################################################################
# can I scale the change somehow?
du.n3 <- filter(du.n, scenario=="rcp85")
jpeg(paste(fpath, "change_by_elev.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du.n3, aes(x=elev,y=change, color=GCM)) +
  geom_point() +
  ylab("Change in Response") +
  xlab("Elevaation (m)") +
  stat_smooth(method = "lm", aes(fill=GCM)) +
  scale_fill_manual(values=cbPalette, name="GCM") +
  scale_color_manual(values=cbPalette, name="GCM") +
  theme(panel.background=element_blank(),plot.background=element_blank(),
        legend.text.align = 0) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scales="free") +
  theme(legend.position="none")
dev.off()
  
  ################################################################################
  ## 1. Sagegrouse management units
  # Make bar plots showing Consensus within each Mgmt. unit
  ################################################################################
  du3 <- du2 %>% group_by(Mgmt_zone,consensus) %>%
    dplyr::summarise(n.mgmt=n()) %>%
    group_by(Mgmt_zone) 
  ggplot(data=du3, aes(x=Mgmt_zone,y=n.mgmt, fill=consensus)) +
    geom_bar(stat="identity") +
    ylab("# (site x GCM x rcp) per category") +
    theme(panel.background=element_blank(),plot.background=element_blank(),
          legend.text.align = 0) +
    geom_hline(yintercept=.5, linetype="dashed") 
  
  
  #### Look at MAT vs. RR
  du5 <- du %>% group_by(site)

ggplot(data=du5, aes(x=RR_class_name, y=MAT)) +
    geom_boxplot()
#################################################################################
head(du2)
ggplot(data=du2, aes(x=consensus, y=MAT)) +
  geom_boxplot(notch=T)

du3 <- filter(du2, scenario=="rcp85" & GCM=="CCSM4")
ggplot(data=du3, aes(x=MAT, y=conf2, color=consensus)) +
  geom_point()

# full data
du2 <- filter(du, scenario=="rcp45" & GCM=="CCSM4")

jpeg(paste(fpath, "change_allpts_rcp45.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAT, y=change)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()

# no outliers
du2 <- filter(du, scenario=="rcp85" & GCM=="CCSM4" & outlier==0)

jpeg(paste(fpath, "change_allpts.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAT, y=change)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()

# no outliers
du2 <- filter(du, scenario=="rcp85" & GCM=="CCSM4" & MAT<=120)

jpeg(paste(fpath, "change_less12c.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAT, y=change)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()

# no outliers
du2 <- filter(du, scenario=="rcp85" & GCM=="CCSM4" & MAT<=120 & MAT>0)

jpeg(paste(fpath, "change_less12c_great0.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAT, y=change)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()

###################################
#MAP
# full data
du2 <- filter(du, scenario=="rcp85" & GCM=="CESM1-CAM5")

jpeg(paste(fpath, "MAP_change_allpts.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAP, y=change)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()

# no outliers
du2 <- filter(du, scenario=="rcp85" & GCM=="CCSM4" & outlier==0)

jpeg(paste(fpath, "MAP_change_allpts_nooutlier.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAP, y=change)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()

# less 12c
du2 <- filter(du, scenario=="rcp85" & GCM=="CCSM4" & MAT<=120)

jpeg(paste(fpath, "change_less12c.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAT, y=change)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()

# less 12 >0
du2 <- filter(du, scenario=="rcp85" & GCM=="CCSM4" & MAT<=120 & MAT>0)

jpeg(paste(fpath, "change_less12c_great0.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAT, y=change)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()

#############################
du2 <- filter(du, scenario=="rcp85" & GCM=="CESM1-CAM5")
MAT <- seq(-30,300,1)
DGVM <- filter(du2, model=="DGVM")
m1 <- lm(data=DGVM, change~MAT)
summary(m1)
predict(m1, data=MAT)
#424.8382 
m1$coefficient[1]/-m1$coefficient[2]

AK <- filter(du2, model=="AK")
m1 <- lm(data=AK, change~MAT)
summary(m1) # .26
m1$coefficient[1]/-m1$coefficient[2] #114.811

GISSM_v1.6.3 <- filter(du2, model=="GISSM_v1.6.3")
m1 <- lm(data=GISSM_v1.6.3, change~MAT+MAP+MAT)
summary(m1) #.06
m1$coefficient[1]/-m1$coefficient[2]

RandFor <- filter(du2, model=="RandFor")
m1 <- lm(data=RandFor, change~MAT)
summary(m1) #.09
m1$coefficient[1]/-m1$coefficient[2]
  
  
  # full data
du2 <- filter(du, scenario=="rcp45" & GCM=="CCSM4")
du3 <- filter(du, scenario=="rcp85" & GCM=="CCSM4")

jpeg(paste(fpath, "change_allpts_rcp45.jpeg", sep=""),
     width = 420, height = 250, units = 'mm', res = 300)
ggplot(data=du2, aes(x=MAT, y=change)) +
  geom_point(color="red") +
  geom_point(data=du3,color="blue") +
  geom_smooth(method="lm") +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")
dev.off()