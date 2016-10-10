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
