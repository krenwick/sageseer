################################################################################
# Make figures for sagebrush manuscript
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=9)) # sized for print
library(rgdal)
library(gridExtra)
library(grid)

#----------------------------------------
# Journal Specifications for figure size
# Global Change Bio:
col1 <- 80 # 1 column width = 80 mm
col2 <- 169 # 2 column width = 169 mm
#--------------------------------------

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Figures/"

# List of "bad" sites
bad <- c(495,496,497,498,580,581,583,632,633,634,668,62)

# Color Palette for GCMs (color-blind friendly)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

################################################################################
# Figure 1:
# Map sagebrush cover + presence points + focal points
################################################################################
# Pull in data and manipulat
# Caroline's list of bad points:
car <- read.csv(paste(dpath, "AZ_pts_no_source_data.csv", sep=""))
carlat <- car$latitude
carlon <- car$longitude

# data from Andy's PCA script
dat <- read.csv(paste(dpath, "pcapts.csv", sep=""))

# Eliminate points on Caroline's "bad" list (WTF don't coords match??)
dat$x2 <- round(dat$x,1)
dat$y2 <- round(dat$y,1)
carlat<- round(carlat,1)
carlon<- round(carlon,1)
dat2 <- dat[!(dat$y2 %in% carlat==TRUE & dat$x2 %in% carlon==TRUE),]
dat2$Comp.1 <- dat2$Comp.1*-1
out<- dat2

# get shapefile for US map
states <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers", "states")
wus <- states[states$STATE_ABBR=="WA"|states$STATE_ABBR=="OR"|states$STATE_ABBR=="CA"
              |states$STATE_ABBR=="ID"|states$STATE_ABBR=="NV"|states$STATE_ABBR=="MT"
              |states$STATE_ABBR=="UT"|states$STATE_ABBR=="NM"|states$STATE_ABBR=="WY"
              |states$STATE_ABBR=="CO"|states$STATE_ABBR=="AZ",]

################################################################################
# Plot PCA with grid

# shortcuts for column names 
ll <- c('x', 'y') # lat long 
sc <- c('Comp.1', 'Comp.2') # pca scores 

# make gridcells
xr <- range(dat[, sc ] )
yr <- range(dat[, sc ] ) 
nb <- 30  # number of grid divisions on each axis for site selection 
grid <- list(seq(xr[1], xr[2], length.out = nb), seq(yr[1], yr[2], length.out = nb) )

labs <- c("(a)","(b)")
p2 <- ggplot(data=out, aes(x=Comp.1, y=Comp.2)) +
  geom_point(color="gray87", size=.5) +
  geom_point(data=out[out$LT_data == 1, sc], color="black", size=.5) +
  geom_point(data=out[out$extra == 1, sc], color="gray48", size=.5) +
  geom_hline( yintercept = grid[[2]], color = 'gray30', size=.3) +
  geom_vline( xintercept = grid[[1]], color = 'gray30', size=.3) +
  scale_x_continuous(limits=c(-12.1,12.15)) +
  scale_y_continuous(limits=c(-6.45,14.95)) +
  xlab("PC1: Temperature") +
  ylab("PC2: Precipitation Seasonality") +
  theme(panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) #+
  #annotate("rect", xmin = -12.1, xmax = -11, ymin = 14, ymax = 14.95, fill = "white") +
  #annotate("text", label = "(a)", x = -12.1, y = 14.95, fontface = 2) 
  #annotate("text", label = "(a)", x = -11.5, y = 14.6, fontface = 2) 

###############################
# Create map working with same data as PCA
# Wrangle data into tidy frame
out2 <- out %>% 
  mutate(type=ifelse(LT_data==1, "LT", "fix")) %>%
  mutate(type=ifelse(extra==1, "extra", type)) %>%
  mutate(type=ifelse(LT_data!=1 & extra!=1, "presence", type))

# Make map
p1 <- ggplot(data=out2, aes(y=y, x=x, color=type)) +
  geom_point(size=.5) +
  # need extra layers: overlapping points hard to see
  geom_point(data=out2[out2$LT_data == 1, ], color="black", size=.5) +
  geom_point(data=out2[out2$extra == 1, ], color="gray48", size=.5) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="gray30", size=.3) +
  coord_fixed(1.3) +
  scale_color_manual(values=c("gray48", "black","gray87"), 
    name="", breaks=c("presence", "LT","extra"),
    labels=c("Presence","Long-term data", "Extra site")) +
  theme(panel.grid.major=element_blank(),legend.position="none",
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  xlab("Longitude") +
  ylab("Latitude") #+
  #annotate("text", label = "(b)", x = -125, y = 49, fontface = 2) +

# get legend
leg <- ggplot(data=out2, aes(y=y, x=x, color=type)) +
  geom_point(size=1) +
  scale_color_manual(values=c("gray48", "black","gray87"), 
                     name="",
                     breaks=c("presence", "LT","extra"),
                     labels=c("Presence","Long-term data", "Extra site"))
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save map and PCA to png
tiff(paste(fpath, "Fig1_PCA_map.tiff", sep=""),
    width = 80, height = 170, units = 'mm', res = 450)
grid.arrange(legend,p2,p1, ncol=1, heights=c(10,80,80))
dev.off()

################################################################################
# Figure xx:
# Scatterplot along MAT gradient, various GCMS, RCP8.5
################################################################################

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_GCM.csv", sep="")) %>%
  mutate(change=(predicted-baseline))
merged2 <- filter(merged, site %in% bad==FALSE)
merged <- merged2
unit <- read.csv(paste(dpath, "focal_sites_by_zone.csv", sep="")) %>%
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
d3 <- filter(d2, site %in% bad==FALSE)

# Color Version:---------------------------------------------
rcp85 <- merged %>% filter(scenario=="rcp85") %>%
  mutate(extirpated=ifelse(baseline>0&predicted==0,1,0))
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln, scenario=="rcp85")
  plot <- ggplot(data=d, aes(x=MAT,y=change)) +
    geom_point(aes(color=GCM), size=.5) +
    scale_color_manual(values=cbPalette, name="GCM") +
    geom_hline(yintercept=0, linetype="dashed") +
    #stat_smooth(aes(fill=GCM, color=GCM),method = "lm") +
    #stat_smooth(aes(fill=GCM, color=GCM)) +
    #scale_fill_manual(values=cbPalette, name="GCM") +
    ylab("Change in Response") +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(),
          legend.text.align = 0,
          plot.margin=unit(c(.1,.1,.1,.1), "cm"),
          axis.title.y = element_text(size = rel(1.3))) +
    xlab("MAT") +
    #scale_x_continuous(limits=c(-1.9,20.8)) +
    ylab(ylab) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=4)
  return(plot)
}
DGVM <- plot_raw_change(rcp85,modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DVM")
CC <- plot_raw_change(rcp85,modeln="RandFor", ylab=expression(paste(Delta," Max % Cover")), title="SC")
AK <- plot_raw_change(rcp85,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TC")
DRS <- plot_raw_change(rcp85,modeln="GISSM_v1.6.3", ylab=expression(paste(Delta," % Regen")), title="SS")
AK

# make legend
leg <- plot_raw_change(merged,modeln="AK", ylab="", title="TC")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save Plot
tiff(paste(fpath, "Fig4_change_GCM_rcp85_color_noline.tiff", sep=""),
    width = 169, height = 169, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
             heights = unit(c(9,160), "mm"))

dev.off()

# Black and White for print:-----------------------------------
# Focus on the model near the center (CESM1-CAM5):
CESM <- filter(merged, GCM=="CESM1-CAM5")
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln)
  plot <- ggplot(data=d, aes(x=MAT,y=change)) +
    geom_point(aes(color=scenario), size=.5) +
    scale_color_manual(values=c("grey","black"), name="Emissions\nScenario") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=scenario, color=scenario),method = "lm") +
    scale_fill_manual(values=c("grey","black"), name="Emissions\nScenario") +
    ylab("Change in Response") +
    theme(legend.position="none", legend.title=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
          legend.text.align = 0,
          plot.margin=unit(c(.1,.1,.1,.1), "cm"),
          axis.title.y = element_text(size = rel(1))) +
    xlab("MAT") +
    #scale_x_continuous(limits=c(-1.9,20.8)) +
    ylab(ylab) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=4)
  return(plot)
}
DGVM <- plot_raw_change(CESM,modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DVM")
DGVM
CC <- plot_raw_change(CESM,modeln="RandFor", ylab=expression(paste(Delta," Max % Cover")), title="SC")
AK <- plot_raw_change(CESM,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TC")
DRS <- plot_raw_change(CESM,modeln="GISSM_v1.6.3", ylab=expression(paste(Delta," % Regen")), title="SS")

# make legend
leg <- plot_raw_change(CESM,modeln="AK", ylab="", title="TC")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save Plot
tiff(paste(fpath, "Fig4_change_scenario_CESM_bw.tiff", sep=""),
     width = 169, height = 169, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
             heights = unit(c(9,160), "mm"))

dev.off()

################################################################################
# Figure xx:
# Bar chart showing variation between GCMs and models
################################################################################
# Manipulate data:
m5 <- m4 %>% mutate(abschange=abs(change)) %>%
  mutate(direction=ifelse(change>0,"increase","decrease")) %>%
  mutate(scenario=ifelse(scenario=="rcp45", "RCP4.5","RCP8.5")) %>%
  group_by(model,GCM,scenario, direction) %>%
  summarise(meanchange=mean(change), lower=meanchange-sd(change)/sqrt(length(change)),
            upper=meanchange+sd(change)/sqrt(length(change)))

# Re-order GCMs from coolest to hottest for 2070-2099:
m5$GCM <- factor(m5$GCM, levels = c("GISS-E2-H-CC","MPI-ESM-LR","CCSM4","CESM1-CAM5","HadGEM2-AO"))

# Make b/w-friendly scale for GCMs:
yelred <- c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
bw <- c('#f7f7f7','#cccccc','#969696','#636363','#252525')

# Make plots in color-----------------------------------------------------
AK <- 
  ggplot(data=m5[m5$model=="AK",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=yelred, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," % Cover"))) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")

CC <- 
  ggplot(data=m5[m5$model=="RandFor",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=yelred, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," Max % Cover"))) +
  scale_y_continuous(limits=c(-11,14)) +
  #annotate("text", x=.5, y = Inf, label = "(a)", vjust=1.3, hjust=1.3, size=4) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")

KR <- 
  ggplot(data=m5[m5$model=="DGVM",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=yelred, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," % Cover"))) +
  theme(axis.title.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")

DRS <- 
  ggplot(data=m5[m5$model=="GISSM_v1.6.3",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black",size=.5) +
  geom_hline(yintercept=0, size=.5) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=yelred, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," % Regen"))) +
  theme(axis.title.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")

# make legend
leg <- ggplot(data=m5[m5$model=="GISSM_v1.6.3",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
  scale_fill_manual(values=yelred, name="GCM") +
  theme(legend.position="none", legend.title=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.text.align = 0,
        plot.margin=unit(c(.1,.1,.1,.1), "cm"),
        axis.title.y = element_text(size = rel(1)))
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save Plot
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(CC))
gp2<- ggplot_gtable(ggplot_build(AK))
gp3<- ggplot_gtable(ggplot_build(KR))
gp4<- ggplot_gtable(ggplot_build(DRS))
#gprects<- ggplot_gtable(ggplot_build(prects))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp3$widths[2:3], gp4$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
gp3$widths[2:3] <- maxWidth
gp4$widths[2:3] <- maxWidth
grid.arrange(arrangeGrob(gp1,gp2,gp3,gp4, ncol=2))

tiff(paste(fpath, "Fig3_GCM_magchange_color.tiff", sep=""),
     width = 169, height = 169, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(gp1,gp2,gp3,gp4, ncol=2,
            heights = unit(c(77,80), "mm")), ncol=1,
             heights = unit(c(9,160), "mm"))
dev.off()

# Make plots in black and white-------------------------------------------------
AK <- 
  ggplot(data=m5[m5$model=="AK",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=bw, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," % Cover"))) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")

CC <- 
  ggplot(data=m5[m5$model=="RandFor",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=bw, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," Max % Cover"))) +
  scale_y_continuous(limits=c(-11,14)) +
  #annotate("text", x=.5, y = Inf, label = "(a)", vjust=1.3, hjust=1.3, size=4) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")

KR <- 
  ggplot(data=m5[m5$model=="DGVM",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=bw, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," % Cover"))) +
  theme(axis.title.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")

DRS <- 
  ggplot(data=m5[m5$model=="GISSM_v1.6.3",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black",size=.5) +
  geom_hline(yintercept=0, size=.5) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=bw, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," % Regen"))) +
  theme(axis.title.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")

# make legend
leg <- ggplot(data=m5[m5$model=="GISSM_v1.6.3",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
  scale_fill_manual(values=bw, name="GCM") +
  theme(legend.position="none", legend.title=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.text.align = 0,
        plot.margin=unit(c(.1,.1,.1,.1), "cm"),
        axis.title.y = element_text(size = rel(1)))
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save Plot
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(CC))
gp2<- ggplot_gtable(ggplot_build(AK))
gp3<- ggplot_gtable(ggplot_build(KR))
gp4<- ggplot_gtable(ggplot_build(DRS))
#gprects<- ggplot_gtable(ggplot_build(prects))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp3$widths[2:3], gp4$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
gp3$widths[2:3] <- maxWidth
gp4$widths[2:3] <- maxWidth
grid.arrange(arrangeGrob(gp1,gp2,gp3,gp4, ncol=2))

tiff(paste(fpath, "Fig3_GCM_magchange_bw.tiff", sep=""),
     width = 169, height = 169, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(gp1,gp2,gp3,gp4, ncol=2,
                                 heights = unit(c(77,80), "mm")), ncol=1,
             heights = unit(c(9,160), "mm"))
dev.off()

################################################################################
# Figure xx:
# Boxplot showing mean climate by response category
################################################################################
d2 <- merged %>%
  filter(model!="MaxEntRaw"&model!="MaxEntBin") %>%
  filter(scenario=="rcp85") %>% #exclude RCP4.5 output
  group_by(site) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0), MAT=mean(bio1)/10) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf_cat=pmax(n.increase,n.decrease)) %>%
  mutate(rel_conf_cat=conf_cat/n) %>%
  mutate(consensus=ifelse(conf_cat==n.increase,"Increase","Decrease")) %>%
  mutate(consensus=ifelse(n.increase==n.decrease&n.increase==conf_cat,"Unsure",consensus)) %>%
  filter(n==max(n))
dim(d2)
d2$consensus <- factor(d2$consensus, levels=c("Decrease","Unsure","Increase"))

tiff(paste(fpath, "Fig5_consensus_MAT_boxplot_bw.tiff", sep=""),
     width = 80, height = 80, units = 'mm', res = 300)
ggplot(data=d2, aes(x=consensus,y=MAT, group=consensus)) +
  geom_boxplot(notch=T) +
  coord_flip() +
  xlab("Change in Performance") +
  ylab("Mean Annual Temperature")
dev.off()

################################################################################
# Plot the change/confidence on Andy's PCA axes
################################################################################
# merge PCA back into manipulated data
m2 <- merged %>% group_by(site) %>% summarise_each(funs(mean)) %>% 
  dplyr::select(site,Comp.1,Comp.2) %>%
  mutate(Comp.1_rev=Comp.1*-1)
d3 <- merge(d2,m2, by="site", all=F) 
dim(d3)
gg3 <- 
  ggplot(data = d3, aes( x = Comp.1_rev, y = Comp.2, fill = conf2 )) + 
  geom_point( size = 1,colour="black",pch=21) + 
  scale_fill_gradient2(low="red", high="blue",
                       name="Model\nAgreement\n",
                       breaks=c(-20,0,20),labels=c(-20,0,20),
                       limits=c(-20,20)) +
  xlab( 'PC1 : (Site Temperature)') +
  ylab( 'PC2 : (Precipitation Seasonality)') +
  theme(
    #legend.position="top",
    legend.justification=c(1,1), legend.position=c(.99,.99),
    panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# Save Plot
tiff(paste(fpath, "Fig6_PCA_model_agreement_color.tiff", sep=""),
    width = 80, height = 100, units = 'mm', res = 300)
gg3
dev.off()
