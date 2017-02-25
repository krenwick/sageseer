################################################################################
# Make figures for sagebrush manuscript
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=9)) # sized for print
library(rgdal)
library(gridExtra)
library(grid)

#***USER MUST CHANGE***
# paths to data and figure folders: Must use full path for readOGR to work
dpath <- "/Users/poulterlab1/version-control/sageseer/"
fpath <- "/Users/poulterlab1/version-control/sageseer/figures/"

#----------------------------------------
# Journal Specifications for figure size
# Global Change Bio:
col1 <- 80 # 1 column width = 80 mm
col2 <- 169 # 2 column width = 169 mm
#--------------------------------------
# Color Palette for GCMs (color-blind friendly)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

################################################################################
# Load and clean data for all figures
################################################################################
# Pull in data and manipulat
# Caroline's list of bad points:
car <- read.csv(paste(dpath, "data/AZ_pts_no_source_data.csv", sep=""))
carlat <- car$latitude
carlon <- car$longitude

# data from Andy's PCA script (original full list of presence points)
dat <- read.csv(paste(dpath, "data/pcapts.csv", sep=""))

# Eliminate points on Caroline's "bad" list (WTH don't coords match??)
dat$x2 <- round(dat$x,1)
dat$y2 <- round(dat$y,1)
carlat<- round(carlat,1)
carlon<- round(carlon,1)
dat2 <- dat[!(dat$y2 %in% carlat==TRUE & dat$x2 %in% carlon==TRUE),]
dat2$Comp.1 <- dat2$Comp.1*-1
out<- dat2

# eliminate dakota pts
max(out$x)
min(out$x)
outb <- filter(out, x<=-103.7)
nrow(outb)
out <- outb

# get shapefile for US map
states <- readOGR(paste(dpath, "data/GIS_baselayers", sep=""), "states")
wus <- states[states$STATE_ABBR=="WA"|states$STATE_ABBR=="OR"|states$STATE_ABBR=="CA"
              |states$STATE_ABBR=="ID"|states$STATE_ABBR=="NV"|states$STATE_ABBR=="MT"
              |states$STATE_ABBR=="UT"|states$STATE_ABBR=="NM"|states$STATE_ABBR=="WY"
              |states$STATE_ABBR=="CO"|states$STATE_ABBR=="AZ",]

# Pull in merged model output and manipulate
merged <- read.csv(paste(dpath, "data/merged_data_GCM.csv", sep="")) %>%
  mutate(change=(predicted-baseline))
unit <- read.csv(paste(dpath, "data/focal_sites_by_zone.csv", sep="")) %>%
  dplyr::select(site,elev:NA_L1NAME)

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

################################################################################
# Figure 1:
# Map sagebrush cover + presence points + focal points
################################################################################
# 1. Plot PCA with grid

# shortcuts for column names 
ll <- c('x', 'y') # lat long 
sc <- c('Comp.1', 'Comp.2') # pca scores 

# make gridcells
xr <- range(dat[, sc ] )
yr <- range(dat[, sc ] ) 
nb <- 30  # number of grid divisions on each axis for site selection 
grid <- list(seq(xr[1], xr[2], length.out = nb), seq(yr[1], yr[2], length.out = nb) )

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
  annotate("text", x=-Inf, y = Inf, label = "(a)", vjust=1.3, hjust=2, size=3,fontface=2) +
  theme(panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        panel.border=element_rect(size=0.2, linetype="solid")) 

# Code to override clipping
gt <- ggplot_gtable(ggplot_build(p2))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
p2 <-gt
plot(p2)

###############################
# 2. Create map working with same data as PCA
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
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        panel.border=element_rect(size=0.2, linetype="solid")) +
  annotate("text", x=-Inf, y = Inf, label = "(b)", vjust=1.3, hjust=2, size=3,fontface=2) +
  xlab("Longitude") +
  ylab("Latitude") 
p1

# Code to override clipping
gt2 <- ggplot_gtable(ggplot_build(p1))
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"
p1 <-gt2


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
# tiff(paste(fpath, "Fig1_PCA_map.tiff", sep=""),
#     width = col1, height = 170, units = 'mm', res = 450)
# grid.arrange(legend,p2,p1, ncol=1, heights=c(10,80,80))
# dev.off()

#try eps
both <- grid.arrange(legend,p2,p1, ncol=1, heights=c(10,80,80))
ggsave(paste(fpath, "PCA_map.eps", sep=""), plot=both, width = col1, height = 170, 
       units = 'mm')

################################################################################
# Figure xx:
# Scatterplot along MAT gradient, various GCMS, RCP8.5
################################################################################

# Color Version:---------------------------------------------
rcp85 <- merged %>% filter(scenario=="rcp85") %>%
  mutate(extirpated=ifelse(baseline>0&predicted==0,1,0))
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln)
  plot <- ggplot(data=d, aes(x=bio1/10,y=change)) +
    geom_point(aes(color=GCM), size=.5) +
    scale_color_manual(values=cbPalette, name="GCM") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=GCM, color=GCM),method = "lm") +
    #stat_smooth(aes(fill=GCM, color=GCM)) +
    scale_fill_manual(values=cbPalette, name="GCM") +
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
CC <- plot_raw_change(rcp85,modeln="randfor", ylab=expression(paste(Delta," Max % Cover")), title="SC")
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
both2 <- grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
                      heights = unit(c(9,160), "mm"))
ggsave(paste(fpath, "change_GCM_MAT_rcp85_color_line.eps", sep=""), plot=both2,
       width = col2, height = col2, units = 'mm')

# Black and White for print:-----------------------------------
# Focus on the model near the center (CESM1-CAM5):
CESM <- filter(merged, GCM=="CESM1-CAM5")
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln)
  plot <- ggplot(data=d, aes(x=bio1/10,y=change)) +
    geom_point(aes(color=scenario), size=.3) +
    scale_color_manual(values=c("grey","black"), name="Emissions\nScenario") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=scenario, color=scenario),method = "lm", size=.5) +
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
CC <- plot_raw_change(CESM,modeln="randfor", ylab=expression(paste(Delta," Max % Cover")), title="SC")
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
# Figure xx (supplemental 3?)
# Scatterplot along precip gradient, various GCMS, RCP8.5
################################################################################
# Color Version:---------------------------------------------
rcp85 <- merged %>% filter(scenario=="rcp85") %>%
  mutate(extirpated=ifelse(baseline>0&predicted==0,1,0))
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln)
  plot <- ggplot(data=d, aes(x=bio12,y=change)) +
    geom_point(aes(color=GCM), size=.5) +
    scale_color_manual(values=cbPalette, name="GCM") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=GCM, color=GCM),method = "lm") +
    #stat_smooth(aes(fill=GCM, color=GCM)) +
    scale_fill_manual(values=cbPalette, name="GCM") +
    ylab("Change in Response") +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(),
          legend.text.align = 0,
          plot.margin=unit(c(.1,.1,.1,.1), "cm"),
          axis.title.y = element_text(size = rel(1.3))) +
    xlab("Mean Annual Precipitation (mm)") +
    #scale_x_continuous(limits=c(-1.9,20.8)) +
    ylab(ylab) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=4)
  return(plot)
}
DGVM <- plot_raw_change(rcp85,modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DVM")
DGVM
CC <- plot_raw_change(rcp85,modeln="randfor", ylab=expression(paste(Delta," Max % Cover")), title="SC")
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
tiff(paste(fpath, "SS3_change_GCM_rcp85_pptgradient.tiff", sep=""),
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
        legend.position="none") +
  annotate("text", x=-Inf, y = Inf, label = "TC", vjust=1.3, hjust=-.2, size=8)
AK

CC <- 
  ggplot(data=m5[m5$model=="randfor",], aes(x=scenario, y=meanchange, fill=GCM)) +
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
        legend.position="none") +
  annotate("text", x=-Inf, y = Inf, label = "SC", vjust=1.3, hjust=-.2, size=8)

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
        legend.position="none") +
  annotate("text", x=-Inf, y = Inf, label = "DGVM", vjust=1.3, hjust=-.2, size=8)

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
        legend.position="none") +
  annotate("text", x=-Inf, y = Inf, label = "SS", vjust=1.3, hjust=-.2, size=8)

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
vj <- 1.5 # vertical adjustment for panel label, pos moves down
hj <- -.1 # horizotal placement of panel label, neg moves right

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
        legend.position="none") +
  annotate("text", x=-Inf, y = Inf, label = "(b) TC", vjust=vj, hjust=hj, size=3)

CC <- 
  ggplot(data=m5[m5$model=="randfor",], aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=bw, name="GCM") +
  xlab("GCM") +
  ylab(expression(paste(Delta," Max % Cover"))) +
  scale_y_continuous(limits=c(-11,14)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none")+
  annotate("text", x=-Inf, y = Inf, label = "(a) SC", vjust=vj, hjust=hj, size=3)

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
        legend.position="none")+
  annotate("text", x=-Inf, y = Inf, label = "(c) DGVM", vjust=vj, hjust=hj, size=3)

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
        legend.position="none") +
  annotate("text", x=-Inf, y = Inf, label = "(d) SS", vjust=vj, hjust=hj, size=3)

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

# tiff(paste(fpath, "Fig3_GCM_magchange_bw.tiff", sep=""),
#      width = 169, height = 169, units = 'mm', res = 300)
# grid.arrange(legend, arrangeGrob(gp1,gp2,gp3,gp4, ncol=2,
#                                  heights = unit(c(77,80), "mm")), ncol=1,
#              heights = unit(c(9,160), "mm"))
# dev.off()

#try eps
GCM_bar_bw <- grid.arrange(legend, arrangeGrob(gp1,gp2,gp3,gp4, ncol=2,
                          heights = unit(c(77,80), "mm")), ncol=1,
                           heights = unit(c(9,160), "mm"))
ggsave(paste(fpath, "GCM_magchange_bw.eps", sep=""), plot=GCM_bar_bw,
       width = col2, height = col2, units = 'mm')

################################################################################
# Figure xx:
# Boxplot showing mean climate by response category (RCP8.5)
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

# Make color for poster, and flip like the old one
myCols = c("red3","dodgerblue3")
ggplot(data=d2[d2$consensus!="Unsure",], aes(x=consensus,y=MAT, group=consensus)) +
  geom_boxplot(notch=T, aes(fill=consensus)) +
  scale_fill_manual(values=myCols) +
  theme_bw(base_size=20) +
  theme(legend.position="none") +
  coord_flip() +
  xlab("Change in Performance") +
  ylab("Mean Annual Temperature")

################################################################################
# Plot scatterplot of consensus categories along MAT gradient
# Add in smoothed histogram
################################################################################

pts <- ggplot(data=d2, aes(x=MAT, y=(conf2))) +
  geom_point(aes(fill=conf2), color="black", pch=21) +
  #geom_smooth(span=.9) +
  scale_fill_gradient2(low="red", high="blue",
                       name="Model\nAgreement\n",
                       breaks=c(-20,0,20),labels=c(-20,0,20),
                       limits=c(-20,20)) +
  geom_hline(yintercept=0) +
  xlab("Mean Annual Temperature") +
  ylab("Vulnerability Index") +
  geom_vline(xintercept=min(d2[d2$consensus=="Decrease",]$MAT), linetype="dashed") +
  geom_vline(xintercept=max(d2[d2$consensus=="Increase",]$MAT), linetype="dashed") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
    legend.justification=c(1,1), legend.position=c(.99,.99),
    #legend.position="top",
    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  xlim(c(-5.9,22.9)) +
  guides(fill = guide_colorbar(barheight = unit(1, "cm")))
pts
# Work on smoothed histogram----------------------------------------------------
# File folder for climatology rasters
ndir <- "~/Box Sync/sageseer/ClimateData/PRISM_tmean_30yr_normal_800mM2_annual_asc/"

library(raster)
# Read in PRISM climatology rasters
MAT <- raster(paste(ndir, "MAT_crop.tif", sep=""))

# Pull in cover map from the climate console
cmap <- raster("/Users/poulterlab1/Documents/GIS_baselayers/Sagebrush_MW5k_1km_latlon.tif")
points <- rasterToPoints(cmap)

# Convert to data frame and filter out zeros
pts2 <- data.frame(points)
pts3 <- pts2[pts2$Sagebrush_MW5k_1km_latlon>0,]
dim(pts2)
dim(pts3)

# Extract MAT data
pts4 <- extract(MAT,pts3[,1:2])
cover <- as.data.frame(pts4)

# Plot a smoothed histogram of cover data
# Split to color by confidence
myCols = c("red","lightgray","blue")

max<- max(d2[d2$consensus=="Increase",]$MAT)
min <- min(d2[d2$consensus=="Decrease",]$MAT)
c2 <- cover %>% 
  mutate(cat=ifelse(pts4>max, "Decrease","Either")) %>%
  mutate(cat=ifelse(pts4<min, "Increase",cat))
           
                      
hist <-
ggplot(data=c2, aes(x=pts4, fill=cat)) +
  #geom_density(adjust=2,alpha = 0.1,color="grey") +
  #geom_histogram(binwidth=.5) +
  geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = .5) + 
  scale_fill_manual(values=myCols) +
  geom_vline(xintercept=min, linetype="dashed") +
  geom_vline(xintercept=max, linetype="dashed") +
  xlab(expression("Mean Annual Temperature ("*~degree*"C)")) +
  ylab("Proportion of Range") +
  xlim(c(-5.9,22.9)) +
  theme(legend.position="none",
    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
hist

# Save Plots
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(pts))
gp2<- ggplot_gtable(ggplot_build(hist))
#gprects<- ggplot_gtable(ggplot_build(prects))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
grid.arrange(arrangeGrob(gp1,gp2, ncol=1))

# Save scatterplot and boxplot to tiff
tiff(paste(fpath, "Figx_vulnerability_rangehist.tiff", sep=""),
     width = 80, height = 152, units = 'mm', res = 450)
grid.arrange(arrangeGrob(gp1,gp2, ncol=1, heights=c(72,80)))
dev.off()

################################################################################
# Plot the change/confidence on Andy's PCA axes (RCP8.5)
################################################################################
# merge PCA back into manipulated data
m2 <- merged %>% group_by(site) %>% summarise_each(funs(mean)) %>% 
  dplyr::select(site,Comp.1,Comp.2) %>%
  mutate(Comp.1_rev=Comp.1*-1)
d3 <- merge(d2,m2, by="site", all=F) # d2 is just RCP8.5
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

################################################################################
# Make map showing change/confidence (RCP8.5)
################################################################################
m6 <- merged %>% group_by(site) %>%
  summarise_each(funs(mean)) %>%
  dplyr::select(site:latitude.x,bio1:bio19)
mapdat <- merge(d2,m6,by="site",all.y=F) 
md2 <- mapdat %>% group_by(site) %>%
  summarise(conf2=mean(conf2), lat=mean(latitude.x), lon=mean(longitude.x))
table(md2$conf2)

png(paste(fpath, "map_agreement_rcp8.5.png", sep=""),
    width = col1, height = col1, units = 'mm', res = 450)
ggplot(data=md2, aes(y=lat, x=lon,fill=conf2)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point( size = 1,colour="black",pch=21) + 
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
