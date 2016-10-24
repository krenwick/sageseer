#############################################################################
# Plots for Peter's RtW talk
# 
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Figures/"

# Color Palette for GCMs (color-blind friendly)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

# List of "bad" sites
bad <- c(495, 579, 494, 582, 580, 634, 633, 632, 668, 496, 497)

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
bads <- filter(d2, site %in% bad)
bads2 <- filter(merged, site %in% bad) %>% dplyr::select(site, model, baseline, change) %>%
  filter(model!="MaxEntBin"&model!="MaxEntRaw")
dim(d3)
table(d3$conf2)
table(d3$consensus)
table(d2$consensus)

# Data for Dominique
latlon <- merged %>% group_by(site) %>% summarise(Lon=mean(longitude.x), Lat=mean(latitude.x))
d3 <- merge(d2, latlon, by="site", all=F) %>%
  dplyr::select(site, Lon, Lat, consensus, conf_cat) %>%
  rename(confidence=conf_cat)
#write.csv(d3, paste(dpath, "LonLat_confidence_cats.csv", sep=""), row.names=F)
################################################################################
# Plot the absolute change, in original scale, MAT gradient, 4-plot grid
################################################################################
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln)
  plot <- ggplot(data=d, aes(x=MAT,y=change)) +
    geom_point(aes(color=GCM), size=1) +
    scale_color_manual(values=cbPalette, name="GCM") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=GCM, color=GCM),method = "lm") +
    #stat_smooth(aes(fill=GCM, color=GCM)) +
    scale_fill_manual(values=cbPalette, name="GCM") +
    ylab("Change in Response") +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          legend.text.align = 0,
          plot.margin=unit(c(.1,.1,.1,.1), "cm"),
          axis.title.y = element_text(size = rel(1.3))) +
    xlab("MAT") +
    scale_x_continuous(limits=c(-1.9,20.8)) +
    ylab(ylab) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=8)
  return(plot)
}
DGVM <- plot_raw_change(merged2,modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DGVM")
CC <- plot_raw_change(merged2,modeln="RandFor", ylab=expression(paste(Delta," Max % Cover")), title="RF")
AK <- plot_raw_change(merged2,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
DRS <- plot_raw_change(merged2,modeln="GISSM_v1.6.3", ylab=expression(paste(Delta," % Regen")), title="GISSM")

# try scaling Andy's change by the baseline (so % change)
AKdat <- merged %>% mutate(change=change/baseline)
AK2 <- AK <- plot_raw_change(AKdat,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
# try removing non-long-term plots
AKdat2 <- merged %>% filter(LT_data==1)
AK3 <- plot_raw_change(AKdat2,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
AK2
AK3

# make legend
leg <- plot_raw_change(merged,modeln="AK", ylab="", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save Plot
png(paste(fpath, "change_temp_GCM_rcp85.png", sep=""),
     width = 420, height = 320, units = 'mm', res = 450)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()

# Save Plots, 1 row
png(paste(fpath, "change_temp_GCM_rcp85_1row.png", sep=""),
     width = 420, height = 250, units = 'mm', res = 450)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=4), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()

################################################################################
# Plot the absolute change, in original scale, MAT gradient- try quadratic fit
################################################################################
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln)
  plot <- ggplot(data=d, aes(x=MAT,y=change)) +
    geom_point(aes(color=GCM), size=1) +
    scale_color_manual(values=cbPalette, name="GCM") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=GCM, color=GCM),method = "lm", formula = y ~ poly(x, 3)) +
    #stat_smooth(aes(fill=GCM, color=GCM)) +
    scale_fill_manual(values=cbPalette, name="GCM") +
    ylab("Change in Response") +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          legend.text.align = 0,
          plot.margin=unit(c(.1,.1,.1,.1), "cm"),
          axis.title.y = element_text(size = rel(1.3))) +
    xlab("MAT") +
    scale_x_continuous(limits=c(-1.9,22.9)) +
    ylab(ylab) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=8)
  return(plot)
}
DGVM <- plot_raw_change(merged,modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DGVM")
CC <- plot_raw_change(merged,modeln="RandFor", ylab=expression(paste(Delta," Max % Cover")), title="RF")
AK <- plot_raw_change(merged,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
DRS <- plot_raw_change(merged,modeln="GISSM_v1.6.3", ylab=expression(paste(Delta," % Regen")), title="GISSM")
AK
DRS
CC
DGVM
# make legend
leg <- plot_raw_change(merged,modeln="AK", ylab="", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save Plot
#png(paste(fpath, "change_temp_GCM_rcp85.png", sep=""),
    width = 420, height = 320, units = 'mm', res = 450)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()

# Save Plots, 1 row
#png(paste(fpath, "change_temp_GCM_rcp85_1row.png", sep=""),
    width = 420, height = 250, units = 'mm', res = 450)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=4), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()

################################################################################
# Plot the absolute change, in original scale, just one model
################################################################################
CESM <- filter(merged, GCM=="CESM1-CAM5")
DGVM <- plot_raw_change(CESM,modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DGVM")
CC <- plot_raw_change(CESM,modeln="RandFor", ylab=expression(paste(Delta," Max % Cover")), title="RF")
AK <- plot_raw_change(CESM,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
DRS <- plot_raw_change(CESM,modeln="GISSM_v1.6.3", ylab=expression(paste(Delta," % Regen")), title="GISSM")

leg <- plot_raw_change(CESM,modeln="AK", ylab="", title="TS")
leg2 <- leg + theme(legend.position="top")
legend <- get_legend(leg2)
# Save Plots, 1 row
png(paste(fpath, "change_temp_CESM1_rcp85_1row.png", sep=""),
     width = 420, height = 250, units = 'mm', res = 450)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=4), ncol=1,
             heights = c(0.2, 2.5,0))
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
  geom_point( size = 2.5,colour="black",pch=21) + 
  scale_fill_gradient2(name="Model\nAgreement") +
  xlab( 'PC1 : (Site Temperature)') +
  ylab( 'PC2 : (Precipitation Seasonality)') +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

# Save Plot
png(paste(fpath, "PCA_model_agreement.png", sep=""),
     width = 320, height = 220, units = 'mm', res = 450)
gg3
dev.off()

################################################################################
# Plot the table of consensus categories
################################################################################
png(paste(fpath, "consensus_table_histogram.png", sep=""),
     width = 220, height = 120, units = 'mm', res = 450)
ggplot(data=d2, aes(x=consensus, fill=consensus)) +
  #geom_bar(color="black") +
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black") +
  scale_fill_manual(labels=c("Decrease","Increase", "Unsure"),
                    values=c("#d7191c","#2c7bb6","grey"),name="Model\nConsensus") +
  xlab("Model Consensus") +
  ylab("Proportion of Sites") +
  theme(legend.position="none")
dev.off()
