#############################################################################
# Graphs of model results from simple climate perturbations
# 
#############################################################################
rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size=14)) # sized for ppt
library(gridExtra)
library(splines)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Documents/sageseer/figures/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_GCM.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() 
m4 <- gather(m3, baseline:predicted,key=time,value=value)

# Color Palette for GCMs (color-blind friendly)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

################################################################################
# Plot the absolute change, in original scale
# Show diff GCM x RCPs in diff colors
################################################################################
plot_raw_change <- function(modeln,title) {
  d <- dplyr::filter(m3,model==modeln)
  plot <- ggplot(data=d, aes(x=MAT,y=change, color=GCM)) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(method = "lm", aes(fill=GCM)) +
    scale_fill_manual(values=cbPalette, name="GCM") +
    scale_color_manual(values=cbPalette, name="GCM") +
    ylab("Change in Response") + 
    facet_wrap(~scenario) +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          legend.text.align = 0) +
    xlab("MAP") +
    # scale_x_continuous(limits=c(-1.9,22.9)) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=8)
  return(plot)
}
GISSM <- plot_raw_change(modeln="GISSM_v1.6.3", title="GISSM")
DGVM <- plot_raw_change(modeln="DGVM",title="DGVM")
RF <- plot_raw_change(modeln="RandFor", title="RF")
TS <- plot_raw_change(modeln="AK", title="TS")
DGVM

# make legend
leg <- plot_raw_change(modeln="AK", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)
pdf(paste(fpath, "GCM_change_response_MAT_gradient.pdf", sep=""),width=12,height=8)
TS
DGVM
GISSM
RF
#ME_raw
#ME_bin
dev.off()

# Save as 4-plot grid
jpeg(paste(fpath, "change_GCM_rcpssame.jpeg", sep=""),
     width = 420, height = 300, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(RF,TS,DGVM,GISSM, ncol=2), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()

################################################################################
# Plot raw change along precip gradient
plot_raw_change <- function(modeln,title) {
  d <- dplyr::filter(m3,model==modeln)
  plot <- ggplot(data=d, aes(x=MAP,y=change, color=GCM)) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(method = "lm", aes(fill=GCM)) +
    scale_fill_manual(values=cbPalette, name="GCM") +
    scale_color_manual(values=cbPalette, name="GCM") +
    ylab("Change in Response") + 
    facet_wrap(~scenario) +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          legend.text.align = 0) +
    xlab("MAP") +
   # scale_x_continuous(limits=c(-1.9,22.9)) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=8)
  return(plot)
}
DRS <- plot_raw_change(modeln="GISSM_v1.6.3", title="GISSM")
DGVM <- plot_raw_change(modeln="DGVM",title="DGVM")
CC <- plot_raw_change(modeln="RandFor", title="RF")
AK <- plot_raw_change(modeln="AK", title="TS")
DGVM

# make legend
leg <- plot_raw_change(modeln="AK", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

jpeg(paste(fpath, "change_GCM_prcp_gradient.jpeg", sep=""),
     width = 420, height = 800, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(AK,DRS,CC,DGVM, ncol=1), ncol=1,
             heights = c(0.2, 2.5,0))
dev.off()
pdf(paste(fpath, "GCM_change_response_prcp.pdf", sep=""),width=12,height=8)
AK
DRS
DGVM
RF
#ME_raw
#ME_bin
dev.off()

################################################################################

################################################################################
# Plot the absolute change, in original scale, MAT gradient, 4-plot grid
################################################################################
m4 <- filter(m3, scenario=="rcp45")

plot_raw_change <- function(modeln, ylab,title) {
  d <- dplyr::filter(m4, model==modeln)
  plot <- ggplot(data=d, aes(x=MAT,y=change)) +
    geom_point(aes(color=GCM), size=1) +
    scale_color_manual(values=cbPalette, name="GCM") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=GCM, color=GCM),method = "lm") +
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
MaxEnt <- plot_raw_change(modeln="MaxEntBin", ylab=expression(paste(Delta," % Yrs with Regen")), title="GISSM")
DGVM <- plot_raw_change(modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DGVM")
CC <- plot_raw_change(modeln="RandFor", ylab=expression(paste(Delta," Max % Cover")), title="RF")
AK <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
DRS <- plot_raw_change(modeln="GISSM_v1.6.3", ylab=expression(paste(Delta," % Regen")), title="GISSM")
DGVM

# make legend
leg <- plot_raw_change(modeln="AK", ylab="", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save Plot
jpeg(paste(fpath, "change_temp_GCM_rcp45.jpeg", sep=""),
     width = 420, height = 420, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()

############ SAME for RCP8.5
m4 <- filter(m3, scenario=="rcp85")

plot_raw_change <- function(modeln, ylab,title) {
  d <- dplyr::filter(m4, model==modeln)
  plot <- ggplot(data=d, aes(x=MAT,y=change)) +
    geom_point(aes(color=GCM), size=1) +
    scale_color_manual(values=cbPalette, name="GCM") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=GCM, color=GCM),method = "lm") +
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
MaxEnt <- plot_raw_change(modeln="MaxEntBin", ylab=expression(paste(Delta," % Yrs with Regen")), title="GISSM")
DGVM <- plot_raw_change(modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DGVM")
CC <- plot_raw_change(modeln="RandFor", ylab=expression(paste(Delta," Max % Cover")), title="RF")
AK <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
DRS <- plot_raw_change(modeln="GISSM_v1.6.3", ylab=expression(paste(Delta," % Regen")), title="GISSM")
DGVM

# make legend
leg <- plot_raw_change(modeln="AK", ylab="", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

# Save Plot
jpeg(paste(fpath, "change_temp_GCM_rcp85.jpeg", sep=""),
     width = 420, height = 300, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()

# Save Plot
jpeg(paste(fpath, "change_temp_GCM_rcp85_1row.jpeg", sep=""),
     width = 420, height = 200, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=4), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()

