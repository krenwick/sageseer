
# intro code to read in and organize data
rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)
library(wesanderson)

# set file path for sageseer- CHANGE BASED ON YOUR COMPUTER
setwd("/Users/poulterlab1/version-control/sageseer/")
#################################

# folder path:
dpath <- "data/"
opath <- "figures/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_perturb.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() 
m4 <- gather(m3, baseline:predicted,key=time,value=value)

################################################################################

## Response to temperature perturbations

# Plot response to temp manipulations along temp gradient
plot_raw_change <- function(modeln, ylab,varn,title) {
  d <- dplyr::filter(m3,var==varn & model==modeln)
  plot <- ggplot(data=d, aes(x=bio1/10,y=change, color=as.factor(mag))) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(method = "lm",aes(fill=as.factor(mag))) +
    scale_color_manual(values = wes_palette("GrandBudapest"),
                       #breaks=c(".2","2","4"),
                       labels=c(expression("+ .2"*~degree*"C"),expression("+ 2"*~degree*"C"),expression("+ 4"*~degree*"C"))) +
    scale_fill_manual(values = wes_palette("GrandBudapest"),
                      #breaks=c(".2","2","4"),
                      labels=c(expression("+ .2"*~degree*"C"),expression("+ 2"*~degree*"C"),expression("+ 4"*~degree*"C"))) +
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
DRS <- plot_raw_change(modeln="DRS", ylab=expression(paste(Delta," % Yrs with Regen")), varn="temp", title="GISSM")
DGVM <- plot_raw_change(modeln="DGVM-full-400ppm", ylab=expression(paste(Delta," % Cover")),varn="temp", title="DGVM")
randfor <- plot_raw_change(modeln="randfor", ylab=expression(paste(Delta," Max % Cover")),varn="temp", title="RF")
AK <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="temp", title="TS")

# make legend
leg <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="temp", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

jpeg(paste(fpath, "change_by_mag_temp.jpeg", sep=""),
     width = 420, height = 320, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(AK,DRS,randfor,DGVM, ncol=2), ncol=1,
             heights = c(0.15, 2.5,0))
dev.off()

################################################################################
# Plot response to precipitation manipulations along temp gradient
plot_raw_changeP <- function(modeln, ylab,varn,title) {
  d <- dplyr::filter(m3,var==varn & model==modeln)
  plot <- ggplot(data=d, aes(x=bio1/10,y=change, color=as.factor(mag))) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(method = "lm",aes(fill=as.factor(mag))) +
    scale_color_manual(values = wes_palette("GrandBudapest"),
    labels=c(expression("-10%"),expression("+10%"),expression("+20%"))) +
    scale_fill_manual(values = wes_palette("GrandBudapest"),
    labels=c(expression("-10%"),expression("+10%"),expression("+20%"))) +
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
DRS <- plot_raw_changeP(modeln="DRS", ylab=expression(paste(Delta," % Yrs with Regen")), varn="ppt", title="GISSM")
DGVM <- plot_raw_changeP(modeln="DGVM-full-400ppm", ylab=expression(paste(Delta," % Cover")),varn="ppt", title="DGVM")
randfor <- plot_raw_changeP(modeln="randfor", ylab=expression(paste(Delta," Max % Cover")),varn="ppt", title="RF")
AK <- plot_raw_changeP(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="ppt", title="TS")

# make legend
leg <- plot_raw_changeP(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="ppt", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

jpeg(paste(fpath, "change_by_mag_ppt.jpeg", sep=""),
     width = 420, height = 320, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(AK,DRS,randfor,DGVM, ncol=2), ncol=1,
             heights = c(0.15, 2.5,0))
dev.off()

################################################################################
# Plot response to precipitation manipulations along precipip gradient
plot_raw_change <- function(modeln, ylab,varn,title) {
  d <- dplyr::filter(m3,var==varn & model==modeln)
  plot <- ggplot(data=d, aes(x=MAP,y=change, color=as.factor(mag))) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(method = "lm",aes(fill=as.factor(mag))) +
    scale_color_manual(values = wes_palette("GrandBudapest"),
                       labels=c(expression("-10%"),expression("+10%"),expression("+20%"))) +
    scale_fill_manual(values = wes_palette("GrandBudapest"),
                      labels=c(expression("-10%"),expression("+10%"),expression("+20%"))) +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          legend.text.align = 0,
          plot.margin=unit(c(.1,.1,.1,.1), "cm"),
          axis.title.y = element_text(size = rel(1.3))) +
    xlab("MAP (cm)") +
    #scale_x_continuous(limits=c(-1.9,22.9)) +
    ylab(ylab) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=8)
  return(plot)
}
DRS <- plot_raw_change(modeln="DRS", ylab=expression(paste(Delta," % Yrs with Regen")), varn="ppt", title="GISSM")
DGVM <- plot_raw_change(modeln="DGVM-full-400ppm", ylab=expression(paste(Delta," % Cover")),varn="ppt", title="DGVM")
randfor <- plot_raw_change(modeln="randfor", ylab=expression(paste(Delta," Max % Cover")),varn="ppt", title="RF")
AK <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="ppt", title="TS")

# make legend
leg <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="ppt", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

jpeg(paste(fpath, "change_by_mag_ppt_ppt_grad.jpeg", sep=""),
     width = 420, height = 320, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(AK,DRS,randfor,DGVM, ncol=2), ncol=1,
             heights = c(0.15, 2.5,0))
dev.off()

################################################################################
# Plot response to temp manipulations along elevation gradient
plot_raw_change <- function(modeln, ylab,varn,title) {
  d <- dplyr::filter(m3,var==varn & model==modeln)
  plot <- ggplot(data=d, aes(x=MAT,y=change, color=as.factor(mag))) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(method = "lm",aes(fill=as.factor(mag))) +
    scale_color_manual(values = wes_palette("GrandBudapest"),
                       #breaks=c(".2","2","4"),
                       labels=c(expression("+ .2"*~degree*"C"),expression("+ 2"*~degree*"C"),expression("+ 4"*~degree*"C"))) +
    scale_fill_manual(values = wes_palette("GrandBudapest"),
                      #breaks=c(".2","2","4"),
                      labels=c(expression("+ .2"*~degree*"C"),expression("+ 2"*~degree*"C"),expression("+ 4"*~degree*"C"))) +
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
DRS <- plot_raw_change(modeln="DRS", ylab=expression(paste(Delta," % Yrs with Regen")), varn="temp", title="GISSM")
DGVM <- plot_raw_change(modeln="DGVM-full-400ppm", ylab=expression(paste(Delta," % Cover")),varn="temp", title="DGVM")
randfor <- plot_raw_change(modeln="randfor", ylab=expression(paste(Delta," Max % Cover")),varn="temp", title="RF")
AK <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="temp", title="TS")

# make legend
leg <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="temp", title="TS")
leg2 <- leg + theme(legend.position="top")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(leg2)

jpeg(paste(fpath, "change_by_mag_temp.jpeg", sep=""),
     width = 420, height = 320, units = 'mm', res = 300)
grid.arrange(legend, arrangeGrob(AK,DRS,randfor,DGVM, ncol=2), ncol=1,
             heights = c(0.15, 2.5,0))
dev.off()

################################################################################
# Show response to temp, but cut extreme hot
################################################################################
m4 <- filter(m3, mag==4, var=="temp")
plot_raw_change <- function(modeln, ylab,magn,varn,title) {
  d <- dplyr::filter(m4,var==varn & model==modeln)
  plot <- ggplot(data=d, aes(x=MAT,y=change)) +
    geom_point(size=1) +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(method = "lm") +
    ylab("Change in Response") +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          legend.text.align = 0,
          plot.margin=unit(c(.1,.1,.1,.1), "cm"),
          axis.title.y = element_text(size = rel(1.3))) +
    xlab("MAT") +
    scale_x_continuous(limits=c(-1.9,17)) +
    #scale_y_continuous(limits=c(-100,100)) +
    ylab(ylab) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=8)
  return(plot)
}
DRS <- plot_raw_change(modeln="DRS", ylab=expression(paste(Delta," % Yrs with Regen")), varn="temp", title="GISSM")
DGVM <- plot_raw_change(modeln="DGVM-full-400ppm", ylab=expression(paste(Delta," % Cover")),varn="temp", title="DGVM")
randfor <- plot_raw_change(modeln="randfor", ylab=expression(paste(Delta," Max % Cover")),varn="temp", title="RF")
AK <- plot_raw_change(modeln="AK", ylab=expression(paste(Delta," % Cover")),varn="temp", title="TS")
DGVM

# Save Plot
jpeg(paste(fpath, "change_temp_cuthot.jpeg", sep=""),
     width = 420, height = 200, units = 'mm', res = 300)
grid.arrange(randfor,AK,DGVM,DRS, ncol=4)
dev.off()


