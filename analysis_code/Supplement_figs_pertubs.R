#############################################################################
# Sensitivity analysis- temp and precip
# Makes: Fig. S1 (temp sensitivity), Fig. S2 (precip sensitivity),
# Fig. S3 (barplot of change mag and direction)
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10)) # sized for ppt
library(gridExtra)
library(splines)
library(grid)

#***USER MUST CHANGE***
# paths to data and figure folders: Must use full path for readOGR to work
dpath <- "/Users/poulterlab1/version-control/sageseer/data/"
fpath <- "/Users/poulterlab1/version-control/sageseer/figures/"

#----------------------------------------
# Journal Specifications for figure size
# Global Change Bio:
col1 <- 80 # 1 column width = 80 mm
col2 <- 169 # 2 column width = 169 mm
#--------------------------------------

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_perturb.csv", sep="")) %>%
  mutate(change=(predicted-baseline))
unit <- read.csv(paste(dpath, "focal_sites_by_zone.csv", sep="")) %>%
  dplyr::select(site,elev:NA_L1NAME)

################################################################################
# Calculate change and direction:
m4 <- merged %>% 
  mutate(cat=ifelse(change>0, "increase", "decrease")) #%>%
# uncomment next line to add "nochange" back in
#mutate(cat=ifelse(change==0, "nochange", cat))

d2 <- m4 %>%
  dplyr::select(site, model:mag,change:cat) %>%
  group_by(site,var,mag) %>%
  summarise(n=n(),n.increase=sum(change>0), n.decrease=sum(change<=0)) %>%
  mutate(conf2=n.increase-n.decrease) %>%
  mutate(conf_cat=pmax(n.increase,n.decrease)) %>%
  mutate(rel_conf_cat=conf_cat/n) %>%
  mutate(consensus=ifelse(conf_cat==n.increase,"increase","decrease")) %>%
  mutate(consensus=ifelse(n.increase==n.decrease&n.increase==conf_cat,"unsure",consensus)) %>%
  filter(n==max(n))

table(d2$consensus,d2$var,d2$mag)

################################################################################
# a. Show that we have more certainty with temp response than precip
################################################################################
a1 <- d2 %>% filter(mag>1)
a2 <- d2 %>% filter(consensus=="unsure")

ggplot(data=a2, aes(x=var, fill=as.factor(mag))) +
  geom_bar(position="dodge") +
  xlab("Variable Manipulated") +
  ylab("# sites w/ no consensus")
# More consensus on impact of temperature increase
# With temperature, models agree more with more extreme change
# With precip, models agree less with more extreme change

################################################################################
# Plot the absolute change, in original scale, MAT gradient, 4-plot grid
################################################################################
tempCol <- c('yellow1','orange2','orangered3')
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln, var=="temp")
  plot <- ggplot(data=d, aes(x=bio1/10,y=change)) +
    geom_point(aes(color=as.factor(mag)), size=1) +
    scale_color_manual(values=tempCol, name="Temperature Change",
                       labels=c(expression("+ 0.2"*~degree*"C"),
                                expression("+ 2"*~degree*"C"),
                                expression("+ 4"*~degree*"C"))) +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=as.factor(mag), color=as.factor(mag)),method = "lm") +
    scale_fill_manual(values=tempCol, name="Temperature Change",
                      labels=c(expression("+ 0.2"*~degree*"C"),
                               expression("+ 2"*~degree*"C"),
                               expression("+ 4"*~degree*"C"))) +
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
DGVM <- plot_raw_change(merged,modeln="DGVM-full-400ppm", ylab=expression(paste(Delta," % Cover")), title="DVM")
DGVM
CC <- plot_raw_change(merged,modeln="randfor", ylab=expression(paste(Delta," Max % Cover")), title="SC")
AK <- plot_raw_change(merged,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TC")
DRS <- plot_raw_change(merged,modeln="DRS", ylab=expression(paste(Delta," % Regen")), title="SS")
CC
AK
DRS
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
temps <- grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
             heights = c(0.2, 2.5,0))
ggsave(paste(fpath, "temp_sensitivity.eps", sep=""), plot=temps, 
       width = col2, height = col2, units = 'mm')


################################################################################
# Plot precip panels
################################################################################
pptCol <- c('darkgoldenrod4','dodgerblue','dodgerblue4')
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln, var=="temp")
  plot <- ggplot(data=d, aes(x=bio12,y=change)) +
    geom_point(aes(color=as.factor(mag)), size=1) +
    scale_color_manual(values=pptCol, name="Precipitation Change",
                       labels=c("-10%","+10%","+20%")) +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=as.factor(mag), color=as.factor(mag)),method = "lm") +
    scale_fill_manual(values=pptCol, name="Precipitation Change",
                      labels=c("-10%","+10%","+20%"))  +
    ylab("Change in Response") +
    theme(legend.position="none", legend.title=element_blank(),
          panel.background=element_blank(),plot.background=element_blank(),
          legend.text.align = 0,
          plot.margin=unit(c(.1,.1,.1,.1), "cm"),
          axis.title.y = element_text(size = rel(1.3))) +
    xlab("Mean Annual Precipitation (mm)") +
    ylab(ylab) +
    annotate("text", x=Inf, y = Inf, label = title, vjust=1.3, hjust=1.3, size=8)
  return(plot)
}
DGVM <- plot_raw_change(merged,modeln="DGVM-full-400ppm", ylab=expression(paste(Delta," % Cover")), title="DVM")
DGVM
CC <- plot_raw_change(merged,modeln="randfor", ylab=expression(paste(Delta," Max % Cover")), title="SC")
AK <- plot_raw_change(merged,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TC")
DRS <- plot_raw_change(merged,modeln="DRS", ylab=expression(paste(Delta," % Regen")), title="SS")
CC
AK
DRS
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
png(paste(fpath, "precip_sensitivity.png", sep=""),
    width = 420, height = 320, units = 'mm', res = 450)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
             heights = c(0.2, 2.5,0))

dev.off()


################################################################################
# Plot change and direction bar plot like with GCMs
################################################################################
# Make plots in black and white-------------------------------------------------
# Back to the bar chart
m5 <- m4 %>% mutate(abschange=abs(change)) %>%
  mutate(direction=ifelse(change>0,"increase","decrease")) %>%
  group_by(model,mag,var, direction) %>%
  summarise(meanchange=mean(change), lower=meanchange-sd(change)/sqrt(length(change)),
            upper=meanchange+sd(change)/sqrt(length(change))) %>%
  mutate(var2=ifelse(var=="ppt", "Precipitation","Temperature"))
head(m5)
m5$mag <- as.factor(m5$mag)
levs = c("+.2C","-10%","+10%","+20%","+2C","+4C")
levels(m5$mag) <- levs

# Set up separate data frame so can annotate individual facet
ann_text <- data.frame(mag = .9,meanchange = Inf,
                       var2 = factor("Precipitation",
                                     levels = c("Precipitation","Temperature")))

vj <- 1.5 # vertical adjustment for panel label, pos moves down
hj <- .1 # horizotal placement of panel label, neg moves right
greys <- c('#f7f7f7','#d9d9d9','#bdbdbd','#969696','#636363','#252525')
AK <- 
  ggplot(data=m5[m5$model=="AK",], aes(x=mag, y=meanchange)) +
  geom_bar(stat="identity", width=1,position=position_dodge(width=NULL), fill="white",
           color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  xlab("Variable") +
  ylab(expression(paste(Delta," % Regen"))) +
  theme(axis.title.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none",
        axis.text.x = element_blank()) +
  facet_wrap(~var2, strip.position = "bottom", scales = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill="white"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size=.1),
        strip.text.x = element_blank()) +
  geom_text(data = ann_text,label = "(b) TC", hjust=hj, vjust=vj, size=3 )

CC <- 
  ggplot(data=m5[m5$model=="randfor",], aes(x=mag, y=meanchange)) +
  geom_bar(stat="identity", width=1,position=position_dodge(width=NULL), fill="white",
           color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  xlab("Variable") +
  ylab(expression(paste(Delta," % Regen"))) +
  theme(axis.title.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none",
        axis.text.x = element_blank()) +
  facet_wrap(~var2, strip.position = "bottom", scales = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill="white"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size=.1),
        strip.text.x = element_blank()) +
  geom_text(data = ann_text,label = "(a) SC", hjust=hj, vjust=vj, size=3 )

KR <- 
  ggplot(data=m5[m5$model=="DGVM-full-400ppm",], aes(x=mag, y=meanchange)) +
  geom_bar(stat="identity", width=1,position=position_dodge(width=NULL), fill="white",
           color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  xlab("Variable") +
  ylab(expression(paste(Delta," % Regen"))) +
  theme(axis.title.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none") +
  facet_wrap(~var2, strip.position = "bottom", scales = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill="white"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size=.1)) +
  geom_text(data = ann_text,label = "(c) DGVM", hjust=hj, vjust=vj, size=3 )





DRS <- 
  ggplot(data=m5[m5$model=="DRS",], aes(x=mag, y=meanchange)) +
  geom_bar(stat="identity", width=1,position=position_dodge(width=NULL), fill="white",
           color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  xlab("Variable") +
  ylab(expression(paste(Delta," % Regen"))) +
  theme(axis.title.x=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position="none") +
  facet_wrap(~var2, strip.position = "bottom", scales = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill="white"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size=.1)) +
   geom_text(data = ann_text,label = "(d) SS", hjust=hj, vjust=vj, size=3 )
DRS


# Save Plot
# first, fix annoying issue with axes not lining up
gp1<- ggplot_gtable(ggplot_build(CC))
gp2<- ggplot_gtable(ggplot_build(AK))
gp3<- ggplot_gtable(ggplot_build(KR))
gp4<- ggplot_gtable(ggplot_build(DRS))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp3$widths[2:3], gp4$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
gp3$widths[2:3] <- maxWidth
gp4$widths[2:3] <- maxWidth

# render and save plot
pmagchange <-grid.arrange(gp1,gp2,gp3,gp4, ncol=2,
                        heights = unit(c(74,80), "mm"))
ggsave(paste(fpath, "perturb_magchange_bw.eps", sep=""), plot=pmagchange, 
       width = col2, height = col2, units = 'mm')

