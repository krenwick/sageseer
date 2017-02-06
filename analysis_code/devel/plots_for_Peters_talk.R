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
bad <- c(495,496,497,498,580,581,583,632,633,634,668,62)

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

# Data for Dominique
latlon <- merged %>% group_by(site) %>% summarise(Lon=mean(longitude.x), Lat=mean(latitude.x))
d3 <- merge(d2, latlon, by="site", all=F) %>%
  dplyr::select(site, Lon, Lat, consensus, conf_cat) %>%
  rename(confidence=conf_cat)
write.csv(d3, paste(dpath, "LonLat_confidence_cats.csv", sep=""), row.names=F)
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
DGVM <- plot_raw_change(merged2,modeln="DGVM", ylab=expression(paste(Delta," % Cover")), title="DVM")
CC <- plot_raw_change(merged2,modeln="RandFor", ylab=expression(paste(Delta," Max % Cover")), title="SC")
AK <- plot_raw_change(merged2,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TC")
DRS <- plot_raw_change(merged2,modeln="GISSM_v1.6.3", ylab=expression(paste(Delta," % Regen")), title="SS")

# try scaling Andy's change by the baseline (so % change)
AKdat <- merged %>% mutate(change=change/baseline)
AK2 <- AK <- plot_raw_change(AKdat,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
# try removing non-long-term plots
AKdat2 <- merged %>% filter(LT_data==1)
AK3 <- plot_raw_change(AKdat2,modeln="AK", ylab=expression(paste(Delta," % Cover")), title="TS")
AK2
AK3

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

# Save Plots, sized for poster
png(paste(fpath, "change_temp_GCM_rcp85_poster.png", sep=""),
    width = 320, height = 160, units = 'mm', res = 450)
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
  scale_fill_gradient2(low="red", high="blue",
                       name="Model\nAgreement\n",
                       breaks=c(-20,0,20),labels=c(-20,0,20),
                       limits=c(-20,20)) +
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

################################################################################
# Plot R & R figure
################################################################################
dat_nrcs_RR <- read.csv(file.path(dpath, "focal_pts_project_extract_all_SMTRegime.csv"), header = TRUE)
rr <- merge(d2, dat_nrcs_RR, by="site", all=F)

dir_prj <- "~/Box Sync/sageseer"
dir_dat <- file.path(dir_prj, "ModelComparison")

mdat <- read.csv(file.path(dir_dat, "merged_data_GCM.csv"), header = TRUE)
#dat_agree <- read.csv(file.path(dir_dat, " agreement_output_env.csv"), header = TRUE)
dat_nrcs_sw <- read.csv(file.path(dir_dat, "merged_data_NRCS_soilMT_regimes.csv"), header = TRUE)
dat_nrcs_RR <- read.csv(file.path(dir_dat, "focal_pts_project_extract_all_SMTRegime.csv"), header = TRUE)
head(dat_nrcs_sw)


#--- PREPARE DATA
# Calculate agreeement
dat2_agree <- mdat %>%
  mutate(change=(predicted-baseline)) %>%
  mutate(cat=ifelse(change>0, "increase", "decrease")) %>%
  # uncomment next line to add "nochange" back in
  #mutate(cat=ifelse(change==0, "nochange", cat)) %>%
  dplyr::select(site:GCM,change,cat) %>%
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
  filter(n==max(n)) %>%
  filter(site %in% bad==FALSE)

table(dat2_agree$consensus)

# Re-classify dummy NRCS variables based on SOILWAT
dat_nrcs_sw$NRCS_SoilTemperatureRegime <- with(dat_nrcs_sw,
        ifelse(as.logical(as.numeric(levels(NRCS_SoilTemperatureRegime_Hyperthermic))[NRCS_SoilTemperatureRegime_Hyperthermic]), "Hyperthermic",
        ifelse(as.logical(as.numeric(levels(NRCS_SoilTemperatureRegime_Thermic))[NRCS_SoilTemperatureRegime_Thermic]), "Thermic",
        ifelse(as.logical(as.numeric(levels(NRCS_SoilTemperatureRegime_Mesic))[NRCS_SoilTemperatureRegime_Mesic]), "Mesic",
        ifelse(as.logical(as.numeric(levels(NRCS_SoilTemperatureRegime_Frigid))[NRCS_SoilTemperatureRegime_Frigid]), "Frigid",
        ifelse(as.logical(as.numeric(levels(NRCS_SoilTemperatureRegime_Cryic))[NRCS_SoilTemperatureRegime_Cryic]), "Cryic",
        ifelse(as.logical(as.numeric(levels(NRCS_SoilTemperatureRegime_Gelic))[NRCS_SoilTemperatureRegime_Gelic]), "Gelic",
        NA)))))))

dat_nrcs_sw$NRCS_SoilMoistureRegime <- with(dat_nrcs_sw,
        ifelse(as.logical(as.numeric(levels(NRCS_SoilMoistureRegime_Anhydrous))[NRCS_SoilMoistureRegime_Anhydrous]), "Anhydrous",
                                                   ifelse(as.logical(as.numeric(levels(NRCS_SoilMoistureRegime_Aridic))[NRCS_SoilMoistureRegime_Aridic]), "Aridic",
                                                          ifelse(as.logical(as.numeric(levels(NRCS_SoilMoistureRegime_Udic))[NRCS_SoilMoistureRegime_Udic]), "Udic",
                                                                 ifelse(as.logical(as.numeric(levels(NRCS_SoilMoistureRegime_Ustic))[NRCS_SoilMoistureRegime_Ustic]), "Ustic",
                                                                        ifelse(as.logical(as.numeric(levels(NRCS_SoilMoistureRegime_Xeric))[NRCS_SoilMoistureRegime_Xeric]), "Xeric",
                                                                               NA))))))



dat_nrcs_sw$NRCS_RR <- with(dat_nrcs_sw,
                      ifelse(as.logical(NRCS_Maestas2016_SagebrushRR_Low), "Low",
                      ifelse(as.logical(NRCS_Maestas2016_SagebrushRR_Moderate), "Moderate",
                      ifelse(as.logical(NRCS_Maestas2016_SagebrushRR_High), "High",
                                                 0))))

table(dat_nrcs_sw$NRCS_RR)
# 0     High      Low Moderate 
# 49        6      250      421 

# What's going on?
nrow(dat_nrcs_sw) # 726
nrow(dat_nrcs_RR) #656
table(dat_nrcs_RR$RR_class_name)
# 0     High       Low     Moderate 
# 14      176      226      240 

# Make big table for 726 sites
dat_agree_NRCS <- dat2_agree[do.call(order, list(dat2_agree$site)),
                             c("site", "n", "consensus")]

temp <- dat_nrcs_sw[order(dat_nrcs_sw$site_id), c("site_id", "NRCS_SoilTemperatureRegime", "NRCS_SoilMoistureRegime", "NRCS_RR")]
dat_agree_NRCS <- cbind(dat_agree_NRCS, temp[match(dat_agree_NRCS$site, temp$site_id), ])

temp <- dat_nrcs_RR[order(dat_nrcs_RR$site), c("site", "temp_regime", "moisture_regime", "RR_class_name")]
dat_agree_NRCS <- cbind(dat_agree_NRCS, temp[match(dat_agree_NRCS$site, temp$site, nomatch = NA), ])

#--- Cross-table: resistance & resilience RR
table(dat_agree_NRCS$NRCS_RR, dat_agree_NRCS$RR_class_name)

################################################################################
# KMR note: Tables below all the same data, just standardized differently
# RR (SWSF): row-standardized
sw_RR_freq <- as.data.frame(with(dat_agree_NRCS, table(NRCS_RR)))
table(dat_agree_NRCS$NRCS_RR) # equivalent to above line
table(dat_agree_NRCS$RR_class_name) # same classes, completed different outcome...
round(with(dat_agree_NRCS, table(sw_RR = NRCS_RR, consensus) / sw_RR_freq$Freq), 2)

# Re-do with RR classes from Caroline:
sw_RR_freq <- as.data.frame(with(dat_agree_NRCS, table(RR_class_name)))
round(with(dat_agree_NRCS, table(Maestas_RR = RR_class_name, consensus) / sw_RR_freq$Freq), 2)

# Here, each row sums to 1:
# read as: of sites with low RR, 64% show increased performance with climate change
# comparing numbers in a column isn't valid.
# consensus
# sw_RR      decrease increase unsure
# High         0.00     1.00   0.00
# Low          0.24     0.64   0.12
# Moderate     0.01     0.97   0.02

# But what is the sample size?
sw_RR_freq

#####################################################

myCols = c("red3","lightgray","dodgerblue3")

High   <- c(      0.00,  0,   1.00)
Low   <-  c(    0.24,  0.12,   0.64)
Moderate  <- c(    0.01,  0.02,   0.97)

Nhigh <-6; Nlow <- 250; Nmod <- 420

png("R&R-barplot.png",height=5,width=5,res=400,units="in")
par(tcl=-0.1,mgp=c(2,0.5,0),mar=c(3,4,1,7),xpd=TRUE,cex.lab=1.2)
xvals=barplot(cbind(Low,Moderate,High),col=myCols,xlab="Resilience Class",
              ylab="Proportion of sites x scenarios")
text(xvals[1],0.5,paste0("n=",Nlow),col="white",cex=1.2)
text(xvals[2],0.5,paste0("n=",Nmod),col="white",cex=1.2)
text(xvals[3],0.5,paste0("n=",Nhigh),col="white",cex=1.2)
legend("right",inset=c(-0.45,0),legend=c("Decrease","Uncertain","Increase"),
       fill=myCols,title="Response")
dev.off()