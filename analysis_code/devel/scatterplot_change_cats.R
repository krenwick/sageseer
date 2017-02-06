#############################################################################
# Make scatterplot of change categories with fit line
# Also make boxplots of variables by a) model, b) change class
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Figures/Scatterplot_changecat_bioclim/"

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
bioclim <- read.csv(paste(dpath, "focal_sites_for_comparison.csv", sep= ""))

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
  mutate(rel_conf_cat=ifelse(consensus=="decrease", rel_conf_cat*-1, rel_conf_cat)) %>%
  filter(n==max(n))
dim(d2)
table(d2$conf2)
table(d2$conf_cat)
table(d2$rel_conf_cat)
table(d2$rel_conf_cat, d2$conf2)
table(d2$consensus)

################################################################################
# Plot scatterplot of consensus categories
du <- merge(d2, bioclim, by="site", all=F) %>%
  filter(conf2!=0) # filter out unsures

png(paste(fpath, "changecat_MAT.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=(bio1/10), y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

# Experiment with other bioclim variables
png(paste(fpath, "changecat_bio2.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio2, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio3.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio3, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio4.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio4, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio5.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio5, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio6.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio6, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio7.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio7, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio8.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio8, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio9.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio9, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio10.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio10, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio11.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio11, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio12.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio12, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio13.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio13, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio14.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio14, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio15.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio15, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio16.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio16, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio17.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio17, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio18.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio18, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

png(paste(fpath, "changecat_bio19.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=bio19, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

du2 <- merge(du,unit, by="site", all=F)
png(paste(fpath, "changecat_elev.png", sep=""),
    width = 220, height = 220, units = 'mm', res = 300)
ggplot(data=du, aes(x=elev, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")
dev.off()

################################################################################
# Deserve a closer look:
# bio5, Max Temperature of Warmest Month
# bio6, Min temp coldest month
# bio 10, bio11, Mean temp. warmest vs. coldest quarter
# bio12, annual precip
# bio13, 14, 16, 17, 18- has cutoff (precip certain quarters)
# oddly, bio15 doesn't work- precip seasonality (how diff are seasons)

# Conclusion: sites that are very wet or very cool will be okay
# Sites that are dry or hot: message is more mixed

# Which of these variables is best able to distinguish performance change?
du <- merge(d2, bioclim, by="site", all=F) %>%
  merge(unit, by="site", all=F)
range <- du %>% 
  dplyr::select(consensus,bio1:elev) %>%
  group_by(consensus) %>%
  summarise_each(funs(min,max))
# 

ggplot(data=du, aes(x=elev, y=(rel_conf_cat))) +
  geom_point(aes(color=rel_conf_cat)) +
  scale_color_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_vline(xintercept=range$elev_max) +
  geom_vline(xintercept=range$elev_min) +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")

######## Try with Mean Temp Coldest Quarter
du0 <- mutate(du, rel_conf_cat=ifelse(consensus=="unsure", 0, rel_conf_cat))
ggplot(data=du0, aes(x=bio1/10, y=(conf2))) +
  geom_point(aes(fill=rel_conf_cat), pch=21) +
  scale_fill_gradient2() +
  #geom_smooth(method="lm") +
  geom_smooth() +
  geom_vline(xintercept=range$bio1_max/10) +
  geom_vline(xintercept=range$bio1_min/10) +
  geom_hline(yintercept=0, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")

 # Undo the negatives?
du3 <- mutate(du, rel_conf_cat=ifelse(rel_conf_cat<0, rel_conf_cat*-1, rel_conf_cat))
ggplot(data=du3, aes(x=elev, y=(rel_conf_cat), color=consensus)) +
  geom_smooth(method="lm") +
  geom_point(aes(alpha=rel_conf_cat)) +
  #scale_color_gradient2() +
  #geom_smooth(method="lm") +
  #geom_smooth() +
  geom_vline(xintercept=range$elev_max) +
  geom_vline(xintercept=range$elev_min) +
  geom_hline(yintercept=.5, linetype="dashed") +
  #xlab("Mean Annual Temperature") +
  ylab("Confidence (% Models)") +
  theme(legend.position="none")

### Linear models- quantitatively, which single variable explains most?
attach(du)
summary(lm(rel_conf_cat~bio1)) #.46 MAT, .40
summary(lm(rel_conf_cat~bio5)) # .28 temp, .22
summary(lm(rel_conf_cat~bio6)) # .38 temp, .33
summary(lm(rel_conf_cat~bio10)) #.36 temp, .29
summary(lm(rel_conf_cat~bio11)) #.47 temp, .41 (Mean temp of coldest quarter- so cold is limiting!)
summary(lm(rel_conf_cat~elev)) # .20 elev, .15
summary(lm(rel_conf_cat~bio14)) #.08, .06 best of precip (precip driest month)

m1 <- stepAIC(lm(rel_conf_cat~bio14+bio11))
summary(m1)


# Try logistic
du2 <- du %>%
  filter(consensus!="unsure") %>%
  mutate(consensus=ifelse(consensus=="decrease",0,1))

m1 <- (glm(data=du2,consensus~bio11, family="binomial"))
summary(m1)

######## Try polynomial fit- better tahn from GGplot?
# Can we find a polynome that fit this function ?
model=lm(conf2 ~ bio11 + I(bio11^2) + I(bio11^3))
summary(model)

# I can get the features of this model :
summary(model)
model$coefficients
summary(model)$adj.r.squared

#For each value of x, I can get the value of y estimated by the model, and the confidence interval around this value.
myPredict <- predict( model , interval="predict" )

#Finally, I can add it to the plot using the line and the polygon function with transparency.
ix <- sort(bio11,index.return=T)$ix
plot(bio11, conf2)
lines(bio11[ix], myPredict[ix , 1], col=2, lwd=2 )  
polygon(c(rev(bio11[ix]), bio11[ix]), c(rev(myPredict[ ix,3]), myPredict[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)

# Make boxplot of change
ggplot(data=du)


