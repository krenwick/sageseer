#############################################################################
# Sensitivity analysis- temp and precip
# 
#############################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)

#***USER MUST CHANGE***
# paths to data and figure folders: Must use full path for readOGR to work
dpath <- "/Users/poulterlab1/version-control/sageseer/data/"
fpath <- "/Users/poulterlab1/version-control/sageseer/figures/"


# Color Palette for GCMs (color-blind friendly)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

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
# b. Show slope of response to temperature change
################################################################################
b1 <- m4 %>% filter(var=="temp") %>% 
  group_by(site,mag) %>%
  mutate(bio1=scale(bio1)) %>%
  mutate(change=abs(change))

# Plot temperature response (not useful)
ggplot(data=b1, aes(x=mag, y=change, color=cat, group=site)) +
  geom_point() +
  geom_line()

# linear models for temp
summary(lm(data=b1[b1$model=="AK",], change~mag*bio1)) # .54
summary(lm(data=b1[b1$model=="randfor",], change~mag*bio1)) # .25
summary(lm(data=b1[b1$model=="DGVM-full-400ppm",], change~mag*bio1)) # 1.8
summary(lm(data=b1[b1$model=="DRS",], change~mag*bio1)) # 7.4

# For precip
b2 <- m4 %>% filter(var=="ppt") %>% mutate(bio1=scale(bio1))
summary(lm(data=b2[b2$model=="AK",], change~mag*bio1)) # 2.9
summary(lm(data=b2[b2$model=="randfor",], change~mag*bio1)) # 9.3
summary(lm(data=b2[b2$model=="DGVM-full-400ppm",], change~mag*bio1)) # 7.3
summary(lm(data=b2[b2$model=="DRS",], change~mag*bio1)) # 11.5

# Hard to compare because precipitation and temperature are different units
# Can't look at slopes- comparing degrees to percent
# also, I should center bio1 if I'm going to do this so zero is mean



################################################################################
# Plot the absolute change, in original scale, MAT gradient, 4-plot grid
################################################################################
tempCol <- c('#fee6ce','#fdae6b','#e6550d')
tempCol <- c('yellow1','orange2','orangered3')
plot_raw_change <- function(data,  modeln, ylab,title) {
  d <- data %>% dplyr::filter(model==modeln, var=="temp")
  plot <- ggplot(data=d, aes(x=bio1/10,y=change)) +
    geom_point(aes(color=as.factor(mag)), size=1) +
    scale_color_manual(values=tempCol, name="GCM") +
    geom_hline(yintercept=0, linetype="dashed") +
    stat_smooth(aes(fill=as.factor(mag), color=as.factor(mag)),method = "lm") +
    #stat_smooth(aes(fill=as.factor(mag), color=as.factor(mag))) +
    scale_fill_manual(values=tempCol, name="Temperature Change") +
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
png(paste(fpath, "temp_sensitivity.png", sep=""),
    width = 420, height = 320, units = 'mm', res = 450)
grid.arrange(legend, arrangeGrob(CC,AK,DGVM,DRS, ncol=2), ncol=1,
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
d4 <- filter(d3, mag==1.1)
gg3 <- 
  ggplot(data = d4, aes( x = Comp.1_rev, y = Comp.2, fill = conf2 )) + 
  geom_point( size = 2.5,colour="black",pch=21) + 
  scale_fill_gradient2(low="red", high="blue",
                       name="Model\nAgreement\n",
                       breaks=c(-4,0,4),labels=c(-4,0,4),
                       limits=c(-4,4)) +
  xlab( 'PC1 : (Site Temperature)') +
  ylab( 'PC2 : (Precipitation Seasonality)') +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

# Save Plot
png(paste(fpath, "PCA_model_agreement_1.1ppt.png", sep=""),
    width = 320, height = 220, units = 'mm', res = 450)
gg3
dev.off()

################################################################################
# Plot the table of consensus categories
################################################################################
png(paste(fpath, "consensus_table_histogram_1.2ppt.png", sep=""),
    width = 220, height = 120, units = 'mm', res = 450)
mag4 <- filter(d2, mag==1.2)
ggplot(data=mag4, aes(x=consensus, fill=consensus)) +
  #geom_bar(color="black") +
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black") +
  scale_fill_manual(labels=c("Decrease","Increase", "Unsure"),
                    values=c("#d7191c","#2c7bb6","grey"),name="Model\nConsensus") +
  xlab("Model Consensus") +
  ylab("Proportion of Sites") +
  theme(legend.position="none")
dev.off()

################################################################################
# Plot baseline vs. predicted to check RF data (looks good!)
################################################################################
# Try a scatteplot showing baseline vs. mean predicted for each model
m5 <- filter(m4, mag==4)
ggplot(data=m4, aes(x=baseline, y=predicted,color=as.factor(mag))) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope=1,intercept=0, linetype="dashed") +
  facet_wrap(~model, scale="free")

################################################################################
# Plot change and direction bar plot like with GCMs
################################################################################
# Back to the bar chart
m5 <- m4 %>% mutate(abschange=abs(change)) %>%
  mutate(direction=ifelse(change>0,"increase","decrease")) %>%
  group_by(model,mag,var, direction) %>%
  summarise(meanchange=mean(change), lower=meanchange-sd(change)/sqrt(length(change)),
            upper=meanchange+sd(change)/sqrt(length(change))) 
head(m5)
ggplot(data=m5, aes(x=as.factor(var), y=meanchange, group=as.factor(mag))) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), fill="white",
           color="black") +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  xlab("Variable") +
  ylab("Mean Change in Model Response") +
  facet_wrap(~model,scale="free")

# Try color by model instead of RCP (first option better for b/w)
ggplot(data=m5, aes(x=scenario, y=meanchange, fill=GCM)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=cbPalette, name="GCM") +
  xlab("GCM") +
  ylab("Mean Change in Model Response") +
  facet_wrap(~model,scale="free_y")
#facet_wrap(~model) +
opts(strip.background=theme_blank())
