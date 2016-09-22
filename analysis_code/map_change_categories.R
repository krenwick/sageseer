#############################################################################
# Make map showing change class for each point
# Code created Aug 2016
#############################################################################
rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)
library(colorspace)
library(rgdal)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Documents/sageseer/Figures/"
opath <- "/Users/poulterlab1/Documents/sageseer/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data-co2.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() 
m3 %>% group_by(model) %>% summarise(mbase=mean(baseline),mpred=mean(predicted))

################################################################################
# Try making categories based on the ratio
m4 <- m3 %>% #na.omit() %>%
  #filter(baseline>1) %>%
  mutate(perc_change=(predicted/baseline)) %>%
  mutate(category=ifelse(perc_change>2, "large_increase", "nochange")) %>%
  mutate(category=ifelse(perc_change>=1.2&perc_change<=2, "small_increase", category)) %>%
  mutate(category=ifelse(perc_change<=.8&perc_change>=.5, "small_decrease", category)) %>%
  mutate(category=ifelse(perc_change<(.5), "large_decrease", category)) %>%
  mutate(cat2=ifelse(perc_change>1, "increase", "decrease")) %>%
  mutate(cat2=ifelse(perc_change<1.2&perc_change>.8, "nochange", cat2)) %>%
  dplyr::select(latitude.x,longitude.x,site, model:mag,category:cat2)

head(m4)
# convert several variables to ordered factors to facilitate plotting
m4$category <- factor(m4$category, levels=c("large_decrease","small_decrease","nochange","small_increase","large_increase"))
m4$mag <- factor(m4$mag, levels=c(.9,1.1,1.2,.2,2,4))
levels(m4$mag) <- c("-10%","+10%","+20%","+.2C","+2C","+4C")

#pal <- choose_palette()
#pal(5) # get color codes so don't have to choose manually to re-create plot
cols <- c("#4A6FE3", "#9DA8E2" ,"#E2E2E2", "#E495A5", "#D33F6A")

# get shapefiles
setwd("~/Documents/GIS_baselayers/")
states <- readOGR(".", "states")
wus <- states[states$STATE_ABBR=="WA"|states$STATE_ABBR=="OR"|states$STATE_ABBR=="CA"
              |states$STATE_ABBR=="ID"|states$STATE_ABBR=="NV"|states$STATE_ABBR=="MT"
              |states$STATE_ABBR=="UT"|states$STATE_ABBR=="NM"|states$STATE_ABBR=="WY"
              |states$STATE_ABBR=="CO"|states$STATE_ABBR=="AZ",]

############ MAKE MAP
#jpeg(paste(fpath, "temp_precp.jpeg", sep=""),
     #width = 254, height = 150, units = 'mm', res = 300)
m5 <- filter(m4, var=="temp"&mag=="+4C")
ggplot(data=m5, aes(y=latitude.x, x=longitude.x,color=category)) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point() +
  scale_color_manual(values = rev(cols),name="Change Class",
                     limits=c("large_decrease","small_decrease","nochange","small_increase","large_increase"),
                   labels=c("Large Decrease","Small Decrease","No Change","Small Increase","Large Increase")) +
  facet_wrap(~model)
#dev.off()
