################################################################################
# Map sagebrush cover + presence points + focal points
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=20)) # sized for ppt
library(rgdal)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Figures/"

# List of "bad" sites
#bad <- c(495, 579, 494, 582, 580, 634, 633, 632, 668, 496, 497)
bad <- c(495,496,497,498,580,581,583,632,633,634,668)

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data_GCM.csv", sep="")) %>%
  mutate(change=(predicted-baseline))
merged2 <- filter(merged, site %in% bad==FALSE)
m <- merged2 %>% group_by(site) %>% mutate(n=n()) %>% 
  dplyr::select(site:latitude.x, n) %>%
  filter(n==60) %>%
  summarise_each(funs(mean))
dim(m)
head(m)

allpts <- read.csv("artr_all_19bioclim_extractNEW.csv")
car <- read.csv("../AZ_pts_no_source_data.csv")

# get shapefiles
states <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers", "states")
wus <- states[states$STATE_ABBR=="WA"|states$STATE_ABBR=="OR"|states$STATE_ABBR=="CA"
              |states$STATE_ABBR=="ID"|states$STATE_ABBR=="NV"|states$STATE_ABBR=="MT"
              |states$STATE_ABBR=="UT"|states$STATE_ABBR=="NM"|states$STATE_ABBR=="WY"
              |states$STATE_ABBR=="CO"|states$STATE_ABBR=="AZ",]

# Pull in cover map from the climate console
cmap <- raster("/Users/poulterlab1/Documents/GIS_baselayers/Sagebrush_MW5k_1km_latlon.tif")
points <- rasterToPoints(cmap)

# Convert to points
pts2 <- data.frame(points)
pts3 <- pts2[pts2$Sagebrush_MW5k_1km_latlon>0,]

# read background points from Daniel
D <- read.csv("~/Documents/SagebrushPresence/DATA/ATpres_comb.csv")
D2 <- filter(D, SOURCE=="VegBank")

################################################################################
# Plot chosen points over background cover data
ggplot(data=m, aes(y=latitude.x, x=longitude.x)) +
  geom_raster(data=pts3, aes(x,y), fill="grey") +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point( size = 1,colour="blue") + 
  ylab("Longitude") +
  xlab("Latitude") +
  coord_fixed(1.3) +
  theme(legend.justification=c(0,0), legend.position=c(0,0))
################################################################################
# Plot chosen points over background points
png(paste(fpath, "map_focal_pts.png", sep=""),
    width = 120, height = 120, units = 'mm', res = 450)
ggplot(data=m, aes(y=latitude.x, x=longitude.x)) +
  geom_point(data=allpts, aes(longitude,latitude), color="grey", size=1.5) +
  geom_point(data=car, aes(longitude,latitude), color="white",size=3) +
  geom_polygon(data=wus, aes(long,lat, group), fill=NA,color="black") +
  geom_point( size = 1.5,colour="blue") + 
  coord_fixed(1.3) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
dev.off()
