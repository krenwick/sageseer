#############################################################################
# Which management unit is each sageseer point in? What is the elevation?
# Does: use sageseer points to extract elev and management unit
  # Sept. 2016: now also extracts ecoregion and Kuchler PNV type
# Output: writes csv of points with additional attributes
#############################################################################
# libraries
rm(list=ls())
library(raster)
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
#require("ggplot2")
#require("plyr")

# Read in sageseer points
pts <- read.csv("/Users/poulterlab1/Box Sync/sageseer/ModelComparison/focal_sites_for_comparison.csv")
# load states map to overlay (ALSO: need for correct CRS info)
states <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers/", "states")
wus <- states[states$STATE_ABBR=="WA"|states$STATE_ABBR=="OR"|states$STATE_ABBR=="CA"
              |states$STATE_ABBR=="ID"|states$STATE_ABBR=="NV"|states$STATE_ABBR=="MT"
              |states$STATE_ABBR=="UT"|states$STATE_ABBR=="NM"|states$STATE_ABBR=="WY"
              |states$STATE_ABBR=="CO"|states$STATE_ABBR=="AZ",]
# Load sagegrouse management units
grouse <- readOGR("/Users/poulterlab1/Box Sync/sageseer/ClimateData/SG_MgmtZones_ver2_20061018","SG_MgmtZones_ver2_20061018")
grouse <- spTransform(grouse, CRS(proj4string(states)))

# Load Kuchler sagebrush map (shapefile from Adler)
artrdist <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers/","bigsagebrush")
artrdist<-spTransform(artrdist, CRS(proj4string(states)))

# Load actual, unclipped Kuchler map
KL <- readOGR("/Users/poulterlab1/Documents/GIS_baselayers/","Potential_Natural_Vegetation_2000_sgca")
KL <- spTransform(KL, CRS(proj4string(states)))

# Load DEM: Read in raster tiles and merge
# data is GTOPO30, 1-km resolution 
edir <- ("/Users/poulterlab1/Documents/GIS_baselayers/")
dem1 <-  raster(paste(edir,"gt30w140n40.tif", sep=""))
dem2 <-  raster(paste(edir,"gt30w140n90.tif", sep=""))
dem <- merge(dem1,dem2)

# convert to points for plotting (takes a while, do once to check)
#val1 <- rasterToPoints(dem)
#val2 <- data.frame(val1)

# Get kuchler data in plottable format (this is just a check)
#artrdist@data$id = rownames(artrdist@data)
#kuch.pt <- fortify(artrdist, region="id")
#kuch.df = join(kuch.pt, artrdist@data, by="id")
ggplot(data=pts, aes(x=longitude, y=latitude)) +
 # geom_raster(data=val2,aes(x,y)) + # takes long ime to draw raster
  #geom_polygon(data=grouse, aes(long,lat, group), fill=NA,color="green") +
  geom_polygon(data=kuch.df, aes(long,lat, fill=as.factor(KUCHLER_),group=group), color=NA) +
  geom_point(data=pts,aes(shape=as.factor(outlier))) +
  geom_polygon(data=wus, aes(long,lat, group),fill=77*NA, color="grey46") +
  theme_bw(base_size = 24, base_family = "Helvetica") +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_equal(1.3)

#load ecoregions
sdir <- ("/Users/poulterlab1/Documents/GIS_baselayers/")
ecoreg <- readOGR(sdir,"us_eco_l3")
ecoreg <- spTransform(ecoreg, CRS(proj4string(states)))

#load SSURGO soil temp and moisture regime classes
ss1 <- read.csv("/Users/poulterlab1/Box Sync/sageseer/ModelComparison/focal_pts_project_extract_all_SMTRegime.csv")
ss2 <- merge(pts,ss1,by="site", all.X=T)


# Extract management unit and elev by points
pts2 <- pts[,1:2]
elev <- raster::extract(dem,pts2)
mgnt.unit <- raster::extract(grouse,pts2) %>%
  dplyr::select(Mgmt_zone:Name)
eco <- raster::extract(ecoreg,pts2) %>%
  dplyr::select(US_L3CODE:NA_L1NAME)
sagetype <- raster::extract(artrdist, pts2) %>%
  dplyr::select(VEGTYP_LAB)
KL2 <- raster::extract(KL, pts2)
KL3 <- KL2 %>%  dplyr::select(PNV_GROUPS)
ss3 <- merge(pts2,ss1,by=c("longitude","latitude"), all=T)

newout <- cbind(pts,elev,mgnt.unit,eco,sagetype,KL3,ss3) 
write.csv(newout,"/Users/poulterlab1/Box Sync/sageseer/ModelComparison/focal_sites_by_zone.csv", row.names=F)

