################################################################################
# Extract time series for MAT and MAP for each site:GCM combo
# RUN THIS ON HYALITE! Would take too long to download data
################################################################################
# load libraries
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)

# ON HYALITE: cd /mnt/lustrefs/store/katie.renwick/sageseer_GCM
latlon <- read.csv("./focal_sites_for_comparison.csv")
ll <- latlon[,c(1,2)]

# for testing splitting code
#test <- "./pr/pr_Amon_CCSM4_rcp45_r1i1p1_MAP.nc4"

######## READ IN MAP for rcp45
mapfiles <- list.files("./pr", pattern="rcp45_r1i1p1_MAP.nc4",full.names=T)
year <- seq(1861,2099,1)

map45<-NULL
for(i in 1:length(mapfiles)){
  file <- mapfiles[i]
    for(time in 1:239) {
      pr1 <- raster(file,varname="pr", band=time)
      pr2 <- raster::extract(pr1,ll)
      m1 <- cbind.data.frame(time,file,pr2)
      map45 <- rbind(map45,m1)
    }
}

######## READ IN MAP for rcp85
mapfiles85 <- list.files("./pr", pattern="rcp85_r1i1p1_MAP.nc4",full.names=T)
map85<-NULL
for(i in 1:length(mapfiles)){
  file <- mapfiles85[i]
  for(time in 1:239) {
    pr1 <- raster(file,varname="pr", band=time)
    pr2 <- raster::extract(pr1,ll)
    m1 <- cbind.data.frame(time,file,pr2)
    map85 <- rbind(map85,m1)
  }
}

######## READ IN MAT for rcp45
matfiles <- list.files("./tas", pattern="rcp45_r1i1p1_MAT.nc4",full.names=T)
mat45<-NULL
for(i in 1:length(mapfiles)){
  file <- matfiles[i]
  for(time in 1:239) {
    pr1 <- raster(file,varname="pr", band=time)
    pr2 <- raster::extract(pr1,ll)
    m1 <- cbind.data.frame(time,file,pr2)
    mat45 <- rbind(mat45,m1)
  }
}

######## READ IN MAT for rcp85
matfiles85 <- list.files("./tas", pattern="rcp85_r1i1p1_MAT.nc4",full.names=T)
mat85<-NULL
for(i in 1:length(mapfiles)){
  file <- matfiles85[i]
  for(time in 1:239) {
    pr1 <- raster(file,varname="pr", band=time)
    pr2 <- raster::extract(pr1,ll)
    m1 <- cbind.data.frame(time,file,pr2)
    mat85 <- rbind(mat85,m1)
  }
}

#####################################
# Combine output into df: lon, lat, year, rcp, GCM, MAT, MAP

save.image("./climdata.RData")


