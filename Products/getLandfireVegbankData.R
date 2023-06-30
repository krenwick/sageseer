#------	PULL PRESENCE RECORDS OF BIG SAGEBRUSH 

#---Synonyms by Shultz, L. M. 2006. The genus Artemisia (Asteraceae: Anthemideae). Pages 503-534 in Flora of North America Editorial Committee, editor. Flora of North America North of Mexico. Oxford University Press, New York, NY.
#	- Shultz, L. M. 2009. Monograph of Artemisia subgenus Tridentatae (Asteraceae-Anthemideae). Systematic Botany Monographs:1-131.
#	- Shultz, L. M. 2012. Pocket guide to sagebrush. Point Reyes Bird Observatory Conservation Science Publication, Petaluma, CA.

#	Artemisia tridentata Nuttall
#		== Seriphidium tridentatum (Nuttall) W. A. Weber
#	Artemisia tridentata Nuttall subsp. tridentata
#		== Artemisia angustifolia (A. Gray) Rydberg
#		== A. tridentata subsp. xericensis Winward ex R. Rosentreter & R. G. Kelsey
#	Artemisia tridentata Nuttall subsp. parishii (A. Gray) H. M. Hall & Clements
#		== Artemisia parishii A. Gray
#		== A. tridentata var. parishii (A. Gray) Jepson
#		== Seriphidium tridentatum (Nuttall) W. A. Weber subsp. parishii (A. Gray) W. A. Weber
#	Artemisia tridentata Nuttall subsp. vaseyana (Rydberg) Beetle
#		== Artemisia vaseyana Rydberg
#		== A. tridentata var. pauciflora Winward & Goodrich
#		== A. tridentata var. vaseyana (Rydberg) B. Boivin
#		== Seriphidium vaseyanum (Rydberg) W. A. Weber
#	Artemisia tridentata Nuttall subsp. wyomingensis Beetle & A. M. Young
#		== Artemisia tridentata var. wyomingensis (Beetle & A. M. Young) S. L. Welsh
#		== Seriphidium tridentatum (Nuttall) W. A. Weber subsp. wyomingense (Beetle & A. M. Young) W. A. Weber


#---Directories
dir.dat <- "~/Documents/SagebrushPresence"
#dir.spocc <- file.path(dir.dat, "ROPENSCI_SPOCC", "ArtemisiaTridentata")
dir.vegbank <- file.path(dir.dat, "VegBank")
dir.lf <- file.path(dir.dat, "LandFire_Reference_Database")

#---3. READ DATA FROM VEGBANK
vb_env <- read.csv(file.path(dir.vegbank, "plot_env.csv"))
vb_env$obsstartyear <- sapply(vb_env$obsstartdate_vb, FUN=function(x){
  res <- try(as.POSIXlt(x), silent=TRUE)
  if(inherits(res, "try-error")){
    return(NA)
  } else {
    return(1900 + res$year)
  }})
vb_taxa <- read.csv(file.path(dir.vegbank, "plot_taxa.csv"))

#Get observation IDs of "Artemisia tridentata" presences and absences
artemisia_species <- levels(temp)[grepl("Artemisia", levels(temp)) & grep("Seriphidium", levels(temp))]
vb_attr <- vb_taxa[grep("Artemisia tridentata", vb_taxa[, "currenttaxoninterp_scientificnamenoauthors"]), c("observation_id", "currenttaxoninterp_scientificnamenoauthors", "cover")]
ids_attr <- unique(vb_attr$observation_id)
ids_wo_attr <- unique(vb_taxa$observation_id[!(vb_taxa$observation_id %in% ids_attr)])

#Keep only observations with good confidentiality and location accuracy
#	- Confidentiality Status = Are the data to be considered confidential? 0=no, 1= 1km radius, 2=10km radius, 3=100km radius, 4=location embargo, 5=public embargo on all plot data, 6=full embargo on all plot data. This applies also to region. 
#	- Location accuracy = Estimated accuracy of the location of the plot. Plot origin has a 95% or greater probability of being within this many meters of the reported location.
#	- plotValidationLevel = Quality of plot as determined by an automated filter system, including values such as (1) sufficient for determining type occurrence, (2) sufficient for inclusion in a classification revision, and (3) fully compliant with recommendations. 
#		-> Problem: only a small number of observations have an assigned plotvalidationlevel 

# convert factors to numeric
vb_env$locationaccuracy <- as.numeric(as.character(vb_env$locationaccuracy))


ids_attr <- unique(vb_env$observation_id[vb_env$observation_id %in% ids_attr &
                                           vb_env$confidentialitystatus == 0 &
                                           (is.na(vb_env$locationaccuracy) | vb_env$locationaccuracy < 30)])

ids_wo_attr <- unique(vb_env$observation_id[vb_env$observation_id %in% ids_wo_attr &
                                              vb_env$confidentialitystatus == 0 &
                                              (is.na(vb_env$locationaccuracy) | vb_env$locationaccuracy < 30)])

#Presences must have cover > 0
#	- cover = Cover, in percent, of the taxon (potentially limitied to one stratum if stratum_ID has a value).
ids_attr <- unique(vb_taxa$observation_id[vb_taxa$observation_id %in% ids_attr & vb_taxa$cover > 0])

#Check whether some plots have multiple ATTR entries for different subspecies
length(levels(factor(vb_attr[,2]))) > 1
sum((plot_reps <- aggregate(vb_attr, by=list(vb_attr[, 1]), FUN=function(x) length(unique(x))))[, 3] > 1) > 0

itax <- unlist(lapply(1:nrow(plot_reps), FUN=function(i) which(plot_reps[i, 1] == vb_attr$observation_id)[1:plot_reps[i, 3]]))
ienv <- unlist(lapply(1:nrow(plot_reps), FUN=function(i) which(plot_reps[i, 1] == vb_env$observation_id)[1:plot_reps[i, 3]]))

#Put presence data together
ATpres_vegbank <- data.frame(
  STATE=vb_env[ienv, "stateprovince_vb"],
  SOURCE="VegBank", 
  LATITUDE=vb_env[ienv, "latitude"], 
  LONGITUDE=vb_env[ienv, "longitude"],
  SUB.SPP=as.character(vb_attr[itax, 2]),
  Year=vb_env[ienv, "obsstartyear"],
  stringsAsFactors=FALSE)

levels(factor(ATpres_vegbank$SUB.SPP))
ATpres_vegbank[ATpres_vegbank$SUB.SPP == "Artemisia tridentata", "SUB.SPP"] <- "Artemisia tridentata Nutt."
ATpres_vegbank[ATpres_vegbank$SUB.SPP == "Artemisia tridentata var. pauciflora", "SUB.SPP"] <- "Artemisia tridentata ssp. vaseyana"
ATpres_vegbank <- ATpres_vegbank[!(ATpres_vegbank$SUB.SPP == "Artemisia tridentata ssp. spiciformis"), ]	# Artemisia spiciformis is considered a separate species by Shultz
ATpres_vegbank[ATpres_vegbank$SUB.SPP == "Artemisia tridentata var. angustifolia", "SUB.SPP"] <- "Artemisia tridentata ssp. tridentata"
ATpres_vegbank <- ATpres_vegbank[!(ATpres_vegbank$SUB.SPP == "Artemisia tridentata ssp. arbuscula"), ]	# Artemisia arbuscula is considered a separate species by Shultz


#Put absence data together
ienv <- vb_env$observation_id %in% ids_wo_attr

ATabs_vegbank <- data.frame(
  STATE=vb_env[ienv, "stateprovince_vb"],
  SOURCE="VegBank", 
  LATITUDE=vb_env[ienv, "latitude"], 
  LONGITUDE=vb_env[ienv, "longitude"],
  SUB.SPP="Absence of ATTR",
  Year=vb_env[ienv, "obsstartyear"],
  stringsAsFactors=FALSE)



#---4. READ DATA FROM PUBLIC LANDFIRE REFERENCE DATABASE
#	- Certain proprietary and/or sensitive data were removed in the public database. Consult the table lutVisitsSourceID in the database regarding data sources.
#	- The Public LFRDB can be used as input to spatial and non-spatial vegetation models and is applicable for ground truthing and accuracy assessments for a variety of modeling and mapping efforts.
library(Hmisc)
get_landfire_mdb <- function(file, species="Artemisia tridentata"){
  stopifnot(require(Hmisc))
  sp_pres <- sp_abs <- NULL
  
  if(all(!grepl(path_mdbtools <- dirname(system('which mdb-tables', intern=TRUE)), Sys.getenv("PATH")))){
    #Sys.getenv("PATH") needs to include proper path, i.e. 'which mdb-tables'
    Sys.setenv(PATH=paste(Sys.getenv("PATH"), path_mdbtools, sep=":"))
  }
  #temp <- mdb.get(file=file, tables=TRUE)
  tblSpecies <- mdb.get(file=file, tables="tblSpecies")
  tblPoints <- mdb.get(file=file, tables="tblPoints")
  tblVisits <- mdb.get(file=file, tables="tblVisits")
  
  #Presence locations
  get_sp <- tblSpecies[grepl(species, tblSpecies$SciName) & tblSpecies$LFRelCov > 0, ]
  if(nrow(get_sp) > 0){
    get_visits <- tblVisits[match(get_sp$EventID, tblVisits$EventID, nomatch=0), ]
    get_points <- tblPoints[match(get_visits$PointID, tblPoints$PointID, nomatch=0), ]
    
    sp_pres <- data.frame(
      STATE=NA,
      SOURCE=basename(file), 
      LATITUDE=get_points$Lat, 
      LONGITUDE=get_points$Long,
      SUB.SPP=as.character(get_sp$SciName),
      Year=get_visits$YYYY,
      stringsAsFactors=FALSE)
  }
  
  #Absence locations: points where species never was detected
  if(nrow(get_sp) > 0){
    get_wo_points <- tblPoints[!(tblPoints$PointID %in% get_points$PointID), ]
  } else {
    get_wo_points <- tblPoints
  }
  if(nrow(get_wo_points) > 0){
    get_wo_visits <- tblVisits[match(get_wo_points$PointID, get_wo_points$PointID, nomatch=0), ]
    
    sp_abs <- data.frame(
      STATE=NA,
      SOURCE=basename(file), 
      LATITUDE=get_wo_points$Lat, 
      LONGITUDE=get_wo_points$Long,
      SUB.SPP=paste0("Absence of ", species),
      Year=get_wo_visits$YYYY,
      stringsAsFactors=FALSE)
  }
  
  return(list(presence=sp_pres, absence=sp_abs))
}

#disturbance_types <- lapply(list.files(file.path(dir.lf, "LFRDB"), pattern=".mdb", full.names=TRUE), FUN=function(file){
#		tblSiteChanges <- mdb.get(file=file, tables="tblSiteChanges")
#		unlist(lapply(2:ncol(tblSiteChanges), FUN=function(icol) levels(factor(tblSiteChanges[, icol]))))})

temp <- lapply(list.files(file.path(dir.lf), pattern=".mdb", full.names=TRUE), FUN=get_landfire_mdb)
ATpres_landfire <- do.call(rbind.data.frame, lapply(temp, FUN=function(x) x$presence))
ATabs_landfire <- do.call(rbind.data.frame, lapply(temp, FUN=function(x) x$absence))
ATpres_landfire[ATpres_landfire$SUB.SPP == "Artemisia tridentata", "SUB.SPP"] <- "Artemisia tridentata Nutt."
ATpres_landfire <- ATpres_landfire[!(ATpres_landfire$SUB.SPP == "Artemisia tridentata ssp. spiciformis"), ]	# Artemisia spiciformis is considered a separate species by Shultz


with(ATabs_landfire, plot(LONGITUDE, LATITUDE, pch=46, col="red"))
with(ATpres_landfire, points(LONGITUDE, LATITUDE, pch=46, col="green"))


#---5a. COMBINE ALL PRESENCE DATA AND CLEAN
# make factors numeric
ATpres_vegbank$LATITUDE <- as.numeric(as.character(ATpres_vegbank$LATITUDE))
ATpres_vegbank$LONGITUDE <- as.numeric(as.character(ATpres_vegbank$LONGITUDE))

ATpres_comb <- rbind(ATpres_vegbank, ATpres_landfire)
ATpres_comb$Year <- as.integer(ATpres_comb$Year)
ATpres_comb$SUB.SPP <- factor(ATpres_comb$SUB.SPP)
levels(ATpres_comb$SUB.SPP)
#if(FALSE) ATpres_comb <- readRDS(file=file.path(dir.prj, "DATA", "20140912_ArtemisiaTridentata_Presences_Uncleaned.rds"))

#Remove empty coordinates (n = 671)
length(idelete <- unique(c(which(is.na(ATpres_comb$LATITUDE)), which(is.na(ATpres_comb$LONGITUDE)))))
ATpres_comb <- ATpres_comb[-idelete, ]

#Remove coordinates outside North America (n = 457)
sum(idelete <- (ATpres_comb$LATITUDE < 25 | ATpres_comb$LATITUDE > 60 | ATpres_comb$LONGITUDE < -125 | ATpres_comb$LONGITUDE > -65))
ATpres_comb <- ATpres_comb[!idelete, ]

#Remove coordinates without minimal accuracy (n = 607)
acc <- 30 # 12 = 5 arcmin, 60 = arcmin, 3600 = arcsec accuracy
epsilon <- 1e-3
length(idelete <- which(abs(round(acc*ATpres_comb$LATITUDE) - acc*ATpres_comb$LATITUDE) <= epsilon & abs(round(acc*ATpres_comb$LONGITUDE) - acc*ATpres_comb$LONGITUDE) <= epsilon))
ATpres_comb <- ATpres_comb[-idelete, ]

#Clean state entries
ATpres_comb$STATE[ATpres_comb$STATE == "Baja california"] <- "Baja California"
ATpres_comb$STATE[ATpres_comb$STATE == "Ca"] <- "California"
ATpres_comb$STATE[ATpres_comb$STATE == ""] <- NA
ATpres_comb$STATE[ATpres_comb$STATE == "ARIZONA"] <- "Arizona"
ATpres_comb$STATE[ATpres_comb$STATE == "WYOMING"] <- "Wyoming"
levels(factor(ATpres_comb$STATE))

##############


#Query state and test with existing entries
library(sp)
library(rgdal)



nrow(ATpres_comb) == 48421

#Use only years from our current period (1980-2010)
#ATpres_comb <- ATpres_comb[!is.na(ATpres_comb$Year) & ATpres_comb&Year >= 1980,]


#---5a. COMBINE ALL ABSENCE DATA AND CLEAN
ATabs_comb <- rbind(ATabs_vegbank, ATabs_landfire)
ATabs_comb$Year <- as.integer(ATabs_comb$Year)
ATabs_comb$SUB.SPP <- factor(ATabs_comb$SUB.SPP)

# make Lat and Lon into numeric
ATabs_comb <- transform(ATabs_comb, LATITUDE = as.numeric((as.character(LATITUDE))), 
          LONGITUDE = as.numeric((as.character(LONGITUDE))))

#Remove empty coordinates (n = 6233)
length(idelete <- unique(c(which(is.na(ATabs_comb$LATITUDE)), which(is.na(ATabs_comb$LONGITUDE)))))
ATabs_comb <- ATabs_comb[-idelete, ]

#Remove coordinates outside North America (n = 908)
sum(idelete <- (ATabs_comb$LATITUDE < 25 | ATabs_comb$LATITUDE > 60 | ATabs_comb$LONGITUDE < -125 | ATabs_comb$LONGITUDE > -65))
ATabs_comb <- ATabs_comb[!idelete, ]

#Remove coordinates without minimal accuracy (n = 174)
acc <- 30 # 12 = 5 arcmin, 60 = arcmin, 3600 = arcsec accuracy
epsilon <- 1e-3
length(idelete <- which(abs(round(acc*ATabs_comb$LATITUDE) - acc*ATabs_comb$LATITUDE) <= epsilon & abs(round(acc*ATabs_comb$LONGITUDE) - acc*ATabs_comb$LONGITUDE) <= epsilon))
ATabs_comb <- ATabs_comb[-idelete, ]

nrow(ATabs_comb) == 244378

#Query state and test with existing entries
ATabs_comb$STATE <- as.character(ATabs_comb$STATE)
ATabs_comb$STATE[ATabs_comb$STATE == "" | is.na(ATabs_comb$STATE)] <- NA


#---6a. SUBSET SPATIAL UNIQUE ENTRIES BY SUBTAXON
#Spatial unique resolution: 1/22 arcsec (about 1 m at 45 degree North)
iround <- paste(round(ATpres_comb$LONGITUDE * 22 * 60^2, 0), round(ATpres_comb$LATITUDE * 22* 60^2, 0), sep="_")
dtemp <- NULL
for(i in 1:nlevels(ATpres_comb$SUB.SPP)){
  itemp <- ATpres_comb$SUB.SPP == levels(ATpres_comb$SUB.SPP)[i]
  if(sum(itemp) > 0){
    dtemp <- rbind(dtemp, ATpres_comb[itemp, ][!duplicated(iround[itemp]),])
  }
}

ATpres_comb <- dtemp
nrow(ATpres_comb) == 42044


#---6b. SUBSET SPATIAL UNIQUE ENTRIES OF ABSENCES
#Spatial unique resolution: 1/22 arcsec (about 1 m at 45 degree North)
iround <- paste(round(ATabs_comb$LONGITUDE * 22 * 60^2, 0), round(ATabs_comb$LATITUDE * 22* 60^2, 0), sep="_")
ATabs_comb <- ATabs_comb[!duplicated(iround),]

nrow(ATabs_comb) == 221327



#Visualize
library(maps)
map("state")
cols <- c("orange", "red", "blue", "green", "magenta")
with(ATabs_comb, points(LONGITUDE, LATITUDE, pch=46, col="gray"))
with(ATpres_comb, points(LONGITUDE, LATITUDE, pch=46, col=cols[SUB.SPP]))
legend(x="topright", legend=c("Absence", levels(ATpres_comb$SUB.SPP)), pch=16, col=c("black", cols), cex=0.5)

map("state", xlim=c(-116.85, -116.82), ylim=c(38.68, 38.7))
axis(1)
axis(2)
with(ATabs_comb, points(LONGITUDE, LATITUDE, pch=16, col="gray"))
with(ATpres_comb, points(LONGITUDE, LATITUDE, pch=16, col=cols[SUB.SPP]))
#	=> Absences can be extremely close to presences
#	TODO: May need a minimum distance to a presence to qualify as absence?

#Save
saveRDS(ATpres_comb, file=file.path(dir.dat, "DATA", "20140912_ArtemisiaTridentata_Presences_Cleaned.rds"))
write.csv(ATpres_comb, file=file.path(dir.dat, "DATA", "20150827_ARTRpres.csv"))
saveRDS(ATabs_comb, file=file.path(dir.dat, "DATA", "20140912_ArtemisiaTridentata_Absences_Cleaned.rds"))
write.csv(ATabs_comb, file=file.path(dir.dat, "DATA", "20150827_ARTRabs.csv"))

nrow(ATpres_comb) # 39,027
nrow(ATabs_comb) # 45,892

