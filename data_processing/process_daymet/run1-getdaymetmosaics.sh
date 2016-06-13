#!/bin/bash
#Dec 2015 katie.renwick updated to rerun for 1980 to 2014

# Script to download individual .nc files from the ORNL
# Daymet server at: http://daymet.ornl.gov
#
# NOTE: Compressed tar.gz files containing all .nc files for 
# a tile are separately available at the above address.
# Downloading the tar.gz files is far faster should you
# require all available data for a tile. 
# 
# Tile IDs must be provided either as a range
# of values, or as a space seperated list. Tile IDs may
# be determined from the Daymet mask of the conterminous 
# United States, visible for example at:
# http://daymet.ornl.gov/gridded
#
# Methods using wget and curl are shown. Each uses a resaonable
# rate limit for downloading data. Users attempting to download
# data at higher rates may be rate limited by the server. 
#
# The example downloads the vp.nc file for given year and tile
# ranges. You may change vp.nc to be any of the following: 
# vp.nc, tmin.nc, tmax.nc, swe.nc, srad.nc, prcp.nc, dayl.nc
#
# Data is also available via the THREDDS web interface at:
# http://daymet.ornl.gov/thredds/fileServer/allcf/fileServer.html
#
# Citation Information is available at:
# http://daymet.ornl.gov/sites/default/files/Daymet_Reference_Citation.pdf
#
# Pete Eby
# Oak Ridge National Lab

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J Run1_getdata
#SBATCH -o Run1_getdata_%j.out
#SBATCH -e Run1_getdata_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=END

for year in {1980..2014}
	do 
	wget --limit-rate=50m http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/${year}/vp_${year}.nc4 -O /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/vp_${year}.nc4
	wget --limit-rate=50m http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/${year}/dayl_${year}.nc4 -O /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/dayl_${year}.nc4
	wget --limit-rate=50m http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/${year}/tmax_${year}.nc4 -O /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/tmax_${year}.nc4
	wget --limit-rate=50m http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/${year}/tmin_${year}.nc4 -O /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/tmin_${year}.nc4
	wget --limit-rate=50m http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/${year}/prcp_${year}.nc4 -O /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/prcp_${year}.nc4
	wget --limit-rate=50m http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/${year}/srad_${year}.nc4 -O /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/srad_${year}.nc4
	wget --limit-rate=50m http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/${year}/swe_${year}.nc4 -O /mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/swe_${year}.nc4

done