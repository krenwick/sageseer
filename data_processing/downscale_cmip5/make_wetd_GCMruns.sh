#!/bin/ksh
#Dec 2015
#DOES: Converts daily data to monthly averages

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J makewetd
#SBATCH -o makewetd_%j.out
#SBATCH -e makewetd_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=END

#################
#Set location to Daymet
fdirIn=/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/daymet/

# make folder for temp files
mkdir $fdirIn/temp

# First, calculate mean wetdays in each month
cdo ymonmean $fdirIn/wetd.nc $fdirIn/temp/meanwetd.nc

# Shift time repeatedly over many years
for year in {1861..2099}; do;
 	
 	cdo setyear,$year $fdirIn/temp/meanwetd.nc $fdirIn/'temp/wet_'$year'.nc'

done 

# delete meanwetd
rm $fdirIn/temp/meanwetd.nc

# merge all newly-created years
cdo mergetime $fdirIn/temp/*.nc GCM_wetd.nc

