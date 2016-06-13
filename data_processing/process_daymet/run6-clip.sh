#!/bin/ksh
#Dec 2015
#DOES: Clips final daymet data to ROI to make file smaller
# converts final into zip format so small enough to download

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J Run6
#SBATCH -o Run6_%j.out
#SBATCH -e Run6_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=END
#SBATCH -t 70:00:00

#################
#Set location to Daymet
fdirOut=/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3

#################
set -A daymetVars wetd swe srad tmax tmin vp prcp dayl tmean srad_24hrinstant

#Calculate monthly means/sums
#Loop through daymet variables
for var in ${daymetVars[@]}; do;

        #Clip the years to a western US region
        # adding -z zip option compresses output file
        cdo sellonlatbox,-124.25,-102.25,31.75,49 $fdirOut/$var'_1980-2014.nc4' $fdirOut/$var'_wusa_1980-2014.nc4'
        cdo -z zip copy $fdirOut/$var'_wusa_1980-2014.nc4' $fdirOut/wusa/$var'_wusa_1980-2014_zip.nc4'

done    #end of variable loop
