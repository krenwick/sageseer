#!/bin/ksh
#Dec 2015
#DOES: Merges individual year files into one netCDF containing all years
# converts merged files to nc4 format to save space (more compression)
# also creates tmean, wetdays, and converts srad to 24-hr instantaneaous

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J Run5
#SBATCH -o Run5_%j.out
#SBATCH -e Run5_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=END
#SBATCH -t 70:00:00

#################
#Set location to Daymet
fdirOut=/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3

#################
set -A daymetVars wetd swe srad tmax tmin vp prcp dayl

# FIRST: merge annual files into one big file
#Loop through daymet variables
for var in ${daymetVars[@]}; do;

 #Merge years to a single 1980-2014 file
    cdo -O -f nc4 mergetime $fdirOut/$var*'_monthly_latlon.nc' $fdirOut/$var'_1980-2014.nc4'

    #Clean up
    rm $fdirOut/$var*'_monthly_latlon.nc'
#done    #end of variable loop

#################

# SECOND: calculate additional variables needed for LPJ-GUESS (tmean and srad-24hr)
#Calculate tmean from tmin and tmax
cdo add $fdirOut/tmin_1980-2014.nc4 $fdirOut/tmax_1980-2014.nc4 $fdirOut/tmean_1980-2014_tmp1.nc4
cdo divc,2 $fdirOut/tmean_1980-2014_tmp1.nc $fdirOut/tmean_1980-2014_tmp2.nc4

#Change variable name
cdo -O chname,tmin,tmean $fdirOut/tmean_1980-2014_tmp2.nc4 $fdirOut/tmean_1980-2014.nc4

# Correct solar radiation
# Daymet srad units: daylight average incident shortwave radiation (W/m2)
# Daymet srad: Daily surfaces of incident solar radiation are generated as a 
# function of Sun-slope geometry and interpolated diurnal temperature range.
# Interpret as instantaneous for daylight only (not 24 hours)
# LPJGUESS - As mean daily instantaneous downward shortwave radiation flux, W/m2)
# for 24 hours

# Daymet SRAD * (daylength in secs) = total srad for all daylight
cdo -b 64 mul $fdirOut/srad_1980-2014.nc4 $fdirOut/dayl_1980-2014.nc $fdirOut/srad_tmp.nc4

# Total SRAD for daylight/(86400) - 86400=seconds in one day
cdo divc,86400 $fdirOut/srad_tmp.nc4	$fdirOut/srad_24hrinstant_1980-2014.nc4

# Change variable longname
# NOTE: not working on hyalite due to NCO issue (Spring 2016)
ncatted -O -a long_name,srad,o,c,"average daily instantaneous incident shortwave radiation" $fdirOut/srad_24hrinstant_1980-2014.nc4

#Clean up
rm $fdirOut/*tmp*
