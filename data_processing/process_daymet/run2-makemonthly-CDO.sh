#!/bin/ksh
#Dec 2015
#DOES: Converts daily data to monthly averages

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J Run2
#SBATCH -o Run2_%j.out
#SBATCH -e Run2_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=END

#################
#Set location to Daymet
fdirIn=/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data
fdirOut=/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data

#################
# First, calculate monthly wetdays from daily precip
#################
#Calculate monthly wet days
#Loop through years

for year in {1980..2014}; do;

		#Calculate number of wet days per month
    	cdo -O ifthenc,1 $fdirIn/'prcp_'$year'.nc4' $fdirOut/'wetd_'$year'_tmp1.nc4'
    	cdo -O setmisstoc,0 $fdirOut/'wetd_'$year'_tmp1.nc4' $fdirOut/'wetd_'$year'_tmp2.nc4'
    	cdo -O monsum $fdirOut/'wetd_'$year'_tmp2.nc4' $fdirOut/'wetd_'$year'_tmp3.nc4'

		#Change variable name
		cdo -O chname,prcp,wetdays $fdirOut/'wetd_'$year'_tmp3.nc4' $fdirOut/'wetd_'$year'_monthly_tmp.nc4'
		
		# keep just the variable of interest- necessary to fix 2014 variable structure issue
			cdo selvar,wetdays $fdirOut/'wetd_'$year'_monthly_tmp.nc4' $fdirOut/'wetd_'$year'_monthly.nc4'

    done    #end daily to monthly conversion

    #Clean up
    rm $fdirOut/*tmp*
    
# NEXT, create monthly files for all other daymet variables
# list all variables except wetdays
set -A daymetVars swe srad tmax tmin vp prcp dayl

#Calculate monthly means/sums
#Loop through daymet variables
for var in ${daymetVars[@]}; do;

    for year in {1980..2014}; do;

        #Make monthly timeseries (swe is the water in the snow pack, so average this)
        if [[ $var = "prcp" ]];then
           cdo -O monsum $fdirOut/$var'_'$year'.nc4' $fdirOut/$var'_'$year'_monthly_tmp.nc4'
        else
            cdo -O monmean $fdirOut/$var'_'$year'.nc4' $fdirOut/$var'_'$year'_monthly_tmp.nc4'
		fi
		# keep just the variable of interest- necessary to fix 2014 variable structure issue
			cdo selvar,$var $fdirOut/$var'_'$year'_monthly_tmp.nc4' $fdirOut/$var'_'$year'_monthly.nc4'
			# test- delete this line later

    done    #end daily to monthly conversion

    #Clean up
    rm $fdirOut/$var'_'*'_monthly_tmp.nc4'
done    #end of variable loop





