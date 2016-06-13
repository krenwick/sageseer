#!/bin/csh
#February 2014
#Dec 2015 katie.renwick updated to use existing weights file
# Katie also added option Opt@LargeFile = TRUE in regridding script


#Must be run from c-shell csh - no change needed if submitting to slurm
#DOES: Converts merged curvilinear grid to rectangular grid 
#Description: 1) Regrid daymet to rectangular/latlon from curvilinear
# Must have grid files already generated using script 3

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J gridall
#SBATCH -o grid_%j.out
#SBATCH -e grid_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=END
#SBATCH -t 70:00:00

#################
#Set location to Daymet
set fdirIn = (/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3)
set fdirOut = (/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3)
set fdirScripts = (/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/scripts_daymetprocessing/scripts)

#################
#Set variable names
set startyear = 1980
set endyear = 2014

#Loop through daymet variable
foreach vname ( wetd swe srad tmax tmin vp prcp dayl )
    echo $vname
    set year=$startyear

    #Loop through daymet years
    while ( $year <= $endyear )
        #Modify the NCL merge code, first copy the original
        cp $fdirScripts/regrid_with_weights-orig.csh $fdirScripts/regrid_with_weights.csh
        
        #Replace line 25 with vname
        sed '25d' $fdirScripts/regrid_with_weights.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/regrid_with_weights.csh
        sed -e "25i\\
        vname        = \"\"$vname"\"\" $fdirScripts/regrid_with_weights.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/regrid_with_weights.csh
        
        #Replace line 26 with year
        sed '26d' $fdirScripts/regrid_with_weights.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/regrid_with_weights.csh
        sed -e "26i\\
        year       = $year" $fdirScripts/regrid_with_weights.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/regrid_with_weights.csh

        #Replace line 28 with method
        sed '28d' $fdirScripts/regrid_with_weights.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/regrid_with_weights.csh
        if( $vname == 'prcp' | $vname == 'wetd') then
            sed -e "28i\\
            method       = \"\"conserve"\"\" $fdirScripts/regrid_with_weights.csh > tmp.csh
        endif

        if( $vname != 'prcp') then
            sed -e "28i\\
            method       = \"\"bilinear"\"\" $fdirScripts/regrid_with_weights.csh > tmp.csh
        endif

        mv $fdirScripts/tmp.csh $fdirScripts/regrid_with_weights.csh

        #Replace line 34 with data directory location
        sed '34d' $fdirScripts/regrid_with_weights.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/regrid_with_weights.csh
        sed -e "34i\\
            DataDirName      = \"\"$fdirIn/"\"\" $fdirScripts/regrid_with_weights.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/regrid_with_weights.csh

        #Run the code
        ncl $fdirScripts/regrid_with_weights.csh
		echo "ran ncl script"
		
        #Clean up
        rm $fdirScripts/"PET0.RegridWeightGen.Log"


        #Update the year
        @ year++
    end
end	#end of variable loop
