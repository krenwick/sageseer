#!/bin/csh
#February 2014
#Dec 2015 katie.renwick updated to just generate weights

#Must be run from c-shell csh - no change needed if submitting to slurm
#Description: 1) Step one to regrid daymet to rectangular/latlon from curvilinear

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J run3
#SBATCH -o run3_%j.out
#SBATCH -e run3_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=END
#SBATCH --exclusive

#################
#Set location to Daymet
set fdirIn = (/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3)
set fdirOut = (/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3)
set fdirScripts = (/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/scripts_daymetprocessing/scripts)

#################
#Set variable names
set vname = vp
set year = 1980

        #Modify the NCL merge code, first copy the original
        cp $fdirScripts/ESMF-weightgen-orig.csh $fdirScripts/ESMF-weightgen.csh

        #Replace line 25 with vname
        sed '25d' $fdirScripts/ESMF-weightgen.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh
        sed -e "25i\\
        	vname        = \"\"$vname"\"\" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh

        #Replace line 26 with year
        sed '26d' $fdirScripts/ESMF-weightgen.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh
        sed -e "26i\\
            year       = $year" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh

        #Replace line 28 with method
        sed '28d' $fdirScripts/ESMF-weightgen.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh
        if( $vname == 'prcp') then
            sed -e "28i\\
                method       = \"\"conserve"\"\" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        endif
        
        if( $vname == 'wetdays') then
            sed -e "28i\\
                method       = \"\"nearestod"\"\" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        endif

        if( $vname != 'prcp' && $vname != 'wetdays') then
            sed -e "28i\\
                method       = \"\"bilinear"\"\" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        endif

        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh

        #Replace line 34 with data directory location
        sed '34d' $fdirScripts/ESMF-weightgen.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh
        sed -e "34i\\
            DataDirName      = \"\"$fdirIn/"\"\" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh

        #Replace line 39 with data directory location
        sed '39d' $fdirScripts/ESMF-weightgen.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh
        sed -e "39i\\
            srcDirName      = \"\"$fdirOut/"\"\" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh

        #Replace line 44 with data directory location
        sed '44d' $fdirScripts/ESMF-weightgen.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh
        sed -e "44i\\
            dstDirName      = \"\"$fdirOut/"\"\" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh

        #Replace line 112 with data directory location
        sed '112d' $fdirScripts/ESMF-weightgen.csh > $fdirScripts/tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh
        sed -e "112i\\
            RegridDirName      = \"\"$fdirOut/"\"\" $fdirScripts/ESMF-weightgen.csh > tmp.csh
        mv $fdirScripts/tmp.csh $fdirScripts/ESMF-weightgen.csh

        #Run the code
        ncl $fdirScripts/ESMF-weightgen.csh
        
        #Clean up
        #rm $fdirScripts/"PET0.RegridWeightGen.Log"

