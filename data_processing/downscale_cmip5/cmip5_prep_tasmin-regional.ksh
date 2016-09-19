#!/bin/ksh
#First run in June 2013, modified for tas in Oct 2013, fixed buggy model simulations in March 2014
#Original script written by benjamin.poulter@lsce.ipsl.fr
#Modified in June 2016 by Katie Renwick to downscale tasmin to wusa for sagebrush project

#This script processes CMIP5 temperature data
#	- Merges historical and future data
#	- Regrids CMIP5 temperature data to Daymet 1-km grid with bilinear interpolation
#	- Bias corrects CMIP5 temperature data using Daymet temperature
#Outputs
	# Global corrected ensemble mean at 0.5 degree
	# Global uncorrected ensemble mean at 0.5 degree
	# Global correct first ensemble member at 0.5 degree
#NOTE: simulations should use just the first ensemble member because variability gets dampened for the ensemble mean


#########################
#Set directories for datasets (the RCP directory is defined in the loop)
fdirCMIP5hist='/mnt/lustrefs/work/katie.renwick/activescripts/wget/data/tasmin/historical/'
fdirDAYMET='/mnt/lustrefs/store/benjamin.poulter/poulterlab/Climate/Processed/Regional/Wusa/daymet/unzippedcompression'
fdirKatieDAYMET='/mnt/lustrefs/work/katie.renwick/activescripts/tempclim/'
fdirGRID='/mnt/lustrefs/work/katie.renwick/activescripts/tempclim/'
fdirGRIDglobal='/mnt/lustrefs/store/benjamin.poulter/poulterlab/Climate/Original/Global/grid'

#########################
#List the RCPs and the Variables and the time steps
set -A rcpArray rcp45 rcp85

#########################
#Loop through RCP
for rcp in ${rcpArray[@]}; do;
        #Track the RCP
        echo $rcp

		#########################
		#Set directories for input and output datasets
		#For input for RCP data
		fdirCMIP5rcp='/mnt/lustrefs/work/katie.renwick/activescripts/wget/data/tasmin'/$rcp/

		#For output for final files
		#Default folder (so as not to delete home directory if not properly specified)
		echo "Making output directory"
		fdirOUT='/mnt/lustrefs/store/katie.renwick/Climate/Processed/Regional/wusa/CMIP5/tasmin'/$rcp/

		#Make output directory if it doesnt already exist
		if [ ! -d ${fdirOUT} ] ; then
			mkdir -p ${fdirOUT}
		fi
		
		#Empty the output directory
		cd $fdirOUT
        rm -rf *
		
		#########################
		#List all the input netcdf files for the historical and for the RCP
		#modelListHist=( $( ls $fdirCMIP5hist*nc ) )
		#modelListRcp=( $( ls $fdirCMIP5rcp*nc ) )
		
		#USE THESE LINES TO SELECT INDIVIDUAL MODELS- for sage project
		modelListHist=( $( #ls $fdirCMIP5hist*_CESM1-CAM5_historical_r1i1p1_*nc
		ls $fdirCMIP5hist*_MPI-ESM-LR_historical_r1i1p1_*nc
		ls $fdirCMIP5hist*_HadGEM2-AO_historical_r1i1p1_*nc
		ls $fdirCMIP5hist*_GISS-E2-H-CC_historical_r1i1p1_*nc))
		ls $fdirCMIP5hist*_CCSM4_historical_r1i1p1_*nc ) )	
		modelListRcp=( $( #ls $fdirCMIP5rcp*_CESM1-CAM5_*_r1i1p1_*nc 
		ls $fdirCMIP5rcp*_MPI-ESM-LR_*_r1i1p1_*nc
		ls $fdirCMIP5rcp*_HadGEM2-AO_*_r1i1p1_*nc
		ls $fdirCMIP5rcp*_GISS-E2-H-CC_*_r1i1p1_*nc))
		ls $fdirCMIP5rcp*_CCSM4_*_r1i1p1_*nc) )

		#########################
		#Merge the time periods for each model X ensemble for the historical period
		echo "Merging historical time slices for each ensemble"
		for fullName in ${modelListHist[@]}; do;

			#Find the model and the ensemble id
			echo $fullName |cut -d'_' -f3 | read model
            echo $fullName |cut -d'_' -f5 | read ensemble
            echo $fullName |cut -d'_' -f6 | read startyearrange
            echo $startyearrange |cut -d'-' -f1 | read startdate

            #Find for the last model X ensemble time slice, the end year/month
			modelListEnd=( $( ls $fdirCMIP5hist*$model*$ensemble*nc ) )
			endModel=${modelListEnd[@]:(-1)}
			echo $endModel |cut -d'_' -f6 | read endyearrange
            echo $endyearrange |cut -d'-' -f2 | read enddate

            #If the start year for the earliest model run is less than 186101, then skip the GCM (the skip occurs only for CanCM4 and MIROC4h)
            if [ $startdate -lt 186101 ]; then
                #If the merged model ensemble exists, then dont merge anymore of the time slices for this model X ensemble
                if [ ! -f $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_1861-2005.nc" ]; then
                    #########################
                    #Merge the historical GCM run time slices for the same ensemble
                    echo $fullName
                    cdo -s mergetime $fdirCMIP5hist/"tasmin_Amon_"$model"_historical_"$ensemble"_"* $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_allyears.nc"

                    #Select 1861-2005 time period. But with conditional for specific model problems - and force to clip all Hadley runs to end in November 2011
                    if [ $enddate = "200511" -o $model = "HadGEM2-ES" ]; then
                        echo Special Case: $fullName
                        #Clip the merged GCM runs to 1861-2005 (November)
                        cdo -s seldate,1861-01-01,2005-11-31 $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_allyears.nc" $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_1861-2005.nc"
                    elif [ $enddate != "200511" ]; then
                        #Clip the merged GCM runs to 1861-2005 (December)
                        cdo -s seldate,1861-01-01,2005-12-31 $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_allyears.nc" $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_1861-2005.nc"
                    fi

                    #Clean up
                    rm -f $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_allyears.nc"
                fi
            fi
		done

        #Merge the time periods for each model X ensemble for the RCP period
		echo "Merging RCP time slices for each ensemble"
		for fullName in ${modelListRcp[@]}; do;
			#Find the model and the ensemble id
			echo $fullName |cut -d'_' -f3 | read model
			echo $fullName |cut -d'_' -f5 | read ensemble
			echo $fullName |cut -d'_' -f6 | read startyearrange
			echo $startyearrange |cut -d'-' -f1 | read startdate
			
			#If the merged model ensemble exists, then dont merge anymore time slices
			if [ ! -f $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc" ]; then
				#########################
				#Merge the RCP climate data for the same ensemble, with special case for CCSM4 and HadGEM2-AO which have repeated files for the same ensemble
                if [[ $model != "HadGEM2-AO" ]]; then
                    echo Normal Merge: $fullName
                    cdo -s mergetime $fdirCMIP5rcp/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_"* $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
                elif [ $model = "HadGEM2-AO" ]; then
                    echo Special Case: $fullName
                    cdo -s mergetime $fdirCMIP5rcp/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_"*209912* $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
                elif [ $model = "CCSM4" -a $rcp = "rcp26" ]; then
                    echo Special Case: $fullName
                    cdo -s mergetime $fdirCMIP5rcp/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_"*200601* $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
                elif [ $model = "CCSM4" -a $rcp = "rcp45" ]; then
                    echo Special Case: $fullName
                    cdo -s mergetime $fdirCMIP5rcp/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_"*200601* $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
                elif [ $model = "CCSM4" -a $rcp = "rcp60" ]; then
                    echo Special Case: $fullName
                    cdo -s mergetime $fdirCMIP5rcp/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_"*200601* $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
                elif [ $model = "CCSM4" -a $rcp = "rcp85" -a $ensemble = "r6i1p1" ]; then
                    echo Special Case: $fullName
                    cdo -s mergetime $fdirCMIP5rcp/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_"*200501* $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
                fi

                #Clip the dates to 2006-2099 and print result to track that ntimesteps == 2868
				if [ $startdate != "200512" ]; then
					#Clip the merged GCM runs to 2006-2099
					cdo -s seldate,2006-01-01,2099-12-31 $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc"
				fi
				if [ $startdate = "200512" ]; then
					#Clip the merged GCM runs to 2006-2099
					cdo -s seldate,2005-12-01,2099-12-31 $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc"
				fi
				
				#Clean up
				rm -f $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
			fi
		done

        #########################
		#List all the sucessfully merged historical netcdf files
		modelListHistClean=( $( ls $fdirOUT*historical*1861-2005.nc ) )
		ls $fdirOUT*historical*1861-2005.nc

		#Merge the time periods for each successful historical and match with its future model X ensemble for the same RCP
		echo "Merging historical and RCP climatologies for each ensemble (check timestep == 2868 months)"
		for fullName in ${modelListHistClean[@]}; do;
			#Find the model and the ensemble id
			echo $fullName |cut -d'_' -f3 | read model
			echo $fullName |cut -d'_' -f5 | read ensemble
			
            #Merge the historical and future GCM runs ONLY if the RCP exists
            echo $fullName

            if [ -f $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc" ]; then
                cdo mergetime $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_1861-2005.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_K.nc"
                cdo -s subc,273.15 $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_K.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099.nc"
            else
                echo "No " $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc" " exists for the historical merge"
            fi
            
            #Clean up
            rm -rf $fdirOUT/"tasmin_Amon_"$model"_historical_"$ensemble"_1861-2005.nc"		#Remove old historical
            rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc"			#Remove old RCP
            rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_K.nc"       #Remove kelvin file
		done

        #########################
		#Fix other annoying issues
        #MIROC5 RCP ends in 2035, so just delete 1861-2099 file
        if [ $rcp = "rcp26" -o $rcp = "rcp45" -o $rcp = "rcp60" -o $rcp = "rcp85" ]; then
            rm -f $fdirOUT/"tasmin_Amon_MIROC5_"$rcp"_r4i1p1_1861-2099.nc"
            rm -f $fdirOUT/"tasmin_Amon_MIROC5_"$rcp"_r5i1p1_1861-2099.nc"
        fi
        #Remove HadCM3 because it ends in 2035
        if [ $rcp = "rcp45" ]; then
            rm -f $fdirOUT/"tasmin_Amon_HadCM3_"*
        fi        #Remove file because it ends in November 2099, not December
        if [ $rcp = "rcp60" ]; then
            rm -f $fdirOUT/"tasmin_Amon_HadGEM2-ES_rcp60_r1i1p1_1861-2099.nc"
        fi
        #Remove corrupt files
        if [ $rcp = "rcp85" ]; then
            #rm -f $fdirOUT/"tasmin_Amon_CESM1-CAM5_rcp85_r1i1p1_1861-2099.nc"
            rm -f $fdirOUT/"tasmin_Amon_CESM1-CAM5_rcp85_r3i1p1_1861-2099.nc"
            rm -f $fdirOUT/"tasmin_Amon_FIO-ESM_rcp85_r1i1p1_1861-2099.nc"
        fi
        #Fix repeated 2099 December month for just a single HadGEM2-ES run
        if [ $rcp = "rcp85" -a -e $fdirOUT/"tasmin_Amon_HadGEM2-ES_rcp85_r1i1p1_1861-2099.nc" ]; then
            cdo -s splitsel,2868 $fdirOUT/"tasmin_Amon_HadGEM2-ES_rcp85_r1i1p1_1861-2099.nc" tmp
            rm -f $fdirOUT/"tasmin_Amon_HadGEM2-ES_rcp85_r1i1p1_1861-2099.nc"
            cp tmp000000.nc $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099.nc"
            rm -f tmp*
        fi

		#########################
		#Check for RCP models that still exist because they had no matching historical dataset and were not cleaned up in the previous step
		echo 'Ignore ls: cannot access (means all RCPs were matched with a historical)'
		modelNoHistorical=( $( ls $fdirOUT/"tasmin_Amon_"*"_"*"_"*"_2006-2099.nc" ) )

		#Loop through these RCP files with no historical match and delete them
        echo "Removing RCP models with no historical match for " $rcp
		for fullName in ${modelNoHistorical[@]}; do;
			#Find the model and the ensemble id
			echo "No history files exists for " $fullName
			
			#Clean up
			rm -rf $fullName
		done

		#########################
		#Clip Daymet
		echo "Rescaling DAYMET data and calculating long-term monthly mean"
		if [ ! -f $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc ]; then				
			#Select wusa for DAYMET
			cdo sellonlatbox,-124.25,-102.25,31.75,49 $fdirDAYMET/tmin_wusa_1980-2014.nc4 $fdirKatieDAYMET/tmin_wusa_1980-2014_daily_latlon_monthly-wusa.nc

			#Shift to same grid
			cdo -s -f nc remapbil,$fdirGRID/wusa_1km $fdirKatieDAYMET/tmin_wusa_1980-2014_daily_latlon_monthly-wusa.nc $fdirKatieDAYMET/tmin_wusa_1980-2014_daily_latlon_monthly-wusa_shift.nc

			#Calculate monthly means of Daymet observations for 1980-1999 observation period
			cdo -s seldate,1980-01-01,1999-12-31 $fdirKatieDAYMET/tmin_wusa_1980-2014_daily_latlon_monthly-wusa_shift.nc $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_tmp.nc
			cdo -s ymonmean $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_tmp.nc $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_ymonmean_allparam.nc
			
			#Select param -3
			cdo selparam,-1 $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_ymonmean_allparam.nc $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc
			
			#Clean up
			rm -rf $fdirKatieDAYMET/tmin_wusa_1980-2014_daily_latlon_monthly-wusa.nc
			rm -rf $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_tmp.nc
			rm -rf $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_ymonmean_allparam.nc
			rm -rf $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly-wusa_shift.nc
		fi

        #########################
		#List all the input netcdf files for the merged 1861-2099 runs
		modelListEns=( $( ls $fdirOUT*$rcp*1861-2099.nc ) )

ls $fdirOUT*$rcp*1861-2099.nc
		#Select wusa for CMIP5
        echo "Clipping CMIP5 to wusa"
		for fullName in ${modelListEns[@]}; do;
			#Find the model and the ensemble id
			echo $fullName |cut -d'_' -f3 | read model
			echo $fullName |cut -d'_' -f5 | read ensemble
			
			#Reproject to 0.5 degrees to standardize the lat/lon vectors
			cdo -s remapbil,$fdirGRIDglobal/cru_05deg $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_05deg.nc"
			
			#Clip to the wusa
			cdo -s sellonlatbox,-124.25,-102.25,31.75,49 $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_05deg.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa.nc"
			
			#Remove global version
			rm -f $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099.nc"
			rm -f $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_05deg.nc" 
		done
		
        #########################
		#Re-List all the input netcdf files for the merged 1861-2099 runs for wusa
		modelListEnswusa=( $( ls $fdirOUT*$rcp*1861-2099-wusa.nc ) )

		#Start the bias correction - the bias correction is only done for all ensembles
		for fullName in ${modelListEnswusa[@]}; do;
			#Find the model and the ensemble id
			echo $fullName |cut -d'_' -f3 | read model
			echo $fullName |cut -d'_' -f5 | read ensemble
            echo Final processing of: $model

			#########################
			#List all the input netcdf ensemble files for the model
			modelListConv=( $( ls $fdirOUT*'_'$model'_'*1861-2099-wusa.nc ) )

			#Calculate the number of ensembles for the model
			ensembleLength=0
			for ensembName in ${modelListConv[@]}; do;
				((ensembleLength=ensembleLength+1))
			done

			#Use the differences approach
			echo "Using the differences approach to calculate anomalies for temperature for ensemble " $ensemble
			if [ ! -f $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_1861-2099_global-cor.nc" ]; then	
				#########################
				#A couple of models have 2 variables by mistake (eg the GFDL model) - solution is to split all ncdfs
				#If the ncdf is correct, then the first split is taken and the second fails
				#If the ncdf has 2 variabes, then the second split is taken and the first written over/deleted
                if [ $model = "GFDL-CM3" ]; then
                    cdo -s splitcode $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099"
                    mv $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-01.nc"* $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa.nc"
                    rm -f $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-02.nc"*
                fi

				#########################
				#Regrid the 0.5 degree GCM climate data to the grid Daymet 1KM
				cdo -s remapbil,$fdirGRID/wusa_1km $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-1km.nc"

                #########################
				#Bias correct the GCM data
				#Calculate monthly means of GCM simulations for 1980-1999 observation period
				cdo -s seldate,1980-01-01,1999-12-31 $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-1km.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1981-1999-1km.nc"
				cdo -s ymonmean $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1981-1999-1km.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1981-1999-1km-ymonmean.nc"

				#Calculate correction factor where positive means Daymet temperature is higher than GCM and negative means daymet temperature is less than GCM
				#Use -b 64 to ensure successful subtraction for all grids - some fail without this
				cdo -s -b 64 sub $fdirKatieDAYMET/tmin_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc  $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1981-1999-1km-ymonmean.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1981-1999-1km-cf.nc"
				
				#########################
				#Apply correction factor to simulation so that low GCM values are increased and high GCM values are reduced
				cdo -s ymonadd $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-1km.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1981-1999-1km-cf.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-1km-cor_global.nc"

				#########################
				#If at end of ensembles/end of model list
				if [ $fullName = ${modelListConv[${#modelListConv[@]}-1]} ]; then
					#Get first ensemble name for backup info - make sure to select r1 first (not r10)
					echo ${modelListConv[0]} |cut -d'_' -f5 | read firstEnsemble
                    if [ $ensembleLength -gt 1 ]; then
                        let i=1
                        while [ ${#firstEnsemble} -eq 7 ]; do
                            echo ${modelListConv[$i]} |cut -d'_' -f5 | read firstEnsemble
                            let i=$i+1
                        done
                    fi

					#Only calculate ensemble mean if you have an ensemble (> more than 1)
					if [ $ensembleLength -gt 1 ]; then
						########################
						#Calculate the ensemble mean for uncorrected
						cdo -s ensmean $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1861-2099-1km.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_1861-2099_globalEns-uncor.nc"

						#Calculate the ensemble mean for corrected
						cdo -s ensmean $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-cor_global.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_1861-2099_globalEns-cor.nc"
					else
						########################
						#Copy the ensemble for uncorrected - can select all ensembles because there is just one... but keep an eye for problems
						cp $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1861-2099-1km.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_1861-2099_globalEns-uncor.nc"

						#Copy the ensemble for corrected - can select all ensembles because there is just one... but keep an eye for problems
						cp $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-cor_global.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_1861-2099_globalEns-cor.nc"
					fi
					
					#########################
					#Back up the first ensemble member ( should be r1 ... with r10 & higher selected after lower ensemble r members)
					cp $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$firstEnsemble"_1861-2099-1km-cor_global.nc" $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_1861-2099_"$firstEnsemble"_global-cor.nc"
					
					#########################
					#Clean up
					#rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099.nc"		#Keep this file to check ensemble variability
					rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa.nc"		#Remove the clipped 0.5 degree CMIP5 file
					rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1981-1999-1km.nc"				#Remove the non-bias corrected 1km CMIP5 file
					rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1981-1999-1km-ymonmean.nc"	#Remove the 1km monthly means for the CMIP5 file
					rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1981-1999-1km-cf.nc"			#Remove the correction factor file
					rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1861-2099-1km.nc"				#Remove all the uncorrected 1km ensembles but keep orginal res. ensemble
					rm -rf $fdirOUT/"tasmin_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-cor_global.nc"	#Remove all the corrected ensembles but keep first ensemble
				fi	# end if for ensemble means
			fi	# end if for subtraction methods
		done
done
