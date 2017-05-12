#!/bin/ksh
#June 2013, changed 1860 to 1861 in Oct 2013 to match NBP, fixed buggy model simulations in March 2012
#Original script written by benjamin.poulter@lsce.ipsl.fr
# script updated June 2016 by Katie Renwick to downscale CMIP5 to 1-km for sagebrush project
#This script processes CMIP5 precipitation data
#	- Merges historical and future data
#	- Rescales the precipitation data to mm per year
#	- Regrids precipitation data to Daymet 1-km grid
#	- Bias corrects precipitation data using Daymet precipitation
#Outputs
	# Corrected first ensemble member at 1-km resolution
#NOTE: The WSL approach calculates precipitation anomalies for the GCM and adds these to the Daymet baseline. The WSL approach solves for the issue where the baseline may have zero precipitation...
#...and the future may have precipitation. It does not appear to solve for when the baseline may have zero precipitation but the GCM does have precipitation. Should discuss with Dirk Schmatz at WSL.
#NOTE: simulations should use just the first ensemble member because variability gets dampened for the ensemble mean
#NOTE: the threshold was changed from 0.005 to 10 to avoid problems from IPSL in S temperate regions where >500% increases in PPT (from 1 to 2 mm) over 100 years caused problems when CRU precip was 85mm and IPSL was < 15

#########################
#Set directories for datasets (the RCP directory is defined in the loop)
fdirCMIP5hist='/mnt/lustrefs/store/benjamin.poulter/poulterlab/Climate/Original/Global/CMIP5/pr/historical/'
fdirDAYMET='/mnt/lustrefs/store/benjamin.poulter/poulterlab/Climate/Processed/Regional/Wusa/daymet/unzippedcompression/'
fdirKatieDAYMET='/mnt/lustrefs/work/katie.renwick/activescripts/tempclim/' # need cause can't write temp files to Ben's directory
fdirGRID='/mnt/lustrefs/work/katie.renwick/activescripts/tempclim/'
fdirGRIDglobal='/mnt/lustrefs/store/benjamin.poulter/poulterlab/Climate/Original/Global/grid/'


#########################
#List the RCPs and the Variables and the time steps
set -A rcpArray rcp45 rcp85

#########################
#Select whether to use the WSL approach or simple approach
ratioApproach=TRUE
wslApproach=FALSE # KMR Note: this isn't working. Odd shape to output files.

#########################
#Loop through RCP
for rcp in ${rcpArray[@]}; do;

		#########################
		#Set directories for input and output datasets
		#For input for RCP data
		fdirCMIP5rcp='/mnt/lustrefs/store/benjamin.poulter/poulterlab/Climate/Original/Global/CMIP5/pr'/$rcp/

		#For output
		#Default folder (so as not to delete home directory if not properly specified)
		echo "Making output directory"
		if [ $wslApproach = "TRUE" ]; then
            fdirOUT='/mnt/lustrefs/store/katie.renwick/Climate/Processed/Regional/wusa/CMIP5/pr'/$rcp'-wsl'/
		fi
		
		if [ $ratioApproach = "TRUE" ]; then
            fdirOUT='/mnt/lustrefs/store/katie.renwick/Climate/Processed/Regional/wusa/CMIP5/pr'/$rcp'-ratio'/
		fi

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
		
		#USE THESE LINES TO SELECT INDIVIDUAL MODELS
		modelListHist=( $( ls $fdirCMIP5hist*_CESM1-CAM5_historical_r1i1p1_*nc
		ls $fdirCMIP5hist*_MPI-ESM-LR_historical_r1i1p1_*nc
		ls $fdirCMIP5hist*_HadGEM2-AO_historical_r1i1p1_*nc
		ls $fdirCMIP5hist*_GISS-E2-H-CC_historical_r1i1p1_*nc))
		ls $fdirCMIP5hist*_CCSM4_historical_r2i1p1_*nc ) )	
		modelListRcp=( $( ls $fdirCMIP5rcp*_CESM1-CAM5_*_r1i1p1_*nc 
		ls $fdirCMIP5rcp*_MPI-ESM-LR_*_r1i1p1_*nc
		ls $fdirCMIP5rcp*_HadGEM2-AO_*_r1i1p1_*nc
		ls $fdirCMIP5rcp*_GISS-E2-H-CC_*_r1i1p1_*nc))
		ls $fdirCMIP5rcp*_CCSM4_*_r2i1p1_*nc))

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
                if [ ! -f $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_1861-2005.nc" ]; then
                    #########################
                    #Merge the historical GCM run time slices for the same ensemble
                    echo $fullName
                    cdo -s mergetime $fdirCMIP5hist/"pr_Amon_"$model"_historical_"$ensemble"_"* $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_allyears.nc"

                    #Select 1861-2005 time period. But with conditional for specific model problems - and force to clip all Hadley runs to end in November 2011
                    if [ $enddate = "200511" -o $model = "HadGEM2-ES" ]; then
                        echo Special Case: $fullName
                        #Clip the merged GCM runs to 1861-2005 (November)
                        cdo -s seldate,1861-01-01,2005-11-31 $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_allyears.nc" $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_1861-2005.nc"
                    elif [ $enddate != "200511" ]; then
                        #Clip the merged GCM runs to 1861-2005 (December)
                        cdo -s seldate,1861-01-01,2005-12-31 $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_allyears.nc" $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_1861-2005.nc"
                    fi

                    #Clean up
                    rm -f $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_allyears.nc"
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
			if [ ! -f $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc" ]; then
				#########################
				#Merge the RCP climate data for the same ensemble, with special case for CCSM4 and HadGEM2-AO which have repeated files for the same ensemble
                if [ $model != "HadGEM2-AO" ]; then
                    echo Normal Merge: $fullName
                    cdo -s mergetime $fdirCMIP5rcp/"pr_Amon_"$model"_"$rcp"_"$ensemble"_"* $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
                elif [ $model = "HadGEM2-AO" ]; then
                    echo Special Case: $fullName
                    cdo -s mergetime $fdirCMIP5rcp/"pr_Amon_"$model"_"$rcp"_"$ensemble"_"*209912* $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
                fi

                #Clip the dates to 2006-2099 and print result to track that ntimesteps == 2868
				if [ $startdate != "200512" ]; then
					#Clip the merged GCM runs to 2006-2099
					cdo -s seldate,2006-01-01,2099-12-31 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc"
				fi
				if [ $startdate = "200512" ]; then
					#Clip the merged GCM runs to 2006-2099
					cdo -s seldate,2005-12-01,2099-12-31 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc"
				fi
				
				#Clean up
				rm -f $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_allyears.nc"
			fi
		done

		#########################
		#List all the merged input netcdf files for the historical run
		modelListHistClean=( $( ls $fdirOUT*historical*1861-2005.nc ) )
		
		#Merge the time periods for each historical and future model X ensemble for the RCP period
		#Convert to mm day
		echo "Merging historical and RCP climatologies for each ensemble"
		for fullName in ${modelListHistClean[@]}; do;
			#Find the model and the ensemble id
			echo $fullName |cut -d'_' -f3 | read model
			echo $fullName |cut -d'_' -f5 | read ensemble
			
            #Merge the historical and future GCM runs ONLY if the RCP exists
            echo $fullName

            if [ -f $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc" ]; then
                cdo mergetime $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_1861-2005.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099kgsec.nc"
                cdo mulc,86400 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099kgsec.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099kgday.nc"
                cdo muldpm $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099kgday.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099mm.nc"
            else
                echo "No " $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc" " exists for the historical merge"
            fi
				
            #Clean up
            rm -rf $fdirOUT/"pr_Amon_"$model"_historical_"$ensemble"_1861-2005.nc"		#Remove old historical
            rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_2006-2099.nc"			#Remove old RCP
            rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099kgsec.nc"		#Remove scaled version 1
            rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099kgday.nc"		#Remove scaled version 2
		done

		#########################
		#Check for RCP models that still exist because they had no matching historical dataset and were not cleaned up in the previous step
		echo 'Ignore ls: cannot access (means all RCPs were matched with a historical)'
		modelNoHistorical=( $( ls $fdirOUT/"pr_Amon_"*"_"*"_"*"_2006-2099.nc" ) )

		#Loop through these RCP files with no historical match and delete them
        echo "Removing RCP models with no historical match for " $rcp
		for fullName in ${modelNoHistorical[@]}; do;
			#Find the model and the ensemble id
			echo "No history files exists for " $fullName
			
			#Clean up
			rm -rf $fullName
		done
		
		#########################
		#Clip Daymet - daymet precipitation is mm/month and matches the CMIP5 precipitation
		echo "Rescaling DAYMET data and calculating long-term monthly mean"
		if [ ! -f $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc ]; then				
			#Select wusa for DAYMET
			cdo sellonlatbox,-124.25,-102.25,31.75,49 $fdirDAYMET/prcp_wusa_1980-2014.nc4 $fdirKatieDAYMET/prcp_wusa_1980-2014_daily_latlon_monthly-wusa.nc

			#Shift to same grid
			cdo -s -f nc remapbil,$fdirGRID/wusa_1km $fdirKatieDAYMET/prcp_wusa_1980-2014_daily_latlon_monthly-wusa.nc $fdirKatieDAYMET/prcp_wusa_1980-2014_daily_latlon_monthly-wusa_shift.nc

			#Calculate monthly means of CRU observations for 1961-1990 observation period
			cdo -s seldate,1980-01-01,1999-12-31 $fdirKatieDAYMET/prcp_wusa_1980-2014_daily_latlon_monthly-wusa_shift.nc $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_tmp.nc
			cdo -s ymonmean $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_tmp.nc $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean_allparam.nc
			
			#Select param -1
			cdo selparam,-1 $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean_allparam.nc $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc
			
			#Clean up
			rm -rf $fdirKatieDAYMET/prcp_wusa_1980-2014_daily_latlon_monthly-wusa.nc
			rm -rf $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_tmp.nc
			rm -rf $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean_allparam.nc
			rm -rf $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly-wusa_shift.nc
		fi

        #########################
		#List all the input netcdf files for the merged 1861-2099 mm runs
		modelListEns=( $( ls $fdirOUT*$rcp*1861-2099mm.nc ) )

		#Select wusa for CMIP5
        echo "Clipping CMIP5 to wusa"
		for fullName in ${modelListEns[@]}; do;
			#Find the model and the ensemble id
			echo $fullName |cut -d'_' -f3 | read model
			echo $fullName |cut -d'_' -f5 | read ensemble
			
			#Reproject to 0.5 degrees to standardize the lat/lon vectors
			cdo -s remapbil,$fdirGRIDglobal/cru_05deg $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_05deg.nc"
			
			#Clip to the wusa
			cdo -s sellonlatbox,-124.25,-102.25,31.75,49 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_05deg.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc"
			
			#Remove global version
			rm -f $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099.nc"
			rm -f $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099_05deg.nc" 
		done
		
        #########################
		#Re-List all the input netcdf files for the merged 1861-2099 runs for wusa
		modelListEnswusa=( $( ls $fdirOUT*$rcp*1861-2099-wusa-mm.nc ) )

		#Start the bias correction
		for fullName in ${modelListEnswusa[@]}; do;
			#Find the model and the ensemble id
			echo $fullName |cut -d'_' -f3 | read model
			echo $fullName |cut -d'_' -f5 | read ensemble
            echo Final processing of: $model

			#########################
			#List all the input netcdf ensemble files for the model
			modelListConv=( $( ls $fdirOUT*'_'$model'_'*1861-2099-wusa-mm.nc ) )

			#Calculate the number of ensembles for the model
			ensembleLength=0
			for ensembName in ${modelListConv[@]}; do;
				((ensembleLength=ensembleLength+1))
			done

			#Use the more complex WSL approach
			if [ $wslApproach = "TRUE" ]; then
				echo "Using the WSL approach to calculate anomalies for precipitation for ensemble " $ensemble
				if [ ! -f $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_global-cor.nc" ]; then
				
					#Set up filenames
					scrain=$fdirOUT'scrain-pranom.nc'
					scnorain=$fdirOUT'scnorain-pranom.nc'
					bprain=$fdirOUT'bprain-pranom.nc'
					bpnorain=$fdirOUT'bpnorain-pranom.nc'
					rain=$fdirOUT'rain-pranom.nc'
					norain=$fdirOUT'norain-pranom.nc'
					norainval=$fdirOUT'norainval-pranom.nc'
					newrain=$fdirOUT'newrain-pranom.nc'
					newrainval=$fdirOUT'newrainval-pranom.nc'
					raingone=$fdirOUT'raingone-pranom.nc'
					raingoneval=$fdirOUT'raingoneval-pranom.nc'

					tmp1=$fdirOUT'tmp1-pranom.nc'
					tmp2=$fdirOUT'tmp2-pranom.nc'
					tmp3=$fdirOUT'tmp3-pranom.nc'
					tmp4=$fdirOUT'tmp4-pranom.nc'
					tmp5=$fdirOUT'tmp5-pranom.nc'
					tmp6=$fdirOUT'tmp6-pranom.nc'
					tmp7=$fdirOUT'tmp7-pranom.nc'
					tmp8=$fdirOUT'tmp8-pranom.nc'
					tmp9=$fdirOUT'tmp9-pranom.nc'
					tmp10=$fdirOUT'tmp10-pranom.nc'
					tmp11=$fdirOUT'tmp11-pranom.nc'
					tmp12=$fdirOUT'tmp12-pranom.nc'
					tmp13=$fdirOUT'tmp13-pranom.nc'
					tmp14=$fdirOUT'tmp14-pranom.nc'
					ofileAnom1=$fdirOUT'anom1.nc'
					ofileAnom2=$fdirOUT'anom2.nc'
					ofileAnom3=$fdirOUT'anom3.nc'
					ofileAnom4=$fdirOUT'anom4.nc'
					ofileAnom5=$fdirOUT'anom5.nc'
					ofileAnom6=$fdirOUT'anom6.nc'
					ofileAnom7=$fdirOUT'anom7.nc'
					ofileAnom8=$fdirOUT'anom8.nc'
					ofileAnom9=$fdirOUT'anom9.nc'
					ofileAnom10=$fdirOUT'anom10.nc'
					ofileAnom11=$fdirOUT'anom11.nc'
					ofileAnom12=$fdirOUT'anom12.nc'
					ofileAnom13=$fdirOUT'anom13.nc'

                    #########################
                    #A couple of models have 2 variables by mistake (eg the GFDL model) - solution is to split all ncdfs
                    #If the ncdf is correct, then the first split is taken and the second fails
                    #If the ncdf has 2 variabes, then the second split is taken and the first written over/deleted
                    if [ $model = "GFDL-CM3" ]; then
                        cdo -s splitcode $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm"
                        mv $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm-01.nc"* $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc"
                        rm -f $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm-02.nc"*
                    fi
					
					#Calculate monthly means of GCM simulations for 1961-1990 observation period
					cdo -s seldate,1980-01-01,1999-12-31 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-wusa-mm.nc"
					cdo -s ymonmean $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-wusa-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-wusa-mm-ymonmean.nc"

					#The WSL approach does a more detailed correction to consider errors that occur when the CRU data and the scenario data have mismatches in rain and no rain months
					#Calculate rain and norain for GCM base period (bp) and the GCM entire projection (sc) with a 0.005 mm threshold

					# decision matrix
        			# +--------+-----------------------+-----------------------+
        			# |    \SC |      <= th            |     > th              |
        			# |BP   \  |                       |                       |
        			# +========+=================+=====+=======================+
        			# |        |            ano  | cor |           ano  | cor  |
        			# +        +-----------------+-----+----------------+------+
        			# |  <= th | norain:   -100% |  0  | newrain: +100% |   2  |
        			# +--------+-----------------+-----+----------------+------+
        			# |  > th  | raingone: -100% |  0  | rain:     calc |Aratio|
        			# +--------+-----------------+-----+----------------+------+
        			#
        			# BP: base period in GCMs SC: scenario time series period, th: threshold, Aratio = RELATIVE anomalies by ratio
        			#Changed to 10.0mm from 0.005 mm (March 2012)
					#Changed to 0.1mm from 10mm (January 2015, Zhen)

					#The WSL approach does a more detailed correction to consider errors that occur when the CRU data and the scenario data have mismatches in rain and no rain months
					#Calculate rain and norain for GCM base period (bp) and the GCM entire projection (sc) with a 0.005 mm threshold
					#Changed to 10.0mm from 0.005 mm (March 2012)
					cdo -s gtc,10.0 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-wusa-mm-ymonmean.nc" $bprain
					cdo -s lec,10.0 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-wusa-mm-ymonmean.nc" $bpnorain
                    cdo -s gtc,10.0 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc" $scrain
                    cdo -s lec,10.0 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc" $scnorain

					#Calculate agreement for rain larger than 10mm threshold for bp and sc (yes = 1, no = 0)
					#Result: If both the base period and scenario have rain, then 1
					cdo -s ymonmul $scrain $bprain $rain
	   
					#Calculate agreement (between bp and sc) for rain below threshold (where yes = -100%, no = 0)
					#Result: If both the base period and scenario have NO rain, then -100
					cdo -s ymonmul $scnorain $bpnorain $norain
					cdo -s mulc,-100 $norain $norainval
	 
					#Find new rain in the sc (scenario) file compared to the bp file (yes = 100%, no = 0)
					cdo -s ymonmul $scrain $bpnorain $newrain
					cdo -s mulc,100 $newrain $newrainval

					#Find where rain is gone in sc file compared to the bc file (yes = -100%, no = 0)
					cdo -s ymonmul $scnorain $bprain $raingone
					cdo -s mulc,-100 $raingone $raingoneval
		  
                    #ZZ now using the equation: RELATIVE anomalies = 100*(val/(base+thresh) - 1)
                    # empirically best values for threshold: PRECIP: 0.005
                    #set thres 0.005
                    cdo mulc,100 -subc,1 -ymondiv $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc" -addc,0.005 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-wusa-mm-ymonmean.nc" $tmp3

					#Now apply the special newrain and raingone cases as filters to the percent change in precipitation (tmp3)
					cdo -s mul $rain $tmp3 $tmp4							#e.g., filter scenario with no rain if there was no rain in the base period
					cdo -s ifthenelse $norain $norainval $tmp4 $tmp5		#e.g., if no rain, select -100, else select scenario rain
					cdo -s ifthenelse $newrain $newrainval $tmp5 $tmp6		#e.g., if new rain, select 100, else select scenario rain
					cdo -s ifthenelse $raingone $raingoneval $tmp6 $tmp7	#e.g., if rain gone, select -100, else select scenario rain
					cdo -s setvrange,-100,10000 $tmp7 $tmp8                 #Set valid ranges for the % anomaly (from tmp3 mainly)
					cdo -s setmisstoc,10000 $tmp8 $ofileAnom1				#Set miss to 10000=101
					
					cdo -s remapbil,$fdirGRID/wusa_1km $ofileAnom1 $ofileAnom2 #Regrid the anomalies for the model
					cdo -s divc,100 $ofileAnom2 $ofileAnom3			#Scale percent to fraction
					cdo -s addc,1 $ofileAnom3  $ofileAnom4			#Add 1 so that the range is 0 to >2 (e.g., -100%=0, + 100%=2)

					#1. extreme event could occur at pixels with correcting factor larger than 8
					cdo -s gtc,8 $ofileAnom4 $ofileAnom5

					#2. determine if this extreme event is caused by abnormal value (divor close to 0.1)
					# The abnormal value could occur at the pixel in climate average (BP) lower than 1 mm (or set it to 0.5, which will be more tolerant to extreme event)
					cdo -s lec,1 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-wusa-mm-ymonmean.nc" $tmp9
					
					#Regrid the monthly average for the model, using bilinear interopolation algorithm
					cdo -s remapbil,$fdirGRID/wusa_1km $tmp9 $tmp10
					cdo -s mulc,0 $tmp10 $tmp11
					cdo -s addc,1 $tmp11 $tmp12
					cdo -s ifthenelse $tmp10 $tmp12 $tmp11 $tmp13
					#get map by multiple this two flag together
					cdo -s ymonmul $ofileAnom5 $tmp13 $ofileAnom6
					#set the correcting factor to 2 for these pixels
					cdo -s mulc,2 $ofileAnom6 $ofileAnom7
					#add the corrected factor to map
					cdo -s ifthenelse $ofileAnom6 $ofileAnom7 $ofileAnom4 $ofileAnom8
					
					#find the abnormal value with precipitation higher than 3000mm
					cdo -s ymonmul $ofileAnom8 $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc $ofileAnom9
					cdo -s gtc,3000 $ofileAnom9 $ofileAnom10
					
					#get the raining region in CRU
					cdo -s gtc,0 $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc $tmp14
					#multiple this two flag together 
					cdo -s ymonmul $ofileAnom10 $tmp14 $ofileAnom11
					#set the correcting factor to 2 for the abnormal pixel
					cdo -s mulc,2 $ofileAnom11 $ofileAnom12
					#add the corrected factor to map
					cdo -s ifthenelse $ofileAnom11 $ofileAnom12 $ofileAnom8 $ofileAnom13
					
					#calculate the final result										
					cdo -s ymonmul $ofileAnom13 $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099mm-1km-cor_global.nc"	#Add the anomalies to CRU baseline

					#Regrid the original GCM climate data to the CRU grid (0.5 degrees), do this just to have the uncorrected data ensemble mean
					cdo -s remapbil,$fdirGRID/wusa_1km $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099mm-1km.nc"
					
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
							cdo -s ensmean $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099mm-1km.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_globalEns-uncor.nc"

							#Calculate the ensemble mean for corrected
							cdo -s ensmean $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099mm-1km-cor_global.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_globalEns-cor.nc"
						else
							########################
							#Copy the ensemble for uncorrected - can select all ensembles because there is just one... but keep an eye for problems
							cp $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099mm-1km.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_globalEns-uncor.nc"

							#Copy the ensemble for corrected - can select all ensembles because there is just one... but keep an eye for problems
							cp $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099mm-1km-cor_global.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_globalEns-cor.nc"
						fi
						
						#########################
						#Back up the first ensemble member to be used for simulations etc...
						cp $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$firstEnsemble"_1861-2099mm-1km-cor_global.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_"$firstEnsemble"_global-cor.nc"

						#########################
						#Clean up
						#rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099mm-"*
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1961-1990-1km-mm-ymonmean.nc"
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1961-1990-1km-mm.nc"
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099mm-1km.nc"	#Remove all the uncorrected 05deg ensembles but keep orginal res. ensemble
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099mm-1km-cor_global.nc"	#Remove all the corrected ensembles but keep first ensemble
						rm -rf $01.nc
						rm -rf $scrain
						rm -rf $scnorain
						rm -rf $bprain
						rm -rf $bpnorain
						rm -rf $rain
						rm -rf $norain
						rm -rf $norainval
						rm -rf $newrain
						rm -rf $newrainval
						rm -rf $raingone
						rm -rf $raingoneval
						rm -rf $tmp1
						rm -rf $tmp2
						rm -rf $tmp3
						rm -rf $tmp4
						rm -rf $tmp5
						rm -rf $tmp6
						rm -rf $tmp7
						rm -rf $tmp8
						rm -rf $tmp9
						rm -rf $tmp10
						rm -rf $tmp11
						rm -rf $tmp12
						rm -rf $tmp13
						rm -rf $tmp14
						rm -rf $ofileAnom1
						rm -rf $ofileAnom2
						rm -rf $ofileAnom3
						rm -rf $ofileAnom4
						rm -rf $ofileAnom5
						rm -rf $ofileAnom6
						rm -rf $ofileAnom7
						rm -rf $ofileAnom8
						rm -rf $ofileAnom9
						rm -rf $ofileAnom10
						rm -rf $ofileAnom11
						rm -rf $ofileAnom12
						rm -rf $ofileAnom13
												
					fi	# end if for ensemble means
				fi #end if for wslApproach methods
			fi #end if for wslApproach query
				
			#Or, use the simpler ratios approach
			if [ $ratioApproach = "TRUE" ]; then
				echo "Using the ratios approach to calculate anomalies for precipitation for ensemble " $ensemble
				if [ ! -f $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_global-cor.nc" ]; then	
					#########################
					#A couple of models have 2 variables by mistake (eg the GFDL model) - solution is to split all ncdfs
					#If the ncdf is correct, then the first split is taken and the second fails
					#If the ncdf has 2 variabes, then the second split is taken and the first written over - this is reversed for GFDL-CM3
					cdo splitcode $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm"
					if [[ $model != "GFDL-CM3" ]]; then
						mv $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm-01.nc2" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc"
						mv $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm-02.nc2" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc"
					fi
					if [[ $model = "GFDL-CM3" ]]; then
						mv $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm-02.nc2" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc"
						mv $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm-01.nc2" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc"
					fi
										
					#########################
					#Regrid the GCM climate data to the Daymet grid (1-km)
					cdo -s remapbil,$fdirGRID/wusa_1km $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-wusa-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-1km-mm.nc"

					#########################
					#Bias correct the GCM data
					#Calculate monthly means of GCM simulations for 1961-1990 observation period
					cdo -s seldate,1980-01-01,1999-12-31 $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-1km-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-1km-mm.nc"
					cdo -s ymonmean $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-1km-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-1km-mm-ymonmean.nc"

					#Calculate correction factor where > 1 means daymet precipitation is higher than GCM and < 1 means CRU precipitation is less than GCM
					#Use -b 64 to ensure successful division for all grids - some fail without this
					cdo -b 64 div $fdirKatieDAYMET/prcp_wusa_1980-1999_daily_latlon_monthly_ymonmean.nc  $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-1km-mm-ymonmean.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-1km-mm-cf.nc"
					
					#########################
					#Apply correction factor to simulation so that low GCM values are increased and high GCM values are reduced
					cdo -s ymonmul $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-1km-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1961-1990-1km-mm-cf.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099-1km-mm-cor_global.nc"

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
							cdo -s ensmean $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_global-uncorEns.nc"

							#Calculate the ensemble mean for corrected
							cdo -s ensmean $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-mm-cor_global.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_globalEns-cor.nc"
						else
							########################
							#Copy the ensemble for uncorrected - can select all ensembles because there is just one... but keep an eye for problems
							cp $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-mm.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_globalEns-uncor.nc"

							#Copy the ensemble for corrected - can select all ensembles because there is just one... but keep an eye for problems
							cp $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-mm-cor_global.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_globalEns-cor.nc"
						fi
						
						#########################
						#Back up the first ensemble member to be used for simulations etc...
						cp $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$firstEnsemble"_1861-2099-1km-mm-cor_global.nc" $fdirOUT/"pr_Amon_"$model"_"$rcp"_1861-2099_"$firstEnsemble"_global-cor.nc"
						
						#########################
						#Clean up
						#rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"$ensemble"_1861-2099mm.nc"	#Keep this file to check ensemble variability
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-mm.nc"		#This is the same file as previous, but at 0.5 degree resolution
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-mm-"*
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1961-1990-1km-mm.nc"
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1961-1990-1km-mm-ymonmean.nc"
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1961-1990-1km-mm-cf.nc"
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1961-1990-1km-mm-cor_global.nc"
						rm -rf $fdirOUT/"pr_Amon_"$model"_"$rcp"_"*"_1861-2099-1km-mm-cor_global.nc"
					fi	# end if for ensemble means
				fi	# end if for ratioApproach methods
			fi	#end if for ratioApproach query
		done
done
