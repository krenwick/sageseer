#!/bin/ksh
#Jun 2016, katie.renwick (at) gmail.com
#DOES: 
#1. Clips GCM time series to future period (1970-2099)
#2. Clips to historic period (1980-1999)
#3. Converts files to zipped, f32 format to cut down on size

#SBATCH -N 1
#SBATCH -n 1
#SBATCH -J clip
#SBATCH -o clip_%j.out
#SBATCH -e clip_%j.err 
#SBATCH -p priority
#SBATCH --mail-user katie.renwick@gmail.com
#SBATCH --mail-type=ALL
#SBATCH -t 30:00:00

#################
#Set directory locations
fdirIn=/mnt/lustrefs/store/katie.renwick/sageseer_GCM/
fdirOutF=/mnt/lustrefs/store/katie.renwick/sageseer_futureclim/
fdirOutH=/mnt/lustrefs/store/katie.renwick/sageseer_historic/

#########################
modelList=( $( ls $fdirIn*nc ) )

for fullName in ${modelList[@]}; do;

			#Find the model and the ensemble id
			echo $fullName |cut -d'/' -f7 | read var1
			echo $var1 |cut -d'_' -f1 | read variable
			echo $fullName |cut -d'_' -f4 | read model
			echo $fullName |cut -d'_' -f5 | read rcp
            echo $fullName |cut -d'_' -f7 | read ensemble

		cdo -f nc4 -b f32 -z zip selyear,2070/2099 $fdirIn/$variable"_Amon_"$model"_"$rcp"_"* $fdirOutF/$variable"_Amon_"$model"_"$rcp"_"$ensemble"_2070-2099.nc4"
		cdo -f nc4 -b f32 -z zip selyear,1980/1999 $fdirIn/$variable"_Amon_"$model"_"$rcp"_"* $fdirOutH/$variable"_Amon_"$model"_"$rcp"_"$ensemble"_1980-1999.nc4"

done

