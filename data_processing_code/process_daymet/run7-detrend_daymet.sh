#!/bin/bash

########################
# Detrend Daymet climate data
# Modified from KE's original by KMR, Jan 2015

# Directory
cd ../data/Processed/merged_data/wusa

#cdo selparam,ID IN-FILE OUT-FILE
cdo -f nc selparam,-1 tmean_wusa_1980-2014_zip.nc4 tmean.nc
cdo -f nc selparam,-1 srad_24hrinstant_wusa_1980-2014_zip.nc4 srad.nc
cdo -f nc selparam,-1 wetd_wusa_1980-2014.nc4 wetd.nc
cdo -f nc selparam,-1 prcp_wusa_1980-2014_zip.nc4 prcp.nc

#cdo trend ifile interceptfile slopefile
cdo trend tmean.nc tmean_int.nc tmean_slope.nc
cdo trend srad.nc srad_int.nc srad_slope.nc
cdo trend wetd.nc wetd_int.nc wetd_slope.nc
cdo trend prcp.nc prcp_int.nc prcp_slope.nc 

#Change intercept to zero (so only slope is detrended)
cdo ifthenc,0 tmean_int.nc temp_t.nc
cdo ifthenc,0 srad_int.nc temp_s.nc
cdo ifthenc,0 wetd_int.nc temp_w.nc
cdo ifthenc,0 prcp_int.nc temp_p.nc

#Rename temp to intercept file (all zeros)
mv temp_t.nc tmean_int.nc
mv temp_s.nc srad_int.nc
mv temp_w.nc wetd_int.nc
mv temp_p.nc prcp_int.nc

#cdo subtrend ifile interceptfile slopefile outfile
cdo subtrend tmean.nc tmean_int.nc tmean_slope.nc tmean_wusa_1980-2014_DETRENDED.nc
cdo subtrend srad.nc srad_int.nc srad_slope.nc srad_wusa_24hrinstant_1980-2014_DETRENDED.nc
cdo subtrend wetd.nc wetd_int.nc wetd_slope.nc wetd_wusa_1980-2014_DETRENDED.nc
cdo subtrend prcp.nc prcp_int.nc prcp_slope.nc prcp_wusa_1980-2014_DETRENDED.nc

#set negative values to zero for precipitation and wetd/wetdays
cdo setrtomiss,-99,0 wetd_wusa_1980-2014_DETRENDED.nc wetd_temp.nc
cdo setmisstoc,0 wetd_temp.nc wetd_wusa_1980-2014_DETRENDED_noneg.nc
cdo setrtomiss,-99,0 prcp_wusa_1980-2014_DETRENDED.nc prcp_temp.nc
cdo setmisstoc,0 prcp_temp.nc prcp_wusa_1980-2014_DETRENDED_noneg.nc

#remove temporary files
rm tmean_int.nc srad_int.nc wetd_int.nc prcp_int.nc
rm tmean_slope.nc srad_slope.nc wetd_slope.nc prcp_slope.nc
rm tmean.nc srad.nc wetd.nc prcp.nc
rm prcp_temp.nc wetd_temp.nc srad_temp.nc
rm prcp_gye_1980-2014_DETRENDED.nc
rm wetd_gye_1980-2014_DETRENDED.nc

