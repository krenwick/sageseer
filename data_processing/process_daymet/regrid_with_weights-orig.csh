;----------------------------------------------------------------------
; daymet_3.ncl
;
; Concepts illustrated:
;   - Reading a Daymet netCDF file containg one 2x2 tile with 1km x 1km data
;   - Use ESMF to interpolate to a rectilinear grd
;   - Write netCDF of regridded variable
;
;   - Assumes v6.1.0 or newer
;----------------------------------------------------------------------
; The file used was obtained via:
; wget --limit-rate=3m http://daymet.ornl.gov/thredds/fileServer/allcf/2010/12100_2010/prcp.nc -O prcp.2010_12100.nc
;----------------------------------------------------------------------

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

;======================================================================
; The main code
;======================================================================

;--- User settings
vname = "dayl"                               ; Daymet variable name
year  =  1980                                ; year

method = "conserve"                         ;--- ESMF regrid method
; "conserve", "bilinear", "patch"

netCDF = True                               ; create netCDF of variable

;---Input data (Daymet) file containing the tile data and grid description
            DataDirName      = "/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/"
DataFileName = vname+"_"+year+"_monthly.nc4"
DataPathName = DataDirName+DataFileName

;---netCDF file to contain the 'SCRIP style' description of the source grid
            srcDirName      = "/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/"
srcGridName  = "DaymetGridInfo."+vname+"_monthly_tmp.nc4"
srcPathName  = srcDirName+srcGridName

;---netCDF file to contain the 'SCRIP style' description of the destination grid
            dstDirName      = "/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/"
dstGridName  = "RegridGridInfo."+vname+"_monthly_tmp.nc4"
dstPathName  = dstDirName+dstGridName

;---netCDF file containing the weights, created in previous step using ESMF_regrid
wgtDirName      = "/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/scripts_daymetprocessing/scripts/"
wgtFileName = "Wgt.Daymet_to_Rect.nc4"
wgtPathName  = wgtDirName+wgtFileName

;---Options
Opt                = True
Opt@SrcTitle       = "Daymet Variable ("+vname+") to rectlinear"   ; optional
Opt@ForceOverwrite = True
Opt@LargeFile = True

;---Open data source file
sfile          = addfile(DataPathName,"r")

;---Get the source file data (Daymet) lat/lon grid: used for src grid description
lat2d = sfile->lat
lon2d = sfile->lon

dim2d = dimsizes(lat2d)
nlat  = dim2d(0)
mlon  = dim2d(1)

;---Get the Daymet source variable
var   = sfile->$vname$         ; (time, y, x)

;---Define assorted source grid (Daymet) options
Opt@SrcGridLat   = lat2d
Opt@SrcGridLon   = lon2d
Opt@SrcRegional  = True
Opt@SrcFileName  = srcPathName              ; grid description of source grid

Opt@SrcMask2D    = where(.not.ismissing(var(0,:,:)),1,0)  ; identify valid data

;---Create the destination lat/lon grid
minlat = min(lat2d)             ;-- retrieve minimum latitude value
minlon = min(lon2d)             ;-- retrieve maximum latitude value
maxlat = max(lat2d)             ;-- retrieve minimum longitude value
maxlon = max(lon2d)             ;-- retrieve maximum longitude value

Opt@DstGridType  = "rectilinear"
Opt@DstGridType    = "0.008333333deg"
Opt@DstLLCorner    =  (/minlat,minlon/)               ;-- min lat and lon for sub-region
Opt@DstURCorner    =  (/maxlat,maxlon/)               ;-- max lat and lon for sub-region
Opt@DstRegional  = True
Opt@DstFileName  = dstPathName        ; grid description of source grid

;---Set method
Opt@InterpMethod = method
Opt@Debug = True
var_regrid = ESMF_regrid_with_weights(var,wgtPathName,Opt)     ; Do the regridding for 'var'

;---Info needed if 'netCDF' is True
dimv = dimsizes(var)
ntim = dimv(0)
nlat = dimv(1)
mlon = dimv(2)

dimvr = dimsizes(var_regrid)
NLAT  = dimvr(1)
MLON  = dimvr(2)

delete(var_regrid@grid_mapping)       ; not for regridded variable

if (netCDF) then
;----------------------------------------------------------------------
; Write regridded variable to netCDF
;----------------------------------------------------------------------
RegridDirName      = "/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/"
RegridFileName = vname+"_"+year+"_monthly_latlon.nc"
RegridPathName = RegridDirName +RegridFileName

;---Assign original 'time' to regridded variable
var_regrid!0   = "time"
var_regrid&time= var&time

;---Create auxilary time variable for user convenience
;yyyyddd        = year + ispan(1,ntim,1)
;yyyyddd@long_name = "year and day of current year (YYYYDDD)"
;yyyyddd!0      = "time"
;yyyyddd&time   =  var&time

;yyyymmdd       = yyyymmdd_time(2014, 2014, "integer") ;FEB 2014: THIS MUST BE HARDWIRED to create a 365 day year
;delete(yyyymmdd&time)                    ; type integer ... will be changed
;delete(yyyymmdd@units)                   ; non-standard
;yyyymmdd@long_name = "current date (YYYYMMDD)"
;yyyymmdd&time      = var&time            ; type double

;---Write regridded variable and auxilary time variables to file
system("/bin/rm -f "+RegridPathName)     ; delete any pre-existing file
rgrd_nc   = addfile(RegridPathName, "c") ; open for writing

global    = True
global@creation_date = systemfunc("date")
global@remap_method  = method
global@remap         = "NCL: ESMF_regrid"
global@title         = "REMAPPED Daymet: "+vname
fileattdef( rgrd_nc, global )         ; copy file attributes

filedimdef( rgrd_nc,"time",-1,True)   ; force an unlimited dimension

;rgrd_nc->yyyyddd   = yyyyddd

;rgrd_nc->yyyymmdd  = yyyymmdd
rgrd_nc->$vname$   = var_regrid
end if
