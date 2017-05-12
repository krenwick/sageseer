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
        	vname        = "srad"
            year       = 2014

                method       = "bilinear"
; "conserve", "bilinear", "patch"

netCDF = True                               ; create netCDF of variable

;---Input data (Daymet) file containing the tile data and grid description
DataDirName      = "/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/"
DataFileName = vname+"_"+year+"_monthly.nc4"
DataPathName = DataDirName+DataFileName

;---netCDF file to contain the 'SCRIP style' description of the source grid
srcDirName      = "/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/Processed/"
srcGridName  = "DaymetGridInfo.nc4"
srcPathName  = srcDirName+srcGridName

;---netCDF file to contain the 'SCRIP style' description of the destination grid
            dstDirName      = "/mnt/lustrefs/store/katie.renwick/Climate_LPJGUESS/Daymet3/data/Processed/"
dstGridName  = "RegridGridInfo.nc4"
dstPathName  = dstDirName+dstGridName

;---Options
Opt                = True
Opt@SrcTitle       = "Daymet 2x2 Tile ("+vname+") to rectlinear"   ; optional
Opt@WgtFileName    = "Wgt.Daymet_to_Rect.nc4"
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
;Opt@DstGridLat   = lat
;Opt@DstGridLon   = lon
Opt@DstGridType    = "0.008333333deg"
Opt@DstLLCorner    =  (/minlat,minlon/)               ;-- min lat and lon for sub-region
Opt@DstURCorner    =  (/maxlat,maxlon/)               ;-- max lat and lon for sub-region
Opt@DstRegional  = True
Opt@DstFileName  = dstPathName        ; grid description of source grid

;---Set method
Opt@InterpMethod = method
Opt@Debug = True
var_regrid = ESMF_regrid(var,Opt)     ; Do the regridding for 'var'

;---Info needed if 'netCDF' is True
dimv = dimsizes(var)
ntim = dimv(0)
nlat = dimv(1)
mlon = dimv(2)

dimvr = dimsizes(var_regrid)
NLAT  = dimvr(1)
MLON  = dimvr(2)

delete(var_regrid@grid_mapping)       ; not for regridded variable