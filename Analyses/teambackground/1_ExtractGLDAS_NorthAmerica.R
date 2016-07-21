# Script to extract historical met data from species ranges
# Author: Christy Rollinson
# Contact: crollinson@gmail.com

# Note: After consultation, Using GLDAS v2.0 -- 1948-2010
#  - Downside: Only goes through 2010
#  Native Spatial Res: 0.25 x 0.25 degree
#  Native Spatial Ext: Global
#  Native Temporal Res: 3-hrly 
#  Native Temporal Extent: 1948-2010
#  Alternatives that I'm aware of and work with regularly:
#    - CRUNCEP*: Global, 6 hrly, 0.5 degree, 1901-2010
#    - BEST: Global, daily, 1.0 degree, 1980-2010 #  URL: http://berkeleyearth.org/data/
#    - NLDAS*: North America-ish (US + S. Canada + N. Mexico), hourly, 0.125 degree, 1979-present
#    - GLDAS*: Global, 3 hourly, 0.25 degree, 1948-present
#    - PRISM: US, daily, 4 km, 1981-present (monthly 1895-present)
#    - Daymet: North America-ish (US + S. Canada + N. Mexico), daily, 1 km, 1980-present
#    * indicates that these datasets provide the full suite of standard meteorological variables:
#        - temperature, preciptiation, shortwave radiation, longwave radiation, pressure, humidity, wind

rm(list=ls())
dir.base <- "~/radcliffe/Analyses/teambackground/"
# dir.base <- "~/Dropbox/Radcliffe_Phenology/radcliffe/Analyses/teambackground/"
setwd(dir.base)

dir.out <- "/projectnb/dietzelab/paleon/met_ensemble/data/paleon_domain/GLDAS_day"
dir.create(dir.out, recursive=T, showWarnings=F)

# Source the extraction function 
source("extract_gldas_region_day.R")

extract.gldas.region(xmin=-170, xmax=-50, ymin=10, ymax=75, yr.min=1949, yr.max=2010, dir.out=dir.out, dir.ranges=dir.ranges, compress=F)
