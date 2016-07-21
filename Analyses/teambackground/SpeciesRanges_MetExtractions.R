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
# dir.base <- "~/radcliffe/Analyses/teambackground/"
dir.base <- "~/Dropbox/Radcliffe_Phenology/radcliffe/Analyses/teambackground/"
setwd(dir.base)

# dir.met <- "~/Desktop/BEST_TempGrids/" # Note: I'm using BEST because it goes back furthest and goes into Canada
# dir.ranges <- "input/little_ranges/" # Note: this is the same folder as on github, but on the desktop to save space & not having it sync
# dir.out <- file.path(dir.base, "output", "SpeciesMet")
dir.ranges <- "~/Desktop/little_ranges/" # Note: this is the same folder as on github, but on the desktop to save space & not having it sync
dir.out <- file.path("~/Desktop", "SpeciesMet")
dir.create(dir.out, recursive=T, showWarnings=F)

# Getting a list of the species we'll extract met for
species <- dir(dir.ranges); 
species <- species[!substr(species, nchar(species)-3, nchar(species))==".zip"] # exclude the zipped files

# Figure out which species are completely done
species.done <- dir(dir.out, ".tar.bz2")
species.done <- substr(species.done, 1, 8)

# exclude done species from the list
species <- species[!species %in% species.done]

# Store species as a list to feed into a parallel script
species.list <- as.list(species); names(species.list) <- species

# Source the extraction function 
source("extract_met_range.R")

# # NOTE: trying to extract in parallel made my computer freak-out.  Probably because of the CURL operations
# # Load the parallel package and execute the extraction in parallel
# library(parallel)
# # mclapply(species.list, FUN=extract.range.met, mc.cores=min(length(species.list), 12),  yr.min=1949, yr.max=2010, dir.out=dir.out, dir.ranges=dir.ranges, compress=T)
# mclapply(species.list, FUN=extract.range.met, mc.cores=2,  yr.min=1949, yr.max=1950, dir.out=dir.out, dir.ranges=dir.ranges, compress=T)

# So instead we're just going to stick with the really old-school loop
for(i in 1:length(species)){
  spp.now = species[i]
  extract.range.met(species=spp.now, yr.min=1949, yr.max=2010, dir.out=dir.out, dir.ranges=dir.ranges, compress=T)
}

