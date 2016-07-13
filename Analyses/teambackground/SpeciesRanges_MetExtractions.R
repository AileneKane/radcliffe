# Script to extract historical met data from species ranges
# Author: Christy Rollinson
# Contact: crollinson@gmail.com

# Note: Right now, I'm using the CRUNCEP for the tradeoff of extent & resolution
#  - It's also very standard for a lot of climatology work
#  - Downside: Only goes through 2010
#  Native Spatial Res: 0.5 x 0.5 degree
#  Native Spatial Ext: Global
#  Native Temporal Res: 6-hrly 
#  Native Temporal Extent: 1901-2010
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
setwd(dir.base)

# libraries you'll need
library(raster); library(rgdal); library(sp); library(maptools); library(maps)
library(ncdf4)
library(ggplot2)
library(lubridate)

# dir.met <- "~/Desktop/BEST_TempGrids/" # Note: I'm using BEST because it goes back furthest and goes into Canada
dir.ranges <- "~/Desktop/little_ranges/" # Note: this is the same folder as on github, but on the desktop to save space & not having it sync
dir.out <- file.path(dire.base, "output", "SpeciesMet")
dir.create(dir.out, recursive=T)

# Base file path for pulling files remotely from web
dap.base <- "http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/"

# Getting a list of the species we'll extract met for
species <- dir(dir.ranges); 
species <- species[!substr(species, nchar(species)-3, nchar(species))==".zip"] # exclude the zipped files

# Looping through each species
for(i in 1:length(species)){
  # Store the species ID
  spp.now <- species[i]
  
  print(paste0("** ", spp.now, " (", i, "/", length(species), ")"))
  
  # Create the output folder
  spp.out <- file.path(dir.out, spp.now)
  dir.create(spp.out, recursive=T)
  
  # Get the species
  # Reading in the species shape file
  species1 <- readShapePoly(file.path(dir.ranges, spp.now, paste0(spp.now, ".shp")))
  
  # Making a template raster for the shapefile
  ext.template <- round(extent(species1), 0) + c(-1, 1, -1, 1) # CRUNCEP is in 0.5 degree resolution; # adding a buffer
  rast.template <- raster(ext=ext.template, crs=CRS("+proj=longlat"), resolution=0.5) # 
  
  # Converting the shapefile into a raster
  species.rast <- rasterize(species1, rast.template)
  
  # Now converting that raster into an array that we can use to mask CRUNCEP data
  spp.mask <- as.array(species.rast)[,,1]
  dimnames(spp.mask) <- list(lat=seq(ext.template[4]-0.25, ext.template[3], by= -0.5), lon=seq(ext.template[1]+0.25, ext.template[2], by=0.5))
  spp.mask <- t(spp.mask)
  
  # Storing the lat/lon vectors to make life easier
  spp.lat <- as.numeric(dimnames(spp.mask)[[2]])
  spp.lon <- as.numeric(dimnames(spp.mask)[[1]])
  
  # Make a figure of the species range for reference
  # - lets us see the range and how the mask is different from Little's range
  png(file.path(spp.out, paste0("Range_", spp.now, ".png")), height=8, width=8, units="in", res=180)
  plot(species.rast)
  plot(species1, add=T)
  map("state", col="red", add=T)
  dev.off()
  
  # Get data for each year
  for(year in 1901:2010){
    print(paste0("   -- ", year))
    # Figure out if we're dealing with a leap year
    nday = ifelse(lubridate:: leap_year(year), 366, 365)
    ntime=nday*4
    
    # Construct the file names
    f.tair <- paste0("mstmip_driver_global_hd_climate_tair_", year, "_v1.nc4")
    f.ppt <- paste0("mstmip_driver_global_hd_climate_rain_", year, "_v1.nc4")
    
    # Open the files
    tair.nc <- nc_open(file.path(dap.base, f.tair))
    ppt.nc <- nc_open(file.path(dap.base, f.ppt))
    
    # Extract the lat/lon index (assume precip is same as temp)
    cru.lat <- ncvar_get(tair.nc, "lat") 
    cru.lon <- ncvar_get(tair.nc, "lon")
    
    # Figuring out what lat/lon we need
    lon.start <- which(cru.lon == spp.lon[1]) # Lon goes min to max
    lat.start <- which(cru.lat == spp.lat[1]) # lat goes max to min
    
    # Extract the actual data
    tair <- ncvar_get(tair.nc, varid="tair", start=c(lon.start, lat.start, 1), count=c(dim(spp.mask)[1], dim(spp.mask)[2], ntime))
    ppt <- ncvar_get(ppt.nc, varid="rain", start=c(lon.start, lat.start, 1), count=c(dim(spp.mask)[1], dim(spp.mask)[2], ntime))
    dimnames(tair) <- list(lon=cru.lon[lon.start:(lon.start + dim(spp.mask)[1]-1)], lat=cru.lat[lat.start:(lat.start + dim(spp.mask)[2]-1)])
    dimnames(ppt) <- list(lon=cru.lon[lon.start:(lon.start + dim(spp.mask)[1]-1)], lat=cru.lat[lat.start:(lat.start + dim(spp.mask)[2]-1)])
    
    # Removing any values that aren't part of the species mask
    # - I think this works but there's a loop below in case it doesn't
    tair[,,] <- ifelse(is.na(spp.mask), NA, tair)
    ppt[,,] <- ifelse(is.na(spp.mask), NA, ppt)
  
    # for(j in 1:dim(tair2)[1]){
    #   # Going through and masking each longitude at a time
    #   tair2[j,,] <- ifelse(is.na(mask.array[,j]), NA, tair2[j,,])
    # }
    
    # Going from 6-hourly to daily; times 4 is because 4 timestamps per day
    tair.day <- array(dim=c(dim(tair)[1:2], nday))
    ppt.day <- array(dim=c(dim(ppt)[1:2], nday))
    for(j in 1:nday){
      tair.day[,,j] <- apply(tair[,,(j*4-3):(j*4)], c(1,2), mean) # mean temp
      ppt.day[,,j] <- apply(ppt[,,(j*4-3):(j*4)], c(1,2), sum) # total precip
    }
    
    # Making this into an array that makes visual sense
    tair.day <- aperm(tair.day, c(2,1,3))
    ppt.day <- aperm(ppt.day, c(2,1,3))
    
    # Saving the file; 1 file per year
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=spp.lat, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=spp.lon, create_dimvar=TRUE)
    time <- ncdim_def(name='doy', units="day", vals=1:nday, create_dimvar=TRUE, unlim=TRUE)
    dim.dat=list(lat,lon,time)
    
    # Putting data together to make life easier
    var.list <- list()
    dat.list <- list()
    
    var.list[["tmean"]] <- ncvar_def(name="tmean", longname="daily mean temperature", units="K", dim=dim.dat, missval=-999)
    var.list[["precip"]] <- ncvar_def(name="precip", longname="daily total precipitation", units="kg m-2 (= mm)", dim=dim.dat, missval=-999)
  
    ## put data in new file
    loc.file <- file.path(spp.out, paste0(spp.now, "_cruncep_", year, ".nc"))
    loc <- nc_create(filename=loc.file, vars=var.list)
    ncvar_put(nc=loc, varid="tmean", vals=tair.day)
    ncvar_put(nc=loc, varid="precip", vals=ppt.day)
    nc_close(loc)
  } # end year loop
  
  # Compress the files as we go to make life easier
  setwd(dir.out) # Go to the output directory so we don't get annoying file paths
  system(paste0("tar -jcvf ", spp.now, ".tar.bz2 ", spp.now)) # Compress the folder
  system(paste0("rm -rf ", spp.now)) # remove the uncompressed folder
  setwd(dir.base) # Go back to our base directory
} # end species loop