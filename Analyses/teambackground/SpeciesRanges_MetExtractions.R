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
# dir.base <- "~/Desktop/Research/Radcliffe_Phenology/radcliffe/Analyses/teambackground/"
setwd(dir.base)

# libraries you'll need
library(raster); library(rgdal); library(sp); library(maptools); library(maps)
library(ncdf4)
library(ggplot2)
library(lubridate)
library(RCurl)
library(XML)
library(stringr)

# dir.met <- "~/Desktop/BEST_TempGrids/" # Note: I'm using BEST because it goes back furthest and goes into Canada
dir.ranges <- "input/little_ranges/" # Note: this is the same folder as on github, but on the desktop to save space & not having it sync
dir.out <- file.path(dir.base, "output", "SpeciesMet")
# dir.ranges <- "~/Desktop/little_ranges/" # Note: this is the same folder as on github, but on the desktop to save space & not having it sync
# dir.out <- file.path("~/Desktop", "SpeciesMet")
dir.create(dir.out, recursive=T, showWarnings=F)

# Base file path for pulling files remotely from web
# dap.base <- "http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/" # CRUNCEP

# GLDAS Path:
# http://hydro1.gesdisc.eosdis.nasa.gov/opendap/GLDAS/GLDAS_NOAH025_3H.2.0/1948/001/GLDAS_NOAH025_3H.A19480101.0300.020.nc4 # Example file
dap_base <- "http://hydro1.gesdisc.eosdis.nasa.gov/opendap/GLDAS/GLDAS_NOAH025_3H.2.0/"

# Getting a list of the species we'll extract met for
species <- dir(dir.ranges); 
species <- species[!substr(species, nchar(species)-3, nchar(species))==".zip"] # exclude the zipped files

# Creating a vector of timestamps
timestamps <- seq(0, 2100, by=300)
timestamps <- str_pad(timestamps, 4, pad="0")
# Looping through each species
#  -- Note: this is slower than workign with 1 raster for all species, but easier to manage hard disk space
for(i in 1:length(species)){
    # Store the species ID
  spp.now <- species[i]
  
  print(paste0("** ", spp.now, " (", i, "/", length(species), ")"))
  
  # Create the output folder
  spp.out <- file.path(dir.out, spp.now)
  dir.create(spp.out, recursive=T, showWarnings=F)
  
  # Get the species
  # Reading in the species shape file
  species1 <- readShapePoly(file.path(dir.ranges, spp.now, paste0(spp.now, ".shp")))
  
  # Making a template raster for the shapefile
  ext.template <- round(extent(species1), 0) + c(-1, 1, -1, 1) # CRUNCEP is in 0.5 degree resolution; # adding a buffer
  rast.template <- raster(ext=ext.template, crs=CRS("+proj=longlat"), resolution=0.25) # 
  
  # Converting the shapefile into a raster
  species.rast <- rasterize(species1, rast.template)
  
  
  # Storing the lat/lon vectors to make life easier
  spp.lat <- seq(ext.template[4]-0.125, ext.template[3], by= -0.25)
  spp.lon <- seq(ext.template[1]+0.125, ext.template[2], by=0.25)
  
  # Now converting that raster into an array that we can use to mask CRUNCEP data
  spp.mask <- as.array(species.rast)[,,1]
  dimnames(spp.mask) <- list(lat=spp.lat, lon=spp.lon)
  spp.mask <- t(spp.mask)
  
  
  # Make a figure of the species range for reference
  # - lets us see the range and how the mask is different from Little's range
  png(file.path(spp.out, paste0("Range_", spp.now, ".png")), height=8, width=8, units="in", res=180)
  plot(species.rast)
  plot(species1, add=T)
  map("state", col="red", add=T)
  dev.off()
  
  # Get data for each year
  #  -- Note: this is hard-coded, which is generally bad, but I'm being lazy for the sake of 
  #           just getting it done
  for(year in 1949:1950){
    print(paste0("   -- ", year))
    # Figure out if we're dealing with a leap year
    nday = ifelse(lubridate:: leap_year(year), 366, 365)
    ntime=nday

    # We'll write out 1 grid per year 
    out.yr <- list()
    out.yr[["tmean" ]] <- array(dim=c(length(spp.lat), length(spp.lon), nday))
    out.yr[["tmax"  ]] <- array(dim=c(length(spp.lat), length(spp.lon), nday))
    out.yr[["tmin"  ]] <- array(dim=c(length(spp.lat), length(spp.lon), nday))
    out.yr[["precip"]] <- array(dim=c(length(spp.lat), length(spp.lon), nday))
    
    # Go by each day
    for(doy in 1:nday){
      doy2 <- str_pad(doy, 3, pad="0")
      print(doy2)
      # Extract each hour into a 3-d array
      tmp <- array(dim=c(length(spp.lat), length(spp.lon), length(timestamps))) # Third dimenions = 8 because there should be 8 3-hr files per day
      ppt <- array(dim=c(length(spp.lat), length(spp.lon), length(timestamps)))
      
      # Some date indexing
      date.now <- as.Date(doy, origin=as.Date(paste0(year-1,"-12-31")))
      mo.now <- str_pad(month(date.now), 2, pad="0")
      day.mo <- str_pad(day(date.now), 2, pad="0")
      
      for(hr in 1:length(timestamps)){
        # Construct the file names
        # GLDAS_NOAH025_3H.A19480101.0300.020.nc4
        file.now <- paste0("GLDAS_NOAH025_3H.A", year, mo.now, day.mo, ".", timestamps[hr], ".020.nc4")
        
        # Open the files
        nc.now <- nc_open(file.path(dap.base, year, doy2, file.now))
                
        # pull lat/lon just to be safe
        # Extract the lat/lon index (assume precip is same as temp)
        raw.lat <- ncvar_get(nc.now, "lat") 
        raw.lon <- ncvar_get(nc.now, "lon")
        
        # Figuring out what lat/lon we need
        lon.start <- which(raw.lon == spp.lon[1]) # Lon goes min to max
        lat.start <- which(raw.lat == spp.lat[1]) # lat goes max to min
        
        # Extract the actual data        
        tmp[,,hr] <- t(ncvar_get(nc.now, varid="Tair_f_inst", start=c(lon.start, lat.start,1), count=c(length(spp.lon), length(spp.lat),1)))
        ppt[,,hr] <- t(ncvar_get(nc.now, varid="Rainf_f_tavg", start=c(lon.start, lat.start,1), count=c(length(spp.lon), length(spp.lat),1)))        
        
        # Close file
        nc_close(nc.now)
      }
            
      # Mask the aggregated data
      # Removing any values that aren't part of the species mask
      # - I think this works but there's a loop below in case it doesn't
      tmp[,,] <- ifelse(is.na(spp.mask), NA, tmp)
      ppt[,,] <- ifelse(is.na(spp.mask), NA, ppt)
      
      # for(j in 1:dim(tair2)[1]){
      #   # Going through and masking each longitude at a time
      #   tmp[j,,] <- ifelse(is.na(mask.array[,j]), NA, tmp[j,,])
      #   ppt[j,,] <- ifelse(is.na(mask.array[,j]), NA, ppt[j,,])
      # }
      
      # Aggregate to day & store it in output list
      out.yr[["tmean" ]][,,doy] <- apply(tmp, c(1,2), FUN=mean)
      out.yr[["tmax"  ]][,,doy] <- apply(tmp, c(1,2), FUN=max )
      out.yr[["tmin"  ]][,,doy] <- apply(tmp, c(1,2), FUN=min )
      out.yr[["precip"]][,,doy] <- apply(ppt, c(1,2), FUN=sum ) # ?? need to change to kg m-2 per day?? =*60*60*24 
      
    }
    
#     # Making this into an array that makes visual sense
#     tair.day <- aperm(tair.day, c(2,1,3))
#     ppt.day <- aperm(ppt.day, c(2,1,3))
    
    # Saving the file; 1 file per year
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=spp.lat, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=spp.lon, create_dimvar=TRUE)
    time <- ncdim_def(name='doy', units="day", vals=1:nday, create_dimvar=TRUE, unlim=TRUE)
    dim.dat=list(lat,lon,time)
    
    # Putting data together to make life easier
    var.list <- list()
    
    var.list[["tmean" ]] <- ncvar_def(name="tmean" , longname="daily mean temperature"   , units="K", dim=dim.dat, missval=-999)
    var.list[["tmin"  ]] <- ncvar_def(name="tmin"  , longname="daily minimum temperature", units="K", dim=dim.dat, missval=-999)
    var.list[["tmax"  ]] <- ncvar_def(name="tmax"  , longname="daily maximum temperature", units="K", dim=dim.dat, missval=-999)
    var.list[["precip"]] <- ncvar_def(name="precip", longname="daily total precipitation", units="kg m-2 (= mm/m2)", dim=dim.dat, missval=-999)
  
    ## put data in new file
    loc.file <- file.path(spp.out, paste0(spp.now, "_gldas2.0_", year, ".nc"))
    loc <- nc_create(filename=loc.file, vars=var.list)
    for(v in names(var.list)){
      ncvar_put(nc=loc, varid=var.list[[v]], vals=out.yr[[v]])
    }
    nc_close(loc)
  } # end year loop
  
  # Compress the files as we go to make life easier
  setwd(dir.out) # Go to the output directory so we don't get annoying file paths
  system(paste0("tar -jcvf ", spp.now, ".tar.bz2 ", spp.now)) # Compress the folder
  system(paste0("rm -rf ", spp.now)) # remove the uncompressed folder
  setwd(dir.base) # Go back to our base directory
} # end species loop