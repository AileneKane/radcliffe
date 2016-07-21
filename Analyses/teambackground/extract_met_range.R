# Creating a function to extract species Ranges to make it easier to parrallelize
# Note: This takes 3-houlry GLDAS 2.0 data (1949-2010) and calculates daily 
#       tmin, tmax, tavg, and precip
# Arguments:
#  1. species    -- name of the species; must match the codes in dir.ranges
#  2. yr.min     -- the first year you want to extra data for
#  3. yr.max     -- the last year you want to extract data for
#  4. dir.out    -- file path to where you want the output daily met rasters to be written
#  5. dir.ranges -- file path to the directory containing the shapefiles of the Little's ranges
#  6. compress   -- whether you want the files to be compressed as you go along or not (T/F) 
#
# Returns: single .nc file per year containing the following variables;
# ** NOTE: Because of size concerns, the output is not stored in memory
#  1. tmean  - daily mean temperature    (K)
#  2. tmin   - daily minimum temperature (K) 
#  3. tmax   - daily maximum temperature (K)
#  4. precip - daily total precipitation (kg m-2 day-1)

extract.range.met <- function(species, yr.min=1949, yr.max=2010, dir.out, dir.ranges, compress=T){
  # libraries you'll need
  library(raster); library(rgdal); library(sp); library(maptools); library(maps)
  library(ncdf4)
  library(ggplot2)
  library(lubridate)
  library(RCurl)
  library(XML)
  library(stringr)
  
  # Creating a vector of timestamps
  timestamps <- seq(0, 2100, by=300)
  timestamps <- str_pad(timestamps, 4, pad="0")

  # Create the output folder
  spp.out <- file.path(dir.out, species)
  if(dir.exists(spp.out)){
    # If we already have some output for the species, 
    # figure out which year we need to start with
    files.done <- dir(spp.out, ".nc")
    
    # Find the year of the last file that was done & start there
    yr.min <- as.numeric(substr(strsplit(files.done[length(files.done)], "_")[[1]][3],1,4))
  } else {
    dir.create(spp.out, recursive=T, showWarnings=F)  
  }
  
  
  # Get the species
  # Reading in the species shape file
  species1 <- readShapePoly(file.path(dir.ranges, species, paste0(species, ".shp")))
  
  # Making a template raster for the shapefile
  ext.template <- round(extent(species1), 0) + c(-1, 1, -1, 1) # GLDAS is in 0.25 degree resolution; # adding a buffer
  rast.template <- raster(ext=ext.template, crs=CRS("+proj=longlat"), resolution=0.25) # 
  
  # Converting the shapefile into a raster
  species.rast <- rasterize(species1, rast.template)
  
  # Storing the lat/lon vectors to make life easier
  spp.lat <- seq(ext.template[4]-0.125, ext.template[3], by=-0.25)
  spp.lon <- seq(ext.template[1]+0.125, ext.template[2], by=0.25)
  
  # Now converting that raster into an array that we can use to mask CRUNCEP data
  spp.mask <- as.array(species.rast)[,,1]
  dimnames(spp.mask) <- list(lat=spp.lat, lon=spp.lon)
  spp.mask <- t(spp.mask)
  
  
  # Make a figure of the species range for reference
  # - lets us see the range and how the mask is different from Little's range
  png(file.path(spp.out, paste0("Range_", species, ".png")), height=8, width=8, units="in", res=180)
  plot(species.rast)
  plot(species1, add=T)
  map("state", col="red", add=T)
  dev.off()
  
  # -------------------------------
  # Get data for each year
  # -------------------------------
  # CRUNCEP Path
  # dap_base <- "http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/" # CRUNCEP
  
  # GLDAS Path:
  # http://hydro1.gesdisc.eosdis.nasa.gov/opendap/GLDAS/GLDAS_NOAH025_3H.2.0/1948/001/GLDAS_NOAH025_3H.A19480101.0300.020.nc4 # Example file
  dap_base <- "http://hydro1.gesdisc.eosdis.nasa.gov/opendap/GLDAS/GLDAS_NOAH025_3H.2.0/"
  for(year in yr.min:yr.max){
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
      # print(doy2)
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
        nc.now <- nc_open(file.path(dap_base, year, doy2, file.now))
        
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
    loc.file <- file.path(spp.out, paste0(species, "_gldas2.0_", year, ".nc"))
    loc <- nc_create(filename=loc.file, vars=var.list)
    for(v in names(var.list)){
      ncvar_put(nc=loc, varid=var.list[[v]], vals=out.yr[[v]])
    }
    nc_close(loc)
  } # end year loop
  # -------------------------------
  
  # Compress the files as we go to make life easier
  if(compress==T){
    setwd(dir.out) # Go to the output directory so we don't get annoying file paths
    system(paste0("tar -jcvf ", species, ".tar.bz2 ", species)) # Compress the folder
    system(paste0("rm -rf ", species)) # remove the uncompressed folder
    setwd(dir.base) # Go back to our base directory
  }
}