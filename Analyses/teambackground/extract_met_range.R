# Creating a function to extract species Ranges to make it easier to parrallelize
# Note: This now runs from GLDAS day that has been downloaded locally & aggregated 
#       to a daily timestep
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

extract.range.met.day <- function(species, yr.min=1949, yr.max=2010, dir.dat, dir.out, dir.ranges, compress=T){
  # libraries you'll need
  library(raster); library(rgdal); library(sp); library(maptools); library(maps)
  library(ncdf4)
  library(ggplot2)
  library(lubridate)
  library(RCurl)
  library(XML)
  library(stringr)
  
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
  spp.lat <- seq(ext.template[3]+0.125, ext.template[4], by=0.25)
  spp.lon <- seq(ext.template[1]+0.125, ext.template[2], by=0.25)
  
  # Now converting that raster into an array that we can use to mask CRUNCEP data
  spp.mask <- as.array(species.rast)[,,1]
  dimnames(spp.mask) <- list(lat=spp.lat[order(spp.lat, decreasing=T)], lon=spp.lon)
  spp.mask <- t(spp.mask)
  
  # # Flipping the dims of spp.mask so that latitude goes from small to big, 
  # #  like it will in GLDAS
  # spp.mask <- spp.mask[order(dimnames(spp.mask)[[1]]),order(dimnames(spp.mask)[[2]])]
  spp.mask <- spp.mask[,order(dimnames(spp.mask)[[2]])]
  # spp.lat <- spp.lat[order(spp.lat)]
  # spp.lon <- spp.lon[order(spp.lon)]
    
  
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
  for(year in yr.min:yr.max){
    print(paste0("   -- ", year))
    # Figure out if we're dealing with a leap year
    nday = ifelse(lubridate:: leap_year(year), 366, 365)
    ntime=nday

    # We'll write out 1 grid per year
    out.yr <- list()
    
    # Open the files
    nc.now <- nc_open(file.path(dir.dat, paste0("GLDAS2.0_day_", year, ".nc")))

    # pull lat/lon just to be safe
    # Extract the lat/lon index (assume precip is same as temp)
    raw.lat <- ncvar_get(nc.now, "latitude")
    raw.lon <- ncvar_get(nc.now, "longitude")

    # Figuring out what lat/lon we need
    lon.start <- which(raw.lon == min(spp.lon)) # Lon goes min to max
    lat.start <- which(raw.lat == min(spp.lat)) # lat goes min to max

    # Extract the actual data
    out.yr[["tmean" ]] <- ncvar_get(nc.now, varid="tmean", start=c(lon.start, lat.start,1), count=c(length(spp.lon), length(spp.lat),nday))
    out.yr[["tmin"  ]] <- ncvar_get(nc.now, varid="tmin", start=c(lon.start, lat.start,1), count=c(length(spp.lon), length(spp.lat),nday))
    out.yr[["tmax"  ]] <- ncvar_get(nc.now, varid="tmax", start=c(lon.start, lat.start,1), count=c(length(spp.lon), length(spp.lat),nday))
    out.yr[["precip"]] <- ncvar_get(nc.now, varid="precip", start=c(lon.start, lat.start,1), count=c(length(spp.lon), length(spp.lat),nday))

    # # # Mask the aggregated data
    for(d in 1:dim(out.yr$tmean)[3]){
      out.yr[["tmean" ]][,,d] <- ifelse(is.na(spp.mask), NA, out.yr[["tmean" ]][,,d])
      out.yr[["tmin"  ]][,,d] <- ifelse(is.na(spp.mask), NA, out.yr[["tmin"  ]][,,d])
      out.yr[["tmax"  ]][,,d] <- ifelse(is.na(spp.mask), NA, out.yr[["tmax"  ]][,,d])
      out.yr[["precip"]][,,d] <- ifelse(is.na(spp.mask), NA, out.yr[["precip"]][,,d])
    }
    # Close file
    nc_close(nc.now)

    # Saving the file; 1 file per year
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=spp.lat, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=spp.lon, create_dimvar=TRUE)
    time <- ncdim_def(name='doy', units="day", vals=1:nday, create_dimvar=TRUE, unlim=TRUE)
    dim.dat=list(lon,lat,time)

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