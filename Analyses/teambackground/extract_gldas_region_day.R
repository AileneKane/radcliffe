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

extract.gldas.region <- function(xmin, xmax, ymin, ymax, yr.min=1949, yr.max=2010, dir.out, compress=T){
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
  # -------------------------------
  # Get data for each year
  # -------------------------------
  # check for years we already have
  yrs.done <- dir(path.out, ".nc")
  for(i in 1:length(yrs.done)){
    yrs.done[i] <- as.numeric(substr(strpslit(yrs.done[i], "_")[[1]][3],1,4))
  }

  
  # CRUNCEP Path
  # dap_base <- "http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/" # CRUNCEP

  # GLDAS Path:
  # http://hydro1.gesdisc.eosdis.nasa.gov/opendap/GLDAS/GLDAS_NOAH025_3H.2.0/1948/001/GLDAS_NOAH025_3H.A19480101.0300.020.nc4 # Example file
  dap_base <- "http://hydro1.gesdisc.eosdis.nasa.gov/opendap/GLDAS/GLDAS_NOAH025_3H.2.0/"
  for(year in yr.min:yr.max){
    
    if(year %in% yrs.done) next

    print(paste0("   -- ", year))
    # Figure out if we're dealing with a leap year
    nday = ifelse(lubridate:: leap_year(year), 366, 365)
    ntime=nday
    
    region.lat <- seq(ymin+0.125, ymax, by=0.25)
    region.lon <- seq(xmin+0.125, xmax, by=0.25)
    

    # We'll write out 1 grid per year
    out.yr <- list()
    out.yr[["tmean" ]] <- array(dim=c(length(region.lon), length(region.lat), nday))
    out.yr[["tmax"  ]] <- array(dim=c(length(region.lon), length(region.lat), nday))
    out.yr[["tmin"  ]] <- array(dim=c(length(region.lon), length(region.lat), nday))
    out.yr[["precip"]] <- array(dim=c(length(region.lon), length(region.lat), nday))

    # Go by each day
    for(doy in 1:nday){
      doy2 <- str_pad(doy, 3, pad="0")
      print(doy2)
      # Extract each hour into a 3-d array
      tmp <- array(dim=c(length(region.lon), length(region.lat), length(timestamps))) # Third dimenions = 8 because there should be 8 3-hr files per day
      ppt <- array(dim=c(length(region.lon), length(region.lat), length(timestamps)))

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
        lon.start <- which(raw.lon == min(region.lon)) # Lon goes min to max
        lat.start <- which(raw.lat == min(region.lat)) # lat goes min to max

        # Extract the actual data
        tmp[,,hr] <- ncvar_get(nc.now, varid="Tair_f_inst", start=c(lon.start, lat.start,1), count=c(length(region.lon), length(region.lat),1))
        ppt[,,hr] <- ncvar_get(nc.now, varid="Rainf_f_tavg", start=c(lon.start, lat.start,1), count=c(length(region.lon), length(region.lat),1))*60*60*24 # Convert to kg m-2 day-1

        # Close file
        nc_close(nc.now)
      }

      # Aggregate to day & store it in output list
      out.yr[["tmean" ]][,,doy] <- apply(tmp, c(1,2), FUN=mean)
      out.yr[["tmax"  ]][,,doy] <- apply(tmp, c(1,2), FUN=max )
      out.yr[["tmin"  ]][,,doy] <- apply(tmp, c(1,2), FUN=min )
      out.yr[["precip"]][,,doy] <- apply(ppt, c(1,2), FUN=mean ) 

    }

    # Saving the file; 1 file per year
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=region.lat, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=region.lon, create_dimvar=TRUE)
    time <- ncdim_def(name='doy', units="day", vals=1:nday, create_dimvar=TRUE, unlim=TRUE)
    dim.dat=list(lon,lat,time)

    # Putting data together to make life easier
    var.list <- list()

    var.list[["tmean" ]] <- ncvar_def(name="tmean" , longname="daily mean temperature"   , units="K", dim=dim.dat, missval=-999)
    var.list[["tmin"  ]] <- ncvar_def(name="tmin"  , longname="daily minimum temperature", units="K", dim=dim.dat, missval=-999)
    var.list[["tmax"  ]] <- ncvar_def(name="tmax"  , longname="daily maximum temperature", units="K", dim=dim.dat, missval=-999)
    var.list[["precip"]] <- ncvar_def(name="precip", longname="daily total precipitation", units="kg m-2 (= mm/m2)", dim=dim.dat, missval=-999)

    ## put data in new file
    loc.file <- file.path(dir.out, paste0("GLDAS2.0_day_", year, ".nc"))
    loc <- nc_create(filename=loc.file, vars=var.list)
    for(v in names(var.list)){
      ncvar_put(nc=loc, varid=var.list[[v]], vals=out.yr[[v]])
    }
    nc_close(loc)
  } # end year loop
  # -------------------------------

  # # Compress the files as we go to make life easier
  # if(compress==T){
  #   setwd(dir.out) # Go to the output directory so we don't get annoying file paths
  #   system(paste0("tar -jcvf ", species, ".tar.bz2 ", species)) # Compress the folder
  #   system(paste0("rm -rf ", species)) # remove the uncompressed folder
  #   setwd(dir.base) # Go back to our base directory
  # }
}
