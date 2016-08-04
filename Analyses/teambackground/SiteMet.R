# Script to extract historical met data from the experimental and observational sites
# Author: Christy Rollinson
# Contact: crollinson@gmail.com
# 13 July 2016

# Note: I'm using GLDAS for the sites so that we can get very recent records for non-US sites
#  URL: http://disc.sci.gsfc.nasa.gov/services/grads-gds/hydrology/documentation/hydro_doc.shtml
#  Native Spatial Res:  1.0-degree (1979-present), 0.25-degree (1848-2010, 2000-present)
#    ** Note: we're going to go with the 1948-2010 data for now
#  Native Spatial Ext: Global
#  Native Temporal Res: 3-hr
#  Native Temporal Extent: 1980-2016
#  Alternatives that I'm aware of and work with regularly:
#    - CRUNCEP*: Global, 6 hrly, 0.5 degree, 1901-2010
#    - BEST: Global, daily, 1.0 degree, 1980-2010 #  URL: http://berkeleyearth.org/data/
#    - NLDAS*: North America-ish (US + S. Canada + N. Mexico), hourly, 0.125 degree, 1979-present
#    - GLDAS*: Global, 3 hourly, 0.25 degree, 1948-present -- NOTE: there are different versions, and unfortuantley the higher-res 
#    - Daymet: North America-ish (US + S. Canada + N. Mexico), daily, 1 km, 1980-present
#    - PRISM: US, daily, 4 km, 1981-present (monthly 1895-present)
#    * indicates that these datasets provide the full suite of standard meteorological variables:
#        - temperature, preciptiation, shortwave radiation, longwave radiation, pressure, humidity, wind

rm(list=ls())

# Key libraries
library(RCurl)
library(XML)
library(lubridate)
library(ncdf4)
library(stringr)

# Define our base directory
dir.base <- "~/Dropbox/Radcliffe_Phenology/radcliffe/Analyses/teambackground/"
setwd(dir.base)

dir.out <- file.path(dir.base, "output", "SiteMet")

# Load in the locations for experimental and observational sites
sites.exp <- read.csv("../expsiteinfo.csv")
sites.obs <- read.csv("../obssiteinfo.csv")

sites.exp[,c("Site", "Lat", "Long")]
sites.obs[,c("Site.code", "Lat", "Long")]

# -------------
# Note that in Sierra Nevadas, there appear to be two sites & these must be two separate rows
# -------------
siernev <- sites.obs[sites.obs$Site.cod=="siernev", ]
sn.lat <- as.numeric(strsplit(paste(siernev[, "Lat"]), split=",")[[1]])
sn.lon <- as.numeric(strsplit(paste(siernev[, "Long"]), split=",")[[1]])

cols <- 1:ncol(siernev)
cols.excl <- which(names(siernev) %in% c("Lat", "Long"))
cols[!cols %in% cols.excl]
siernev <- siernev[,cols[!cols %in% cols.excl] ]

siernev <- merge(siernev, data.frame(Lat=sn.lat, Long=sn.lon), all=T)
siernev$Site.code <- c(paste0("siernev", 1:nrow(siernev)))

# Merging sierra nevadas into the origin site.obs file
#  -- Note: need to first drop siernev & then convert lat/lon to numeric
sites.obs <- sites.obs[sites.obs$Site.code!="siernev", ]
sites.obs$Lat <- as.numeric(paste(sites.obs$Lat))
sites.obs$Long <- as.numeric(paste(sites.obs$Long))
summary(sites.obs)
sites.obs <- merge(sites.obs, siernev, all=T)
summary(sites.obs)
# -------------


# Everything that follows here is modified from a script I wrote for the PalEON & Pecan projects
# -- Available from Pecan Project along with a bunch of other scripts: https://github.com/PecanProject/pecan
# source("download.GLDAS.R")

# http://hydro1.sci.gsfc.nasa.gov/thredds/catalog/catalog.html

# Base file path to the OpenDAP GLDAS records
dap_base <- "http://hydro1.gesdisc.eosdis.nasa.gov/opendap/GLDAS/GLDAS_NOAH025_3H.2.0/"
# File paths will be base/[year]/[doy]/GLDAS_NOAH025_3H.A[year][month][day][hour].020.nc4
# Note: the following path works, but only goes to 2010
# http://hydro1.gesdisc.eosdis.nasa.gov/opendap/GLDAS/GLDAS_NOAH025_3H.2.0/1948/001/GLDAS_NOAH025_3H.A19480101.0300.020.nc4



# URL to query 1980-2016 data, return as .csv
# http://hydro1.sci.gsfc.nasa.gov/thredds/ncss/grid/GLDAS_NOAH10SUBP_3H/2016/162/GLDAS_NOAH10SUBP_3H.A2016162.2100.001.2016172123757.grb/pointDataset.html
# "http://hydro1.sci.gsfc.nasa.gov/thredds/ncss/grid/GLDAS_NOAH10SUBP_3H/2016/162/GLDAS_NOAH10SUBP_3H.A2016162.2100.001.2016172123757.grb?var=Average_Surface_Temperature&var=Rainfall_rate&latitude=42.54&longitude=-78.24&temporal=all&time_start=2016-06-10T21%3A00%3A00Z&time_end=2016-06-10T21%3A00%3A00Z&time=2016-06-10T21%3A00%3A00Z&vertCoord=&accept=csv&point=true")
# -------------
# Loop through the observational data
# -------------
for(i in 1:nrow(sites.obs)){
  site.now <- sites.obs$Site.code[i]
  lat.in = sites.obs$Lat[i]
  lon.in = sites.obs$Long[i]
  for (i in 1980:2015){
    year = ylist[i]    
    
    # figure out how many days we're working with
    if(rows>1 & i!=1 & i!=rows){ # If we have multiple years and we're not in the first or last year, we're taking a whole year
      nday  = ifelse(lubridate:: leap_year(year), 366, 365) # leap year or not; days per year
      days.use = 1:nday
    } else if(rows==1){
      # if we're working with only 1 year, lets only pull what we need to
      nday  = ifelse(lubridate:: leap_year(year), 366, 365) # leap year or not; days per year
      day1 <- yday(start_date)
      # Now we need to check whether we're ending on the right day
      day2 <- yday(end_date)
      days.use = day1:day2
      nday=length(days.use) # Update nday
    } else if(i==1) {
      # If this is the first of many years, we only need to worry about the start date
      nday  = ifelse(lubridate:: leap_year(year), 366, 365) # leap year or not; days per year
      day1 <- yday(start_date)
      days.use = day1:nday
      nday=length(days.use) # Update nday
    } else if(i==rows) {
      # If this is the last of many years, we only need to worry about the start date
      nday  = ifelse(lubridate:: leap_year(year), 366, 365) # leap year or not; days per year
      day2 <- yday(end_date)
      days.use = 1:day2
      nday=length(days.use) # Update nday
    }
    ntime = nday*8 # leap year or not*time slice (3-hourly)
    
    loc.file = file.path(outfolder,paste("GLDAS",year,"nc",sep="."))
    
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
    time <- ncdim_def(name='time', units="sec", vals=seq((min(days.use)*24*360), (max(days.use)+1-1/8)*24*360, length.out=ntime), create_dimvar=TRUE, unlim=TRUE)
    dim=list(lat,lon,time)
    
    var.list = list()
    dat.list = list()
    
    # Defining our dimensions up front
    for(j in 1:nrow(var)){
      var.list[[j]] = ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=dim, missval=-999, verbose=verbose)
      dat.list[[j]] <- array(NA, dim=c(length(lat.in), length(lon.in), ntime)) # Go ahead and make the arrays
    }
    names(var.list) <- names(dat.list) <- var$CF.name
    
    ## get data off OpenDAP
    for(j in 1:length(days.use)){
      date.now <- as.Date(days.use[j], origin=as.Date(paste0(year-1,"-12-31")))
      mo.now <- str_pad(month(date.now), 2, pad="0")
      day.mo <- str_pad(day(date.now), 2, pad="0")
      doy <- str_pad(days.use[j], 3, pad="0")
      
      # Because the suffixes are really different for these files, lets get a list and go through each day
      dap.log <- data.frame(readHTMLTable(paste0(dap_base, "/",year, "/", doy, "/catalog.html")))
      dap.log <- dap.log[order(dap.log[,1],decreasing=F),] # Sort them so that we go from 0 to 21
      
      for(h in seq_len(nrow(dap.log))[-1]){
        dap_file = paste0(dap_base, "/",year, "/", doy, "/",dap.log[h,1],".ascii?")
        
        # Query lat/lon
        latlon <- getURL(paste0(dap_file,"lat[0:1:599],lon[0:1:1439]"))
        lat.ind <- gregexpr("lat", latlon)
        lon.ind <- gregexpr("lon", latlon)
        lats <- as.vector(read.table(con <- textConnection(substr(latlon, lat.ind[[1]][3], lon.ind[[1]][3]-1)), sep=",", fileEncoding="\n", skip=1))
        lons <- as.vector(read.table(con <- textConnection(substr(latlon, lon.ind[[1]][3], nchar(latlon))), sep=",", fileEncoding="\n", skip=1))
        
        lat.use <- which(lats-0.25/2<=lat.in & lats+0.25/2>=lat.in)
        lon.use <- which(lons-0.25/2<=lon.in & lons+0.25/2>=lon.in)
        
        # Set up the query for all of the met variables
        dap_query=""
        for(v in 1:nrow(var)){
          dap_query <- paste(dap_query, paste0(var$DAP.name[v], "[0:1:0]", "[",lat.use,"][",lon.use,"]"), sep=",")  
        }
        dap_query=substr(dap_query,2,nchar(dap_query))
        
        dap.out <- getURL(paste0(dap_file,dap_query))
        for (v in 1:nrow(var)) {
          var.now <- var$DAP.name[v]
          ind.1 <- gregexpr(paste(var.now,var.now, sep="."), dap.out)
          end.1 <- gregexpr(paste(var.now,"time", sep="."), dap.out)
          dat.list[[v]][,,(j*8)-8+h-1] <- read.delim(con <- textConnection(substr(dap.out, ind.1[[1]][1], end.1[[1]][2])), sep=",", fileEncoding="\n" )[1,1]
        } # end variable loop
      } # end hour
    } # end day
    ## change units of precip to kg/m2/s instead of hour accumulated precip
    # dat.list[["precipitation_flux"]] = dat.list[["precipitation_flux"]]/360
    
    ## put data in new file
    loc <- nc_create(filename=loc.file, vars=var.list, verbose=verbose)
    for(j in 1:nrow(var)){
      ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=dat.list[[j]])
    }
    nc_close(loc)
    
    results$file[i] <- loc.file
    #     results$host[i] <- fqdn()
    results$startdate[i] <- paste0(year,"-01-01 00:00:00")
    results$enddate[i] <- paste0(year,"-12-31 23:59:59")
    results$mimetype[i] <- 'application/x-netcdf'
    results$formatname[i] <- 'CF Meteorology'
    
  }
  
}
# -------------
