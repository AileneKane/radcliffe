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
# dir.base <- "~/Dropbox/Radcliffe_Phenology/radcliffe/Analyses/teambackground/"
# dir.base <- "~/Desktop/Research/Radcliffe_Phenology/radcliffe/Analyses/teambackground/"
dir.base <- "~/radcliffe/Analyses/teambackground"
setwd(dir.base)

dir.out <- file.path(dir.base, "output", "SiteMet")
if(!dir.exists(dir.out)) dir.create(dir.out, recursive=T)


# Load in the locations for experimental and observational sites
sites.exp <- read.csv("../expsiteinfo.csv")
sites.obs <- read.csv("../obssiteinfo.csv")

sites.exp[,c("DatasetID", "Lat", "Long")]
sites.obs[,c("Site.code", "Lat", "Long")]

# -------------
# Note that in Sierra Nevadas, there appear to be two sites & these must be two separate rows
# -------------
siernev <- sites.obs[sites.obs$Site.code=="siernev", ]
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

# -------------
# Merge & Subset the data
# NOTE: GLDAS Data access policy changed, so I only have data for North America at the moment 
#  -- need to subset the sites until I can figure out a work-around 
# -------------
sites.master <- rbind(data.frame(site=sites.obs$Site.code, lat=sites.obs$Lat, lon=sites.obs$Long, type="obs"),
                      data.frame(site=sites.exp$DatasetID, lat=sites.exp$Lat, lon=sites.exp$Long, type="exp"))
sites.master <- sites.master[complete.cases(sites.master),]
summary(sites.master)

gldas.path <- "/projectnb/dietzelab/paleon/met_ensemble/data/paleon_domain/GLDAS_day/"
files.gldas <- dir(gldas.path)
yrs.gldas <- as.numeric(substr(files.gldas, nchar(files.gldas)-6, nchar(files.gldas)-3))

# Find the lat/lon range of the data
ncT <- nc_open(file.path(gldas.path, files.gldas[1]))
lat <- ncvar_get(ncT, "latitude")
lon <- ncvar_get(ncT, "longitude")
nc_close(ncT)
# -------------


# -------------
# Loop through to create a .csv file for each site 
# Note: This is definitely not the fastest way to do it, but I suspect will make life
#       easier in terms of data management.  I would not do it this way if I were 
#       querying the raw data!
# -------------
for(s in 1:nrow(sites.master)){
  sname <- sites.master[s, "site"]
  slat <- sites.master[s, "lat"]
  slon <- sites.master[s, "lon"]
  print(paste0(" === ", sname, " === ")) # Just to help us track progress
  
  # Skip over anything we can't get right now
  if(!(slat>=min(lat) & slat<=max(lat) & slon>=min(lon) & slon<=max(lon))) { 
    print("  -- outside domain!, skipping"); 
    next
  }
    
  # Finding our lat & lon index; making this left-truncated
  lat.ind <- which((lat-0.125)<slat & (lat+0.125)>=slat)
  lon.ind <- which((lon-0.125)<slon & (lon+0.125)>=slon)
  
  for(i in 1:length(files.gldas)){  
    # Setting up all the date information that would be handy to have
    yr.now <- as.numeric(substr(files.gldas[i], nchar(files.gldas[i])-6, nchar(files.gldas[i])-3))
    nday = ifelse(lubridate:: leap_year(yr.now), 366, 365)
    dates.now <- as.Date(1:nday, origin=as.Date(paste0(yr.now-1,"-12-31")))
    mo.now <- str_pad(month(dates.now), 2, pad="0")
    day.mo <- str_pad(day(dates.now), 2, pad="0")
    
    print(yr.now)
    
    dat.tmp <- data.frame(site=sname, date=dates.now, year=yr.now, DOY=str_pad(yday(dates.now), 3, pad="0"), mo=mo.now, day=day.mo)
    
    ncT <- nc_open(file.path(gldas.path, files.gldas[i]))
    dat.tmp$tmean  <- ncvar_get(ncT, "tmean" )[lon.ind,lat.ind,]
    dat.tmp$tmin   <- ncvar_get(ncT, "tmin"  )[lon.ind,lat.ind,]
    dat.tmp$tmax   <- ncvar_get(ncT, "tmax"  )[lon.ind,lat.ind,]
    dat.tmp$precip <- ncvar_get(ncT, "precip")[lon.ind,lat.ind,]
    nc_close(ncT)
    
    # Append this year to the rest of our data
    if(i==1){
      dat.out <- dat.tmp
    } else {
      dat.out <- rbind(dat.out, dat.tmp)
    }
  }  # End file loop
  # Write all met for each site into it's own .csv file
  write.csv(dat.out, file.path(dir.out, paste0(sname, "_gldas_2.0_", min(dat.out$year), "-", max(dat.out$year), ".nc")), row.names=F)
} # End site loop
# -------------
