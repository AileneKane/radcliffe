## Started 27 March 2017 ##
## Quick file to check if Ben's data for species ranges for team background look good ##

library(ncdf4)

setwd("~/Documents/git/projects/meta_ep2/radcliffe/Analyses/teambackground/scratch")

acesac <- nc_open("acersacr_clim.nc")
print(acesac)

lon <- ncvar_get(acesac,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(acesac,"lat",verbose=FALSE)
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))
