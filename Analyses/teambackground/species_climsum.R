#Started 12 September 2017
#By Ailene

#This code aims to:
#Calculate the climate in: 
#(a) the southern range (measured as the warmest quartile of cells in the range, using the 4 temp variables we have)
#(b) the core climate (measured at the centroid (using the 4 temp variables we have))
# for all species in ClimRanges folder, and put them into one summary file 

#the summary file should contain the tmean, tmax, tmin, precip for the southern range and the core climate

#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#setup
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/radcliffe/Analyses/teambackground") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("~/git/radcliffe/Analyses/teambackground")
}else 
  setwd("~/Documents/git/radcliffe/Analyses/teambackground")

library(ncdf4)

# for each species for which we have climate data 
spfiles<-list.files(path="input/ClimRanges",pattern = ".nc", full.names = TRUE, ignore.case = TRUE)

for(i in c(spfiles)){ # i = "input/ClimRanges/acerpens_clim.nc"
  
  # Open the file
  jx <- nc_open("input/ClimRanges/acerpens_clim.nc")
  jx$dim$lon
  jx$dim$lat
  
  #get the climate data from the centroid of the range
  
  #get the climate data from the "warmest quartile" of the range
 nc_close(jx)
