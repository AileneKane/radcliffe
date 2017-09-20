#some basic stats on our E3 database:

#Read in experimental climate and phenology data
setwd("~/git/radcliffe")
expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)

length(unique(paste(expclim$site,expclim$year, sep=".")))#study years
length(unique(paste(expclim$site,expclim$year,expclim$doy, sep=".")))#study years
