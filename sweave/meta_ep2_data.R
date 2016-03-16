### Started 16 March 2016 ###
### By Lizzie ###

### Quick look at the data for Ailene's Radcliffe meeting ###
### Code designed to slot into meta_ep2_data.Rnw ###

## housekeeping 
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## some setup
setwd("~/Documents/git/projects/meta_ep2/radcliffe")

## get the data!
# be patient, some of these files are large
obsdata <- read.csv("radmeeting/obspheno.csv", header=TRUE)
expdata <- read.csv("radmeeting/exppheno.csv", header=TRUE)
eclim <- read.csv("radmeeting/expclim.csv", header=TRUE)

## look at the data
head(obsdata)
head(expdata)

unique(expdata$site)
unique(obsdata$site)

# hist(expdata$year) needs a fix
hist(obsdata$year)

table(expdata$site, expdata$event)
table(obsdata$site, obsdata$event)

# par(mfrow=c(3,3))
expsitez <- unique(expdata$site)
# loop is not working even when I remove the Clark sites
# expsitez.sm <- expsitez[which(!expsitez %in% c("clarkharvard", "clarkduke"))]
# for (site in seq_along(expsitez.sm)){
#     subby <- subset(expdata, site==expsitez.sm[site])
#     hist(subby$doy)
# }

obsdata$doy <- as.numeric(obsdata$doy)
obsagg <- aggregate(obsdata[c("doy")], obsdata[c("site", "year")], FUN=mean, na.action = na.omit)

# simple plot, need to add a legend
obsitez <- unique(obsdata$site)
somecolors <- rainbow(length(obsitez))
plot(doy~year, data=obsagg, type="n")
for (i in seq_along(obsitez)){
     subby <- subset(obsagg, site==obsitez[i])
     lines(doy~year, data=subby, col=somecolors[i])
}

## look at species numbers and overlap
expdata$latbi <- paste(expdata$genus, expdata$species)
obsdata$latbi <- paste(obsdata$genus, obsdata$species)

length(expdata$latbi)
length(obsdata$latbi)

unique(expdata$latbi)[which(unique(expdata$latbi) %in% unique(obsdata$latbi))]
