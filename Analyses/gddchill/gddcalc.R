### Started 3 April 2016 ###
### By Lizzie (for now), other authors welcome! ###

### Looking at the GDD crit ### 
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Read in climate and phenology data
# setwd("~/GitHub/radcliffe/radmeeting")
setwd("~/Documents/git/projects/meta_ep2/radcliffe/Analyses")

## get some f(x)s I wrote to calculate GDD
## someone else should check these more!
source("source/gddme.R")

## setup
expclim<-read.csv("expclim.csv", header=TRUE)
head(expclim)

expclim$meanairtemp <- (expclim$airtemp_min+expclim$airtemp_max)/2
expclim$meansoil1temp <- (expclim$soiltemp1_min+expclim$soiltemp1_max)/2

##
## now make the threshold data
##
thresh <- 5 # set the base temp as 5
expclim$gddthreshmeanair <- makethreshold.data(expclim, "meanairtemp", thresh)
expclim$gddthreshmeansoil <- makethreshold.data(expclim, "meansoil1temp", thresh)


# Okay, need to calculate GDD for air mean and soil mean temps
# for each plot x site x year

expclim$giantcol <- paste(expclim$site, expclim$plot, expclim$temptreat, expclim$preciptreat, expclim$year)
expclim <- subset(expclim, is.na(doy)==FALSE) 
uniquethings <- unique(expclim$giantcol)
length(uniquethings) # super

expclim.wgdd <- expclim[1,]
expclim.wgdd$gddmeanairNA <- NA
expclim.wgdd$gddmeansoilNA <- NA
expclim.wgdd$gddmeanair <- NA
expclim.wgdd$gddmeansoil <- NA
expclim.wgdd <- expclim.wgdd[-1,]

# now we loop through each plot x site x year and
# calculate the GDD for soil and for air
# and count how many NAs we treated as 0


for (j in 1:length(uniquethings)){
    print(j)
    subby <- subset(expclim, giantcol==uniquethings[j])
    subby <- subby[order(as.numeric(subby$doy)),]
    subby$gddmeanair <- makegdd.data.skipNA(subby, "gddthreshmeanair", "doy", 1, 5)
    subby$gddmeansoil <- makegdd.data.skipNA(subby, "gddthreshmeansoil", "doy", 1, 5)
    subby$gddmeanairNA <- countNA.pergdd(subby, "gddthreshmeanair", "doy", 1, 5)
    subby$gddmeansoilNA <- countNA.pergdd(subby, "gddthreshmeansoil", "doy", 1, 5)
    expclim.wgdd <- rbind(expclim.wgdd, subby)
}

write.csv(expclim.wgdd, "gddchill/expclim.wgdd.csv", row.names=FALSE)


# if you want to change the factors used ....
# you change what is in (subby, "gddthreshmeanair", "doy", 1, 5)
# to (what data frame to call, what column to use to calc GDD, name of column for doy, ...
# what day to start counting, what doy to require data before (see f(x) file for more info) )
    

