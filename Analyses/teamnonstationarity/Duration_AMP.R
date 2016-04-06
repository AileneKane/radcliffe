#Main question: How does temperature sensitivity change over time in experiments and in observational studies?
#For this analysis, we are focusing only on Gothic Datasets

#required libraries (added by AME)
library(data.table)
source("Analyses/source/gddme.R")

#importing data
obsdata <- read.csv("Analyses/obspheno.csv", header=TRUE)
obsclim<-read.csv("Analyses/obsclim.csv", header=TRUE)
expdata <- read.csv("Analyses/exppheno.csv", header=TRUE)
expclim<-read.csv("Analyses/expclim.csv", header=TRUE)
expinfo<-read.csv("Analyses/expsiteinfo.csv", header=TRUE)
exptreat<-read.csv("Analyses/treats.csv", header=TRUE)
snow<-read.csv("Analyses/teamnonstationarity/meltdate1991.1998.csv") #snowmelt data is from zone C only.  consider pulling mean date for entire plot from papers
gdd<-read.csv("Analyses/gddchill/expclim.wgdd.csv", header=TRUE)

###Adding a column to edata and eclim that has the start year of the experiment/observations
#getting start years
expinfo.start<-expinfo[,c("Site","exp_startyear")]
expinfo.start<-setnames(expinfo.start, "Site", "site") 

#merging duration with data files
expdata<-merge(expdata, expinfo.start, all=TRUE)
expclim<-merge(expclim, expinfo.start, all=TRUE)

#Creating a column with the number of years into the experiment (or the observational study) that data was collected
expdata$year.num<-expdata$year-expdata$exp_startyear
expclim$year.num<-expclim$year-expclim$exp_startyear

#subsetting to just get Gothic data
odata<-subset(obsdata, site %in% c("gothic")) #counts 162352 / 211952 (77% of all obs pheno data)
odata<-droplevels(odata)
oclim<-subset(obsclim, site %in% c("gothic")) #49044/980880 (5% of all obs climate data)
oclim<-droplevels(oclim)

edata<-subset(expdata, site %in% c("price", "dunne")) #12,252/76,359 (16% of all experimental pheno data)
edata<-droplevels(edata)
rownames(edata)<-NULL
eclim<-subset(expclim, site %in% c("price", "dunne")) #17979/327632 (5% of all experimental climate data)
eclim<-droplevels(eclim)
eclim<-eclim[,c("site", "temptreat", "plot", "year", "doy", "soiltemp1_min", "soiltemp1_max", "soiltemp1_mean" )]
rownames(eclim)<-NULL

#adding start data column to observational data (for gothic only)
oclim$obs_startyear<-1973
odata$obs_startyear<-1973
oclim$year.num<-oclim$year-oclim$obs_startyear
odata$year.num<-odata$year-odata$obs_startyear

#getting snowmelt data in the format to merge
snow<-melt(snow, id=c("plot"), measured=c("X1991" ,"X1992", "X1993", "X1994" ,"X1995" ,"X1996", "X1997", "X1998"))
colnames(snow)[1:3]<-c("plot", "year", "meltdate")
snow$year<-as.numeric(substr(as.factor(snow$year), 2, 5))

#merging snowmelt date with edata
edata1<-merge(edata, snow, all=TRUE)
eclim<-subset(eclim, year!="1999") #there is no phenology data from 1999; we can remove this line once the expclim.csv has been updated
eclim<-droplevels(eclim)

#currently there is no plot 10 climate data from either site- this needs to be fixed
snow$plot <- as.factor(snow$plot) #AME merge only works if class(plot) matches in eclim & snow
eclim<-merge(eclim, snow, all=TRUE)
eclim<-subset(eclim, plot!="10") #remove this row of code once Ailene fixes climate data
eclim <- droplevels(eclim) #AME useful because plot is a factor

write.csv(eclim, "Analyses/teamnonstationarity/eclim.csv", row.names=FALSE)

eclim <- read.csv("Analyses/teamnonstationarity/eclim.csv") #AME so I can back out easily and reset eclim

#####using eclim to generate GDD for each doy and DSM (days since snowmelt)
###Lizzie's code

## setup
eclim<-setnames(eclim, "soiltemp1_mean", "meansoil1temp")

## now make the threshold data
thresh <- 5 # set the base temp as 5
eclim$gddthreshmeansoil <- makethreshold.data(eclim, "meansoil1temp", thresh)

#AME: and, if doy < meltdate, then gddthreshmeansoil = 0

eclim$gddthreshmeansoil[eclim$doy < eclim$meltdate] <- 0

#create new variable gddusethresh which zeros out gddthreshmeansoil
eclim$gddusethresh<-eclim$gddthreshmeansoil
eclim$gddusethresh[is.na(eclim$gddusethresh)]<-0

#########################Maybe a better GDD calculator for gothic data?###############
####Uncomment next line and run

##eclim.soil.gdd <- aggregate(gddusethresh~plot+year+site+doy, data=eclim, cumsum)

####
#######################################################################################


# Okay, need to calculate GDD for soil mean temps
# for each plot x site x year
eclim$giantcol <- paste(eclim$site, eclim$plot, eclim$temptreat, eclim$year)
eclim <- subset(eclim, is.na(doy)==FALSE) 
uniquethings <- unique(eclim$giantcol)
length(uniquethings) # super

eclim.wgdd <- eclim[1,]
eclim.wgdd$gddmeansoilNA <- NA
eclim.wgdd$gddmeansoil <- NA
eclim.wgdd <- eclim.wgdd[-1,]

# now we loop through each plot x site x year and
# calculate the GDD for soil and for air
# and count how many NAs we treated as 0

for (j in 1:length(uniquethings)){
  print(j)
  subby <- subset(eclim, giantcol==uniquethings[j])
  subby <- subby[order(as.numeric(subby$doy)),]
  subby$gddmeansoil <- makegdd.data.skipNA(subby, "gddthreshmeansoil", "doy", 1, 5)
  subby$gddmeansoilNA <- countNA.pergdd(subby, "gddthreshmeansoil", "doy", 1, 5)
  eclim.wgdd <- rbind(eclim.wgdd, subby)
}

#trying this for using meltdate as a start date
for (j in 1:length(uniquethings))
  print(j)
  subby <- subset(eclim, giantcol==uniquethings[j])
  subby <- subby[order(as.numeric(subby$doy)),]
  subby$gddmeansoil <- makegdd.data.skipNA(subby, "gddthreshmeansoil", "doy", 1, 365)
  subby$gddmeansoilNA <- countNA.pergdd(subby, "gddthreshmeansoil", "doy", 1, 365)
  eclim.wgdd <- rbind(eclim.wgdd, subby)
}
write.csv(eclim.wgdd, "Analyses/teamnonstationarity/eclim.wgdd_gothic.csv", row.names=FALSE)


# if you want to change the factors used ....
# you change what is in (subby, "gddthreshmeanair", "doy", 1, 5)
# to (what data frame to call, what column to use to calc GDD, name of column for doy, ...
# what day to start counting, what doy to require data before (see f(x) file for more info) )




