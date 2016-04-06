### Started 3 April 2016 ###
### By Lizzie (for now ###

### Looking at the GDD crit ### 

## choose 2 or 4 base, do air and soil separate
## look at phen by event type

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## setup
library(ggplot2)
library(tidyr) # I don't actually use this below, but I *want* to
library(plyr)
library(lme4)

## set working directory (to each his own)
# setwd("~/GitHub/radcliffe/radmeeting")
#setwd("~/Documents/git/projects/meta_ep2/radcliffe")
setwd("/Users/Yann1/Documents/Postdoc Davos/Colloques/2016/Harward April 2016/radmeeting/github/radcliffe/Analyses")

gdd <- read.csv("gddest.csv", header=TRUE)
head(gdd)

#read the data
expclim<-read.csv("expclim.csv", header=T)
exppheno<-read.csv("exppheno.csv", header=T)
obspheno<-read.csv("obspheno.csv", header=T)
obsclim<-read.csv("obsclim.csv", header=T)

head(obsclim)

#Now, some preliminary analyses:
#first, add gdd to expclim file:
###Add GDD and cumulative gdd: soiltemp-tbase, cumulative GDD for that year (sum up to that date)

names(obsclim)

#obsclim<-data.frame(airtemp_max=1:30)
tbase<-c(0,2,4,6,8, 10)

#loop for making Tmax or Tmin - Tbase
for(i in tbase){
  col<-rep(NA,nrow(obsclim))
  obsclim<-data.frame(obsclim,col)
  obsclim[,ncol(obsclim)]<-((obsclim$airtemp_max+obsclim$airtemp_min)/2)-i
}

for(i in tbase){
  col<-rep(NA,nrow(obsclim))
  obsclim<-data.frame(obsclim,col)
  obsclim[,ncol(obsclim)]<-obsclim$airtemp_max-i
}

names(obsclim)[6:11] <- paste(rep('meanbase',6),tbase, sep="")
names(obsclim)[12:17] <- paste(rep('maxbase',6),tbase, sep="")

head(obsclim, 5)
head(obspheno)
length(climpheno$site)

obspheno$date <- as.POSIXct(strptime(obspheno$date, "%Y-%m-%d"))

#we remove the negative values (ie. when temp was below Tbase)
for(i in 6:17){
  obsclim[,i][obsclim[,i]<0] <- 0
  i+1
  }

mindate <- min(obspheno$date)
obspheno$Juliandate <- obspheno$date - mindate

#difftime(obspheno$date, mindate, units="days")
obspheno$Juliandatepheno <- round(as.numeric(obspheno$Juliandate, units = "days"), 0)
head(obspheno)

#fichier climat on crée une colonne Julian date
head(obsclim)
head(obspheno)
obsclim$date <- paste(obsclim$year, obsclim$doy, sep="-")
obsclim$date <- as.POSIXct(strptime(obsclim$date, "%Y-%j"))

#obsclim$date <-as.Date(obsclim$date, format="%Y-%j")
#obsclim$date <- as.POSIXct(strptime(obspheno$date, "%Y-%j"))

obsclim$Juliandate <- difftime(obsclim$date, mindate, units="days")

obspheno$species <- paste(obspheno$genus, obspheno$species,sep="_")
head(obspheno)
head(obsclim)
names(obspheno)
temppheno <- obspheno[,c(1,4,8,10)]
head(temppheno)

sit<-as.character(unique(temppheno$site))
#store<-list(list())
df<-data.frame(matrix(NA,ncol=16))
names(df)<-c(names(temppheno),names(obsclim[6:17]))
for(k in 1:length(sit)){
  subclim<-subset(obsclim,obsclim$site==sit[k]) #just one site
  subpheno<-subset(temppheno,temppheno$site==sit[k])
  for (i in 1:nrow(subpheno)){ #need to do this for every phenological phase
    clim<-subclim[which(subclim$Juliandate>=subpheno$Juliandatepheno[i]-60&
                          subclim$Juliandate<subpheno$Juliandatepheno[i]),]
    sumclim<-colSums(clim[,6:17])
    orig<-subpheno[i,]
    tot<-cbind(orig,t(as.data.frame(sumclim)))
    df<-rbind(df,tot)
    #store[[k]][[i]]<-sumclim
    if(i%%1000==0){print(i)}
  }
  print(k)
}
final<-df[-1,] #removes NAs in first row -- sorry, messy coding.

head(final)



write.table(final, "/Users/Yann1/Documents/Postdoc Davos/Colloques/2016/Harward April 2016/radmeeting/github/radcliffe/Analyses/GDD2months.txt", sep=";", col.names=TRUE, row.names=FALSE, dec=".")











head(obspheno)
$Juliandate)

?cumsum

obsclim$year1 <- obsclim$year + 1

names(obsclim)
obsclimpreviousyear <- subset(obsclim[,c(1:5, 16)], doy>243)
obsclimcurrentyear <- subset(obsclim, doy<244)
head(obsclimcurrentyear)

merge(obsclimcurrentyear, obsclimpreviousyear, by)    

max(c(2,4,6))
for (i in 1:length(tbase)){
  colnames(expclim)[14+(i-1)+i]<-paste("gdd_soil",tbase[i],sep=".")
  colnames(expclim)[15+(i-1)+i]<-paste("gdd_air",tbase[i],sep=".")
}
#check
aggregate(expclim[, 15:24], list(expclim$site), mean, na.rm=T)
#now add columns for cumulative
cumsumnona <- function(x){cumsum(ifelse(is.na(x), 0, x)) + x*0}
for (i in 1:length(tbase)){
  expclim[,24+(i-1)+i]<-ave(expclim[,14+(i-1)+i],list(expclim$site,expclim$plot,expclim$year), FUN=cumsumnona)
  expclim[,25+(i-1)+i]<-ave(expclim[,15+(i-1)+i],list(expclim$site,expclim$plot,expclim$year), FUN=cumsumnona)
}
for (i in 1:length(tbase)){
  colnames(expclim)[24+(i-1)+i]<-paste("cumgdd_soil",tbase[i],sep=".")
  colnames(expclim)[25+(i-1)+i]<-paste("cumgdd_air",tbase[i],sep=".")
}
expclim$alltreat<-paste(expclim$temptreat,expclim$preciptreat,sep=".")



## let's start small and look at one site
## bace, sherry and force have multiple levels of precip
## only bace has multiple temp and precip treatments

##
## bace
##

bace <- subset(gdd, site=="bace")
plot(gdd.est~tbase, data=bace) # perhaps not suprisingly there is a fundamental relationship between GDD and tbase (lower tbase, higher GDD)

# we'll look quickly at base 2 and base 10 to see if they change what you see
unique(bace$species)
ggplot(subset(bace, tbase==2), aes(x=temptreat, y=gdd.est, fill=species)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
quartz()
ggplot(subset(bace, species=="Acer.rubrum" & tbase==2),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

ggplot(subset(bace, tbase==10), aes(x=temptreat, y=gdd.est, fill=species)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
quartz()
ggplot(subset(bace, species=="Acer.rubrum" & tbase==10),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

# under base 10 you get more variability in some treatments and the droughts are more similar for temp=0|1 than in base 2, but then less similar in  temp=2|3 ... also, note how very small the GDDs are


##
## sherry
##

sher <- subset(gdd, site=="sherry")
plot(gdd.est~tbase, data=sher) # ditto what I said above

# we'll look quickly at base 2 and base 10 to see if they change what you see
unique(sher$species)
ggplot(subset(sher, tbase==2), aes(x=temptreat, y=gdd.est, fill=species)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)
quartz()
ggplot(subset(sher, species=="Ambrosia.psilostchya" & tbase==2),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

quartz()
ggplot(subset(sher, species=="Schizachyrium.scoparium" & tbase==2),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

quartz()
ggplot(subset(sher, species=="Panicum.virgatum" & tbase==2),
  aes(x=temptreat, y=gdd.est, fill=as.factor(preciptreat))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

##
## looking at tbase more
## need to model-select best tbase FIRST
##
findbetterway <- ddply(bace, c("plot", "species"), summarise,
       modaic=min(modaic))

bace.tbase <- merge(findbetterway, bace, all.x=TRUE)

plot(gdd.est~tbase, data=bace.tbase) # base 10 is selected, probably because errors look small when data are not standardized .....


##
## let's catapult ahead!
## and try some big models
##

gdd2 <- subset(gdd, tbase==2)
gdd2.warmonly <- subset(gdd2, preciptreat==0)

quartz()
ggplot(gdd2,
  aes(x=temptreat, y=gdd.est, fill=site)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
                outlier.size=4)

mod <- lmer(gdd.est~temptreat+ (1|site/species), data=gdd2.warmonly)
