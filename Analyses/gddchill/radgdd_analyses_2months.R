### Started 5 April 2016 ###


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

#read the data
expclim<-read.csv("expclim.csv", header=T)
exppheno<-read.csv("exppheno.csv", header=T)
obspheno<-read.csv("obspheno.csv", header=T)
obsclim<-read.csv("obsclim.csv", header=T)

head(expclim)

#Different base temperature
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

obsclim$date <- paste(obsclim$year, obsclim$doy, sep="-")
obsclim$date <- as.POSIXct(strptime(obsclim$date, "%Y-%j"))

mindate <- min(obspheno$date)
obspheno$Juliandate <- obspheno$date - mindate

#difftime(obspheno$date, mindate, units="days")
obspheno$Juliandatepheno <- round(as.numeric(obspheno$Juliandate, units = "days"), 0)
head(obspheno)

#fichier climat on crée une colonne Julian date
head(obsclim)
head(obspheno)



#obsclim$date <-as.Date(obsclim$date, format="%Y-%j")
#obsclim$date <- as.POSIXct(strptime(obspheno$date, "%Y-%j"))

obsclim$Juliandate <- difftime(obsclim$date, mindate, units="days")

#Make a column species
obspheno$species <- paste(obspheno$genus, obspheno$species,sep="_")
head(obspheno)
head(obsclim)
names(obspheno)

#create a temp file with the interesting variables
temppheno <- obspheno[,c(1,3,4,5,8,10)]
head(temppheno)

############make the loop for the calculation of the different GDD for OBSERVATIONS
sit<-as.character(unique(temppheno$site))
#store<-list(list())
df<-data.frame(matrix(NA,ncol=18))
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
finalGDD_OBS<-df[-1,] #removes NAs in first row -- sorry, messy coding.


#plot(meanbase0~year, data=final)subset(final, site=="fitter" & species=="Acer_campestre"))

write.table(finalGDD_OBS, "/Users/Yann1/Documents/Postdoc Davos/Colloques/2016/Harward April 2016/radmeeting/github/radcliffe/Analyses/GDD2monthsOBS.txt", sep=";", col.names=TRUE, row.names=FALSE, dec=".")

########################## Same GDD calculations using Exp data ##################
expclim<-read.csv("expclim.csv", header=T)
exppheno<-read.csv("exppheno.csv", header=T)

head(expclim)
tbase
#loop for making Tmax or Tmin - Tbase AIR TEMPERATURE
names(expclim)
for(i in tbase){
  col<-rep(NA,nrow(expclim))
  expclim<-data.frame(expclim,col)
  expclim[,ncol(expclim)]<-((expclim$airtemp_max+expclim$airtemp_min)/2)-i
}

for(i in tbase){
  col<-rep(NA,nrow(expclim))
  expclim<-data.frame(expclim,col)
  expclim[,ncol(expclim)]<-expclim$airtemp_max-i
}

names(expclim)
names(expclim)[16:21] <- paste(rep('meanairbase',6),tbase, sep="")
names(expclim)[22:27] <- paste(rep('maxairbase',6),tbase, sep="")

#loop for making Tmax or Tmin - Tbase SOIL TEMPERATURE
names(expclim)
for(i in tbase){
  col<-rep(NA,nrow(expclim))
  expclim<-data.frame(expclim,col)
  expclim[,ncol(expclim)]<-expclim$soiltemp1_mean-i
}

for(i in tbase){
  col<-rep(NA,nrow(expclim))
  expclim<-data.frame(expclim,col)
  expclim[,ncol(expclim)]<-expclim$soiltemp1_max-i
}

names(expclim)
names(expclim)[28:33] <- paste(rep('meansoilbase',6),tbase, sep="")
names(expclim)[34:39] <- paste(rep('maxsoilbase',6),tbase, sep="")

#we remove the negative values (ie. when temp was below Tbase)
for(i in 16:39){
  expclim[,i][expclim[,i]<0] <- 0
  i+1
}

#display the Julian date
names(exppheno)

exppheno$date <- paste(exppheno$year, exppheno$doy, sep="-")
exppheno$date <- as.POSIXct(strptime(exppheno$date, "%Y-%j"))


mindate <- min(exppheno$date)
exppheno$Juliandate <- exppheno$date - mindate

#difftime(obspheno$date, mindate, units="days")
exppheno$Juliandatepheno <- round(as.numeric(exppheno$Juliandate, units = "days"), 0)
head(exppheno)

#fichier climat on crée une colonne Julian date
head(exppheno)
head(expclim)

expclim$Juliandate <- difftime(expclim$date, mindate, units="days")

#Make a column species
exppheno$species <- paste(exppheno$genus, exppheno$species,sep="_")

#create a temp file with the interesting variables
names(exppheno)
#unique(exppheno$plot[exppheno$site=="chuine"])
temppheno <- exppheno[,c(1,2,3,4,6,7,8,10)]
head(temppheno)

############make the loop for the calculation of the different GDD FOR EXPERIMENTS
sit<-as.character(unique(temppheno$site))
#store<-list(list())
df<-data.frame(matrix(NA,ncol=20))
names(df)<-c(names(temppheno),names(expclim[6:17]))
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
finalGDD_EXP<-df[-1,] #removes NAs in first row -- sorry, messy coding.

#plot(meanbase0~year, data=final)subset(final, site=="fitter" & species=="Acer_campestre"))

write.table(finalGDD_EXP, "/Users/Yann1/Documents/Postdoc Davos/Colloques/2016/Harward April 2016/radmeeting/github/radcliffe/Analyses/GDD2monthsEXP.txt", sep=";", col.names=TRUE, row.names=FALSE, dec=".")


############----------- CHILLING----------#########


chillmin <- 0
chillmax <- 10
obsclim$Chillunit <-0

names(obsclim)
head(obsclim)

is.na(obsclim$airtemp_max[i] )

for (i in 1:nrow(obsclim)){ 
  if (is.na(obsclim$airtemp_max[i]) | is.na(obsclim$airtemp_min[i]) )
    {obsclim$Chillunit[i] <- NA } else { if (( obsclim$airtemp_max[i]<chillmax & obsclim$airtemp_max[i]>chillmin ) | 
  (obsclim$airtemp_min[i]<chillmax & obsclim$airtemp_min[i]>chillmin ))
      { obsclim$Chillunit[i] <- 1   }
      }
  
    if(i%%1000==0){print(i)}
}


############make the loop for the calculation of the different CHILLING FOR OBSERVATIONS
names(temppheno)
head(temppheno)
head(obsclim)
names(subclim)
names(obsclim)

temppheno <- obspheno[,c(1,3,4,5,8,10)]
head(temppheno)

sit<-as.character(unique(temppheno$site))

#store<-list(list())
df<-data.frame(matrix(NA,ncol=7))
names(df)<-c(names(temppheno),names(obsclim[20]))
for(k in 1:length(sit)){
  subclim<-subset(obsclim,obsclim$site==sit[k]) #just one site
  subpheno<-subset(temppheno,temppheno$site==sit[k])
  for (i in 1:nrow(subpheno)){ #need to do this for every phenological phase
    clim<-subclim[which(subclim$Juliandate<subpheno$Juliandatepheno[i]-60),]
    clim2<-clim[which(clim$Juliandate>=clim$Juliandate[clim$doy==244]),]
    sumclim<-sum(clim[,20])
    orig<-subpheno[i,]
    tot<-cbind(orig,t(as.data.frame(sumclim)))
    df<-rbind(df,tot)
    #store[[k]][[i]]<-sumclim
    if(i%%1000==0){print(i)}
  }
}
?cbind
names(clim)
