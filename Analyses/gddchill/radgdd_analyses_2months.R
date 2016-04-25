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
head(obspheno)

###################-------------GDD and Chilling for OBSERVATIONS---------######################
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

obspheno$date <- as.POSIXct(strptime(obspheno$date, "%Y-%m-%d"))

#we remove the negative values (ie. when temp was below Tbase)
for(i in 6:17){
  obsclim[,i][obsclim[,i]<0] <- 0
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

obsclim$Juliandate <- difftime(obsclim$date, mindate, units="days")

#Make a column species
obspheno$speciesfull <- paste(obspheno$genus, obspheno$species,sep="_")
head(obspheno)
head(obsclim)
names(obspheno)


#create a temp file with the interesting variables
temppheno <- obspheno[,c(1,3,4,5,6,10,11)]
head(temppheno)
head(obsclim)
min(obsclim$Juliandate, na.rm=T)
min(obsclim$date, na.rm=T)
summary(obspheno)

############make the loop for the calculation of the different GDD for OBSERVATIONS
sit<-as.character(unique(temppheno$site))
#store<-list(list())
df<-data.frame(matrix(NA,ncol=19))
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
head(df)
finalGDD_OBS<-df[-1,] #removes NAs in first row -- sorry, messy coding.
head(finalGDD_OBS)

#plot(meanbase0~year, data=subset(finalGDD_OBS, site=="fitter" & species=="Acer_campestre"))

write.table(finalGDD_OBS, "/Users/Yann1/Documents/Postdoc Davos/Colloques/2016/Harward April 2016/radmeeting/github/radcliffe/Analyses/gddchill/GDD2monthsOBS.txt", sep=";", col.names=TRUE, row.names=FALSE, dec=".")

## now chilling units from 1st September to 2 months prior the pheno event
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

head(obsclim, 20)
############make the loop for the calculation of the different CHILLING FOR OBSERVATIONS


######---loop chilling for OBSERVATIONS from Myriam- april 2016---##########
names(obspheno)
temppheno <- obspheno[,c(1,3,4,5,10,11)]
head(obspheno)
head(temppheno)
names(obsclim)

sit<-as.character(unique(temppheno$site))

df2<-data.frame(matrix(NA,ncol=ncol(temppheno)+ 1 )) #FILL IN NUMBER: how many chilling columns?
names(df2)<-c(names(temppheno),names(obsclim[20])) #FILL IN RANGE: which are the chilling columns in obsclim?
for(k in 1:length(sit)){
  subclim<-subset(obsclim,obsclim$site==sit[k]) #just one site
  subpheno<-subset(temppheno,temppheno$site==sit[k]) #same site
  subclim_order<-subclim[order(subclim$Juliandate),] #make sure climate dataframe is ordered by Julian day
  for (i in 1:nrow(subpheno)){ #need to do this for every phenological phase
    boundingrow1<-max(which(subclim_order$Juliandate <= subpheno$Juliandatepheno[i]-60)) #end accumulation here
    boundingrow2<-min(which(subclim_order$doy >= 244 & subclim_order$year==subpheno$year[i]-1))#start accumulation here
    #there is an issue when one of these bounds does not exist due to NAs (usually only an issue with boundingrow1,
    #i.e. there is no data after doy244 in the year before the event):
    if(!is.finite(boundingrow1)||!is.finite(boundingrow2)) {sumclim=rep(NA,length(obsclim[20]))} #if either isn't finite, get NA
    if(is.finite(boundingrow1) & is.finite(boundingrow2)) {
      subclim2<-subclim_order[boundingrow2:boundingrow1,] #subset climate dataframe for just relevant time period
      #sumclim<-colSums(subclim2[,20])} #FILL IN RANGE. Add up the relevant columns
      sumclim<-sum(subclim2[,20])} #FILL IN RANGE. Add up the relevant columns
    orig<-subpheno[i,]
    tot<-cbind(orig,t(as.data.frame(sumclim)))
    names(tot)<-names(df2)
    df2<-rbind(df2,tot)
    #store[[k]][[i]]<-sumclim
    if(i%%1000==0){print(paste(ceiling((i/nrow(subpheno))*100),"% done with site", sit[k]))}
  }
  print(paste(sit[k], "finished"))
}

finaldf2<-df2[-1,] #removes NAs in first row
head(finaldf2)

finalGDD_OBS<-read.csv("gddchill/GDD2monthsOBS.txt", header=T, sep=";")
names(finalGDD_OBS)
names(finaldf2)

finaldf2 <- finaldf2[order(finaldf2$site, finaldf2$speciesfull, finaldf2$event , finaldf2$year),]
head(finaldf2)
finalGDD_OBS <- finalGDD_OBS[order(finalGDD_OBS$site,finalGDD_OBS$speciesfull, finalGDD_OBS$event, finalGDD_OBS$year),]
head(finalGDD_OBS)
finalGDDChill_OBS <- cbind(finalGDD_OBS, Chillunit=finaldf2$Chillunit)
head(finalGDDChill_OBS)

#finalGDDChill_OBS <- merge(finalGDD_OBS,finaldf2, by=c("site","event","year", "doy" , "Juliandatepheno","speciesfull"))
head(finalGDDChill_OBS)
length(finalGDDChill_OBS$site)
finalGDDChill_OBS$Chillunit[finalGDDChill_OBS$Chillunit>1000] <- NA

write.table(finalGDDChill_OBS, "/Users/Yann1/Documents/Postdoc Davos/Colloques/2016/Harward April 2016/radmeeting/github/radcliffe/Analyses/gddchill/GDDCHILL_2monthsOBS.txt", sep=";", col.names=TRUE, row.names=FALSE, dec=".")

class(finalGDDChill_OBS$Chillunit)
plot(finalGDDChill_OBS$Chillunit~finalGDD_OBS$year)
unique(finalGDDChill_OBS$speciesfull)
unique(obspheno$species[obspheno$genus=="Acer"])
names(finalGDDChill_OBS)
qplot(year, meanbase6, data = finalGDDChill_OBS[finalGDDChill_OBS$year>1949 & finalGDDChill_OBS$site=="harvard",], geom = c("point", "smooth"), facets = . ~ site)



###################-------------GDD and Chilling for EXPERIMENTS---------######################

#GDD calculations using Exp data #
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
names(expclim)[22:27] <- paste(rep('meanairbase',6),tbase, sep="")
names(expclim)[28:33] <- paste(rep('maxairbase',6),tbase, sep="")

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
names(expclim)[34:39] <- paste(rep('meansoilbase',6),tbase, sep="")
names(expclim)[40:45] <- paste(rep('maxsoilbase',6),tbase, sep="")

#we remove the negative values (ie. when temp was below Tbase)
for(i in 16:39){
  expclim[,i][expclim[,i]<0] <- 0
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
expclim$date <- paste(expclim$year, expclim$doy, sep="-")
expclim$date <- as.POSIXct(strptime(expclim$date, "%Y-%j"))
expclim$Juliandate <- difftime(expclim$date, mindate, units="days")
expclim$Juliandate <- round(as.numeric(expclim$Juliandate, units = "days"), 0)

#Make a column species
exppheno$speciesfull <- paste(exppheno$genus, exppheno$species,sep="_")

head(exppheno)

#create a temp file with the interesting variables
names(exppheno)
names(expclim)
head(expclim)
summary(expclim)
length(expclim$airtemp_min)
#unique(exppheno$plot[exppheno$site=="chuine"])
temppheno <- exppheno[,c(1,2,3,4,7,8,10,11)]
head(temppheno)

############make the loop for the calculation of the different GDD FOR EXPERIMENTS
temppheno$siteplot <- paste(temppheno$site, temppheno$plot, sep="_")
expclim$siteplot <-  paste(expclim$site, expclim$plot, sep="_")
names(expclim)
head(expclim)
sit<-as.character(unique(temppheno$siteplot))
length(sit)
length(unique(temppheno$siteplot))## !!! explclim=241 site-plots and exppheno=261!!!
#store<-list(list())
df3<-data.frame(matrix(NA,ncol=33))
names(df3)<-c(names(temppheno),names(expclim[22:45]))
for(k in 1:length(sit)){
  subclim<-subset(expclim,expclim$siteplot==sit[k]) #just one site and plot!
  subpheno<-subset(temppheno,temppheno$siteplot==sit[k])
  for (i in 1:nrow(subpheno)){ #need to do this for every phenological phase
    clim<-subclim[which(subclim$Juliandate>=subpheno$Juliandatepheno[i]-60&
                          subclim$Juliandate<subpheno$Juliandatepheno[i]),]
    sumclim<-colSums(clim[,22:45])
    orig<-subpheno[i,]
    tot<-cbind(orig,t(as.data.frame(sumclim)))
    df3<-rbind(df3,tot)
    #store[[k]][[i]]<-sumclim
    if(i%%1000==0){print(i)}
  }
  print(k)
}

head(df3)

finalGDD_EXP<-df3[-1,] #removes NAs in first row -- sorry, messy coding.
head(finalGDD_EXP)
summary(finalGDD_EXP)
finalGDD_EXP[finalGDD_EXP$site=='marchin' & finalGDD_EXP$species=="Acer_rubrum",]

#plot(meanbase0~year, data=final)subset(final, site=="fitter" & species=="Acer_campestre"))

write.table(finalGDD_EXP, "/Users/Yann1/Documents/Postdoc Davos/Colloques/2016/Harward April 2016/radmeeting/github/radcliffe/Analyses/gddchill/GDD2monthsEXP.txt", sep=";", col.names=TRUE, row.names=FALSE, dec=".")


############----------- CHILLING----------#########

######---loop chilling for EXP from Myriam- april 2016---##########


## now chilling units from 1st September to 2 months prior the pheno event
chillmin <- 0
chillmax <- 10
expclim$Chillunit <-0

names(expclim)
head(expclim)

is.na(expclim$airtemp_max[i] )

for (i in 1:nrow(expclim)){ 
  if (is.na(expclim$airtemp_max[i]) | is.na(expclim$airtemp_min[i]) )
  {expclim$Chillunit[i] <- NA } else { if (( expclim$airtemp_max[i]<chillmax & expclim$airtemp_max[i]>chillmin ) | 
                                           (expclim$airtemp_min[i]<chillmax & expclim$airtemp_min[i]>chillmin ))
  { expclim$Chillunit[i] <- 1   }
  }
  
  if(i%%1000==0){print(i)}
}

head(expclim, 20)
############make the loop for the calculation of the different CHILLING FOR OBSERVATIONS
head(exppheno)
#obspheno$species <- paste(obspheno$genus, obspheno$species,sep="_")

######---loop chilling for OBSERVATIONS from Myriam- april 2016---##########
names(exppheno)

temppheno <- exppheno[,c(1,2,3,4,7,8,10,11)]
temppheno$siteplot <- paste(temppheno$site, temppheno$plot, sep="_")
head(exppheno)
head(expclim)
names(expclim)
names(temppheno)
sit<-as.character(unique(temppheno$siteplot))

df4<-data.frame(matrix(NA,ncol=ncol(temppheno)+ 1 )) #FILL IN NUMBER: how many chilling columns?
names(df4)<-c(names(temppheno),names(expclim[49])) #FILL IN RANGE: which are the chilling columns in obsclim?
for(k in 1:length(sit)){
  subclim<-subset(expclim,expclim$siteplot==sit[k]) #just one site & plot
  subpheno<-subset(temppheno,temppheno$siteplot==sit[k]) #same siteplot
  subclim_order<-subclim[order(subclim$Juliandate),] #make sure climate dataframe is ordered by Julian day
  for (i in 1:nrow(subpheno)){ #need to do this for every phenological phase
    boundingrow1<-max(which(subclim_order$Juliandate <= subpheno$Juliandatepheno[i]-60)) #end accumulation here
    boundingrow2<-min(which(subclim_order$doy >= 244 & subclim_order$year==subpheno$year[i]-1))#start accumulation here
    #there is an issue when one of these bounds does not exist due to NAs (usually only an issue with boundingrow1,
    #i.e. there is no data after doy244 in the year before the event):
    if(!is.finite(boundingrow1)||!is.finite(boundingrow2)) {sumclim=rep(NA,length(expclim[49]))} #if either isn't finite, get NA
    if(is.finite(boundingrow1) & is.finite(boundingrow2)) {
      subclim2<-subclim_order[boundingrow2:boundingrow1,] #subset climate dataframe for just relevant time period
      #sumclim<-colSums(subclim2[,20])} #FILL IN RANGE. Add up the relevant columns
      sumclim<-sum(subclim2[,49])} #FILL IN RANGE. Add up the relevant columns
    orig<-subpheno[i,]
    tot<-cbind(orig,t(as.data.frame(sumclim)))
    names(tot)<-names(df4)
    df4<-rbind(df4,tot)
    #store[[k]][[i]]<-sumclim
    if(i%%1000==0){print(paste(ceiling((i/nrow(subpheno))*100),"% done with site", sit[k]))}
  }
  print(paste(sit[k], "finished"))
}

finaldf4<-df4[-1,] #removes NAs in first row
head(finaldf4)
summary(finaldf4$Chillunit)
#finaldf$speciesfull <- obspheno$speciesfull

finalGDD_EXP<-read.csv("gddchill/GDD2monthsEXP.txt", header=T, sep=";")
head(finalGDD_EXP)
head(finaldf4)
length(finaldf4$site)
length(finalGDD_EXP$site)

finaldf4 <- finaldf4[order(finaldf4$siteplot, finaldf4$speciesfull, finaldf4$event , finaldf4$year),]
head(finaldf4)
finalGDD_EXP <- finalGDD_EXP[order(finalGDD_EXP$siteplot,finalGDD_EXP$speciesfull, finalGDD_EXP$event, finalGDD_EXP$year),]
head(finalGDD_EXP)

finalGDDChill_EXP <- cbind(finalGDD_EXP, Chillunit=finaldf4$Chillunit)
head(finalGDDChill_EXP)

write.table(finalGDDChill_EXP, "/Users/Yann1/Documents/Postdoc Davos/Colloques/2016/Harward April 2016/radmeeting/github/radcliffe/Analyses/gddchill/GDDCHILL_2monthsEXP.txt", sep=";", col.names=TRUE, row.names=FALSE, dec=".")

qplot(year, Chillunit, data = finalGDDChill_EXP, geom = c("point", "smooth"), facets = . ~ site)
qplot(year, meanairbase4, data = finalGDDChill_EXP, geom = c("point", "smooth"), facets = . ~ site)



