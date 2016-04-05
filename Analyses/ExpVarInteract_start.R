#Variables we have (or will have) of interest:
  #Canopy temperature (Jeff will standardize or derive from air temp)
  #Soil moisture - at up to two depths
      #what is the difference between treatments at different depths?
  #Soil temperature - at up to two depths
  #Site (use as random effect)
library(ggplot2)

expsiteinfo<-read.csv("/home/miriam/Documents/Harvard/PhenologyWorkshop_2016/radcliffe/Analyses/expsiteinfo.csv")
expclim<-read.csv("/home/miriam/Documents/Harvard/PhenologyWorkshop_2016/radcliffe/Analyses/expclim.csv")

#####CHUNK 1: Looking at soil moisture and temperature by depth
#Fix this now that I have better numbers (pull from GitHub)
#Look at difference in soil moisture by depth - just use depth 1 and 2 for now
mois_both<-expclim[which(!is.na(expclim$soilmois1)),]
mois_both<-mois_both[which(!is.na(mois_both$soilmois2)),] #Where we have moisture at both depths

ggplot(mois_both,aes(x=soilmois1,y=soilmois2,col=site))+
  scale_x_continuous(name="Soil Moisture (shallower)")+ scale_y_continuous(name="Soil Moisture (deeper)")+
  geom_point()

#Look at difference in soil temperature by depth - just use depth 1 and 2 for now
stemp_both_min<-expclim[which(!is.na(expclim$soiltemp1_min)),]
stemp_both_min<-stemp_both_min[which(!is.na(stemp_both_min$soiltemp2_min)),]
stemp_both_max<-expclim[which(!is.na(expclim$soiltemp1_max)),]
stemp_both_max<-stemp_both_max[which(!is.na(stemp_both_max$soiltemp2_max)),]

#Minimum soil temp
ggplot(stemp_both_min,aes(x=soiltemp1_min,y=soiltemp2_min,col=site))+
  scale_x_continuous(name="Soil Temp (shallower), MIN")+ scale_y_continuous(name="Soil Temp (deeper), MIN")+
  geom_point()

#Maximum soil temp
ggplot(stemp_both_max,aes(x=soiltemp1_max,y=soiltemp2_max,col=site))+
  scale_x_continuous(name="Soil Temp (shallower), MAX")+ scale_y_continuous(name="Soil Temp (deeper), MAX")+
  geom_point() #Check some of these high values

#QAQC on soil moisture MAX 
VhighT<-subset(expclim, expclim$soiltemp2_max>60) #A. Ellison suggests getting rid of these -- added as an issue on Wiki 
ggplot(stemp_both_max[which(stemp_both_max$soiltemp2_max<60),],aes(x=soiltemp1_max,y=soiltemp2_max,col=site))+
  scale_x_continuous(name="Soil Temp (shallower), MAX")+ scale_y_continuous(name="Soil Temp (deeper), MAX")+
  geom_point() #Check some of these high values

#Add columns: meaning of temptreat, soil depth 1, soil depth 2
  #Note: right now, expsiteinfo doesn't have separate depth columns for soil temperature and moisture, but
  #it should. Can't assume that soil temperature and moisture were taken at the same depths

#(1) For temperature associated with temp treat:
#This is really slow. Also -- NAs??
# expclim$temp<-rep(NA,nrow(expclim))
# for (i in 1:nrow(expclim)){  
#   sit<-as.character(expclim[i,which(names(expclim)=="site")]) #Which site?
#   trts<-as.numeric(expsiteinfo[which(expsiteinfo$Site== sit),25:33]) #How are temperatures coded at this site?
#   if(expclim$temptreat[i]==0){expclim$temp[i]=0} 
#   if(expclim$temptreat[i]=="sham"){expclim$temp[i]="sham"}
#   if(expclim$temptreat[i]!=0&expclim$temptreat[i]!="sham"){expclim$temp[i]=trts[as.numeric(as.character(expclim$temptreat[i]))]}
#   if(i%%20==0){print(i)} 
# }  

#Possible faster way:
fin<-data.frame(ncol=ncol(expclim))
names(fin)<-names(expclim)
sites<-as.character(unique(expclim$site))
for (i in 1:length(sites)){
  sub<-subset(expclim,expclim$site==sites[i])
  trts<-as.numeric(expsiteinfo[which(expsiteinfo$Site==sites[i]),25:33])
  for(k in 1:length(unique(sub$temptreat))){
    sub2<-subset(sub,sub$temptreat==unique(sub$temptreat)[k])
    sub2$temp<-trts[k]
    fin<-merge(fin,sub2)
    
  }
  print(i)}
      
#       [which(sub2$temptreat==0)]<-0 
#     sub2$temp[which(sub2$temptreat=="sham"]<-"sham"
#     if(sub2$temptreat!=0&sub2$temptreat!="sham"){sub2$temp=trts[as.numeric(as.character(sub2$temptreat))]}
#   }
# }

#What is the climate space of soil moisture/temperature that we see in experiments?
#Color code by experiment

#Shams vs. controls - compare on abiotic variables 
sc<-subset(expclim,expclim$temptreat=="sham"|expclim$temptreat==0)



#####CHUNK 2: Random Effects Models

####################################################

