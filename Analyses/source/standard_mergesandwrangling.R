#Standard data wrangling to merge expclim and treats, and expclim and expheno
#AND to get one column for above ground temperature (COMBINING SURFACE, CANOPY, AND AIR)
#uses three files () to get two commonly used databases:
#1) experimental climate with one variable for above ground warming and all treatments added in
#2) exppgdd database with climate data (gddcrit,etc)


#First add treatment information to expclim
#dim(expclim)#343734     22
#dim(treats)#379  10
#unique(expclim$site)#15
#replace NA with "none" for blocks in treats file
treats[which(is.na(treats$target)),]$target<-0
treats[which(is.na(treats$preciptreat_amt)),]$preciptreat_amt<-100
expclim2<-left_join(expclim,treats, by=c("site", "block", "plot","temptreat","preciptreat"), match="all", copy=TRUE)
#dim(expclim2)#343734     27
#head(expclim2)
#unique(expclim2$site)#15
#make a column for styear (study year, as opposed to calendar year)
expclim2$styear<-NA#start by giving all studies year 1 (exp2 and exp8 had only 1 year each),then adjust each study by hand
#make a column for styear (study year, as opposed to calendar year)
sites<-unique(expclim2$site)
for (i in 1:length(sites)){
  sitedat<-expclim2[expclim2$site==sites[i],]
  styears<-unique(sitedat$year)
  #print(styears)
  for (j in 1:length(styears)){
    expclim2$styear[expclim2$site==sites[i] & expclim2$year==styears[j]]<-j
  }
}

#merge phenology data with experimental climate to get gdd crit
#dim(exppheno)#74512     8
expclim2$site<-as.character(expclim2$site)
expclim2$block<-as.character(expclim2$block)
expclim2$plot<-as.character(expclim2$plot)
exppheno$site<-as.character(exppheno$site)
exppheno$block<-as.character(exppheno$block)
exppheno$plot<-as.character(exppheno$plot)

expgdd<-inner_join(exppheno,expclim2, by=c("site", "block", "plot","year","doy"), match="all", copy=TRUE)
#dim(expgdd)#63734    31
#unique(expgdd$site)#14 sites
#Need surftemp_mean (laready have airtempmean)
expclim2$surftemp_mean<-(expclim2$surftemp_min+expclim2$surftemp_max)/2
expclim2$cantemp_mean<-(expclim2$cantemp_min+expclim2$cantemp_max)/2

#Get one variable for aboveground warming (could be surface, canopy, air) expclim2
expclim2$agtemp_min<-expclim2$airtemp_min
expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$cantemp_min)),]$agtemp_min<-expclim2[which(is.na(expclim2$airtemp_min) & !is.na(expclim2$cantemp_min)),]$cantemp_min
expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$surftemp_min)),]$agtemp_min<-expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$surftemp_min)),]$surftemp_min
expclim2$agtemp_max<-expclim2$airtemp_max
expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$cantemp_max)),]$agtemp_max<-expclim2[which(is.na(expclim2$airtemp_max) & !is.na(expclim2$cantemp_max)),]$cantemp_max
expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$surftemp_max)),]$agtemp_max<-expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$surftemp_max)),]$surftemp_max

#calculate mean above-ground temp
expclim2$agtemp_mean<-expclim2$airtemp_mean
expclim2[which(is.na(expclim2$airtemp_mean) & !is.na(expclim2$cantemp_mean)),]$agtemp_mean<-expclim2[which(is.na(expclim2$airtemp_mean) & !is.na(expclim2$cantemp_mean)),]$cantemp_mean
expclim2[which(is.na(expclim2$agtemp_mean) & !is.na(expclim2$surftemp_mean)),]$agtemp_mean<-expclim2[which(is.na(expclim2$agtemp_mean) & !is.na(expclim2$surftemp_mean)),]$surftemp_mean

#Replace NAs with more helpful information for block
expclim2[which(is.na(expclim2$block)),]$block<-"none"
expgdd[which(is.na(expgdd$block)),]$block<-"none"

#Remove conifers, IF DESIRED
if(remove.conifers==TRUE){
  expgdd<-expgdd[expgdd$genus!="Pinus",]
  }#6 species of pines from 4 sites...removing them makes model fail to converge. also makes interaction significant...
print(dim(expgdd))
#make a column for combined genus species
expgdd$genus.species<-paste(expgdd$genus,expgdd$species,sep=".")


