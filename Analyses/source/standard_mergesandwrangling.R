#Standard data wrangling to merge expclim and treats, and expclim and expheno
#plus to get one column for above ground temperature
#uses three files () to get two commonly used databases:
#1) experimental climate with one variable for above ground warming and all treatments added in
#2) exppgdd database with climate data (gddcrit,etc)
#First add treatment information to expclim
#dim(expclim)#223558
#dim(treats)#258
#replace NA with "none" for blocks in treats file
treats[which(is.na(treats$target)),]$target<-0
treats[which(is.na(treats$preciptreat_amt)),]$preciptreat_amt<-100
expclim2<-left_join(expclim,treats, by=c("site", "block", "plot","temptreat","preciptreat"), match="all", copy=TRUE)
#dim(expclim2)#221683     46
#head(expclim2)
#make a column for styear (study year, as opposed to calendar year)
expclim2$styear<-NA#start by giving all studies year 1 (exp2 and exp8 had only 1 year each),then adjust each study by hand
#make a column for styear (study year, as opposed to calendar year)
sites<-unique(expclim2$site)
for (i in 1:length(sites)){
  sitedat<-expclim2[expclim2$site==sites[i],]
  styears<-unique(sitedat$year)
  print(styears)
  for (j in 1:length(styears)){
    expclim2$styear[expclim2$site==sites[i] & expclim2$year==styears[j]]<-j
  }
}

#merge phenology data with experimental climate to get gdd crit
dim(exppheno)#74403 rows
expgdd<-inner_join(exppheno,expclim2, by=c("site", "block", "plot","year","doy"), match="all", copy=TRUE)
dim(expgdd)#63768   rows

#Get one variable for aboveground warming (could be surface, canopy, air) expclim2
expclim2$agtemp_min<-expclim2$airtemp_min
expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$cantemp_min)),]$agtemp_min<-expclim2[which(is.na(expclim2$airtemp_min) & !is.na(expclim2$cantemp_min)),]$cantemp_min
expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$surftemp_min)),]$agtemp_min<-expclim2[which(is.na(expclim2$agtemp_min) & !is.na(expclim2$surftemp_min)),]$surftemp_min
expclim2$agtemp_max<-expclim2$airtemp_max
expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$cantemp_max)),]$agtemp_max<-expclim2[which(is.na(expclim2$airtemp_max) & !is.na(expclim2$cantemp_max)),]$cantemp_max
expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$surftemp_max)),]$agtemp_max<-expclim2[which(is.na(expclim2$agtemp_max) & !is.na(expclim2$surftemp_max)),]$surftemp_max
#calculate mean above-ground temp
expclim2$agtemp_mn<-(expclim2$agtemp_max+expclim2$agtemp_min)/2
#Replace NAs with more helpful information for block
expclim2[which(is.na(expclim2$block)),]$block<-"none"
expgdd[which(is.na(expgdd$block)),]$block<-"none"

#make a column for combined genus species
expgdd$genus.species<-paste(expgdd$genus,expgdd$species,sep=".")

