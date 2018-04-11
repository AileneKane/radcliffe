expclim<-read.csv("../../Analyses/expclim.csv", header=T)
treats<-read.csv("../../Analyses/treats_detail.csv", header=T)
treats[which(is.na(treats$target)),]$target<-0
treats[which(is.na(treats$preciptreat_amt)),]$preciptreat_amt<-100
expclim2<-left_join(expclim,treats, by=c("site", "block", "plot","temptreat","preciptreat"), match="all", copy=TRUE)
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
expclimt<-expclim2[which(expclim2$preciptreat==0|is.na(expclim2$preciptreat)),]#select only plots with unmanipulated precip
expclimt$agtemp_min<-expclimt$airtemp_min

expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$cantemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$airtemp_min) & !is.na(expclimt$cantemp_min)),]$cantemp_min

expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$agtemp_min<-expclimt[which(is.na(expclimt$agtemp_min) & !is.na(expclimt$surftemp_min)),]$surftemp_min

expclimt$agtemp_max<-expclimt$airtemp_max

expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$cantemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$airtemp_max) & !is.na(expclimt$cantemp_max)),]$cantemp_max

expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$agtemp_max<-expclimt[which(is.na(expclimt$agtemp_max) & !is.na(expclimt$surftemp_max)),]$surftemp_max

expclimt$agtemp_mean<-(expclimt$agtemp_min+expclimt$agtemp_max)/2
expclimt$dtr<-expclimt$agtemp_max-expclimt$agtemp_min
expclimt$sdtr<-expclimt$soiltemp1_max-expclimt$soiltemp1_min
#remove site 2 (chuine) because these are not real temp measurements
#remove site 5 (cleland) because only soil moisture measured (no temp)
#remove site 15 because it is the only study with just ambient controls
expclimt<-expclimt[-which(expclimt$site=="exp02"|expclimt$site=="exp05"|expclimt$site=="exp15"),]
#Question: use structural controls to be  consistent (more have structural controls)
expclimt<-expclimt[-which(expclimt$temptreat=="ambient"),]#remove ambient controlsto be consistent (for those studies with both)

expclimt$styear<-as.factor(expclimt$styear)
expclimt$year<-as.factor(expclimt$year)
expclimt$site<-as.factor(expclimt$site)
expclimt[which(expclimt$temptreat=="0"),]$target<-0#for structural controls
expclimt$target<-as.numeric(expclimt$target)
expclimt$dtr<-as.numeric(expclimt$dtr)
expclimt$agtemp_min<-as.numeric(expclimt$agtemp_min)
expclimt$agtemp_max<-as.numeric(expclimt$agtemp_max)

#Tables of aboveground min versus max to show that DTR is affected:
#Now see how warming treatment affects dtr, and if warming increases min and max temp the same magnitude as mean
