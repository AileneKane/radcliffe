#code to make table s1 with basic info on study designs

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(xtable)
require(dplyr)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/meta_ep2/radcliffe/documents/expwarm") 
} else
  setwd("/Users/aileneettinger/git/radcliffe/documents/expwarm")



expsites2 <- read.csv("../../Analyses/expsiteinfo.csv", header=TRUE)
expsites2$Target<-paste(expsites2$temptreat_1,expsites2$temptreat_2,expsites2$temptreat_3, sep=", ")
expsites2$Target[expsites2$Target=="1.5, 2, 2.5"]<-"1.5-5.5"
expsites2$Target[grep("NA, NA",expsites2$Target)]<-substr(expsites2$Target[grep("NA, NA",expsites2$Target)],1,nchar(expsites2$Target[grep("NA, NA",expsites2$Target)])-8)
expsites2$Target[grep(", NA",expsites2$Target)]<-substr(expsites2$Target[grep(", NA",expsites2$Target)],1,nchar(expsites2$Target[grep(", NA",expsites2$Target)])-4)
expsites2$ptreat<-paste(expsites2$preciptreat_1,expsites2$preciptreat_.1,sep=", ")
expsites2$ptreat[expsites2$ptreat=="NA, NA"]<-" "
expsites2$ptreat[expsites2$ptreat=="NA, 70"]<-"70, 100"
expsites2$ptreat[expsites2$ptreat=="150, NA"]<-"100, 150"
expsites2$ptreat[expsites2$ptreat=="120, NA"]<-"100, 120"
expsites2$ptreat[expsites2$ptreat=="200, NA"]<-"100, 200"
expsites2$ptreat[expsites2$ptreat=="150, 50"]<-"50, 100, 150"

expsites2$Tsoildepths<-paste(expsites2$Tsoildepth1_cm,
                             expsites2$Tsoildepth2_cm, sep=", ")

expsites2$Tsoildepths[expsites2$Tsoildepths=="NA, NA"]<-" "
expsites2$Tsoildepths[expsites2$Tsoildepths=="3, NA"]<-"3"
expsites2$Tsoildepths[expsites2$Tsoildepths=="15, NA"]<-"15"
expsites2$Tsoildepths[expsites2$Tsoildepths=="12, NA"]<-"12"
expsites2$Tsoildepths[expsites2$Tsoildepths=="10, NA"]<-"10"
expsites2$Tsoildepths[expsites2$Tsoildepths=="5, NA"]<-"5"
expsites2$Msoildepths<-paste(expsites2$Msoildepth1_cm,expsites2$Msoildepth2_cm, sep=", ")
expsites2$Msoildepths[expsites2$Msoildepths=="NA, NA"]<-" "
expsites2$Msoildepths[expsites2$Msoildepths=="30, NA"]<-"30"
expsites2$Msoildepths[expsites2$Msoildepths=="15, NA"]<-"15"
expsites2$Msoildepths[expsites2$Msoildepths=="8, NA"]<-"8"
expsites2$sitetreat<-paste(expsites2$DatasetID,expsites2$target,expsites2$preciptreat)
expsites2$data_years<-paste(expsites2$data_startyear,expsites2$data_endyear,sep="-")
expsites2$AGtemp<-c("canopy"," ","air (30)","air (30)"," "," ","air (22)", " ","surface","air (22)"," ","air (14)")
#expsites2$SMmeth<-c("VWC","VWC","VWC","VWC","TDR, VWC"," ","TDR, VWC", " ","VWC"," "," ","VWC")
sitetable<-expsites2 %>% 
  dplyr::select(DatasetID,Target,ptreat,AGtemp,Tsoildepths,Msoildepths)
sitetable<- sitetable[order(sitetable$DatasetID),]
#Add other basic info about study:
sitetable$location<-c("Waltham, MA, USA","Montpelier, France","Duke Forest, NC, USA",
          "Harvard Forest, MA, USA","Jasper Ridge Biological Preserve, CA, USA",
          "Rocky Mountain Biological Lab, CO, USA","Harvard Forest, MA, USA",
          "Harvard Forest, MA, USA","Stone Valley Forest, PA, USA",
          "Duke Forest, NC, USA","Rocky Mountain Biological Lab, CO, USA","Kessler Farm Field Laboratory, OK, USA")
          
#sitetable$source<-as.character(c("Hoeppner and Dukes 2012","Morin et al. 2010","Clark et al. 2014",
 #        "Clark et al. 2014","Cleland et al. 2007","Dunne et al. 2003",
  #       "Pelini et al. 2011","Farnsworth et al. 1995","Rollinson and Kaye 2012",
   #      "Marchin et al. 2015","Price and Wasser 1998","Sherry et al. 2007"))
sitetable$data_years<-c("2009-2011","2004","2009-2014","2009-2012","1998-2002","1995-1998",
         "2010-2015","1993","2009-2010","2010-2013","1991-1994","2003")
sitetable$type<-c("infrared","infrared","forced air and soil warming",
        "forced air and soil warming","infrared","infrared","forced air",
        "soil warming","infrared","forced air","infrared","infrared")
sitetable2<-subset(sitetable, select=c(DatasetID, location, data_years,type,Target,ptreat,AGtemp, Tsoildepths, Msoildepths))

colnames(sitetable2)<-c("study","location","data years", "warming type","warming treatment","precip treatment","above-ground temperature","soil temperature depth","soil moisture depth")
