#code to make table s1 with basic info on study designs

options(stringsAsFactors = FALSE)

#require(dplyr)

# Set working directory: 
#setwd("/Users/aileneettinger/Documents/GitHub/radcliffe")



expsites2 <- read.csv("Analyses/expsiteinfo.csv", header=TRUE)
exsitetable<-expsites2 %>% 
  dplyr::select(DatasetID,no_spp,habitat)
sitetable<- exsitetable[order(exsitetable$DatasetID),]
#Add other basic info about study:
sitetable$location<-c("Waltham, MA, USA","Montpelier, France","Duke Forest, NC, USA",
          "Harvard Forest, MA, USA","Jasper Ridge Biological Preserve, CA, USA",
          "Rocky Mountain Biological Lab, CO, USA","Harvard Forest, MA, USA",
          "Harvard Forest, MA, USA","Stone Valley Forest, PA, USA",
          "Duke Forest, NC, USA","Rocky Mountain Biological Lab, CO, USA","Kessler Farm Field Laboratory, OK, USA",
          "Haibei Alpine Grassland Research Station, China","Cedar Creek, MN, USA","Oak Ridge, TN, USA")
  
sitetable$source<-as.character(c("Hoeppner and Dukes 2012","Morin et al. 2010","Clark et al. 2014",
         "Clark et al. 2014","Cleland et al. 2007","Dunne et al. 2003",
       "Pelini et al. 2011","Farnsworth et al. 1995","Rollinson and Kaye 2012","Marchin et al. 2015","Price and Wasser 1998","Sherry et al. 2007","Suonan et al. 2017","Whittington et al 2015","Gunderson et al 2015"))
sitetable$data_years<-c("2009-2011","2004","2009-2014","2009-2012","1998-2002","1995-1998",
         "2010-2015","1993","2009-2010","2010-2013","1991-1994","2003","2012-2014","2009-2011","2003-2005")

sitetable$phenophase<-as.character(c("bb,lo,fl",
                                     "fl,fr",
                                     "bb,lo", 
                                     "bb,lo",
                                     "",
                                     "",
                                     "bb,lo,sen",
                                     "",
                                     "lo,fl,fr,sen",
                                     "bb,fl",
                                     "",
                                     "fl,fr",
                                     "","",""))

sitetable$no_spp[sitetable$DatasetID=="exp01"]<-"44"
sitetable$no_spp[sitetable$DatasetID=="exp02"]<-"5"
sitetable$no_spp[sitetable$DatasetID=="exp03"]<-"37"
sitetable$no_spp[sitetable$DatasetID=="exp04"]<-"29"
sitetable$no_spp[sitetable$DatasetID=="exp07"]<-"8"
sitetable$no_spp[sitetable$DatasetID=="exp09"]<-"120"
sitetable$no_spp[sitetable$DatasetID=="exp10"]<-"11"

sitetable2<-subset(sitetable, select=c(DatasetID, location, source,data_years,habitat,no_spp,phenophase))

colnames(sitetable2)<-c("study","location","source","data years", "ecosystem","species","phenophases")
sitetable2<-sitetable2[!sitetable2$phenophase=="",]

                       