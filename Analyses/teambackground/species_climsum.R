#Started 12 September 2017
#By Ailene

#This code aims to:
#Get the climate data from Ben's netcdf files, and then use them to summarized climate in:
#1) the core of the range (i.e. the centroid)
#2) the warmest quartile of the range

#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#setup
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/radcliffe/Analyses/teambackground") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("~/git/radcliffe/Analyses/teambackground")
}else 
  setwd("~/Documents/git/radcliffe/Analyses/teambackground")

library(ncdf4)
library(geosphere)

# for each species for which we have climate data 
spfiles<-list.files(path="input/ClimRanges",pattern = ".nc", full.names = FALSE, ignore.case = TRUE)
input.folder="/Users/aileneettinger/git/radcliffe/Analyses/teambackground/input/ClimRanges"

#climate variables included
climvars<-c("clim_gdd0_JFM","clim_gdd5_JFM","clim_gdd0_JanMay","clim_gdd5_JanMay","clim_prec_OctMar", "clim_prec_AprSep", "clim_tmean_warm","clim_tmean_cold","clim_chill_SOND") 

#1) create a dataframe that that is species x climvars for the climate at the center of the range
climdat.cent <- as.data.frame(matrix(ncol=length(climvars), nrow=length(spfiles)))
colnames(climdat.cent)<-climvars
for(i in 1:length(spfiles)){ # i = 19
  file <- file.path(input.folder,spfiles[grep(spfiles[i], spfiles)])
  # Open the file
  jx <- nc_open(file)
  #list climate variables
  climvars<-names(jx$var)
   
       for(j in 1:length(climvars)){#j=1
        clim<- ncvar_get(jx,climvars[j])
        #select out the middle 50% percent of lats and longs to represent core of the range
        cclim<-clim[max(which(abs(jx$dim$lon$vals)>quantile(abs(jx$dim$lon$vals))[4])):min(which(abs(jx$dim$lon$vals)<quantile(abs(jx$dim$lon$vals))[2])),max(which(abs(jx$dim$lat$vals)<quantile(abs(jx$dim$lat$vals))[2])):min(which(abs(jx$dim$lat$vals)>quantile(abs(jx$dim$lat$vals))[4]))]
        climdat.cent[i,j]<- mean(cclim, na.rm=TRUE)
         }     
  rownames(climdat.cent)[i]<-substr(spfiles[i],1,8)
  nc_close(jx)
   }
 write.csv(climdat.cent,"output/clim_centroid.csv",row.names=TRUE)

#2) create a dataframe that that is species x climvars for the climate in the southern range (measured as the warmest quartile of cells in the range
  climdat.warm <- as.data.frame(matrix(ncol=length(climvars), nrow=length(spfiles)))
  climvars<-c("clim_gdd0_JFM","clim_gdd5_JFM","clim_gdd0_JanMay","clim_gdd5_JanMay","clim_prec_OctMar", "clim_prec_AprSep", "clim_tmean_warm","clim_tmean_cold","clim_chill_SOND") 
  colnames(climdat.warm)<-climvars
  
  for(i in 1:length(spfiles)){ # i = 1
    file <- file.path(input.folder,spfiles[grep(spfiles[i], spfiles)])
    jx <- nc_open(file)# Open the file
    #get warmest quartile of range, then find centroid of that portion to extract climate data
    wlats<-jx$dim$lat$vals[jx$dim$lat$vals<quantile(jx$dim$lat$vals)[2]]
    climvars<-names(jx$var)
     for(j in 1:length(climvars)){#j=1
      clim<- ncvar_get(jx,climvars[j])
      wclim<-clim[,abs(jx$dim$lat$vals)<quantile(abs(jx$dim$lat$vals))[2]]
      #take mean of all 9 climate variables in the warmest quartile of the range  
      climdat.warm[i,j]<- mean(wclim, na.rm=TRUE)
      }     
    rownames(climdat.warm)[i]<-substr(spfiles[i],1,8)
    nc_close(jx)
  }
  #the above code almost works but not quite...there are 4 species for which there are no climate data in cell that is the centroid of the range. need to deal with this but ignoring for now... 
  write.csv(climdat.warm,"output/clim_warm.csv",row.names=TRUE)
 
  ##A different approach to calculating the core climate:
  #the below selects out just the central grid cell of the range to get the core climate. 
  #the problem is that sometimes the central grid cell does not have cliamte data (4 species are missing data here)
  #1a) create a dataframe that that is species x climvars for the climate at the center of the range
 #
  climdat.cent2 <- as.data.frame(matrix(ncol=length(climvars), nrow=length(spfiles)))
  colnames(climdat.cent2)<-climvars
  for(i in 1:length(spfiles)){ # i = 1
    file <- file.path(input.folder,spfiles[grep(spfiles[i], spfiles)])
    jx <- nc_open(file)
    # Open the file
    #get centroid of lat,longs to find extract climate from the "core" (center) of the range
    clon<-median(jx$dim$lon$vals)#median long in range
    clat<-median(jx$dim$lat$vals)#median lat in range
    climvars<-names(jx$var)
    diff.long.cell <- abs(jx$dim$lon$vals-as.numeric(clon))#differences between all longitudes & latitudes and the means
    diff.lat.cell <- abs(jx$dim$lat$vals-as.numeric(clat))
    long.cell <- which(diff.long.cell==min(diff.long.cell))[1] #select the closest longitude & latitude with climate data to mean
    lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
    for(j in 1:length(climvars)){#j=1
      climdat.cent2[i,j]<- ncvar_get(jx,climvars[j],start=c(long.cell,lat.cell),count=c(1,1))
    }     
    rownames(climdat.cent2)[i]<-substr(spfiles[i],1,8)
    nc_close(jx)
  }
  #the above code almost works but not quite...there are 4 species for which there are no climate data in cell that is the centroid of the range. need to deal with this but ignoring for now... 
  write.csv(climdat.cent2,"output/clim_centroid_cell.csv",row.names=TRUE)

  #Do some checks
  #compare two metrics of core climate
  for(i in 1:9){
    plot(climdat.cent[,i],climdat.cent2[,i])
  }
  #these two methods of quantifying core climate are correlated but not equal
  
  #compare core climate to climate in warmest quartile
  for(i in 1:9){
    plot(climdat.cent[,i],climdat.warm[,i])
  }
  #these two methods are also generally correlated but not always linearly
  
  