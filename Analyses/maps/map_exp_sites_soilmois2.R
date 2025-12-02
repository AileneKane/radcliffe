## Make map of soil moisture sites
## Ailene Ettinger
## May 1, 2025

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## libraries
library(ggplot2)
theme_set(theme_bw())

library(sf)
library(maps)
library(RColorBrewer)
## set working directory
setwd("~/GitHub/radcliffe")

## get the sites
expsites2 <- read.csv("Analyses/expsiteinfo.csv", header=TRUE)
exsitetable<-expsites2 %>% 
  dplyr::select(DatasetID,Lat,Long,no_spp,habitat)
sitetable<- exsitetable[order(exsitetable$DatasetID),]
sitetable$phenophase<-as.character(c("bb,lo,fl",
                                     "fl,fr*",
                                     "bb,lo", 
                                     "bb,lo",
                                     "",
                                     "",
                                     "bb,lo,sen*",
                                     "",
                                     "lo,fl,fr*,sen*",
                                     "bb,fl",
                                     "",
                                     "fl,fr*",
                                     "","",""))
sitetable<-as.data.frame(sitetable)
colnames(sitetable)[2:3]<-c("Latitude","Longitude")
sitetable2<-sitetable[!sitetable$phenophase=="",]

world <- st_as_sf(map("world", plot = FALSE, fill = TRUE))
head(world)
sitecols<-c(brewer.pal(n = 8, name = "Set3"))
habshapes<-c(23,21,21,21,21,21,21,23)
sitenames<-sitetable2$DatasetID
pdf("Analyses/maps/expsites_soilmois2.pdf", width=9,height=4)

ggplot(data = world) +
  geom_sf() +
  
  geom_point(data = sitetable2, aes(x = Longitude, y = Latitude, color=DatasetID), size = 4, 
             shape = habshapes, fill = sitecols, alpha=.8) +
  coord_sf(xlim = c(-170, 50), ylim = c(24.5, 75), expand = FALSE) +
  scale_color_manual(
    values = sitecols,
    labels = sitenames)+
  scale_fill_manual(
    values = sitecols,
    labels = sitenames)+
  scale_shape_manual(
    values = habshapes,
    labels = sitenames)

dev.off()



ggplot() + 
  geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
  coord_cartesian(ylim=c(10, 90), xlim=c(-180, 40)) +
  geom_point(data=expsites, 
             aes(x=Long, y=Lat, size=studylength, fill=warming_type), 
             colour="black", pch=21) +
  theme.tanmap

# # library(rworldmap)

# Load in the locations for experimental and observational sites

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




#levels(expsites$warming_type) <- c("infrared", "forced air", "soil warming", "forced air & \nsoil warming")
warming.shapes <- c(5, 1, 2, 0)
warming.cols <- c("black", "blue2", "red3", "purple3")

png("soilmoisLocations_Experiments.png", width=8, height=4, units="in", res=220)
ggplot(data=expsites) +
  theme_bw() +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude") +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude") +
  # geom_point(aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1.1, alpha=1, position=position_jitter(width=0.75, height=0.75)) +
  geom_point(data=expsites[expsites$StudySite!="price"       ,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="price"       ,], aes(x=Long+0.5, y=Lat+1, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  # scale_shape_manual(values=c(18, 19, 17, 15)) +
  scale_shape_manual(values=warming.shapes) +
  scale_color_manual(values=warming.cols) +
  scale_size(range=c(1.0,5)) +
  guides(shape=guide_legend(title="Warming Type", override.aes = list(size=4), order=1, nrow=2, byrow=T),
         color=guide_legend(title="Warming Type", order=1, nrow=2, byrow=T),
         size=guide_legend((title="Study Length\n(years)"), override.aes = list(shape=1), order=2, nrow=2, byrow=T)) +
  theme(legend.position="top") +
  theme(legend.title=element_text(face="bold"),
        legend.key.size=unit(1.25, units="lines")) +
  coord_equal()
dev.off()



set.seed(1138)
sites.jitter <- c("Harvard Forest, MA", "Duke Forest, NC")
png("RadcliffeLocations_Experiments_Open_jitter.png", width=8, height=4, units="in", res=220)
ggplot(data=expsites) +
  theme_bw() +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude") +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude") +
  geom_point(data=expsites[expsites$StudySite!="price"       ,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1, position=position_jitter(width=1, height=1)) +
  geom_point(data=expsites[expsites$StudySite=="price"       ,], aes(x=Long+1, y=Lat+1, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  # geom_point(data=expsites[!expsites$Location %in% sites.jitter & !expsites$StudySite %in% c("dunne", "price"),], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1.1, alpha=1) +
  # geom_point(data=expsites[expsites$StudySite=="dunne"       ,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  # geom_point(data=expsites[expsites$StudySite=="price"       ,], aes(x=Long+0.75, y=Lat+1.2, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  # geom_point(data=expsites[expsites$Location %in% sites.jitter,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1.1, alpha=1, position=position_jitter(width=1, height=1)) +
  # scale_shape_manual(values=c(18, 19, 17, 15)) +
  # scale_shape_manual(values=c(5, 1, 2, 0)) +
  # scale_color_manual(values=c("firebrick3", "darkorange3", "black", "purple4")) +
  scale_shape_manual(values=warming.shapes) +
  scale_color_manual(values=warming.cols) +
  scale_size(range=c(1.0,5)) +
  guides(shape=guide_legend(title="Warming Type", override.aes = list(size=4), order=1, nrow=2, byrow=T),
         color=guide_legend(title="Warming Type", order=1, nrow=2, byrow=T),
         size=guide_legend((title="Study Length\n(years)"), override.aes = list(shape=1), order=2, nrow=2, byrow=T)) +
  theme(legend.position="top") +
  theme(legend.title=element_text(face="bold"),
        legend.key.size=unit(1.25, units="lines")) +
  coord_equal(xlim=range(rast.table$x), ylim=range(rast.table$y), expand=0, ratio=1)
dev.off()

# set.seed(1138)
# sites.jitter <- c("Harvard Forest, MA", "Duke Forest, NC")
png("RadcliffeLocations_Experiments_Open_jitter_manual.png", width=8, height=4, units="in", res=220)
ggplot(data=expsites) +
  theme_bw() +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude") +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude") +
  geom_point(data=expsites[!expsites$Location %in% sites.jitter & !expsites$StudySite %in% c("dunne", "price"),], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1.1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="dunne"       ,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="price"       ,], aes(x=Long+0.75, y=Lat+1.2, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="clarkduke"       ,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="clarkharvard"       ,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="marchin"       ,], aes(x=Long+0.5, y=Lat+0.5, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="ellison"       ,], aes(x=Long-1.25, y=Lat-1.25, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="farnsworth"       ,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  # scale_shape_manual(values=c(18, 19, 17, 15)) +
  # scale_shape_manual(values=c(5, 1, 2, 0)) +
  # scale_color_manual(values=c("firebrick3", "darkorange3", "black", "purple4")) +
  scale_shape_manual(values=c(1,0,5,2)) +
  scale_color_manual(values=c("blue2", "purple3", "black", "red3")) +
  scale_size(range=c(1.0,5)) +
  guides(shape=guide_legend(title="Warming Type", override.aes = list(size=4), order=1, nrow=2, byrow=T),
         color=guide_legend(title="Warming Type", order=1, nrow=2, byrow=T),
         size=guide_legend((title="Study Length\n(years)"), override.aes = list(shape=1), order=2, nrow=2, byrow=T)) +
  theme(legend.position="top") +
  theme(legend.title=element_text(face="bold"),
        legend.key.size=unit(1.25, units="lines")) +
  coord_equal(xlim=range(rast.table$x), ylim=range(rast.table$y), expand=0, ratio=1)
dev.off()


png("RadcliffeLocations_Experiments_Closed.png", width=8, height=4, units="in", res=220)
ggplot(data=expsites) +
  theme_bw() +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude") +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude") +
  geom_point(data=expsites[expsites$StudySite=="bace"        ,], aes(x=Long, y=Lat), shape=5, size=3.75, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite!="price"       ,], aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength),alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="dunne"       ,], aes(x=Long, y=Lat), shape=5, size=3.4, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="price"       ,], aes(x=Long+0.5, y=Lat+1, shape=warming_type, color=warming_type, size=studylength), stroke=1, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="price"       ,], aes(x=Long+0.5, y=Lat+1), shape=5, size=3.5, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="chuine"      ,], aes(x=Long, y=Lat), shape=5, size=3, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="clarkduke"   ,], aes(x=Long, y=Lat), shape=0, size=4.5, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="clarkharvard",], aes(x=Long, y=Lat), shape=0, size=4.5, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="cleland"     ,], aes(x=Long, y=Lat), shape=5, size=2.75, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="ellison"     ,], aes(x=Long, y=Lat), shape=1, size=3.5, color="black", stroke=0.25, alpha=1) +
  # geom_point(data=expsites[expsites$StudySite=="farnsworth"  ,], aes(x=Long, y=Lat), shape=5, size=3, color="black", stroke=0.5, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="force"       ,], aes(x=Long, y=Lat), shape=5, size=2, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="marchin"     ,], aes(x=Long, y=Lat), shape=1, size=4.25, color="black", stroke=0.25, alpha=1) +
  geom_point(data=expsites[expsites$StudySite=="sherry"      ,], aes(x=Long, y=Lat), shape=5, size=1, color="black", stroke=0.25, alpha=1) +
  
  scale_shape_manual(values=c(18, 19, 17, 15)) +
  # scale_shape_manual(values=c(5, 1, 2, 0)) +
  scale_color_manual(values=warming.cols) +
  scale_size(range=c(1.0,5)) +
  guides(shape=guide_legend(title="Warming Type", override.aes = list(size=4), order=1),
         color=guide_legend(title="Warming Type", order=1),
         size=guide_legend((title="Study Length\n(years)"), override.aes = list(shape=1), order=2)) +
  theme(legend.position="right") +
  theme(legend.title=element_text(face="bold"),
        legend.key.size=unit(1.25, units="lines")) +
  coord_equal()
dev.off()

expsites[,c("StudySite", "Lat", "Long", "Location", "warming_type", "studylength")]

#plots of temperature and soil moisture by site and year?
bbmeansm_siteyr<-aggregate(datalist.bbd$mois,by=list(datalist.bbd$site,datalist.bbd$year), mean)
bbmeantemp_siteyr<-aggregate(datalist.bbd$temp,by=list(datalist.bbd$site,datalist.bbd$year),mean)
datalist.bbd$site.yr<-paste(datalist.bbd$site,datalist.bbd$year,sep=".")
sitecols<-c(brewer.pal(n = 5, name = "Set3"))
sitecollist<-c(sitecols[1],sitecols[1],sitecols[2],sitecols[2],sitecols[2],sitecols[2],sitecols[3],sitecols[3],sitecols[3],sitecols[4],sitecols[4],sitecols[5],sitecols[5])

boxplot(datalist.bbd$mois~datalist.bbd$site.yr, col=c(sitecollist))


boxplot(datalist.bbd$temp~datalist.bbd$site.yr, col=c(sitecollist))
library(maps) 
map(database = "world") 

# marking points on map 
points(x = df$lat[1:500], y = df$lng[1:500], col = "Red")