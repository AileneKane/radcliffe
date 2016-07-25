## A slighly fancier map showing experimental & observational sites locations ##
# Christy Rollinson 
# 25 July 2016


## housekeeping
rm(list=ls()) 

# Key libraries
library(ggplot2); library(maps)
# # library(rworldmap)

# Define our base directory
# Load in the locations for experimental and observational sites
sites.exp <- read.csv("../expsiteinfo.csv")
sites.obs <- read.csv("../obssiteinfo.csv")

sites.exp[,c("DatasetID", "Lat", "Long")]
sites.obs[,c("Site.code", "Lat", "Long")]

# -------------
# Note that in Sierra Nevadas, there appear to be two sites & these must be two separate rows
# -------------
siernev <- sites.obs[sites.obs$Site.cod=="siernev", ]
sn.lat <- as.numeric(strsplit(paste(siernev[, "Lat"]), split=",")[[1]])
sn.lon <- as.numeric(strsplit(paste(siernev[, "Long"]), split=",")[[1]])

cols <- 1:ncol(siernev)
cols.excl <- which(names(siernev) %in% c("Lat", "Long"))
cols[!cols %in% cols.excl]
siernev <- siernev[,cols[!cols %in% cols.excl] ]

siernev <- merge(siernev, data.frame(Lat=sn.lat, Long=sn.lon), all=T)
siernev$Site.code <- c(paste0("siernev", 1:nrow(siernev)))

# Merging sierra nevadas into the origin site.obs file
#  -- Note: need to first drop siernev & then convert lat/lon to numeric
sites.obs <- sites.obs[sites.obs$Site.code!="siernev", ]
sites.obs$Lat <- as.numeric(paste(sites.obs$Lat))
sites.obs$Long <- as.numeric(paste(sites.obs$Long))
summary(sites.obs)
sites.obs <- merge(sites.obs, siernev, all=T)
summary(sites.obs)
# -------------

sites.exp[,c("DatasetID", "Lat", "Long")]
summary(sites.exp[,c("DatasetID", "Lat", "Long")])
summary(sites.obs[,c("Site.code", "Lat", "Long")])

sites.obs[,c("Site.code", "Site", "Lat", "Long")]


dat.map <- data.frame(Site= c(sites.exp$DatasetID, sites.obs$Site.code),
                      Lat = c(sites.exp$Lat, sites.obs$Lat),
                      Lon = c(sites.exp$Long, sites.obs$Long),
                      type= c(rep("Experiment", nrow(sites.exp)),rep("Observations", nrow(sites.obs)))
                      )
summary(dat.map)

# library(ggmap)
# map <- get_map(location=c(min(dat.map$Lon, na.rm=T), min(dat.map$Lat, na.rm=T), max(dat.map$Lon, na.rm=T), max(dat.map$Lat, na.rm=T)), zoom=3)
# map


rad.region <- map_data("world")
rad.region$region <- as.factor(rad.region$region)
rad.region$subregion <- as.factor(rad.region$subregion)
summary(rad.region)

region.names <- unique(rad.region[rad.region$long>=min(dat.map$Lon, na.rm=T)-1 & rad.region$long<=max(dat.map$Lon, na.rm=T)+1 &
                                    rad.region$lat>=min(dat.map$Lat, na.rm=T)-1 & rad.region$lat<=max(dat.map$Lat, na.rm=T)+1 ,
                                  "region"])

rad.region2 <- rad.region[rad.region$region %in% region.names,]

png("RadcliffeLocations_Experiments_Observations.png", width=10, height=5, units="in", res=220)
ggplot(data=dat.map) +
  geom_path(data=rad.region, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=Lon, y=Lat, color=type), size=2.5, alpha=0.75) +
  scale_x_continuous(limits=c(min(dat.map$Lon, na.rm=T)-5, max(dat.map$Lon, na.rm=T)+5), expand=c(0,0)) +
  scale_y_continuous(limits=c(min(dat.map$Lat, na.rm=T)-8, max(dat.map$Lat, na.rm=T)+5), expand=c(0,0)) +
  scale_color_manual(values=c("red", "blue")) +
  theme_bw() +
  theme(legend.position="top") +
  coord_equal()
dev.off()