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



library(raster)
nat.earth <- stack("~/Desktop/NE1_50M_SR_W/NE1_50M_SR_W.tif")
nat.crop <- crop(nat.earth, y=c(min(dat.map$Lon, na.rm=T)-5, max(dat.map$Lon, na.rm=T)+5, min(dat.map$Lat, na.rm=T)-10, max(dat.map$Lat, na.rm=T)+10))
nat.crop <- aggregate(nat.crop, fact=5, fun=mean)


rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE1_50M_SR_W.1,
                                       NE1_50M_SR_W.2,
                                       NE1_50M_SR_W.3,
                                       1))


# Note: the natural earth data takes quite a while to plot!`
png("RadcliffeLocations_Experiments_Observations.png", width=10, height=5, units="in", res=220)
ggplot(data=dat.map) +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude") +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude") +
  geom_point(aes(x=Lon, y=Lat, color=type), size=2.5, alpha=0.75) +
  scale_color_manual(values=c("red", "blue"), name="Data Type") +
  theme_bw() +
  theme(legend.position="top") +
  coord_equal()
dev.off()

