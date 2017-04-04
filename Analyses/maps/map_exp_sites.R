## A slightly fancier map showing experimental sites & some info about those ##
# Christy Rollinson 
# 2 February 2017


## housekeeping
rm(list=ls()) 

# Key libraries
library(ggplot2); library(maps)
# # library(rworldmap)

# Define our base directory
# Load in the locations for experimental and observational sites
expsites <- read.csv("../expsiteinfo.csv")

summary(expsites)

expsites$studylength <- (expsites$data_endyear-expsites$data_startyear)+1

# Reading in Natural earth data as a backdrop
# Natural Earth Data can be downloaded here: http://www.naturalearthdata.com/downloads/
# The specific file downloaded (Hypsometric tint with water) can be downloaded from here: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/raster/HYP_50M_SR_W.zip 
library(raster)
nat.earth <- stack("~/Desktop/SpatialData/NaturalEarth/HYP_50M_SR_W/HYP_50M_SR_W.tif")
nat.crop <- crop(nat.earth, y=c(min(expsites$Long, na.rm=T)-10, max(expsites$Long, na.rm=T)+10, min(expsites$Lat, na.rm=T)-35, max(expsites$Lat, na.rm=T)+35))
# nat.crop <- aggregate(nat.crop, fact=8, fun=mean) # This was just to help make faster graphs to test sizes, etc


rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(HYP_50M_SR_W.1,
                                       HYP_50M_SR_W.2,
                                       HYP_50M_SR_W.3,
                                       1))


# Note: the natural earth data takes quite a while to plot!
set.seed(1134)
png("RadcliffeLocations_Experiments.png", width=10, height=5, units="in", res=320)
ggplot(data=expsites) +
  theme_bw() +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude") +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude") +
  geom_point(aes(x=Long, y=Lat, shape=warming_type, color=warming_type, size=studylength), alpha=0.8, position=position_jitter(width=0.5, height=0.5)) +
  scale_shape_manual(values=c(19, 18, 15, 17)) +
  scale_color_manual(values=c("darkorange2", "firebrick3", "purple4", "black")) +
  scale_size(range=c(2.5,5)) +
  guides(shape=guide_legend(title="Warming Type", override.aes = list(size=6)),
         color=guide_legend(title="Warming Type"),
         size=guide_legend((title="Study Length"))) +
  theme(legend.position="right") +
  theme(legend.title=element_text(face="bold")) +
  coord_equal()
dev.off()

