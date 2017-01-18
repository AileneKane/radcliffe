### Started 24 March 2016 ###
### By Lizzie (for now) ###

## A quick map of the experimental sites ##


## housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## setting up
library(ggplot2)
library(rgdal)

setwd("~/git/radcliffe/Analyses")
## get the sites
expsites <- read.csv("expsiteinfo.csv", header=TRUE)
expsites$studylength <- (expsites$data_endyear-expsites$data_startyear)+1

##
## get map shape file (see map_notes.R for details)
##

wmap <- readOGR("maps/input/ne_110m_land", layer="ne_110m_land")
wmap.df <- fortify(wmap)

##
## plot, plot
##

theme.tanmap <- list(theme(panel.grid.minor = element_blank(),
                        # panel.grid.major = element_blank(),
                        panel.background = element_rect(fill = "grey90",colour = NA),
                        # plot.background = element_rect(fill=NA),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22),
                        legend.position = "left"))

pdf("maps/expsites.pdf", width=9,height=4)

ggplot() + 
  geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
  coord_cartesian(ylim=c(10, 90), xlim=c(-180, 40)) +
  geom_point(data=expsites, 
             aes(x=Long, y=Lat, size=studylength, fill=warming_type), 
             colour="black", pch=21) +
  theme.tanmap

dev.off()
