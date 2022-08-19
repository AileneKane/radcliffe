#Make a map of experiments included in database
library(dplyr)
library(rworldmap)
# Key libraries
library(ggplot2); library(maps)
library(car)

setwd("~/GitHub/radcliffe")
#read in site info
expsites <- read.csv("Analyses/expsiteinfo.csv", header=TRUE)
expsites2 <- read.csv("Analyses/output/exps_table_sm.csv", header=TRUE)

#pull out lat longs from expsites
exsitetable<-expsites %>% 
  dplyr::select(DatasetID,Lat,Long)
colnames(exsitetable)[1]<-"study"

#add lat long to expsites2
expsites3<-left_join(expsites2 ,exsitetable)


# Reading in Natural earth data as a backdrop
# Natural Earth Data can be downloaded here: http://www.naturalearthdata.com/downloads/
# The specific file downloaded (Hypsometric tint with water) can be downloaded from here: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/raster/HYP_50M_SR_W.zip 
library(raster)
nat.earth <- stack("Data/HYP_50M_SR_W.tif")
nat.crop <- crop(nat.earth, y=c(min(expsites3$Long, na.rm=T)-10, max(expsites3$Long, na.rm=T)+10, min(expsites3$Lat, na.rm=T)-35, max(expsites3$Lat, na.rm=T)+35))
nat.crop <- aggregate(nat.crop, fact=8, fun=mean) # This was just to help make faster graphs to test sizes, etc


rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(HYP_50M_SR_W.1,
                                       HYP_50M_SR_W.2,
                                       HYP_50M_SR_W.3,
                                       1))


expsites3$ecosystem[expsites3$ecosystem=="temperate deciduous forest"] <- "forest"

eco.shapes <- c(5, 1)
eco.cols <- c("black", "black")#"goldenrod", "green3"

# Note: the natural earth data takes quite a while to plot!
# set.seed(1134)
png("Analyses/maps/soilms_experiments_map.png", width=8, height=4, units="in", res=220)
ggplot(data=expsites3) +
  theme_bw() +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude") +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude") +
   geom_point(aes(x=Long, y=Lat, shape=ecosystem, color=ecosystem), stroke=1.1, alpha=1, position=position_jitter(width=0.9, height=0.9)) +
   #scale_shape_manual(values=c(18, 19)) +
  scale_shape_manual(values=eco.shapes) +
  scale_color_manual(values=eco.cols) +
  scale_size(.9) +
  guides(shape=guide_legend(title="Ecosystem", override.aes = list(size=4), order=1, nrow=2, byrow=T),
         color=guide_legend(title="Ecosystem", order=1, nrow=2, byrow=T)) +
  theme(legend.position="top") +
  theme(legend.title=element_text(face="bold"),
        legend.key.size=unit(1.25, units="lines")) +
  coord_equal()
dev.off()




pdf(figname,height=12, width=6)
#quartz(height=12, width=6)
  par(omi=c(.5,.5,.5,.5), mfrow=c(2,2))

newmap <- getMap(resolution = "low")

#windows(height=10, width=10)
mapname<-paste("analyses/figures/exphenmap.pdf")

pdf(mapname,height=8, width=8)
}
plot(newmap, xlim = c(-120, 10), ylim = c(30, 51), asp = 1)
#d$Lat<-as.numeric(d$Lat);d$Long<-as.numeric(d$Long)
#points(d$Long[d$FishArea=="07"],d$Lat[d$FishArea=="07"], type="p",och=16, col="green")
#points(d$Long[d$FishArea=="06"],d$Lat[d$FishArea=="06"], type="p", col="blue")
#points(d$Long[d$FishArea=="06"],d$Lat[d$FishArea=="06"], type="p",col="blue")
#points(d$Long[d$FishArea=="05"],d$Lat[d$FishArea=="05"], type="p",col="red")
#points(d$Long[d$FishArea=="04"],d$Lat[d$FishArea=="04"], type="p",col="orange")
#points(d$Long[d$FishArea=="03"],d$Lat[d$FishArea=="03"], type="p",col="yellow")
#points(d$Long[d$FishArea=="02"],d$Lat[d$FishArea=="02"], type="p",col="yellow")
#points(d$Long[d$FishArea=="01"],d$Lat[d$FishArea=="01"], type="p",col="yellow")
#points(d$Long[d$FishArea=="09"],d$Lat[d$FishArea=="09"], type="p",col="purple")
#points(d$Long[d$FishArea=="81"],d$Lat[d$FishArea=="81"], type="p",col="gray")
#points(d$Long[d$FishArea=="82"],d$Lat[d$FishArea=="82"], type="p",col="gray")
#points(d$Long[d$FishArea=="12"],d$Lat[d$FishArea=="12"], type="p",col="lightblue")
#points(d$Long[d$FishArea=="11"],d$Lat[d$FishArea=="11"], type="p",col="lightgreen")
#points(d$Long[d$FishArea=="13"],d$Lat[d$FishArea=="13"], type="p",col="darkgreen")
#points(d$Long[d$FishArea=="10"],d$Lat[d$FishArea=="10"], type="p",col="pink")
#points(d$Long[d$FishArea=="18C"],d$Lat[d$FishArea=="18C"], type="p",col="lightgreen")
#points(d$Long[d$FishArea=="19C"],d$Lat[d$FishArea=="19C"], type="p",col="darkred")
#points(d$Long[d$FishArea=="20C"],d$Lat[d$FishArea=="20C"], type="p",col="pink")
#points(d$Long[d$FishArea=="21C"],d$Lat[d$FishArea=="21C"], type="p",col="purple")

points(meannumdays$lon[meannumdays$region=="ps"],meannumdays$lat[meannumdays$region=="ps"],type="p",pch=16, col=adjustcolor("salmon",alpha.f=0.6),cex=log(meannumdays$mnnumdays, base=10))
points(meannumdays$lon[meannumdays$region=="uss"],meannumdays$lat[meannumdays$region=="uss"],type="p",pch=16, col=adjustcolor("darkblue",alpha.f=0.4),cex=log(meannumdays$mnnumdays, base=10))
points(meannumdays$lon[meannumdays$region=="jf"],meannumdays$lat[meannumdays$region=="jf"],type="p",pch=16, col="darkgreen",cex=log(meannumdays$mnnumdays, base=10))

legend(-122.3,52,legend=c("Central Salish Sea","Puget Sound proper","# days per year= 2","# days per year= 80", "Lime Kiln", "Albion Test Fishery"),
       pch=c(16,16,16,16,8,17), col=c(adjustcolor("darkblue",alpha.f=0.4),adjustcolor("salmon",alpha.f=0.6),adjustcolor("darkblue",alpha.f=0.4),adjustcolor("darkblue",alpha.f=0.4),"black","black"), pt.cex=c(log(7, base=10),log(7, base=10),log(2, base=10), log(80, base=10),1,1))
#add  lime kiln
points(-123.1510,48.5160,pch=8,col="black" )
#add  albion test fishery

points(-122.62275,49.2104,pch=17,col="black" )

