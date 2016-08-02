#Climate space work
#AMEllison, BCook, JDukes, AMPanetta, CRollinson

#4 April 2016

#libraries needed

library(ggplot2)
library(plyr)
library(aplpack)

expclim <-read.csv("expclim.csv")
obsclim <- read.csv("obsclim.csv")


#extract airtemp only from experiments
expclim.1 <- droplevels(expclim[,1:8])
obsclim.1 <- droplevels(obsclim)

#add tags for experimental vs. observational 
expclim.1$tag <- "Experimental"
obsclim.1$tag <- "Observational"


#merge experimental and observational into one file
allclim.1<-merge(expclim.1, obsclim.1, all=TRUE)
allclim.1$tag <- as.factor(allclim.1$tag)

#change preciptreat "NA" into "none", then combine into "0" if no change, and change to factor
allclim.1$preciptreat <- ifelse(is.na(allclim.1$preciptreat), "none", paste(allclim.1$preciptreat))
allclim.1$preciptreat <- ifelse(allclim.1$preciptreat %in% c("0", "none", "outside"), "0", paste(allclim.1$preciptreat))
allclim.1$preciptreat <- as.factor(allclim.1$preciptreat)

#code temptreat into warmed or not warmed and change to factor
allclim.1$temptreat <- ifelse(allclim.1$temptreat %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"), "warmed", "not warmed")
allclim.1$temptreat <- as.factor(allclim.1$temptreat)

#remove rows without temp data

allclim.2 <- allclim.1[!is.na(allclim.1$airtemp_min),]

#construct convex hulls; can vary groups within .( ... )

chulls <- ddply(allclim.2, .(temptreat, preciptreat), function(df) df[chull(df$airtemp_min, df$airtemp_max), ])

#plot. can vary according to facet_wrap( ... )

ggplot(allclim.2, aes(x=airtemp_min, y=airtemp_max, color=temptreat) ) +
	geom_point(shape=".") +
	geom_polygon(data=chulls, aes(x=airtemp_min, y=airtemp_max, fill=temptreat), alpha=0.2) +
	facet_wrap(~preciptreat, nrow=1) +
	geom_smooth(method="lm") +
	labs(list(x="Minimum air temperature C", y="Maximum air temperature C"))


#next steps
#1. redo with clean data
#2. bagplots
######do this with bagplots to get distributions of data:
#######https://gist.github.com/benmarwick/00772ccea2dd0b0f1745
#3. separate out matching data (for identical sites; with precip data)
#4. plot by season 90-day segments
#5, 

