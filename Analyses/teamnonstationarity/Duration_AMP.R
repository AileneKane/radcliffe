#Main question: How does temperature sensitivity change over time in experiments and in observational studies?
#For this analysis, we are focusing only on Gothic Datasets
rm(list=ls()) 
ls()
#AMP: setwd("~/Desktop/Radcliffe/radcliffe")

#required libraries (added by AME)
library(data.table)
library(ggplot2)
source("Analyses/source/gddme.R")

#used for ggplot2
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#colors for ggplot2
colors_treatment<-c("Warmed"="#d7191c", "Control"="#2c7bb6")
linetype_treatment<-c("Warmed"="#d7191c", "Control"="#2c7bb6")

#importing data
obsdata <- read.csv("Analyses/obspheno.csv", header=TRUE)
obsclim<-read.csv("Analyses/obsclim.csv", header=TRUE)
expdata <- read.csv("Analyses/exppheno.csv", header=TRUE)
expclim<-read.csv("Analyses/expclim.csv", header=TRUE)
expinfo<-read.csv("Analyses/expsiteinfo.csv", header=TRUE)
exptreat<-read.csv("Analyses/treats.csv", header=TRUE)
snow<-read.csv("Analyses/teamnonstationarity/meltdate1991.1998.csv") #snowmelt data is from zone C only.  consider pulling mean date for entire plot from papers
gdd<-read.csv("Analyses/gddchill/expclim.wgdd.csv", header=TRUE)

###Adding a column to edata and eclim that has the start year of the experiment/observations
#getting start years
expinfo.start<-expinfo[,c("Site","exp_startyear")]
expinfo.start<-setnames(expinfo.start, "Site", "site") 

#merging duration with data files
expdata<-merge(expdata, expinfo.start, all=TRUE)
expclim<-merge(expclim, expinfo.start, all=TRUE)

#Creating a column with the number of years into the experiment (or the observational study) that data was collected
expdata$year.num<-expdata$year-expdata$exp_startyear
expclim$year.num<-expclim$year-expclim$exp_startyear

#subsetting to just get Gothic data
odata<-subset(obsdata, site %in% c("gothic")) #counts 162352 / 211952 (77% of all obs pheno data)
odata<-droplevels(odata)
oclim<-subset(obsclim, site %in% c("gothic")) #49044/980880 (5% of all obs climate data)
oclim<-droplevels(oclim)
edata<-subset(expdata, site %in% c("price", "dunne")) #12,252/76,359 (16% of all experimental pheno data)
edata<-droplevels(edata)

#dropping rows with missing plot information
edata<-subset(edata, plot %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
edata$plot<-as.factor(edata$plot)
edata<-droplevels(edata)
rownames(edata)<-NULL

eclim<-subset(expclim, site %in% c("price", "dunne")) #17979/327632 (5% of all experimental climate data)
eclim<-droplevels(eclim)
eclim<-eclim[,c("site", "temptreat", "plot", "year", "doy", "soiltemp1_min", "soiltemp1_max", "soiltemp1_mean" )]
rownames(eclim)<-NULL
levels(eclim$plot)

#adding start data column to observational data (for gothic only)
oclim$obs_startyear<-1973
odata$obs_startyear<-1973
oclim$year.num<-oclim$year-oclim$obs_startyear
odata$year.num<-odata$year-odata$obs_startyear

#getting snowmelt data in the format to merge
snow<-melt(snow, id=c("plot"), measured=c("X1991" ,"X1992", "X1993", "X1994" ,"X1995" ,"X1996", "X1997", "X1998"))
colnames(snow)[1:3]<-c("plot", "year", "meltdate")
snow$year<-as.numeric(substr(as.factor(snow$year), 2, 5))
snow$plot<-as.factor(snow$plot)

#merging snowmelt date with edata
edata<-merge(edata, snow, all=TRUE)

#dropping 1990 data because we don't have snowmelt
edata<-edata[edata$year!=1990,]

#dropping years from climate data that we don't have phenology data
eclim<-subset(eclim, year!="1999") #there is no phenology data from 1999; we can remove this line once the expclim.csv has been updated
eclim<-droplevels(eclim)

#merging snowmelt date with climate data
eclim<-merge(eclim, snow, all=TRUE)
 
write.csv(eclim, "Analyses/teamnonstationarity/eclim.csv", row.names=FALSE)
write.csv(edata, "Analyses/teamnonstationarity/edata.csv", row.names=FALSE)

##### Calculating temperature sensitivity #################
#I can't get GGD to work right now so our measure of sensitivity will be PMD (post-melt date; the # of days
# the event occured after snowmelt).  After this section, I try to work on GDD

edata<-read.csv("Analyses/teamnonstationarity/edata.csv")

eclim$PMD<-eclim$doy-eclim$meltdate #PMD= postmelt days, my measure of GDD
edata$PMD<-edata$doy-edata$meltdate

#adding treatment to data files
eclim$treatment<-ifelse(eclim$plot=="1"|eclim$plot=="3"| eclim$plot=="5"|eclim$plot=="7"|eclim$plot=="9", "Control", "Warmed")
edata$treatment<-ifelse(edata$plot=="1"|edata$plot=="3"| edata$plot=="5"|edata$plot=="7"|edata$plot=="9", "Control", "Warmed")

#Looking at temperature sensitivity by event type
# First Flower Date 
edata_ffd<-subset(edata, event=="ffd")
edata_ffd<-droplevels(edata_ffd)

edata_summaryffd<-summarySE(edata_ffd, measurevar="PMD", groupvars=c("treatment", "year.num"))
edata_summaryffd$treatment<-as.factor(edata_summaryffd$treatment)

ffd<-ggplot(edata_summaryffd, aes(x=year.num, y=PMD, color=treatment)) +
  geom_errorbar(aes(ymin=PMD-ci, ymax=PMD+ci), size=1, width=.2) +
  geom_point(size=7)+
  geom_line(aes(group=treatment),size=2) +
  scale_color_manual(values=colors_treatment) +
  scale_linetype_manual(values=linetype_treatment)+
  theme(axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18),
        axis.title.x=element_text(size=20, hjust=.5, vjust=0),
        axis.title.y=element_text(size=20, hjust=.5, vjust=1),
        legend.text=element_text(size=16, hjust=1, vjust=0),
        legend.title=element_text(size=16),
        plot.title=element_text(size=24, vjust=1),
        panel.background=element_rect(),
        panel.border=element_rect(fill=NA, color="black", size=1))+
  ylab("Post-melt Flowering Date")+
  xlab("Experiment Duration")
ffd

# First Fruit Date
edata_ffrd<-subset(edata, event=="ffrd")
edata_ffrd<-droplevels(edata_ffrd)

edata_summaryffrd<-summarySE(edata_ffrd, measurevar="PMD", groupvars=c("treatment", "year.num"))
edata_summaryffrd$treatment<-as.factor(edata_summaryffrd$treatment)

ffrd<-ggplot(edata_summaryffrd, aes(x=year.num, y=PMD, color=treatment)) +
  geom_errorbar(aes(ymin=PMD-ci, ymax=PMD+ci), size=1, width=.2) +
  geom_point(size=7)+
  geom_line(aes(group=treatment),size=2) +
  scale_color_manual(values=colors_treatment) +
  scale_linetype_manual(values=linetype_treatment)+
  theme(axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18),
        axis.title.x=element_text(size=20, hjust=.5, vjust=0),
        axis.title.y=element_text(size=20, hjust=.5, vjust=1),
        legend.text=element_text(size=16, hjust=1, vjust=0),
        legend.title=element_text(size=16),
        plot.title=element_text(size=24, vjust=1),
        panel.background=element_rect(),
        panel.border=element_rect(fill=NA, color="black", size=1))+
  ylab("Post-melt Fruit Date")+
  xlab("Experiment Duration")
ffrd

# First Seed Date
edata_sd<-subset(edata, event=="sd")
edata_sd<-droplevels(edata_sd)

edata_summarysd<-summarySE(edata_sd, measurevar="PMD", groupvars=c("treatment", "year.num"))
edata_summarysd$treatment<-as.factor(edata_summarysd$treatment)

sd<-ggplot(edata_summarysd, aes(x=year.num, y=PMD, color=treatment)) +
  geom_errorbar(aes(ymin=PMD-ci, ymax=PMD+ci), size=1, width=.2) +
  geom_point(size=7)+
  geom_line(aes(group=treatment),size=2) +
  scale_color_manual(values=colors_treatment) +
  scale_linetype_manual(values=linetype_treatment)+
  theme(axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18),
        axis.title.x=element_text(size=20, hjust=.5, vjust=0),
        axis.title.y=element_text(size=20, hjust=.5, vjust=1),
        legend.text=element_text(size=16, hjust=1, vjust=0),
        legend.title=element_text(size=16),
        plot.title=element_text(size=24, vjust=1),
        panel.background=element_rect(),
        panel.border=element_rect(fill=NA, color="black", size=1))+
  ylab("Post-melt Seed Date")+
  xlab("Experiment Duration")
sd

#Summary of these trends:
# There is a 4 year lag in experimental warming induced temperature sensitivity change (relative to control plots)  
# Beginning in year 2 (which was 1993, temperature sensitivities increase in both warmed and observational (control) plots.
# This trend supports the hypothesis that warming decreases temperature sensitivity of phenology.
# We now want to see if we find the same trend David Inouye's observational study.
# For David's data, we can't look temparature sensitivity using plot-level PMD; we don't have meltdate data
## We really need GDD data for this.
# For now, let's make a big assumtion that snowmelt date in David's plots is the same as the average meltdate of warming meadow plots.

#calculating average control plot meltdate per year (1991-1998)
meltC<-subset(snow, plot %in% c("1", "3", "5", "7", "9"))
meltC_mean<-as.data.frame(ddply(meltC, .(year), plyr::summarize, meltdate=mean(meltdate), .drop=FALSE))

#attaching yearly meltdates to David's data
odata_short<-subset(odata, year>="1991"& year<="1998")
odata_short<-merge(odata, meltC_mean)

#Calculating PMD
odata_short$PMD<-odata_short$doy-odata_short$meltdate

#Looking at temperature sensitivity 
# First Flower Date 
odata_summaryffd<-summarySE(odata_short, measurevar="PMD", groupvars=c("year.num"))

ffd<-ggplot(odata_summaryffd, aes(x=year.num, y=PMD)) +
  geom_errorbar(aes(ymin=PMD-ci, ymax=PMD+ci), size=1, width=.2) +
  geom_point(size=7)+
  geom_line(size=2) +
  theme(axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18),
        axis.title.x=element_text(size=20, hjust=.5, vjust=0),
        axis.title.y=element_text(size=20, hjust=.5, vjust=1),
        legend.text=element_text(size=16, hjust=1, vjust=0),
        legend.title=element_text(size=16),
        plot.title=element_text(size=24, vjust=1),
        panel.background=element_rect(),
        panel.border=element_rect(fill=NA, color="black", size=1))+
  ylab("Post-melt Flowering Date")+
  xlab("Observatinal Duration")
ffd


###Beginning here, Anne Marie has trouble and could use help!
#####Calculating GDD for each doy (and ideally each DSM- Days Since Snowmelt)

#reading in warming meadow experimental climate data
eclim <- read.csv("Analyses/teamnonstationarity/eclim.csv") #AME so I can back out easily and reset eclim
## setup
eclim<-setnames(eclim, "soiltemp1_mean", "meansoil1temp") #changing column name to match Lizzie's code

###Lizzie's code
## now make the threshold data
thresh <- 5 # set the base temp as 5, AMP: we may want to make this 1C
eclim$gddthreshmeansoil <- makethreshold.data(eclim, "meansoil1temp", thresh)

#AME: and, if doy < meltdate, then gddthreshmeansoil = 0
eclim$gddthreshmeansoil[eclim$doy < eclim$meltdate] <- 0

#create new variable gddusethresh which zeros out gddthreshmeansoil; fills in zero for every NA 
#we may not want to do this; we might want to estimate NAs
eclim$gddusethresh<-eclim$gddthreshmeansoil
eclim$gddusethresh[is.na(eclim$gddusethresh)]<-0


#########################Maybe a better GDD calculator for gothic data?###############
#AME-Uncomment below line and run
#AMP- Right now I don't know how to convert a list (which is formed for ggdusethresh below using aggregate ()) into a numeric column
#AMP-Also, we only want to sum soil temperature when it is greater than our threshold (5C) ; I don't think the code below does this.

#ggdusethresh=soiltemperature 

#eclim.soil.gdd<- aggregate(gddusethresh~plot+year+site+doy, data=eclim, cumsum)

#write.csv(eclim.soil.gdd, "Analyses/teamnonstationarity/eclim.soil.gdd", row.names=FALSE)
#######################################################################################


# Okay, need to calculate GDD for soil mean temps
# for each plot x site x year
eclim$giantcol <- paste(eclim$site, eclim$plot, eclim$temptreat, eclim$year)
eclim <- subset(eclim, is.na(doy)==FALSE) 
uniquethings <- unique(eclim$giantcol)
length(uniquethings) # super

eclim.wgdd <- eclim[1,]
eclim.wgdd$gddmeansoilNA <- NA
eclim.wgdd$gddmeansoil <- NA
eclim.wgdd <- eclim.wgdd[-1,]

# now we loop through each plot x site x year and calculate the GDD for soil and for air and count how many NAs we treated as 0

for (j in 1:length(uniquethings)){
  print(j)
  subby <- subset(eclim, giantcol==uniquethings[j])
  subby <- subby[order(as.numeric(subby$doy)),]
  subby$gddmeansoil <- makegdd.data.skipNA(subby, "gddusethresh", "doy", 1, 5)
  subby$gddmeansoilNA <- countNA.pergdd(subby, "gddusethresh", "doy", 1, 5)
  eclim.wgdd <- rbind(eclim.wgdd, subby)
}

#trying this for using meltdate as a start date
for (j in 1:length(uniquethings))
  print(j)
  subby <- subset(eclim, giantcol==uniquethings[j])
  subby <- subby[order(as.numeric(subby$doy)),]
  subby$gddmeansoil <- makegdd.data.skipNA(subby, "gddthreshmeansoil", "doy", 1, 365)
  subby$gddmeansoilNA <- countNA.pergdd(subby, "gddthreshmeansoil", "doy", 1, 365)
  eclim.wgdd <- rbind(eclim.wgdd, subby)
}
write.csv(eclim.wgdd, "Analyses/teamnonstationarity/eclim.wgdd_gothic.csv", row.names=FALSE)


# if you want to change the factors used ....
# you change what is in (subby, "gddthreshmeanair", "doy", 1, 5)
# to (what data frame to call, what column to use to calc GDD, name of column for doy, ...
# what day to start counting, what doy to require data before (see f(x) file for more info) )




