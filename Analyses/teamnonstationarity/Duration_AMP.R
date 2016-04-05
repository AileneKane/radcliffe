#Main question: How does temperature sensitivity change over time in experiments and in observational studies?
#For this analysis, we are focusing only on Gothic Datasets

#importing data
obsdata <- read.csv("Analyses/obspheno.csv", header=TRUE)
obsclim<-read.csv("Analyses/obsclim.csv", header=TRUE)
expdata <- read.csv("Analyses/exppheno.csv", header=TRUE)
expclim<-read.csv("Analyses/expclim.csv", header=TRUE)
expinfo<-read.csv("Analyses/expsiteinfo.csv", header=TRUE)
exptreat<-read.csv("Analyses/treats.csv", header=TRUE)

#subsetting to just get Gothic data
odata<-subset(odata, site %in% c("gothic")) #counts 162352 / 211952 (77% of all obs pheno data)
odata<-droplevels(odata)
oclim<-subset(obsclim, site %in% c("gothic")) #49044/980880 (5% of all obs climate data)
oclim<-droplevels(oclim)

edata<-subset(expdata, site %in% c("price", "dunne")) #12,252/76,359 (16% of all experimental pheno data)
edata<-droplevels(edata)
eclim<-subset(expclim, site %in% c("price", "dunne")) #17979/327632 (5% of all experimental climate data)
eclim<-droplevels(eclim)

###Adding a column to edata and eclim that has the start year of the experiment
#getting start years
expinfo.start<-subset(expinfo, Site %in% c("price", "dunne"))
expinfo.start<-expinfo.start[,c("Site","exp_startyear")]
expinfo.start<-setnames(expinfo.start, "Site", "site")

#merging duration with data files
edata<-merge(edata, expinfo.start, all=TRUE)
eclim<-merge(eclim, expinfo.start, all=TRUE)

#adding start data column to obs
oclim$obs_startyear<-1973
odata$obs_startyear<-1973

#Creating a column with the number of years into the experiment (or the observational study) that data was collected
oclim$year.num<-oclim$year-oclim$obs_startyear
eclim$year.num<-eclim$year-eclim$exp_startyear
edata$year.num<-edata$year-edata$exp_startyear
eclim$year.num<-eclim$year-eclim$exp_startyear



