#Add life form to species
#life forms include: tree, shrub, grass, perenial

#Need to run the following before this can be run:

#Read in experimental climate and phenology data
#expclim<-read.csv("Analyses/gddchill/expclim.wchillgdd.csv", header=TRUE)
#exppheno<-read.csv("Analyses/exppheno.csv", header=TRUE)
# treats<-read.csv("Analyses/treats_detail.csv", header=T)
# #Make some choices about how to restrict the data and which model to use:
# remove.conifers=TRUE
# use.airtemp=TRUE
# use.centmod=FALSE
# 
# source("Analyses/source/standard_mergesandwrangling.R")
# 
# #summarize climate data by plot (annual and seasonal temp, soil mois), 
# #merge in with expgdd file, and select out only sites with soil moisture and air temperature data, and remove NAs
# source("Analyses/source/climsum_byplot.R")
# 
# #Prepare data for phenology models in stan
# source("Analyses/source/stanprep_phenmods.R")
##Now something with functional group
#source("Analyses/source/get_lifeform.R")

##get ecosystem
#source("Analyses/source/get_ecosystem.R")

##which species are most common (i.e., at the most sites?)
spsitetab<-table(expgdd_bbd$site,expgdd_bbd$genus.species)
spsitetab[which(spsitetab>0)]<-1
commonbbsp<-names(which(colSums(spsitetab)>1))

#for lo
##which species are most common (i.e., at the most sites?)
lospsitetab<-table(expgdd_lod$site,expgdd_lod$genus.species)
lospsitetab[which(lospsitetab>0)]<-1
commonlosp<-names(which(colSums(lospsitetab)>1))

#for fl
##which species are most common (i.e., at the most sites?)
flspsitetab<-table(expgdd_ffd$site,expgdd_ffd$genus.species)
flspsitetab[which(flspsitetab>0)]<-1
commonflsp<-names(which(colSums(flspsitetab)>1))



