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


#For bb
splegbb<- expgdd_bbd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegbb<-splegbb[order(splegbb$genus.species),]
colnames(splegbb)[2]<-"spnumbb"
dim(splegbb)
bbform<-left_join(splegbb,forms)
bbform$sp.name[bbform$sp.name=="Vaccinium.sp"]<-"shrub"
bbform$sp.name[bbform$sp.name=="Viburnum.cassinoides"]<-"shrub"
bbform$sp.name[bbform$sp.name=="Acer.barbatum"]<-"tree"
bbform$sp.name[bbform$sp.name=="Acer.pensylvanicum"]<-"tree"

#For leafout
spleglo<- expgdd_lod %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
spleglo<-spleglo[order(spleglo$genus.species),]
colnames(spleglo)[2]<-"spnumlo"
dim(spleglo)
loform<-left_join(spleglo,forms)
loform$form[loform$sp.name=="Vaccinium.sp"]<-"shrub"
loform$form[loform$sp.name=="Viburnum.cassinoides"]<-"shrub"
loform$form[loform$sp.name=="Viburnum.sp"]<-"shrub"
loform$form[loform$sp.name=="Acer.barbatum"]<-"tree"
loform$form[loform$sp.name=="Acer.pensylvanicum"]<-"tree"
loform$form[loform$sp.name=="Actea.racemosa"]<-"forb"
loform$form[loform$sp.name=="Amphicarpa.bracteata"]<-"forb"#should be Amphicarpaea bracteata
loform$form[loform$sp.name=="Anemone.americana"]<-"forb"
loform$form[loform$sp.name=="Ceanothus.americana"]<-"shrub"
loform$form[loform$sp.name=="Celastrus.orbiculatus"]<-"shrub"
loform$form[loform$sp.name=="Clematis.virginiana"]<-"forb"
loform$form[loform$sp.name=="Crataegus.sp"]<-"shrub"
loform$form[loform$sp.name=="Dianthus.ameria"]<-"forb"#should be   	Dianthus armeria L. 
loform$form[loform$sp.name=="Dioscorea.sp"]<-"forb"
loform$form[loform$sp.name=="Erechtites.hieraciifolia"]<-"forb"
loform$form[loform$sp.name=="Erigeron.anuus"]<-"forb"
loform$form[loform$sp.name=="Fraxinus.sp"]<-"tree"
loform$form[loform$sp.name=="Celastrus.orbiculatus"]<-"shrub"
loform$form[loform$sp.name=="Parthenocissus.quinquefolia"]<-"shrub"
loform$form[loform$sp.name=="Rosa.multiflora"]<-"shrub"
loform$form[loform$sp.name=="Rubus.sp"]<-"shrub"
loform$form[loform$sp.name=="Rubus.occidentalis"]<-"shrub"
loform$form[loform$sp.name=="Sambucus.canadensis"]<-"shrub"
loform$form[loform$sp.name=="Smilax.sp"]<-"shrub"
loform$form[loform$sp.name=="Ulmus.sp"]<-"tree"
loform$form[loform$sp.name=="Ranunculus.abortivis"]<-"forb"
loform$form[loform$sp.name=="Symphyotrichum.lavae"]<-"forb"
loform$form[loform$sp.name=="Viola.soria"]<-"forb"
loform$form[loform$sp.name=="Viola.sp"]<-"forb"
loform$form[loform$sp.name=="Vitis.sp"]<-"shrub"

splegfl<- expgdd_ffd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegfl<-splegfl[order(splegfl$genus.species),]
colnames(splegfl)[2]<-"spnumfl"
dim(splegfl)

#
splegfr<- expgdd_ffrd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegfr<-splegfr[order(splegfr$genus.species),]
colnames(splegfr)[2]<-"spnumfr"
dim(splegfr)

#
splegsen<- expgdd_sen %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegsen<-splegsen[order(splegsen$genus.species),]
colnames(splegsen)[2]<-"spnumsen"
dim(splegsen)
