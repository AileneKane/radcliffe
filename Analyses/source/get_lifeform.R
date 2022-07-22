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

#remove some rows when species are listed as both trees and shrubs

forms<-forms[-which(forms$sp.name=="Acer.saccharum" & forms$form=="shrub"),]
forms<-forms[-which(forms$sp.name=="Cercis.canadensis" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Cornus.florida" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Hamamelis.virginiana" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Ilex.opaca" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Ilex.verticillata" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Ilex.vomitoria" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Lindera.benzoin" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Magnolia.virginiana" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Ostrya.virginiana" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Oxydendrum.arboreum" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Prunus.pensylvanica" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Prunus.serotina" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Rhus.glabra" & forms$form=="tree"),]
forms<-forms[-which(forms$sp.name=="Sassafras.albidum" & forms$form=="shrub"),]
forms<-forms[-which(forms$sp.name=="Viburnum.prunifolium" & forms$form=="tree"),]

#For bb
splegbb<- expgdd_bbd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegbb<-splegbb[order(splegbb$genus.species),]
colnames(splegbb)[2]<-"spnumbb"
dim(splegbb)
bbform<-left_join(splegbb,forms)
bbform$form[bbform$sp.name=="Vaccinium.sp"]<-"shrub"
bbform$form[bbform$sp.name=="Viburnum.cassinoides"]<-"shrub"
bbform$form[bbform$sp.name=="Acer.barbatum"]<-"tree"
bbform$form[bbform$sp.name=="Acer.pensylvanicum "]<-"tree"

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
loform$form[loform$sp.name=="Acer.pensylvanicum "]<-"tree"
loform$form[loform$sp.name=="Anemone.americana"]<-"forb"
loform$form[loform$sp.name=="Celastrus.orbiculatus"]<-"shrub"
loform$form[loform$sp.name=="Circaea.canadensis"]<-"forb"
loform$form[loform$sp.name=="Clematis.virginiana"]<-"forb"
loform$form[loform$sp.name=="Crataegus.sp"]<-"shrub"
loform$form[loform$sp.name=="Dioscorea.sp"]<-"forb"
loform$form[loform$sp.name=="Fraxinus.sp"]<-"tree"
loform$form[loform$sp.name=="Lonicera.sp"]<-"shrub"
loform$form[loform$sp.name=="Parthenocissus.quinquefolia"]<-"shrub"
loform$form[loform$sp.name=="Populus.sp"]<-"tree"
loform$form[loform$sp.name=="Rosa.multiflora"]<-"shrub"
#loform$form[loform$sp.name=="Rubus.sp"]<-"shrub"
loform$form[loform$sp.name=="Rubus.occidentalis"]<-"shrub"
loform$form[loform$sp.name=="Sambucus.canadensis"]<-"shrub"
loform$form[loform$sp.name=="Smilax.sp"]<-"shrub"
loform$form[loform$sp.name=="Symphyotrichum.lavae"]<-"forb"
loform$form[loform$sp.name=="Ulmus.sp"]<-"tree"
loform$form[loform$sp.name=="Viola.soria"]<-"forb"
loform$form[loform$sp.name=="Viola.sp"]<-"forb"
loform$form[loform$sp.name=="Vitis.sp"]<-"shrub"


#For flowering
splegfl<- expgdd_ffd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegfl<-splegfl[order(splegfl$genus.species),]
colnames(splegfl)[2]<-"spnumfl"
dim(splegfl)

flform<-left_join(splegfl,forms)

flform$form[flform$sp.name=="Actea.racemosa"]<-"forb"
flform$form[flform$sp.name=="Ambrosia.psilostchya"]<-"forb"#should be Ambrosia psilostachy
flform$form[flform$sp.name=="Anemone.americana"]<-"forb"
flform$form[flform$sp.name=="Ceanothus.americana"]<-"shrub"
flform$form[flform$sp.name=="Chimaphila.maculata"]<-"forb"
flform$form[flform$sp.name=="Circaea.canadensis"]<-"forb"
flform$form[flform$sp.name=="Dianthus.ameria"]<-"grass"#should be armeria
flform$form[flform$sp.name=="Erechtites.hieraciifolia"]<-"forb"
flform$form[flform$sp.name=="Erigeron.anuus"]<-"forb"
flform$form[flform$sp.name=="Euthamia.gramanifolia"]<-"forb"
flform$form[flform$sp.name=="Festuca.sp"]<-"grass"
flform$form[flform$sp.name=="Galinsoga.ciliata"]<-"forb"
flform$form[flform$sp.name=="Lamium.amplexicaula"]<-"forb"#should be "Lamium amplexicaule"
flform$form[flform$sp.name=="Ranunculus.abortivis"]<-"forb"#should be "ranunculus abortivus
flform$form[flform$sp.name=="Rosa.multiflora"]<-"shrub"
flform$form[flform$sp.name=="Rubus.occidentalis"]<-"shrub"
flform$form[flform$sp.name=="Rubus.sp"]<-"shrub"
flform$form[flform$sp.name=="Setaria.glauca"]<-"grass"
flform$form[flform$sp.name=="Silene.alba"]<-"forb"
flform$form[flform$sp.name=="Symphyotrichum.lavae"]<-"forb"#should be laeve
flform$form[flform$sp.name=="Viola.soria"]<-"forb"
flform$form[flform$sp.name=="Viola.sp"]<-"forb"

#Fruiting and senescence- ignoring for now
# splegfr<- expgdd_ffrd %>% # start with the data frame
#   distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
#   dplyr::select(sp.name,genus.species)
# splegfr<-splegfr[order(splegfr$genus.species),]
# colnames(splegfr)[2]<-"spnumfr"
# dim(splegfr)
# frform<-left_join(splegfr,forms)
# frform$form[frform$sp.name=="Anemone.americana"]<-"forb"
# frform$form[frform$sp.name=="Actea.racemosa"]<-"forb"
# frform$form[frform$sp.name=="Ambrosia.psilostchya"]<-"forb"#should be Ambrosia psilostachy
# frform$form[frform$sp.name=="Amphicarpa.bracteata"]<-"forb"#should be Amphicarpaea bracteata
# 
# frform$form[frform$sp.name=="Ceanothus.americana"]<-"shrub"
# frform$form[frform$sp.name=="Circaea.canadensis"]<-"forb"
# 
# frform$form[frform$sp.name=="Dianthus.ameria"]<-"grass"#should be armeria
# frform$form[frform$sp.name=="Erechtites.hieraciifolia"]<-"forb"
# frform$form[frform$sp.name=="Erigeron.anuus"]<-"forb"
# frform$form[frform$sp.name=="Euthamia.gramanifolia"]<-"forb"
# 
# frform$form[frform$sp.name=="Lonicera.sp"]<-"shrub"
# frform$form[frform$sp.name=="Ranunculus.abortivis"]<-"forb"#should be "ranunculus abortivus
# frform$form[frform$sp.name=="Rosa.multiflora"]<-"shrub"
# frform$form[frform$sp.name=="Rubus.occidentalis"]<-"shrub"
# frform$form[frform$sp.name=="Rubus.sp"]<-"shrub"
# frform$form[frform$sp.name=="Symphyotrichum.lavae"]<-"forb"#should be laeve
# frform$form[frform$sp.name=="Viola.soria"]<-"forb"
# #
# splegsen<- expgdd_sen %>% # start with the data frame
#   distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
#   dplyr::select(sp.name,genus.species)
# splegsen<-splegsen[order(splegsen$genus.species),]
# colnames(splegsen)[2]<-"spnumsen"
# dim(splegsen)
# senform<-left_join(splegsen,forms)
# senform$form[senform$sp.name=="Lonicera.sp"]<-"shrub"
# senform$form[senform$sp.name=="Fraxinus.sp"]<-"shrub"
# senform$form[senform$sp.name=="Acer.pensylvanicum "]<-"tree"
# senform$form[senform$sp.name=="Actea.racemosa"]<-"forb"
# senform$form[senform$sp.name=="Amphicarpa.bracteata"]<-"forb"
# 
# senform$form[senform$sp.name=="Ranunculus.abortivis"]<-"forb"
# senform$form[senform$sp.name=="Rosa.multiflora"]<-"shrub"
# senform$form[senform$sp.name=="Rubus.occidentalis"]<-"shrub"
# senform$form[senform$sp.name=="Rubus.sp"]<-"shrub"
# 
# senform$form[senform$sp.name=="Rubus.sp"]<-"shrub"
# senform$form[senform$sp.name=="Smilax.sp"]<-"shrub"
# 
# senform$form[senform$sp.name=="Symphyotrichum.lavae"]<-"forb"#should be laeve
# senform$form[senform$sp.name=="Vaccinium.sp "]<-"shrub"
# senform$form[senform$sp.name=="Viburnum.cassinoides"]<-"shrub"
# senform$form[senform$sp.name=="Viola.soria"]<-"forb"
# senform$form[senform$sp.name=="Vitis.sp"]<-"shrub"

