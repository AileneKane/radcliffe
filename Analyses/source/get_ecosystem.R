#budburst
expgdd_bbd$ecosystem<-"forest"
expgdd_bbd$ecosystem[expgdd_bbd$site2=="exp01"]<-"grassland"
expgdd_bbd$sp.eco<-paste(expgdd_bbd$sp.name,expgdd_bbd$ecosystem)
bbecos<- expgdd_bbd %>% # start with the data frame
  distinct(sp.eco, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species,site2,ecosystem)
bbecos<-bbecos[order(bbecos$genus.species),]
colnames(bbecos)[2]<-"spnum"
dim(bbecos)

#leafout
expgdd_lod$ecosystem<-"forest"
expgdd_lod$ecosystem[expgdd_lod$site2=="exp01"]<-"grassland"
expgdd_lod$sp.eco<-paste(expgdd_lod$sp.name,expgdd_lod$ecosystem)
loecos<- expgdd_lod %>% # start with the data frame
  distinct(sp.eco, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species,site2,ecosystem)
loecos<-loecos[order(loecos$genus.species),]
colnames(loecos)[2]<-"spnum"
dim(loecos)

#flowering
expgdd_ffd$ecosystem<-"forest"
expgdd_ffd$ecosystem[expgdd_ffd$site2=="exp01"]<-"grassland"
expgdd_ffd$ecosystem[expgdd_ffd$site2=="exp12"]<-"grassland"
expgdd_ffd$sp.eco<-paste(expgdd_ffd$sp.name,expgdd_ffd$ecosystem)
ffecos<- expgdd_ffd %>% # start with the data frame
  distinct(sp.eco, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species,site2,ecosystem)
ffecos<-ffecos[order(ffecos$genus.species),]
colnames(ffecos)[2]<-"spnum"

