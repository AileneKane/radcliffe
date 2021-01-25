#Prepare data for phenology models
#Center, remove NAs, make things factors that need to be, divide by phenophase

expclim2$target_cent<-scale(expclim2$target, center = TRUE, scale = TRUE)
expclim2$preciptreat_amt_cent<-scale(expclim2$preciptreat_amt, center = TRUE, scale = TRUE)
expclim2a<-subset(expclim2,select=c(site,year,doy,target_cent,preciptreat_amt,target,preciptreat_amt_cent,soilmois1,agtemp_mean))
expclim2a<- expclim2a [apply(expclim2a , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expclim2a$doy<-as.factor(expclim2a$doy)
expclim2a$year<-as.factor(expclim2a$year)
expclim2a$site<-as.factor(expclim2a$site)
expclim2a$preciptreat_prop<-expclim2a$preciptreat_amt/100


#Want to fit a model with soil moisture and above-ground temperature as predictors for doy of phenological event
#Start by looking at which studies have both SM and AG temp data
#which(tapply(expclim2$agtemp_mn,expclim2$site,mean,na.rm=T)>0)
#which(tapply(expclim2$soilmois1,expclim2$site,mean,na.rm=T)>0)

#Prep the data for Stan model
expgdd_subs$sp.name<-expgdd_subs$genus.species
expgdd_subs$genus.species<-as.numeric(as.factor(expgdd_subs$genus.species))
expgdd_subs$site2<-expgdd_subs$site
expgdd_subs$site<-as.numeric(as.factor(expgdd_subs$site))
expgdd_subs$year<-as.numeric(as.factor(expgdd_subs$year))
#models don't converge when perc is used....
#expgdd_subs$soilmoisperc_janmar<-as.numeric(expgdd_subs$soilmois_janmar)*100
#expgdd_subs$soilmoisperc_aprjun<-as.numeric(expgdd_subs$soilmois_aprjun)*100


#1) Divide by phenophase:

expgdd_bbd<-expgdd_subs[which(expgdd_subs$event=="bbd"),]#bud burst data
expgdd_bbd <- expgdd_bbd[apply(expgdd_bbd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expgdd_bbd_cont<-expgdd_bbd[expgdd_bbd$target==0,]


expgdd_lod<-expgdd_subs[which(expgdd_subs$event=="lod"),]#leaf out data
expgdd_lod <- expgdd_lod[apply(expgdd_lod, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expgdd_lod_cont<-expgdd_lod[expgdd_lod$target==0,]

#expgdd_lud<-expgdd_subs[which(expgdd_subs$event=="lud"),]#leaf unfolding data
#expgdd_lud <- expgdd_lud[apply(expgdd_lud, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

expgdd_ffd<-expgdd_subs[which(expgdd_subs$event=="ffd"),]#leaf unfolding data
expgdd_ffd <- expgdd_ffd[apply(expgdd_ffd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expgdd_ffd_cont<-expgdd_ffd[expgdd_ffd$target==0,]

expgdd_ffrd<-expgdd_subs[which(expgdd_subs$event=="ffrd"),]#leaf unfolding data
expgdd_ffrd <- expgdd_ffrd[apply(expgdd_ffrd, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expgdd_ffrd_cont<-expgdd_ffrd[expgdd_ffrd$target==0,]

expgdd_sen<-expgdd_subs[which(expgdd_subs$event=="sen"),]
expgdd_sen <- expgdd_sen[apply(expgdd_sen, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expgdd_sen_cont<-expgdd_sen[expgdd_sen$target==0,]

#For lod and lud, use only species which have all lod and lud, to see what is driving differences between these two models
#unique(expgdd_lud$genus.species)#many fewer species have lud- do not use this one!
#unique(expgdd_lod$genus.species)
#common.spp<-unique(expgdd_lud$genus.species[expgdd_lud$genus.species%in%expgdd_lod$genus.species])
#unique(expgdd_sen$genus.species)
#expgdd_lod_cs<-expgdd_lod[which(expgdd_lod$genus.species%in%common.spp),]
#expgdd_lud_cs<-expgdd_lud[which(expgdd_lud$genus.species%in%common.spp),]

# For centering data:
expgdd_bbd$sm_cent <- scale(expgdd_bbd$sm, center=TRUE, scale=TRUE)
expgdd_bbd$smjm_cent<-scale(expgdd_bbd$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_bbd$ag_min_jm_cent<-scale(expgdd_bbd$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_bbd$agtmax_cent<-scale(expgdd_bbd$agtmax, center = TRUE, scale = TRUE)

# For centering data:
expgdd_bbd_cont$sm_cent <- scale(expgdd_bbd_cont$sm, center=TRUE, scale=TRUE)
expgdd_bbd_cont$smjm_cent<-scale(expgdd_bbd_cont$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_bbd_cont$ag_min_jm_cent<-scale(expgdd_bbd_cont$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_bbd_cont$agtmax_cent<-scale(expgdd_bbd_cont$agtmax, center = TRUE, scale = TRUE)


expgdd_lod$sm_cent <- scale(expgdd_lod$sm, center=TRUE, scale=TRUE)
expgdd_lod$smjm_cent<-scale(expgdd_lod$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_lod$ag_min_jm_cent<-scale(expgdd_lod$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_lod$agtmax_cent<-scale(expgdd_lod$agtmax, center = TRUE, scale = TRUE)
expgdd_lod$ag_min_aprjun_cent<-scale(expgdd_lod$ag_min_aprjun, center = TRUE, scale = TRUE)
expgdd_lod$soilmois_aprjun_cent<-scale(expgdd_lod$soilmois_aprjun, center = TRUE, scale = TRUE)


#expgdd_lud$sm_cent <- scale(expgdd_lud$sm, center=TRUE, scale=TRUE)
#expgdd_lud$smjm_cent<-scale(expgdd_lud$soilmois_janmar, center = TRUE, scale = TRUE)
#expgdd_lud$ag_min_jm_cent<-scale(expgdd_lud$ag_min_janmar, center = TRUE, scale = TRUE)
#expgdd_lud$agtmax_cent<-scale(expgdd_lud$agtmax, center = TRUE, scale = TRUE)

expgdd_ffd$sm_cent <- scale(expgdd_ffd$sm, center=TRUE, scale=TRUE)
expgdd_ffd$agtmin_cent<-scale(expgdd_ffd$agtmin, center = TRUE, scale = TRUE)
expgdd_ffd$agtmax_cent<-scale(expgdd_ffd$agtmax, center = TRUE, scale = TRUE)

expgdd_ffrd$sm_cent <- scale(expgdd_ffrd$sm, center=TRUE, scale=TRUE)
expgdd_ffrd$agtmin_cent<-scale(expgdd_ffrd$agtmin, center = TRUE, scale = TRUE)
expgdd_ffrd$agtmax_cent<-scale(expgdd_ffrd$agtmax, center = TRUE, scale = TRUE)

expgdd_sen$sm_cent <- scale(expgdd_sen$sm, center=TRUE, scale=TRUE)
expgdd_sen$agtmin_cent<-scale(expgdd_sen$agtmin, center = TRUE, scale = TRUE)
expgdd_sen$agtmax_cent<-scale(expgdd_sen$agtmax, center = TRUE, scale = TRUE)



# Centering control only data:
expgdd_bbd_cont$sm_cent <- scale(expgdd_bbd_cont$sm, center=TRUE, scale=TRUE)
expgdd_bbd_cont$smjm_cent<-scale(expgdd_bbd_cont$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_bbd_cont$ag_min_jm_cent<-scale(expgdd_bbd_cont$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_bbd_cont$agtmax_cent<-scale(expgdd_bbd_cont$agtmax, center = TRUE, scale = TRUE)

#make sure that species match between control-only and full dataset
contsp<-unique(expgdd_bbd_cont$genus.species)
expgdd_bbdmatchcsp<-expgdd_bbd[expgdd_bbd$genus.species %in% contsp,]
#only lose 9 rows of data (5 species)
expgdd_bbdmatchcsp$sm_cent <- scale(expgdd_bbdmatchcsp$sm, center=TRUE, scale=TRUE)
expgdd_bbdmatchcsp$smjm_cent<-scale(expgdd_bbdmatchcsp$soilmois_janmar, center = TRUE, scale = TRUE)
expgdd_bbdmatchcsp$ag_min_jm_cent<-scale(expgdd_bbdmatchcsp$ag_min_janmar, center = TRUE, scale = TRUE)
expgdd_bbdmatchcsp$agtmax_cent<-scale(expgdd_bbdmatchcsp$agtmax, center = TRUE, scale = TRUE)

datalist.bbd <- with(expgdd_bbd, 
                     list(y = doy, 
                          temp = ag_min_janmar, #above-ground minimum air temp
                          mois = soilmois_janmar, #soil moisture
                          sp = genus.species,
                          site = site,
                          year = styear,
                          N = nrow(expgdd_bbd),
                          n_sp = length(unique(expgdd_bbd$genus.species))
                     )
)

datalist.bbd.cent <- with(expgdd_bbd, 
                          list(y = doy, 
                               temp = ag_min_jm_cent[,1], #above-ground minimum air temp
                               mois = smjm_cent[,1], #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbd),
                               n_sp = length(unique(expgdd_bbd$genus.species))
                          )
)
datalist.bbdcont <- with(expgdd_bbd_cont, 
                         list(y = doy, 
                              temp = ag_min_janmar, #above-ground minimum air temp
                              mois = soilmois_janmar, #soil moisture
                              sp = genus.species,
                              site = site,
                              year = year,
                              N = nrow(expgdd_bbd_cont),
                              n_sp = length(unique(expgdd_bbd_cont$genus.species))
                         )
)

datalist.bbdcont.cent <- with(expgdd_bbd_cont, 
                              list(y = doy, 
                                   temp = ag_min_jm_cent, #above-ground minimum air temp
                                   mois = smjm_cent, #soil moisture
                                   sp = genus.species,
                                   site = site,
                                   year = year,
                                   N = nrow(expgdd_bbd_cont),
                                   n_sp = length(unique(expgdd_bbd_cont$genus.species))
                              )
)

datalist.lod<- with(expgdd_lod, 
                    list(y = doy, 
                         temp = ag_min_aprjun, #above-ground minimum air temp
                         mois = soilmois_aprjun, #soil moisture
                         sp = genus.species,
                         site = site,
                         year = year,
                         N = nrow(expgdd_lod),
                         n_sp = length(unique(expgdd_lod$genus.species))
                    )
)


datalist.lod.cent <- with(expgdd_lod, 
                          list(y = doy, 
                               temp = ag_min_aprjun_cent, #above-ground minimum air temp
                               mois = soilmois_aprjun_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_lod),
                               n_sp = length(unique(expgdd_lod$genus.species))
                          )
)


#datalist.lud.cent <- with(expgdd_lud, 
#                         list(y = doy, 
#                             temp = ag_min_jm_cent, #above-ground minimum air temp
#                            mois = smjm_cent, #soil moisture
#                           sp = genus.species,
#                          site = site,
#                         year = year,
#                        N = nrow(expgdd_bbd),
#                       n_sp = length(unique(expgdd_bbd$genus.species))
#                 )
#)

#datalist.lud <- with(expgdd_lud, 
#                          list(y = doy, 
#                               temp = ag_min_janmar, #above-ground minimum air temp
#                               mois = soilmois_janmar, #soil moisture
#                               sp = genus.species,
#                               site = site,
#                               year = year,
#                               N = nrow(expgdd_bbd),
#                               n_sp = length(unique(expgdd_bbd$genus.species))
#                          )
#)

datalist.ffd.cent <- with(expgdd_ffd, 
                          list(y = doy, 
                               temp = agtmin_cent, #above-ground minimum air temp
                               mois = sm_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_ffd),
                               n_sp = length(unique(expgdd_ffd$genus.species))
                          )
)

datalist.ffrd.cent <- with(expgdd_ffrd, 
                           list(y = doy, 
                                temp = agtmin_cent, #above-ground minimum air temp
                                mois = sm_cent, #soil moisture
                                sp = genus.species,
                                site = site,
                                year = year,
                                N = nrow(expgdd_ffrd),
                                n_sp = length(unique(expgdd_ffrd$genus.species))
                           )
)

datalist.sen.cent <- with(expgdd_sen, 
                          list(y = doy, 
                               temp = agtmin_cent, #above-ground minimum air temp
                               mois = sm_cent, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_sen),
                               n_sp = length(unique(expgdd_sen$genus.species))
                          )
)

#set up datalists for bb and lo with matching sepcies

splegbb<- expgdd_bbd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)
splegbb<-splegbb[order(splegbb$genus.species),]
colnames(splegbb)[2]<-"spnumbb"
spleglo<-expgdd_lod %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)    
spleglo<-spleglo[order(spleglo$genus.species),]
colnames(spleglo)[2]<-"spnumlo"

splegfl<-expgdd_ffd %>% # start with the data frame
  distinct(sp.name, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(sp.name,genus.species)    
splegfl<-splegfl[order(splegfl$genus.species),]
colnames(splegfl)[2]<-"spnumfl"

spbblo<-full_join(splegbb,spleglo,by = "sp.name")
spbblo<- spbblo [apply(spbblo, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
dim(spbblo)

splofl<-full_join(spleglo,splegfl,by = "sp.name")
splofl<- splofl [apply(splofl, 1, function(x) all(!is.na(x))),] # only keep rows of all not na


#datalists for these more models fit to these more limited datasets
expgdd_bbdlo<-expgdd_bbd[expgdd_bbd$genus.species %in% spbblo$spnumbb,]

datalist.bbdlo.cent <- with(expgdd_bbdlo, 
                          list(y = doy, 
                               temp = ag_min_jm_cent[,1], #above-ground minimum air temp
                               mois = smjm_cent[,1], #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_bbdlo),
                               n_sp = length(unique(expgdd_bbdlo$genus.species))
                          )
)

expgdd_lodbb<-expgdd_lod[expgdd_lod$genus.species %in% spbblo$spnumlo,]

datalist.lodbb.cent <- with(expgdd_lodbb, 
                            list(y = doy, 
                                 temp = ag_min_jm_cent[,1], #above-ground minimum air temp
                                 mois = smjm_cent[,1], #soil moisture
                                 sp = genus.species,
                                 site = site,
                                 year = year,
                                 N = nrow(expgdd_lodbb),
                                 n_sp = length(unique(expgdd_lodbb$genus.species))
                            )
)

