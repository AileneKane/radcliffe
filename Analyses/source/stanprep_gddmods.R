
expgdd_bb<-expgdd_gdd[expgdd_gdd$event=="bbd",]
expgdd_lo<-expgdd_gdd[expgdd_gdd$event=="lod",]
expgdd_fl<-expgdd_gdd[expgdd_gdd$event=="ffd",]

datalist.gddbb <- with(expgdd_bb, 
                     list(y = cumgdd_air, 
                          mois = soilmois_janmar, #soil moisture
                          sp = genus.species,
                          site = site,
                          year = styear,
                          N = nrow(expgdd_bbd),
                          n_sp = length(unique(expgdd_bbd$genus.species))
                     )
)


datalist.gddlo<- with(expgdd_lo, 
                    list(y = cumgdd_air, 
                         mois = soilmois_aprjun, #soil moisture
                         sp = genus.species,
                         site = site,
                         year = year,
                         N = nrow(expgdd_lo),
                         n_sp = length(unique(expgdd_lo$genus.species))
                    )
)



datalist.ffd <- with(expgdd_fl, 
                          list(y = doy, 
                               mois = sm, #soil moisture
                               sp = genus.species,
                               site = site,
                               year = year,
                               N = nrow(expgdd_ffd),
                               n_sp = length(unique(expgdd_ffd$genus.species))
                          )
)
