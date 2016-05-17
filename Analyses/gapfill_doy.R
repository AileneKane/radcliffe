# Making a function to gapfill data: 
# # NOTE: need at least some data during the year to train the model
# #       i.e. this won't work for Price & Dunne where there's no air temp
# # NOTE: I tried to get it to preserve the order of the original data frame
# Function requirements (columns): 
#   site == name of the site you're working at
#   fillvar == name of the data variable you're trying to gapfill
#   treatvar == name of the treatment variable you want to borrow strength from
#   doy.by == what factor you want to fit the day of year cycle to; recomended: site or site.treatment
#   year == year of observation 
#   doy  == julian day of year


gapfill.doy <- function(gap.data, fillvar, treatvar, doy.by, data.by){
  # make a dummy dataset with a genearlized name structure
  data.temp <- gap.data
  data.temp[,c("metvar", "treatvar", "doy.by", "data.by")] <- data.temp[,c(fillvar, treatvar, doy.by, data.by)]
  
  # 1. Create a model for the annual temperature swing *by site*
  #  This assumes that all plots etc. at a site share a basic climatology
  if(length(unique(data.temp$doy.by))>1){
    doy.cycle <- gam(metvar ~ s(doy, by=doy.by) + doy.by, data=data.temp, na.action=na.omit)
  } else {
    doy.cycle <- gam(metvar ~ s(doy, by=doy.by), data=data.temp, na.action=na.omit)
  }
  data.temp$met.doy <- predict(doy.cycle, newdata=data.temp, na.action=na.omit)
  
  # 2. Calculate the residuals (temperature anomaly from expected doy cycle)
  data.temp[!is.na(data.temp$metvar),"resid.raw"] <- resid(doy.cycle)
  
  # 2.a. Observed treatment-based residuals for year/day
  resid.treat <- aggregate(data.temp[,"resid.raw"], 
                           by=data.temp[,c("doy.by", "data.by", "treatvar", "year", "doy")],
                           FUN=mean, na.rm=T)
  names(resid.treat)[ncol(resid.treat)] <- "resid.raw"
  
  # 2.b. Modeling treatment anomalies for year/day
  # 2.b.1. Calculate site-level anomaly
  resid.site <- aggregate(data.temp[,c("resid.raw")], 
                          by=data.temp[,c("data.by", "year", "doy")],
                          FUN=mean, na.rm=T)
  names(resid.site)[ncol(resid.site)] <- "resid.site"
  
  # 2.b.2. Calculate treatment effect on site-level residuals
  resid.treat <- merge(resid.treat, resid.site[,!(names(resid.site)=="treatvar")], sort=F)   # merge site level anomalies in
  resid.treat$dev <- resid.treat$resid.raw - resid.treat$resid.site # find the treatment deviation from site-level anomaly
  
  treat.effect <- lm(dev ~ treatvar, data=resid.treat, na.action=na.omit) # use a linear model to look at the effect of treatments
  
  # 2.b.3. modeled treatment residual is the site level residual + the treatment effect on anomalies
  resid.treat$resid2 <- resid.treat$resid.site + predict(treat.effect, newdata=resid.treat) 
  
  # 2.b.4. figure out which residual is our best estimate of temperature using a tiered system:
  #         1. if we have a treatement-level anomaly, use that (fewer levels of modeling)
  #         2. if treatment has no anomaly, estimate the treatment anomaly from the site-level mean
  resid.treat$resid.use <- ifelse(!is.na(resid.treat$resid.raw), resid.treat$resid.raw, resid.treat$resid2)

  # 2.b.5 Figure out a treatvar adjustment for the doy cycle
  data.temp$doy.use <- data.temp$met.doy + predict(treat.effect, newdata=data.temp) 
  
  # 3. Gapfill the data!!
  data.temp <- merge(data.temp, resid.treat[,c("doy.by", "data.by", "year", "doy", "treatvar", "resid.use")])
  
  data.temp$met.filled <- ifelse(!is.na(data.temp$metvar), data.temp$metvar, 
                                 ifelse(is.na(data.temp$resid.use), data.temp$doy.use,
                                        data.temp$met.doy + data.temp$resid.use))
  data.temp$met.flag <- as.factor(ifelse(!is.na(data.temp$metvar), "observed", 
                                         ifelse(is.na(data.temp$resid.use), "doy.adj",
                                                "doy.resid")))
  
  return(data.temp) 
}
