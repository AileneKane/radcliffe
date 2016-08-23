# Analysis of the effective (observed) warming of each experiment:
library(car); library(reshape2)
options(stringsAsFactors = FALSE)

# ------------------------------
# 1. read & format the RAW data
# ------------------------------
{
expclim <- read.csv("expclim.csv")
expclim <- expclim[!is.na(expclim$doy),]
expclim$year.frac <- expclim$year + expclim$doy/366
summary(expclim)

# First creating a single vector for aboveground warming (tagging what its source is)
expclim[,"AGtemp_max"] <- apply(expclim[,c("airtemp_max", "cantemp_max", "surftemp_max")], 1, min, na.rm=T)
expclim[expclim$AGtemp_max==Inf,"AGtemp_max"] <- NA

expclim[,"AGtemp_min"] <- apply(expclim[,c("airtemp_min", "cantemp_min", "surftemp_min")], 1, min, na.rm=T)
expclim[expclim$AGtemp_min==Inf,"AGtemp_min"] <- NA

# Add a flag just to make it clear which kind of temperature I'm using
expclim[which(!is.na(expclim$surftemp_max)), "AG.type"] <- "surface"
expclim[which(!is.na(expclim$cantemp_max)), "AG.type"] <- "canopy"
expclim[which(!is.na(expclim$airtemp_max)), "AG.type"] <- "air"
expclim$AG.type <- as.factor(expclim$AG.type)

summary(expclim)

# Check the plot IDs
summary(expclim$plot)
}
# ------------------------------


# ------------------------------
# Calculate the day-level deviations from the control
# Both Aboveground and Belowground when possible
# ------------------------------
expclim.orig <- expclim
exp.dev <- NULL # Making the deviation dataframe NULL to start with
for(s in unique(expclim$site)){
  print(paste0(" ** processing site: ", s))
  dat.tmp <- expclim[expclim$site==s, ]
  dat.tmp$year.frac <- as.ordered(dat.tmp$year.frac)
  
  # identify the control plots; note: it didn't look like anything had both 0 and ambient, 
  # so I'm using these interchangeably
  plot.control <- unique(dat.tmp[dat.tmp$temptreat %in% c(NA, "0", "ambient") & dat.tmp$preciptreat %in% c(NA, "0", "ambient"), "plot"])
  plot.all <- unique(dat.tmp$plot)
  
  # Subsetting the datasets we want in a very clunky manner
  ag.max <- recast(dat.tmp[, c("year.frac", "plot", "AGtemp_max")], year.frac ~ plot, fun.aggregate=mean, na.rm=T)
  ag.min <- recast(dat.tmp[, c("year.frac", "plot", "AGtemp_min")], year.frac ~ plot, fun.aggregate=mean, na.rm=T)
  bg.max <- recast(dat.tmp[, c("year.frac", "plot", "soiltemp1_max")], year.frac ~ plot, fun.aggregate=mean, na.rm=T)
  bg.min <- recast(dat.tmp[, c("year.frac", "plot", "soiltemp1_min")], year.frac ~ plot, fun.aggregate=mean, na.rm=T)
  
  # Calculating the mean temp
  ag.mean <- (ag.max[,2:ncol(ag.max)] + ag.min[,2:ncol(ag.min)])/2
  ag.mean$year.frac <- ag.max$year.frac
  bg.mean <- (bg.max[,2:ncol(bg.max)] + bg.min[,2:ncol(bg.min)])/2
  bg.mean$year.frac <- bg.max$year.frac
  
  # Calculate the plot-level deviation from the mean of the control
  ag.max[,paste(plot.all)] <- ag.max[,paste(plot.all)] - apply(ag.max[,paste(plot.control)], 1, mean, na.rm=T)
  ag.min[,paste(plot.all)] <- ag.min[,paste(plot.all)] - apply(ag.min[,paste(plot.control)], 1, mean, na.rm=T)
  ag.mean[,paste(plot.all)] <- ag.mean[,paste(plot.all)] - apply(ag.mean[,paste(plot.control)], 1, mean, na.rm=T)

  bg.max[,paste(plot.all)] <- bg.max[,paste(plot.all)] - apply(bg.max[,paste(plot.control)], 1, mean, na.rm=T)
  bg.min[,paste(plot.all)] <- bg.min[,paste(plot.all)] - apply(bg.min[,paste(plot.control)], 1, mean, na.rm=T)
  bg.mean[,paste(plot.all)] <- bg.mean[,paste(plot.all)] - apply(bg.mean[,paste(plot.control)], 1, mean, na.rm=T)
  
  # Stack everything together and merge it back into the original dataset
  # "_dev" stands for deviation from control
  dat.stack <- stack(ag.max[,paste(plot.all)])
  names(dat.stack) <- c("AGtemp_max_dev", "plot")
  dat.stack$year.frac <- ag.max$year.frac
  dat.stack$site <- as.factor(s)
  dat.stack$AGtemp_min_dev  <- stack(ag.min [,paste(plot.all)])[,1]
  dat.stack$AGtemp_mean_dev <- stack(ag.mean[,paste(plot.all)])[,1]
  dat.stack$BGtemp_max_dev  <- stack(bg.max [,paste(plot.all)])[,1]
  dat.stack$BGtemp_min_dev  <- stack(bg.min [,paste(plot.all)])[,1]
  dat.stack$BGtemp_mean_dev <- stack(bg.mean[,paste(plot.all)])[,1]
  
  # make a data frame to store all of the deviations in
  # The merge needs to happen last otherwise it freaks out with the NAs when it tries to merge
  if(is.null(exp.dev)){
    exp.dev <- dat.stack
  } else {
    exp.dev <- rbind(exp.dev, dat.stack)
  }
}

# Check the plot IDs on exp.dev
summary(exp.dev$plot)

# Merge the deviations into expclim
expclim <- merge(expclim, exp.dev, all.x=T)
summary(expclim)

# Check the plot IDs on exp.dev
summary(expclim$plot)

# ------------------------------


# ------------------------------
# Aggregate the experimental data to produce
# treatment- and plot-level stats
# ------------------------------
{
treatvars <- c("AGtemp_max_dev", "AGtemp_min_dev", "AGtemp_mean_dev", 
                "BGtemp_max_dev", "BGtemp_min_dev", "BGtemp_mean_dev")

# Note: need to make NAs in our aggregation variables a dummy name to get aggregation to work right
expclim$plot        <- as.factor(ifelse(is.na(expclim$plot)       , "BLANK", paste(expclim$plot)       ))
expclim$temptreat   <- as.factor(ifelse(is.na(expclim$temptreat)  , "BLANK", paste(expclim$temptreat)  ))
expclim$preciptreat <- as.factor(ifelse(is.na(expclim$preciptreat), "BLANK", paste(expclim$preciptreat)))
expclim$block       <- as.factor(ifelse(is.na(expclim$block)      , "BLANK", paste(expclim$block)      ))
expclim$AG.type     <- as.factor(ifelse(is.na(expclim$AG.type)    , "BLANK", paste(expclim$AG.type)    ))

# ----------------------
# Aggregating by plot
# ----------------------
effect.plot <- aggregate(expclim[,treatvars],
                         by=expclim[,c("site", "plot", "temptreat", "preciptreat", "block", "AG.type")],
                         FUN=mean, na.rm=T)
# put our NAs back in 
effect.plot$plot        <- as.factor(ifelse(effect.plot$plot       =="BLANK", NA, paste(effect.plot$plot)       ))
effect.plot$temptreat   <- as.factor(ifelse(effect.plot$temptreat  =="BLANK", NA, paste(effect.plot$temptreat)  ))
effect.plot$preciptreat <- as.factor(ifelse(effect.plot$preciptreat=="BLANK", NA, paste(effect.plot$preciptreat)))
effect.plot$block       <- as.factor(ifelse(effect.plot$block      =="BLANK", NA, paste(effect.plot$block)      ))
effect.plot$AG.type     <- as.factor(ifelse(effect.plot$AG.type    =="BLANK", NA, paste(effect.plot$AG.type)    ))

summary(effect.plot)

# check plotIDs 
summary(effect.plot$plot)


# Save as a csv
write.csv(effect.plot, "EffectiveWarming_Plot.csv", row.names=F, eol="\n")
# ----------------------

# ----------------------
# Aggregating by Treatment directly from raw
#  NOTE: if you want to go through plot first to remove any bias, that can easily be done
# ----------------------
effect.treat <- aggregate(expclim[,treatvars],
                         by=expclim[,c("site", "temptreat", "preciptreat", "AG.type")],
                         FUN=mean, na.rm=T)
# put our NAs back in 
effect.treat$temptreat   <- as.factor(ifelse(effect.treat$temptreat  =="BLANK", NA, paste(effect.treat$temptreat)  ))
effect.treat$preciptreat <- as.factor(ifelse(effect.treat$preciptreat=="BLANK", NA, paste(effect.treat$preciptreat)))
effect.treat$AG.type     <- as.factor(ifelse(effect.treat$AG.type    =="BLANK", NA, paste(effect.treat$AG.type)    ))

summary(effect.treat)

# Save as a csv
write.csv(effect.treat, "EffectiveWarming_Treatment.csv", row.names=F, eol="\n")
# ----------------------

}
# ------------------------------
