# ------------------------------------------
# Gapfilling Experimental Met Data
# C. Rollinson, crollinson@gmail.com 
# 
# I've made a function to gapfill data using an annual cycle that
# you can fit as you see fit; see gapfill_doy.R for more details.
# In this script I use that function to gapfill the data using 
# year-specific doy cycles for each site.  
#
# Note: Right now the script is set up to gap fill by site & year
#       so each year has a slightly different shape & amplitude 
#       to the cycle. This means that only site-year combinations 
#       that have at least some data can be gapfilled.  
#
# ** PLEASE look at the pdf files in the figures directory before 
#    using these data!! ***
# ------------------------------------------
rm(list=ls())
# ------------------------------------------
# Load Libraries & Datasets
# ------------------------------------------
library(ggplot2)
library(mgcv)

# Read in experimental met data 
expclim <- read.csv("../expclim.csv")
expclim <- expclim[!is.na(expclim$doy),] # get rid of data that has no day of year (bc it's unusable)
expclim$year.frac <- expclim$year + expclim$doy/366 # Add a continuous time variable that is fractional years

summary(expclim)


#2021: Removing chuind/exp02 for now becuase it is giving me problems- not sure why ! it worked before...
expclim<-expclim[expclim$site!="exp02",]

# Saving the raw for now
expclim.orig <- expclim
# changing NA to a factor of "none" to make the gapfilling work (will change it back to it's original state after we add missing days)
expclim$temptreat   <- as.factor(ifelse(is.na(expclim$temptreat), "none", paste(expclim$temptreat)))
expclim$preciptreat <- as.factor(ifelse(is.na(expclim$preciptreat), "none", paste(expclim$preciptreat)))
expclim$plot        <- as.factor(ifelse(is.na(expclim$plot), "none", paste(expclim$plot)))
expclim$block       <- as.factor(ifelse(is.na(expclim$block), "none", paste(expclim$block)))

# For aboveground temperature, make a single variable that uses air, 
#    In case there's more than one measurement (I don't think there is),
#    we'll use the min reported temp since IRRs tend to over-estimate temp
expclim[,"temp_max"] <- apply(expclim[,c("airtemp_max", "cantemp_max", "surftemp_max")], 1, min, na.rm=T)
expclim[expclim$temp_max==Inf,"temp_max"] <- NA

expclim[,"temp_min"] <- apply(expclim[,c("airtemp_min", "cantemp_min", "surftemp_min")], 1, min, na.rm=T)
expclim[expclim$temp_min==Inf,"temp_min"] <- NA
summary(expclim)

# Add a flag just to make it clear which kind of temperature I'm using
expclim[which(!is.na(expclim$surftemp_max)), "type.temp"] <- "surface"
expclim[which(!is.na(expclim$cantemp_max)), "type.temp"] <- "canopy"
expclim[which(!is.na(expclim$airtemp_max)), "type.temp"] <- "air"
expclim[which(is.na(expclim$temp_max)), "type.temp"] <- "none"
expclim$type.temp <- as.factor(expclim$type.temp)
summary(expclim)


# # Something's going very odd in our raw data, so we're going to do a quick aggregate to make sure we don't have duplicate days
expclim <- aggregate(expclim[,c("temp_max", "temp_min", "soiltemp1_max", "soiltemp1_min")],
                     by=expclim[,c("site", "temptreat", "preciptreat", "plot", "block", "year", "doy", "year.frac", "type.temp")],
                     FUN=mean, na.rm=T)
summary(expclim)


# Making sure we don't have missing rows in between the max and min for each site-year
doy <- 1:365
expclim$year.doy <- as.factor(paste(expclim$year, expclim$doy, sep="."))
for(s in unique(expclim$site)){
  print(paste0("processing site: ", s))
  yr.doy <- unique(expclim[expclim$site==s,"year.doy"])
  # Getting the unique treatment & plot info for each site to help the merge work
  site.info <- aggregate(expclim[expclim$site==s,"year.frac"], 
                         by=expclim[expclim$site==s,c("site", "temptreat", "preciptreat", "plot", "block")],
                         FUN=min, na.rm=F)
  site.info <- site.info[,1:(ncol(site.info)-1)]
#   site.info
  
  # Finding the start and end for each site
  yf.min <- min(expclim[expclim$site==s, "year.frac"])
  yf.max <- max(expclim[expclim$site==s, "year.frac"])
  
  yrs <- data.frame(year=min(expclim[expclim$site==s, "year"]):max(expclim[expclim$site==s, "year"]))
  site.dates <- merge(yrs, data.frame(doy=doy))

  site.dates$year.frac <- site.dates$year + site.dates$doy/365
  site.dates$year.doy <- as.factor(paste(site.dates$year, site.dates$doy, sep="."))

  site.dates <- site.dates[site.dates$year.frac>=yf.min & site.dates$year.frac<=yf.max,]
  
  site.dates <- merge(site.info, site.dates[,], all.x=T, all.y=T)
  expclim <- merge(expclim, site.dates[,c("site","plot", "year", "doy", "temptreat", "preciptreat", "block")], all.x=T, all.y=T)
}


cols.keep <- c("plot", "year", "doy", "year.frac")
# test1 <- expclim[expclim$site=="exp09 & expclim$plot=="1A" & expclim$year==2009,]
test1 <- expclim[expclim$site=="exp02" & expclim$plot=="a1" & expclim$year==2004,]
test1$doy <- as.factor(test1$doy)
summary(test1$doy)
#test1[test1$doy=="18",]
# 
# test1b <- expclim.orig[expclim.orig$site=="exp09" & expclim.orig$plot=="1A" & expclim.orig$year==2009,]
# test1b$doy <- as.factor(test1b$doy)
# test1b[test1b$doy=="5",]
# summary(test1b$doy)

# Redo year.frac
expclim$year.frac <- expclim$year + expclim$doy/366 # Add a continuous time variable that is fractional years

# # Note: BACE canopy temps (modeled) extremely high, so I'm excluding the ones that are flat out impossible (>100 C = above boiling)
# # Note: Ailene checked with everybody and said the values that are there should be left alone, so I'm doing so
# expclim[!is.na(expclim$temp_max) & expclim$temp_max>100,"temp_max"] <- NA

# Putting our NAs back where appropriate
# expclim[expclim$temptreat  =="none", "temptreat"  ] <- NA
# expclim[expclim$preciptreat=="none", "preciptreat"] <- NA
# expclim[expclim$plot       =="none", "plot"       ] <- NA
# summary(expclim)

# Making some useful indicies
expclim$site.year <- as.factor(paste(expclim$site, expclim$year, sep="."))
expclim$plot <- as.ordered(expclim$plot)
expclim$temptreat3 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "ambient"), "0", paste(expclim$temptreat)))
# ------------------------------------------


# ------------------------------------------
# Gap Filling: 
#   1. Small plot-level gaps (e.g. Harvard/Ellison): 
#       - Here we are missing data for individual plots that can be filled
#         with low uncertainty the actual temperature value and experimental 
#         deviation from ambient using other observations from the same 
#         experiment (ideally same treatment)
#   2. Large site-level data gaps (e.g. Sherry air temp)
#       - Here we have decent met data for periods that let us estimate the
#         difference among plots/treatments & estimate the offset for an additional
#         site-level met station (e.g. FORCE, Harvard, BACE)
#
#
# Workflow:
#   1. Generic Model that fits by site with additional effects by treatment
#       - Temperature ~ DOY*Year
#   2. Calculate the anomalies for each day as a function of deviation from 
#      the general pattern
#       - Anomaly ~ Plot|Treatment # maybe do average plot bais, at very least treatment
# ------------------------------------------

source("gapfill_doy.R")

# ---------------------------------
# Airtemp_max 
# ---------------------------------
# 1. Figure out which sites have at least some air data
airmax.orig <- expclim
for(s in unique(expclim$site)){
  print(paste0(" ----- ",s, " ----- "))
  if(!max(expclim[expclim$site==s,"temp_max"], na.rm=T)>0){ 
    print(paste0("   no Tmax"))
    airmax.orig <- airmax.orig[!airmax.orig$site==s &!is.na(airmax.orig$site), ] 
  } else {
    time.min <- min(airmax.orig[airmax.orig$site==s & !is.na(airmax.orig$temp_max), "year.frac"])
    time.max <- max(airmax.orig[airmax.orig$site==s & !is.na(airmax.orig$temp_max), "year.frac"])
    airmax.orig <- airmax.orig[!airmax.orig$site==s | (airmax.orig$site==s & airmax.orig$year.frac>=time.min & airmax.orig$year.frac<=time.max),]
  }
}
airmax.orig$site.treat <- as.factor(paste(airmax.orig$site, airmax.orig$temptreat3, sep="."))
summary(airmax.orig)

# Recode no plot code to "other
airmax.orig[,"plot"] <- as.factor(ifelse(is.na(airmax.orig$plot), "other", paste(airmax.orig$plot)))

# code plots approrpiately 
for(s in unique(airmax.orig$site)){
  for(p in unique(airmax.orig[airmax.orig$site==s, "plot"])){
    if(!length(airmax.orig[!is.na(airmax.orig$temp_max) & airmax.orig$site==s & airmax.orig$plot==p, "temp_max"])>0){
      airmax.orig[airmax.orig$site==s & airmax.orig$plot==p,"plot2"] <- NA
    } else {
      airmax.orig[airmax.orig$site==s & airmax.orig$plot==p & !is.na(airmax.orig$plot==p),"plot2"] <- p
    }
  }
}
# Dealing with plots that don't have any data 
#  -- for these, try to find an appropriate pair based on temp & precip treatment
#  -- for simplicity's sake, we're going to match with the first plot of those matches
for(s in unique(airmax.orig[is.na(airmax.orig$plot2),"site"])){
  plots.bad <- unique(airmax.orig[is.na(airmax.orig$plot2) & airmax.orig$site==s,"plot"])
  for(p in plots.bad){
    t.treat <- unique(airmax.orig[airmax.orig$plot==p & airmax.orig$site==s, "temptreat"])
    p.treat <- unique(airmax.orig[airmax.orig$plot==p & airmax.orig$site==s, "preciptreat"])
    plot.match <- unique(airmax.orig[airmax.orig$site==s & airmax.orig$temptreat==t.treat & airmax.orig$preciptreat==p.treat & !airmax.orig$plot %in% plots.bad, "plot"])
    if(length(plot.match)==0){ # if no matches, do temperature treatment, even if it means getting rid of plots
#       next
      t.treat <- unique(airmax.orig[airmax.orig$site==s & airmax.orig$plot==p, "temptreat3"]  )
      if(length(t.treat) == 0){print(paste("**WARNING**  site: ", s, "; plot: ", p, " -- NO MATCHES")); next}
      airmax.orig[airmax.orig$site==s & airmax.orig$temptreat3==t.treat, "plot2"] <- t.treat
    } else { # if there is one or more matches, use the first one
      airmax.orig[airmax.orig$site==s & airmax.orig$plot==p, "plot2"] <- plot.match[1]
    }
    
  }
}
summary(airmax.orig)

# Making a variable to do adjustments by that's plot (or treatment) by plot
airmax.orig$treatvar <- as.factor(paste(airmax.orig$site, airmax.orig$plot2, sep="."))

# Run the gapfilling model!
#try running this one site at a time...#exp13 has no data
dim(airmax.orig)
#airmax.orig02<-airmax.orig[airmax.orig$site=="exp02",]#trying to figure out what's wrong...
#dim(airmax.orig15)

fill.airtmax <- gapfill.doy(gap.data=airmax.orig, fillvar="temp_max", treatvar="treatvar", doy.by="site.year", data.by="site")
summary(fill.airtmax)

pdf("../figures/Gapfill_airtemp_max2021.pdf", height=8.5, width=11.5)
for(s in unique(fill.airtmax$site)){
  print(
    ggplot(data=fill.airtmax[fill.airtmax$site==s,]) +
      facet_wrap(~plot) +
      # geom_point(aes(x=year.frac, y=met.filled, color=met.flag), size=0.5) +
      geom_line(aes(x=year.frac, y=temp_max), size=0.5, color="black") +
      geom_point(data=fill.airtmax[fill.airtmax$site==s & is.na(fill.airtmax$temp_max),], aes(x=year.frac, y=met.filled), size=0.5, color="red") +
      ggtitle(s) +
      scale_x_continuous(name="Year") +
      scale_y_continuous(name="Maximum Daily Air Temperature (degrees C)")
  )
}
dev.off()
# ---------------------------------

# ---------------------------------
# Airtemp_min 
# ---------------------------------
airmin.orig <- expclim
for(s in unique(expclim$site)){
  if(!max(expclim[expclim$site==s,"temp_min"], na.rm=T)>0){ 
    airmin.orig <- airmin.orig[!airmin.orig$site==s, ] 
  } else {
    time.min <- min(airmin.orig[airmin.orig$site==s & !is.na(airmin.orig$temp_min), "year.frac"])
    time.max <- max(airmin.orig[airmin.orig$site==s & !is.na(airmin.orig$temp_min), "year.frac"])
    airmin.orig <- airmin.orig[!airmin.orig$site==s | (airmin.orig$site==s & airmin.orig$year.frac>=time.min & airmin.orig$year.frac<=time.max),]
  }
}
airmin.orig$site.treat <- as.factor(paste(airmin.orig$site, airmin.orig$temptreat3, sep="."))
summary(airmin.orig)


# Recode no plot code to "other
airmin.orig[,"plot"] <- as.factor(ifelse(is.na(airmin.orig$plot), "other", paste(airmin.orig$plot)))

# code plots approrpiately 
for(s in unique(airmin.orig$site)){
  for(p in unique(airmin.orig[airmin.orig$site==s, "plot"])){
    if(!length(airmin.orig[!is.na(airmin.orig$temp_min) & airmin.orig$site==s & airmin.orig$plot==p, "temp_min"])>0){
      airmin.orig[airmin.orig$site==s & airmin.orig$plot==p,"plot2"] <- NA
    } else {
      airmin.orig[airmin.orig$site==s & airmin.orig$plot==p & !is.na(airmin.orig$plot==p),"plot2"] <- p
    }
  }
}
# Dealing with plots that don't have any data 
#  -- for these, try to find an appropriate pair based on temp & precip treatment
#  -- for simplicity's sake, we're going to match with the first plot of those matches
for(s in unique(airmin.orig[is.na(airmin.orig$plot2),"site"])){
  plots.bad <- unique(airmin.orig[is.na(airmin.orig$plot2) & airmin.orig$site==s,"plot"])
  for(p in plots.bad){
    t.treat <- unique(airmin.orig[airmin.orig$plot==p & airmin.orig$site==s, "temptreat"])
    p.treat <- unique(airmin.orig[airmin.orig$plot==p & airmin.orig$site==s, "preciptreat"])
    plot.match <- unique(airmin.orig[airmin.orig$site==s & airmin.orig$temptreat==t.treat & airmin.orig$preciptreat==p.treat & !airmin.orig$plot %in% plots.bad, "plot"])
    if(length(plot.match)==0){ # if no matches, do temperature treatment, even if it means getting rid of plots
      #       next
      t.treat <- unique(airmin.orig[airmin.orig$site==s & airmin.orig$plot==p, "temptreat3"]  )
      if(length(t.treat) == 0){print(paste("**WARNING**  site: ", s, "; plot: ", p, " -- NO MATCHES")); next}
      airmin.orig[airmin.orig$site==s & airmin.orig$temptreat3==t.treat, "plot2"] <- t.treat
    } else { # if there is one or more matches, use the first one
      airmin.orig[airmin.orig$site==s & airmin.orig$plot==p, "plot2"] <- plot.match[1]
    }
    
  }
}
summary(airmin.orig)

# Making a variable to do adjustments by that's plot (or treatment) by plot
airmin.orig$treatvar <- as.factor(paste(airmin.orig$site, airmin.orig$plot2, sep="."))

# Run the gapfilling model!
fill.airtmin <- gapfill.doy(gap.data=airmin.orig, fillvar="temp_min", treatvar="treatvar", doy.by="site.year", data.by="site")
summary(fill.airtmin)

pdf("../figures/Gapfill_airtemp_min2021.pdf", height=8.5, width=11.5)
for(s in unique(fill.airtmin$site)){
  print(
    ggplot(data=fill.airtmin[fill.airtmin$site==s,]) +
      facet_wrap(~plot) +
      geom_line(aes(x=year.frac, y=temp_min), size=0.5, color="black") +
      geom_point(data=fill.airtmin[fill.airtmin$site==s & is.na(fill.airtmin$temp_min),], aes(x=year.frac, y=met.filled), size=0.5, color="red") +
      ggtitle(s) +
      scale_x_continuous(name="Year") +
      scale_y_continuous(name="Minimum Daily Air Temperature (degrees C)")
  )
}
dev.off()
# ---------------------------------

# ---------------------------------
# Soiltemp_max 
# ---------------------------------
soilmax.orig <- expclim
for(s in unique(expclim$site)){
  if(!max(expclim[expclim$site==s,"soiltemp1_max"], na.rm=T)>0){ 
    soilmax.orig <- soilmax.orig[!soilmax.orig$site==s, ] 
  } else {
    time.min <- min(soilmax.orig[soilmax.orig$site==s & !is.na(soilmax.orig$soiltemp1_max), "year.frac"])
    time.max <- max(soilmax.orig[soilmax.orig$site==s & !is.na(soilmax.orig$soiltemp1_max), "year.frac"])
    soilmax.orig <- soilmax.orig[!soilmax.orig$site==s | (soilmax.orig$site==s & soilmax.orig$year.frac>=time.min & soilmax.orig$year.frac<=time.max),]
  }
}
soilmax.orig$site.treat <- as.factor(paste(soilmax.orig$site, soilmax.orig$temptreat3, sep="."))
summary(soilmax.orig)


# Recode no plot code to "other
soilmax.orig[,"plot"] <- as.factor(ifelse(is.na(soilmax.orig$plot), "other", paste(soilmax.orig$plot)))

# code plots approrpiately 
for(s in unique(soilmax.orig$site)){
  for(p in unique(soilmax.orig[soilmax.orig$site==s, "plot"])){
    if(!length(soilmax.orig[!is.na(soilmax.orig$soiltemp1_max) & soilmax.orig$site==s & soilmax.orig$plot==p, "soiltemp1_max"])>0){
      soilmax.orig[soilmax.orig$site==s & soilmax.orig$plot==p,"plot2"] <- NA
    } else {
      soilmax.orig[soilmax.orig$site==s & soilmax.orig$plot==p & !is.na(soilmax.orig$plot==p),"plot2"] <- p
    }
  }
}
# Dealing with plots that don't have any data 
#  -- for these, try to find an appropriate pair based on temp & precip treatment
#  -- for simplicity's sake, we're going to match with the first plot of those matches
for(s in unique(soilmax.orig[is.na(soilmax.orig$plot2),"site"])){
  plots.bad <- unique(soilmax.orig[is.na(soilmax.orig$plot2) & soilmax.orig$site==s,"plot"])
  for(p in plots.bad){
    t.treat <- unique(soilmax.orig[soilmax.orig$plot==p & soilmax.orig$site==s, "temptreat"])
    p.treat <- unique(soilmax.orig[soilmax.orig$plot==p & soilmax.orig$site==s, "preciptreat"])
    plot.match <- unique(soilmax.orig[soilmax.orig$site==s & soilmax.orig$temptreat==t.treat & soilmax.orig$preciptreat==p.treat & !soilmax.orig$plot %in% plots.bad, "plot"])
    if(length(plot.match)==0){ # if no matches, do temperature treatment, even if it means getting rid of plots
      #       next
      t.treat <- unique(soilmax.orig[soilmax.orig$site==s & soilmax.orig$plot==p, "temptreat3"]  )
      if(length(t.treat) == 0){print(paste("**WARNING**  site: ", s, "; plot: ", p, " -- NO MATCHES")); next}
      soilmax.orig[soilmax.orig$site==s & soilmax.orig$temptreat3==t.treat, "plot2"] <- t.treat
    } else { # if there is one or more matches, use the first one
      soilmax.orig[soilmax.orig$site==s & soilmax.orig$plot==p, "plot2"] <- plot.match[1]
    }
    
  }
}
summary(soilmax.orig)

# Problem: The DOY cycle for exp06 & exp11 is tricky because they're missing most winter data
# Solution: Create a dummy doy and single year that runs April to April so the tails are in a place of almost linear change
soilmax.orig$year.orig <- soilmax.orig$year
soilmax.orig$doy.orig  <- soilmax.orig$doy
soilmax.orig[soilmax.orig$site %in% c("exp11", "exp06"), "year"] <- 9999 # Dummy year that clearly isn't real

# Rewriting the site.year with the dummy year
soilmax.orig$site.year <- as.factor(paste(soilmax.orig$site, soilmax.orig$year, sep="."))

# Making a variable to do adjustments by that's plot (or treatment) by plot
soilmax.orig$treatvar <- as.factor(paste(soilmax.orig$site, soilmax.orig$plot2, sep="."))
#remove exp14, which is causing problems; we won't be using soil temnp anything i think?
soilmax.orig$treatvar <- as.factor(paste(soilmax.orig$site, soilmax.orig$plot2, sep="."))

# Run the gapfilling model!
fill.soiltmax <- gapfill.doy(gap.data=soilmax.orig, fillvar="soiltemp1_max", treatvar="treatvar", doy.by="site.year", data.by="site")
summary(fill.soiltmax)

# Putting things back to how they should be
fill.soiltmax$year <- soilmax.orig$year.orig
fill.soiltmax$doy  <- soilmax.orig$doy.orig
fill.soiltmax$site.year <- as.factor(paste(fill.soiltmax$site, fill.soiltmax$year, sep="."))


# exp06 has a problem with missing winter data, so we're going to say that when the temp 
# drops below the minimum observed, just call it the minimum
exp06.min <- quantile(fill.soiltmax[fill.soiltmax$site=="exp06","soiltemp1_max"], 0.005, na.rm=T) # fill with a very low value (but not the min, because it might be an outlier)
fill.soiltmax[,"met.flag"] <- as.factor(ifelse(!is.na(fill.soiltmax$met.filled) & fill.soiltmax$site=="exp06" & fill.soiltmax$met.filled < exp06.min, "forced_min", paste(fill.soiltmax$met.flag)))
fill.soiltmax[!is.na(fill.soiltmax$met.filled) & fill.soiltmax$site=="exp06" & fill.soiltmax$met.filled < exp06.min,"met.filled"] <- exp06.min

pdf("../figures/Gapfill_soiltemp1_max2021.pdf", height=8.5, width=11.5)
for(s in unique(fill.soiltmax$site)){
  print(
    ggplot(data=fill.soiltmax[fill.soiltmax$site==s,]) +
      facet_wrap(~plot) +
      geom_line(aes(x=year.frac, y=soiltemp1_max), size=0.5, color="black") +
      geom_point(data=fill.soiltmax[fill.soiltmax$site==s & is.na(fill.soiltmax$soiltemp1_max),], aes(x=year.frac, y=met.filled), size=0.5, color="red") +
      ggtitle(s) +
      scale_x_continuous(name="Year") +
      scale_y_continuous(name="maximum Daily Soil Temperature (degrees C)")
  )
}
dev.off()
# ---------------------------------


# ---------------------------------
# Soiltemp_min 
# ---------------------------------
soilmin.orig <- expclim
for(s in unique(expclim$site)){
  if(!max(expclim[expclim$site==s,"soiltemp1_min"], na.rm=T)>0){ 
    soilmin.orig <- soilmin.orig[!soilmin.orig$site==s, ] 
  } else {
    time.min <- min(soilmin.orig[soilmin.orig$site==s & !is.na(soilmin.orig$soiltemp1_min), "year.frac"])
    time.max <- max(soilmin.orig[soilmin.orig$site==s & !is.na(soilmin.orig$soiltemp1_min), "year.frac"])
    soilmin.orig <- soilmin.orig[!soilmin.orig$site==s | (soilmin.orig$site==s & soilmin.orig$year.frac>=time.min & soilmin.orig$year.frac<=time.max),]
  }
}
soilmin.orig$site.treat <- as.factor(paste(soilmin.orig$site, soilmin.orig$temptreat3, sep="."))
summary(soilmin.orig)

# Recode no plot code to "other
soilmin.orig[,"plot"] <- as.factor(ifelse(is.na(soilmin.orig$plot), "other", paste(soilmin.orig$plot)))

# code plots approrpiately 
for(s in unique(soilmin.orig$site)){
  for(p in unique(soilmin.orig[soilmin.orig$site==s, "plot"])){
    if(!length(soilmin.orig[!is.na(soilmin.orig$soiltemp1_min) & soilmin.orig$site==s & soilmin.orig$plot==p, "soiltemp1_min"])>0){
      soilmin.orig[soilmin.orig$site==s & soilmin.orig$plot==p,"plot2"] <- NA
    } else {
      soilmin.orig[soilmin.orig$site==s & soilmin.orig$plot==p & !is.na(soilmin.orig$plot==p),"plot2"] <- p
    }
  }
}
# Dealing with plots that don't have any data 
#  -- for these, try to find an appropriate pair based on temp & precip treatment
#  -- for simplicity's sake, we're going to match with the first plot of those matches
for(s in unique(soilmin.orig[is.na(soilmin.orig$plot2),"site"])){
  plots.bad <- unique(soilmin.orig[is.na(soilmin.orig$plot2) & soilmin.orig$site==s,"plot"])
  for(p in plots.bad){
    t.treat <- unique(soilmin.orig[soilmin.orig$plot==p & soilmin.orig$site==s, "temptreat"])
    p.treat <- unique(soilmin.orig[soilmin.orig$plot==p & soilmin.orig$site==s, "preciptreat"])
    plot.match <- unique(soilmin.orig[soilmin.orig$site==s & soilmin.orig$temptreat==t.treat & soilmin.orig$preciptreat==p.treat & !soilmin.orig$plot %in% plots.bad, "plot"])
    if(length(plot.match)==0){ # if no matches, do temperature treatment, even if it means getting rid of plots
      #       next
      t.treat <- unique(soilmin.orig[soilmin.orig$site==s & soilmin.orig$plot==p, "temptreat3"]  )
      if(length(t.treat) == 0){print(paste("**WARNING**  site: ", s, "; plot: ", p, " -- NO MATCHES")); next}
      soilmin.orig[soilmin.orig$site==s & soilmin.orig$temptreat3==t.treat, "plot2"] <- t.treat
    } else { # if there is one or more matches, use the first one
      soilmin.orig[soilmin.orig$site==s & soilmin.orig$plot==p, "plot2"] <- plot.match[1]
    }
    
  }
}
summary(soilmin.orig)

# Making a variable to do adjustments by that's plot (or treatment) by plot
soilmin.orig$treatvar <- as.factor(paste(soilmin.orig$site, soilmin.orig$plot2, sep="."))

# Problem: The DOY cycle for exp06 & exp11 is tricky because they're missing most winter data
# Solution: Create a dummy doy and single year that runs April to April so the tails are in a place of almost linear change
soilmin.orig$year.orig <- soilmin.orig$year
soilmin.orig$doy.orig <- soilmin.orig$doy
soilmin.orig[soilmin.orig$site %in% c("exp11", "exp06"), "year"] <- 9999 # Dummy year that clearly isn't real

soilmin.orig$site.year <- as.factor(paste(soilmin.orig$site, soilmin.orig$year, sep="."))

# Run the gapfilling model!
fill.soiltmin <- gapfill.doy(gap.data=soilmin.orig, fillvar="soiltemp1_min", treatvar="treatvar", doy.by="site.year", data.by="site")
summary(fill.soiltmin)

# Putting the original doy & year in place
fill.soiltmin$doy <- fill.soiltmin$doy.orig
fill.soiltmin$year <- fill.soiltmin$year.orig

# Need to do something about exp06 because it has no winter temps & does weird things
min.exp06 <- quantile(fill.soiltmin[fill.soiltmin$site=="exp06","soiltemp1_min"], 0.005, na.rm=T) # fill with a very low value (but not the min, because it might be an outlier)
fill.soiltmin[,"met.flag"] <- as.factor(ifelse(!is.na(fill.soiltmin$met.filled) & fill.soiltmin$site=="exp06" & (fill.soiltmin$met.filled<min.exp06 | (fill.soiltmin$met.flag=="doy.adj" & fill.soiltmin$doy<90)), "forced_min", paste(fill.soiltmin$met.flag)))
fill.soiltmin[!is.na(fill.soiltmin$met.filled) & fill.soiltmin$site=="exp06" & (fill.soiltmin$met.filled<min.exp06 | fill.soiltmin$met.flag=="forced_min"),"met.filled"] <- min.exp06

pdf("../figures/Gapfill_soiltemp1_min2021.pdf", height=8.5, width=11.5)
for(s in unique(fill.soiltmin$site)){
  print(
    ggplot(data=fill.soiltmin[fill.soiltmin$site==s,]) +
      facet_wrap(~plot) +
      geom_line(aes(x=year.frac, y=soiltemp1_min), size=0.5, color="black") +
      geom_point(data=fill.soiltmin[fill.soiltmin$site==s & is.na(fill.soiltmin$soiltemp1_min),], aes(x=year.frac, y=met.filled), size=0.5, color="red") +
      ggtitle(s) +
      scale_x_continuous(name="Year") +
      scale_y_continuous(name="Minimum Daily Soil Temperature (degrees C)")
  )
}
dev.off()
# ---------------------------------
# --------------------------------------------------------------

# --------------------------------------------------------------
# Merge the gapfilled climates in with 
# --------------------------------------------------------------
ag.type <- data.frame(site=unique(expclim[,"site"]))
for(s in unique(ag.type$site)){
  ag.type[ag.type$site==s, "AG.type"] <- unique(expclim[expclim$site==s ,"type.temp"])[1]
}
summary(ag.type)
ag.type


names(fill.airtmin)[names(fill.airtmin) %in% c("met.filled", "met.flag")] <- c("fill.AGtemp_min", "flag.AGtemp_min")
names(fill.airtmax)[names(fill.airtmax) %in% c("met.filled", "met.flag")] <- c("fill.AGtemp_max", "flag.AGtemp_max")
names(fill.soiltmin)[names(fill.soiltmin) %in% c("met.filled", "met.flag")] <- c("fill.soiltemp_min", "flag.soiltemp_min")
names(fill.soiltmax)[names(fill.soiltmax) %in% c("met.filled", "met.flag")] <- c("fill.soiltemp_max", "flag.soiltemp_max")

# Merging datasets together
cols.keep <- c("site", "plot", "year", "doy", "temptreat", "preciptreat", "block")
fill.tair  <- merge(fill.airtmin[,c(cols.keep, "temp_min", "fill.AGtemp_min", "flag.AGtemp_min")], fill.airtmax[,c(cols.keep, "temp_max", "fill.AGtemp_max", "flag.AGtemp_max")], all.x=T, all.y=T)
fill.tsoil <- merge(fill.soiltmin[,c(cols.keep, "soiltemp1_min", "fill.soiltemp_min", "flag.soiltemp_min")], fill.soiltmax[,c(cols.keep, "soiltemp1_max", "fill.soiltemp_max", "flag.soiltemp_max")], all.x=T, all.y=T)

#exp.gapfill <- merge(fill.tair, fill.tsoil, all.x=T, all.y=T)
#April 30, 2021: skipping soil for now...
exp.gapfill <- fill.tair

summary(exp.gapfill)

exp.gapfill <- merge(exp.gapfill, ag.type, all.x=T, all.y=F)
summary(exp.gapfill)

# Aggregate to get rid of duplicate days
#exp.gapfill <- aggregate(exp.gapfill[,c("temp_min", "fill.AGtemp_min", "temp_max", "fill.AGtemp_max", "soiltemp1_min", "fill.soiltemp_min", "soiltemp1_max", "fill.soiltemp_max")],
 #                        by=exp.gapfill[,c(cols.keep, "flag.AGtemp_min", "flag.AGtemp_max", "flag.soiltemp_min", "flag.soiltemp_max")],
  #                       FUN=mean, na.rm=T)
exp.gapfill <- aggregate(exp.gapfill[,c("temp_min", "fill.AGtemp_min", "temp_max", "fill.AGtemp_max")],
                         by=exp.gapfill[,c(cols.keep, "flag.AGtemp_min", "flag.AGtemp_max")],
                         FUN=mean, na.rm=T)

dim(exp.gapfill)

dim(expclim.orig)#not the same as above...not sure why.

# Put the original NAs back in
exp.gapfill[exp.gapfill$temptreat  =="none", "temptreat"  ] <- NA
exp.gapfill[exp.gapfill$preciptreat=="none", "preciptreat"] <- NA
exp.gapfill[exp.gapfill$plot       =="none", "plot"       ] <- NA
exp.gapfill[exp.gapfill$block      =="none", "block"      ] <- NA

write.csv(exp.gapfill, "../expclim_gapfillairtemp2021.csv", row.names=F, eol="\n")
# --------------------------------------------------------------



