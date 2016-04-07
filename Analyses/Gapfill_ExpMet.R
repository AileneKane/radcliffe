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

# ------------------------------------------
# Load Libraries & Datasets
# ------------------------------------------
library(ggplot2)
library(mgcv)

# Read in experimental met data 
expclim <- read.csv("expclim.csv")
expclim <- expclim[!is.na(expclim$doy),] # get rid of data that has no day of year (bc it's unusable)
expclim$year.frac <- expclim$year + expclim$doy/366 # Add a continuous time variable that is fractional years


# Making sure we don't have missing rows in between the max and min for each site-year
doy.frac <- (1:366)/366
for(s in unique(expclim$site)){
  print(paste0("processing site: ", s))
  
  # Getting the unique treatment & plot info for each site to help the merge work
  site.info <- aggregate(expclim[expclim$site==s,"year.frac"], 
                         by=expclim[expclim$site==s,c("site", "temptreat", "preciptreat", "plot")],
                         FUN=min, na.rm=T)
  site.info <- site.info[,1:(ncol(site.info)-1)]
  site.info
  
  # Finding the start and end for each site
  yf.min <- min(expclim[expclim$site==s, "year.frac"])
  yf.max <- max(expclim[expclim$site==s, "year.frac"])
  
  yrs <- data.frame(year=min(expclim[expclim$site==s, "year"]):max(expclim[expclim$site==s, "year"]))
  site.dates <- merge(yrs, data.frame(doy=doy.frac))
  site.dates$year.frac <- site.dates$year + site.dates$doy
  site.dates$doy <- site.dates$doy*366

  site.dates <- site.dates[site.dates$year.frac>=yf.min & site.dates$year.frac<=yf.max,]

  # Creating a time vector for all days that should be in the record
  yf.site  <- seq(yf.min, yf.max, by=1/366)
  yr.site  <- as.numeric(substr(yf.site, 1, 4))
  doy.site <- as.numeric(substr(yf.site, 5, nchar(yf.site)))*366
  
  site.dates <- merge(site.info, site.dates, all.x=T, all.y=T)
  expclim <- merge(expclim, site.dates, all.x=T, all.y=T)
}



# lump non-warmed & non-precip manipulations together
expclim$temptreat2 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "ambient"), "ambient", "warming"))
expclim$temptreat3 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "ambient"), "0", paste(expclim$temptreat)))
expclim$preciptreat2 <- as.factor(ifelse(expclim$preciptreat=="1", "+ precip", ifelse(expclim$preciptreat=="-1", "- precip", "ambient")))
expclim$preciptreat2 <- as.factor(ifelse(is.na(expclim$preciptreat2), "ambient", paste(expclim$preciptreat2)))

# Making some useful indicies
expclim$site.year <- as.factor(paste(expclim$site, expclim$year, sep="."))
expclim$plot <- as.ordered(expclim$plot)

summary(expclim)

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

# ----------------------
# Prototyping workflow with a single site/plot
# ----------------------
source("gapfill_doy.R")

# ggplot(data=expclim[,]) +
#   facet_wrap(~site, scale="free_x") +
#   geom_line(aes(x=year.frac, y=airtemp_max, color=temptreat)) 

# # Seeing the breakdown of plot data at each site
# for(s in unique(expclim$site)){
#   if(!max(expclim[expclim$site==s,"airtemp_max"], na.rm=T)>0) next
#   print(
#     ggplot(data=expclim[expclim$site==s,]) +
#       facet_wrap(~plot) +
#       geom_line(aes(x=year.frac, y=airtemp_max, color=plot)) +
#       ggtitle(s)
#   )
# }


ggplot(data=data.temp) +
  facet_wrap(~plot) +
  geom_line(aes(x=year.frac, y=metvar), color="black", size=0.5) +
  geom_point(data=data.temp[is.na(data.temp$airtemp_max),], aes(x=year.frac, y=met.filled), color="red", size=0.5) 

# ---------------------------------
# Airtemp_max 
# ---------------------------------
# 1. Figure out which sites have at least some air data
airmax.orig <- expclim
for(s in unique(expclim$site)){
  if(!max(expclim[expclim$site==s,"airtemp_max"], na.rm=T)>0){ 
    airmax.orig <- airmax.orig[!airmax.orig$site==s, ] 
  } else {
    time.min <- min(airmax.orig[airmax.orig$site==s & !is.na(airmax.orig$airtemp_max), "year.frac"])
    time.max <- max(airmax.orig[airmax.orig$site==s & !is.na(airmax.orig$airtemp_max), "year.frac"])
    airmax.orig <- airmax.orig[!airmax.orig$site==s | (airmax.orig$site==s & airmax.orig$year.frac>=time.min & airmax.orig$year.frac<=time.max),]
  }
}
airmax.orig$site.treat <- as.factor(paste(airmax.orig$site, airmax.orig$temptreat3, sep="."))
summary(airmax.orig)

fill.airtmax <- gapfill.doy(gap.data=airmax.orig, fillvar="airtemp_max", treatvar="temptreat3", doy.by="site.year", data.by="site")
fill.airtmax$plot <- as.ordered(fill.airtmax$plot)
summary(fill.airtmax)

pdf("figures/Gapfill_airtemp_max.pdf", height=8.5, width=11.5)
for(s in unique(fill.airtmax$site)){
  print(
    ggplot(data=fill.airtmax[fill.airtmax$site==s,]) +
      facet_wrap(~plot) +
      geom_line(aes(x=year.frac, y=airtemp_max), size=0.5, color="black") +
      geom_point(data=fill.airtmax[fill.airtmax$site==s & is.na(fill.airtmax$airtemp_min),], aes(x=year.frac, y=met.filled), size=0.5, color="red") +
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
  if(!max(expclim[expclim$site==s,"airtemp_min"], na.rm=T)>0){ 
    airmin.orig <- airmin.orig[!airmin.orig$site==s, ] 
  } else {
    time.min <- min(airmin.orig[airmin.orig$site==s & !is.na(airmin.orig$airtemp_min), "year.frac"])
    time.max <- max(airmin.orig[airmin.orig$site==s & !is.na(airmin.orig$airtemp_min), "year.frac"])
    airmin.orig <- airmin.orig[!airmin.orig$site==s | (airmin.orig$site==s & airmin.orig$year.frac>=time.min & airmin.orig$year.frac<=time.max),]
  }
}
airmin.orig$site.treat <- as.factor(paste(airmin.orig$site, airmin.orig$temptreat3, sep="."))
summary(airmin.orig)

fill.airtmin <- gapfill.doy(gap.data=airmin.orig, fillvar="airtemp_min", treatvar="temptreat3", doy.by="site.year", data.by="site")
fill.airtmin$plot <- as.ordered(fill.airtmin$plot)
summary(fill.airtmin)

pdf("figures/Gapfill_airtemp_min.pdf", height=8.5, width=11.5)
for(s in unique(fill.airtmin$site)){
  print(
    ggplot(data=fill.airtmin[fill.airtmin$site==s,]) +
      facet_wrap(~plot) +
      geom_line(aes(x=year.frac, y=airtemp_min), size=0.5, color="black") +
      geom_point(data=fill.airtmin[fill.airtmin$site==s & is.na(fill.airtmin$airtemp_min),], aes(x=year.frac, y=met.filled), size=0.5, color="red") +
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

fill.soiltmax <- gapfill.doy(gap.data=soilmax.orig, fillvar="soiltemp1_max", treatvar="temptreat3", doy.by="site.year", data.by="site")
fill.soiltmax$plot <- as.ordered(fill.soiltmax$plot)
summary(fill.soiltmax)

pdf("figures/Gapfill_soiltemp1_max.pdf", height=8.5, width=11.5)
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

fill.soiltmin <- gapfill.doy(gap.data=soilmin.orig, fillvar="soiltemp1_min", treatvar="temptreat3", doy.by="site.year", data.by="site")
fill.soiltmin$plot <- as.ordered(fill.soiltmin$plot)
summary(fill.soiltmin)

pdf("figures/Gapfill_soiltemp1_min.pdf", height=8.5, width=11.5)
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
names(fill.airtmin)[names(fill.airtmin) %in% c("met.filled", "met.flag")] <- c("fill.airtemp_min", "flag.airtemp_min")
names(fill.airtmax)[names(fill.airtmax) %in% c("met.filled", "met.flag")] <- c("fill.airtemp_max", "flag.airtemp_max")
names(fill.soiltmin)[names(fill.soiltmin) %in% c("met.filled", "met.flag")] <- c("fill.soiltemp1_min", "flag.soiltemp1_min")
names(fill.soiltmax)[names(fill.soiltmax) %in% c("met.filled", "met.flag")] <- c("fill.soiltemp1_max", "flag.soiltemp1_max")

cols.keep <- c("year", "doy", "temptreat", "preciptreat", "plot", "site")
fill.tair  <- merge(fill.airtmin[,c(cols.keep, "fill.airtemp_min", "flag.airtemp_min")], fill.airtmax[,c(cols.keep, "fill.airtemp_max", "flag.airtemp_max")], all.x=T, all.y=T)
fill.tsoil <- merge(fill.soiltmin[,c(cols.keep, "fill.soiltemp_min", "flag.soiltemp_min")], fill.soiltmax[,c(cols.keep, "fill.soiltemp_max", "flag.soiltemp_max")], all.x=T, all.y=T)

exp.gapfill <- merge(fill.tair, fill.tsoil, all.x=T, all.y=T)
summary(exp.gapfill)
dim(exp.gapfill); dim(fill.tsoil)

write.csv(exp.gapfill, "expclim_gapfill.csv", row.names=F, eol="\r\n")
# --------------------------------------------------------------



