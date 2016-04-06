# Comparing the effect of warming (& precip) on temperature & moisture 
# C. Rollinson, J. Dukes, A. Ettinger, M. Johnston

# Reading in the experimental Climate data 
library(ggplot2)

# Read in the raw data
expclim <- read.csv("expclim.csv")
expclim$temptreat2 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham"), "ambient", "warming"))
expclim$temptreat3 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham"), "0", paste(expclim$temptreat)))
expclim$preciptreat2 <- as.factor(ifelse(expclim$preciptreat=="1", "+ precip", ifelse(expclim$preciptreat=="-1", "- precip", "ambient")))
expclim$preciptreat2 <- as.factor(ifelse(is.na(expclim$preciptreat2), "ambient", paste(expclim$preciptreat2)))
expclim <- expclim[!is.na(expclim$doy),]
expclim$year.frac <- expclim$year + expclim$doy/366
summary(expclim)

# Some handy day indices to help line up months & doy
dpm   <- c(31,28,31,30,31,30,31,31,30,31,30,31) #days per month
# dpm.l <- c(31,29,31,30,31,30,31,31,30,31,30,31) #leap year days per month
doy.start <- vector(length=12)
doy.start[1] <- 0
for(i in 2:length(dpm)){
  doy.start[i] <- doy.start[i-1] + dpm[i]
}
doy.start
 





# --------------------
# Expoloratory graphing by season
# --------------------
expclim[expclim$doy>=doy.start[3] & expclim$doy<=doy.start[5],"season"] <- "spring"
expclim[expclim$doy>=doy.start[6] & expclim$doy<=doy.start[8],"season"] <- "summer"
expclim$season <- as.factor(expclim$season)
summary(expclim)


png("figures/Exploratory_Boxplot_SoilMoist_Warming_Season.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!is.na(expclim$season) & !is.na(expclim$soilmois1),]) +
  facet_wrap(~site, scales="free_x") +
  geom_boxplot(aes(x=season, y=soilmois1, fill=temptreat3)) +
  scale_y_continuous(name="Daily Mean Soil Moisture")
dev.off()

png("figures/Exploratory_Boxplot_AirTempMax_Warming_Season.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!is.na(expclim$season) & !is.na(expclim$airtemp_max),]) +
  facet_wrap(~site, scales="free_x") +
  geom_boxplot(aes(x=season, y=airtemp_max, fill=temptreat3)) +
  scale_y_continuous(name="Daily Max Air/Surface Temp")
dev.off()

png("figures/Exploratory_Boxplot_SoilTempMax_Warming_Season.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!is.na(expclim$season) & !is.na(expclim$soiltemp1_max),]) +
  facet_wrap(~site, scales="free_x") +
  geom_boxplot(aes(x=season, y=soiltemp1_max, fill=temptreat3)) +
  scale_y_continuous(name="Daily Max Soil Temp")
dev.off()

ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
  geom_point(aes(x=doy, y=airtemp_max, color=site), size=0.05, alpha=0.5)

png("analyses/figures/Exploratory_Scatter_Tairmax_Tsoilmax.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
  geom_point(aes(x=airtemp_max, y=soiltemp1_max, color=site), size=0.05, alpha=0.25)
#   stat_smooth(aes(x=airtemp_max, y=soiltemp1_max, color=site, fill=site), method="lm")
dev.off()

png("analyses/figures/Exploratory_LM_Tairmax_Tsoilmax.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
#   geom_point(aes(x=airtemp_max, y=soiltemp1_max, color=site), size=0.05, alpha=0.25)
  stat_smooth(aes(x=airtemp_max, y=soiltemp1_max, color=site, fill=site), method="lm")
dev.off()

ggplot(data=expclim[!expclim$preciptreat2=="- precip" & !is.na(expclim$airtemp_max),]) +
  facet_wrap( ~ site) +
  #   geom_point(aes(x=airtemp_max, y=soiltemp1_max, color=site), size=0.05, alpha=0.25)
  stat_smooth(aes(x=airtemp_max, y=soiltemp1_max, color=temptreat, fill=temptreat), method="lm")


png("analyses/figures/Exploratory_Scatter_Tsoilmax_SoilMoist.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
  geom_point(aes(x=soiltemp1_max, y=soilmois1, color=site), size=0.05, alpha=0.25)
#   stat_smooth(aes(x=airtemp_max, y=soiltemp1_max, color=site, fill=site), method="lm")
dev.off()

png("analyses/figures/Exploratory_LM_Tsoilmax_SoilMoist.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
  #   geom_point(aes(x=airtemp_max, y=soiltemp1_max, color=site), size=0.05, alpha=0.25)
  stat_smooth(aes(x=soiltemp1_max, y=soilmois1, color=site, fill=site), method="lm")
dev.off()
# --------------------


# --------------------
# Exploratory graphing of time series
# --------------------
vars.clim <- c("airtemp_min", "airtemp_max", 
               "soiltemp1_min", "soiltemp2_min", 
               "soiltemp1_max", "soiltemp2_max", 
               "soilmois1", "soilmois2")

expclim.agg <- aggregate(expclim[,vars.clim], 
                         by=expclim[,c("site", "temptreat3", "year", "doy", "year.frac")], 
                         FUN=mean, na.rm=T)
expclim.agg[,paste0(vars.clim, ".sd")] <- aggregate(expclim[,vars.clim], 
                                                    by=expclim[,c("site", "temptreat3", "year", "doy", "year.frac")], 
                                                    FUN=sd, na.rm=T)[,vars.clim]

summary(expclim.agg)

expclim.agg <- expclim.agg[order(expclim.agg$site, expclim.agg$temptreat3, expclim.agg$year.frac),]

ggplot(data=expclim.agg[,]) +
  facet_wrap(~site, scales="free_x") +
#   geom_ribbon(aes(x=year.frac, ymin=soilmois1-soilmois1.sd, ymax=soilmois1+soilmois1.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=year.frac, y=soilmois1, color=temptreat3), size=0.5) +
#   scale_y_continuous(limits=c(0,1)) +
  ggtitle("Soil Moisture by Treatment Through Time")
  

ggplot(data=expclim.agg[,]) +
  facet_wrap(~site, scales="free_x") +
#   geom_ribbon(aes(x=year.frac, ymin=soiltemp1_max-soiltemp1_max.sd, ymax=soiltemp1_max+soiltemp1_max.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=year.frac, y=soiltemp1_max, color=temptreat3), size=0.5) +
  ggtitle("Soil Temperature by Treatment Through Time")


ggplot(data=expclim.agg[,]) +
  facet_wrap(~site, scales="free_x") +
#   geom_ribbon(aes(x=year.frac, ymin=airtemp_max-airtemp_max.sd, ymax=airtemp_max+airtemp_max.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=year.frac, y=airtemp_max, color=temptreat3), size=0.5) +
  ggtitle("Air Temperature by Treatment Through Time")


ggplot(data=expclim.agg[expclim.agg$site=="ellison",]) +
  facet_wrap(~temptreat3, scales="free_x") +
  #   geom_ribbon(aes(x=year.frac, ymin=airtemp_max-airtemp_max.sd, ymax=airtemp_max+airtemp_max.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=year.frac, y=airtemp_max, color=temptreat3), size=0.5) +
  ggtitle("Air Temperature by Treatment Through Time")

ggplot(data=expclim.agg[expclim.agg$site=="sherry",]) +
  facet_wrap(~temptreat3, scales="free_x") +
  #   geom_ribbon(aes(x=year.frac, ymin=airtemp_max-airtemp_max.sd, ymax=airtemp_max+airtemp_max.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=year.frac, y=airtemp_max, color=temptreat3), size=0.5) +
  ggtitle("Air Temperature by Treatment Through Time")

expclim.agg2a <- aggregate(expclim[,vars.clim], 
                         by=expclim[,c("site", "temptreat3", "plot", "doy")], 
                         FUN=mean, na.rm=T)
# expclim.agg2a[,paste0(vars.clim, ".sd")] <- aggregate(expclim[,vars.clim], 
#                                                     by=expclim[,c("site", "temptreat3", "plot", "doy")], 
#                                                     FUN=sd)[,vars.clim]
summary(expclim.agg2a)


expclim.agg2b <- aggregate(expclim.agg2a[,vars.clim], 
                           by=expclim.agg2a[,c("site", "temptreat3", "doy")], 
                           FUN=mean, na.rm=T)
expclim.agg2b[,paste0(vars.clim, ".sd")] <- aggregate(expclim.agg2a[,vars.clim], 
                                                      by=expclim.agg2a[,c("site", "temptreat3", "doy")], 
                                                      FUN=sd, na.rm=T)[,vars.clim]

summary(expclim.agg2b)

ggplot(data=expclim.agg2b[,]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=soilmois1-soilmois1.sd, ymax=soilmois1+soilmois1.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=doy, y=soilmois1, color=temptreat3), size=0.5) +
  ggtitle("Soil Moisture by Treatment Through Time")


ggplot(data=expclim.agg2b[,]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=soiltemp1_max-soiltemp1_max.sd, ymax=soiltemp1_max+soiltemp1_max.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=doy, y=soiltemp1_max, color=temptreat3), size=0.5) +
  ggtitle("Soil Temperature by Treatment Through Time")


ggplot(data=expclim.agg2b[,]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=airtemp_max-airtemp_max.sd, ymax=airtemp_max+airtemp_max.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=doy, y=airtemp_max, color=temptreat3), size=0.5) +
  ggtitle("Air Temperature by Treatment Through Time")


ggplot(data=expclim.agg2b[,]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=airtemp_min-airtemp_min.sd, ymax=airtemp_min+airtemp_min.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=doy, y=airtemp_min, color=temptreat3), size=0.5) +
  ggtitle("Air Temperature by Treatment Through Time")

# --------------------


# --------------------
# Graphing the differences through time
# --------------------
vars.clim <- c("airtemp_min", "airtemp_max", 
               "soiltemp1_min", "soiltemp2_min", 
               "soiltemp1_max", "soiltemp2_max", 
               "soilmois1", "soilmois2")
summary(expclim)

expclim.control <- aggregate(expclim[expclim$temptreat3=="0",vars.clim], 
                         by=expclim[expclim$temptreat3=="0",c("site", "year", "doy")], 
                         FUN=mean, na.rm=T)
names(expclim.control)[which(names(expclim.control) %in% vars.clim)] <- paste0(vars.clim, ".control")
summary(expclim.control)

ggplot(data=expclim.control[,]) +
  facet_wrap(~site, scales="fixed") +
#   geom_ribbon(aes(x=doy, ymin=soiltemp1_max-soiltemp1_max.sd, ymax=soiltemp1_max+soiltemp1_max.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=doy, y=soiltemp1_max.control, color=as.factor(year)), size=0.5) +
  ggtitle("Soil Temperature by Treatment Through Time")

expclim2 <- merge(expclim, expclim.control, all.x=T)
expclim2[,paste0(vars.clim, ".dev")] <- expclim2[,vars.clim] - expclim2[,paste0(vars.clim, ".control")]
for(v in vars.clim){
  expclim2[is.na(expclim2[,paste0(v, ".control")]),paste0(v, ".dev")] <- NA
}
summary(expclim2)
dim(expclim); dim(expclim2)

expclim.agg3a <- aggregate(expclim2[,paste0(vars.clim, ".dev")], 
                           by=expclim2[,c("site", "temptreat3", "plot", "doy")], 
                           FUN=mean, na.rm=T)
summary(expclim.agg3a)


agg.dev <- aggregate(expclim.agg3a[,paste0(vars.clim, ".dev")], 
                           by=expclim.agg3a[,c("site", "temptreat3", "doy")], 
                           FUN=mean, na.rm=T)
agg.dev[,paste0(vars.clim, ".dev.lo")] <- aggregate(expclim.agg3a[,paste0(vars.clim, ".dev")], 
                                                      by=expclim.agg3a[,c("site", "temptreat3", "doy")], 
                                                      FUN=quantile, 0.025, na.rm=T)[,paste0(vars.clim, ".dev")]
agg.dev[,paste0(vars.clim, ".dev.hi")] <- aggregate(expclim.agg3a[,paste0(vars.clim, ".dev")], 
                                                    by=expclim.agg3a[,c("site", "temptreat3", "doy")], 
                                                    FUN=quantile, 0.975, na.rm=T)[,paste0(vars.clim, ".dev")]

summary(agg.dev)

agg.dev.graph <- agg.dev

agg.dev.graph[agg.dev.graph$soilmois1.dev.lo<= min(agg.dev.graph$soilmois1.dev, na.rm=T) & !is.na(agg.dev.graph$soilmois1.dev.lo),"soilmois1.dev.lo"] <- min(agg.dev.graph$soilmois1.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$soilmois1.dev.hi>= max(agg.dev.graph$soilmois1.dev, na.rm=T) & !is.na(agg.dev.graph$soilmois1.dev.hi),"soilmois1.dev.hi"] <- max(agg.dev.graph$soilmois1.dev, na.rm=T)
summary(agg.dev.graph)

png("figures/Exploratory_TimeSeries_SoilMoist_Deviation.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=soilmois1.dev.lo, ymax=soilmois1.dev.hi, fill=temptreat3), alpha=0.3) +
  geom_line(aes(x=doy, y=soilmois1.dev, color=temptreat3), size=0.5) +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed (m3 H2O/mg3 soil ??)") +
  ggtitle("Daily Mean Soil Moisture Difference")
dev.off()

agg.dev.graph[agg.dev.graph$soiltemp1_min.dev.lo<= min(agg.dev.graph$soiltemp1_min.dev, na.rm=T) & !is.na(agg.dev.graph$soiltemp1_min.dev.lo),"soiltemp1_min.dev.lo"] <- min(agg.dev.graph$soiltemp1_min.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$soiltemp1_min.dev.hi>= max(agg.dev.graph$soiltemp1_min.dev, na.rm=T)  & !is.na(agg.dev.graph$soiltemp1_min.dev.hi),"soiltemp1_min.dev.hi"] <- max(agg.dev.graph$soiltemp1_min.dev, na.rm=T)
summary(agg.dev.graph)

png("figures/Exploratory_TimeSeries_SoilTemp1Min_Deviation.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$soiltemp1_min.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=soiltemp1_min.dev.lo, ymax=soiltemp1_min.dev.hi, fill=temptreat3), alpha=0.3) +
  geom_line(aes(x=doy, y=soiltemp1_min.dev, color=temptreat3), size=0.5) +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed (degrees C)") +
  ggtitle("Daily Min Soil Temperature Difference")
dev.off()

agg.dev.graph[agg.dev.graph$soiltemp1_max.dev.lo<= min(agg.dev.graph$soiltemp1_max.dev, na.rm=T) & !is.na(agg.dev.graph$soiltemp1_max.dev.lo),"soiltemp1_max.dev.lo"] <- min(agg.dev.graph$soiltemp1_max.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$soiltemp1_max.dev.hi>= max(agg.dev.graph$soiltemp1_max.dev, na.rm=T)  & !is.na(agg.dev.graph$soiltemp1_max.dev.hi),"soiltemp1_max.dev.hi"] <- max(agg.dev.graph$soiltemp1_max.dev, na.rm=T)
summary(agg.dev.graph)

png("figures/Exploratory_TimeSeries_SoilTemp1Max_Deviation.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$soiltemp1_max.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=soiltemp1_max.dev.lo, ymax=soiltemp1_max.dev.hi, fill=temptreat3), alpha=0.3) +
  geom_line(aes(x=doy, y=soiltemp1_max.dev, color=temptreat3), size=0.5) +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed (degrees C)") +
  ggtitle("Daily Max Soil Temperature Difference")
dev.off()


agg.dev.graph[agg.dev.graph$airtemp_max.dev.lo<= min(agg.dev.graph$airtemp_max.dev, na.rm=T) & !is.na(agg.dev.graph$airtemp_max.dev.lo),"airtemp_max.dev.lo"] <- min(agg.dev.graph$airtemp_max.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$airtemp_max.dev.hi>= max(agg.dev.graph$airtemp_max.dev, na.rm=T)  & !is.na(agg.dev.graph$airtemp_max.dev.hi),"airtemp_max.dev.hi"] <- max(agg.dev.graph$airtemp_max.dev, na.rm=T)
summary(agg.dev.graph)

png("figures/Exploratory_TimeSeries_AirTempMax_Deviation.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$airtemp_max.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=airtemp_max.dev.lo, ymax=airtemp_max.dev.hi, fill=temptreat3), alpha=0.3) +
  geom_line(aes(x=doy, y=airtemp_max.dev, color=temptreat3), size=0.5) +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed  (degrees C)") +
  ggtitle("Daily Max Air Temperature Difference")
dev.off()


agg.dev.graph[agg.dev.graph$airtemp_min.dev.lo<= min(agg.dev.graph$airtemp_min.dev, na.rm=T) & !is.na(agg.dev.graph$airtemp_min.dev.lo),"airtemp_min.dev.lo"] <- min(agg.dev.graph$airtemp_min.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$airtemp_min.dev.hi>= max(agg.dev.graph$airtemp_min.dev, na.rm=T)  & !is.na(agg.dev.graph$airtemp_min.dev.hi),"airtemp_min.dev.hi"] <- max(agg.dev.graph$airtemp_min.dev, na.rm=T)
summary(agg.dev.graph)

png("figures/Exploratory_TimeSeries_AirTempMin_Deviation.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$airtemp_min.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=airtemp_min.dev.lo, ymax=airtemp_min.dev.hi, fill=temptreat3), alpha=0.3) +
  geom_line(aes(x=doy, y=airtemp_min.dev, color=temptreat3), size=0.5) +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed (degrees C)") +
  ggtitle("Daily Min Air Temperature Difference")
dev.off()

# --------------------
