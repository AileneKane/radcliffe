# Comparing the effect of warming (& precip) on temperature & moisture during the growing season
# C. Rollinson, J. Dukes, A. Ettinger, M. Johnston

# Reading in the experimental Climate data 
library(ggplot2)

# ----------------
# Read in the raw data
# ----------------
expclim <- read.csv("../expclim.csv")
expclim <- expclim[!is.na(expclim$doy),] # get rid of observations without day of year because we can't use them
expclim$year.frac <- expclim$year + expclim$doy/366 # Make a continuous time variable
summary(expclim)
summary(expclim$temptreat)


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


expclim$temptreat2 <- as.factor(ifelse(expclim$temptreat=="0", "control", paste(expclim$temptreat)))
expclim$temptreat3 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham", "ambient"), "ambient", "warming"))
expclim$preciptreat2 <- as.factor(ifelse(expclim$preciptreat=="1", "+ precip", ifelse(expclim$preciptreat=="-1", "- precip", "0")))
expclim$preciptreat2 <- as.factor(ifelse(is.na(expclim$preciptreat2), "0", paste(expclim$preciptreat2)))

# # Ordering target & temptreat2 so non-warmed are next to each other & first
# expclim$temptreat2 <- factor(expclim$temptreat2, levels=c("ambient", "control", paste(1:9)))
# colors.temptreat2 <- c("gray30", "gray50", "mediumpurple", "mediumorchid", "palevioletred", "salmon", "goldenrod", "orange", "coral2", "orangered", "red")

# Getting mean aboveground and soil temp1 if not already reported
# -- mean didn't work in the ifelse statement -- don't know why, but the sum/2 seems to work just fine.  
expclim$AGtemp_mean <- rowMeans(expclim[,c("AGtemp_max", "AGtemp_min")], na.rm=F)
expclim$BGtemp_mean <- ifelse(is.na(expclim$soiltemp1_mean), sum(expclim[,c("soiltemp1_max", "soiltemp1_min")], na.rm=F)/2, expclim$soiltemp1_mean) 
summary(expclim)
# ----------------



# ----------------
# Read in some metadata about the intended warming levels
# ----------------
targets <- read.csv("../treats_detail.csv", na.strings=c("NA"))
#targets <- read.csv("Analyses/treats_detail.csv", na.strings=c("NA"))

# # Fill missing targets with what is given for the temptreat level
# for(s in unique(targets[is.na(targets$target), "site"])){
#  tmiss <- unique(targets[targets$site==s & is.na(targets$target),"temptreat"]) 
#  for(t in tmiss){
#    t.use <- unique(targets[targets$site==s & targets$temptreat==t & !is.na(targets$target),"target"]) 
#    targets[targets$site==s & targets$temptreat==t & is.na(targets$target),"target"] <- t.use
#  }
# }
# summary(targets[is.na(targets$target),])
# targets[is.na(targets$target), "target"] <- "unknown" # List unreported targets that aren't controls as unknwon
# targets$target <- as.factor(targets$target) # make this a readable factor
# summary(targets)
# summary(targets[targets$target=="unknown",])

# ----------------


# Merge the targets into the expclim file
expclim <- merge(expclim, targets, all.x=T)
summary(expclim)
summary(expclim$target)

# Checking on more NAs in expclim$target

# # Fixing a little bit more with the target temptreats
# # Fill missing targets with what is given for the temptreat level
# Fill missing targets with what is given for the temptreat level
expclim[expclim$temptreat %in% c(0, "ambient"), "target"] <- 0 # If this isn't a wasn't warmed, target = 0
summary(expclim$target)
for(s in unique(expclim[is.na(expclim$target), "site"])){
  tmiss <- unique(expclim[expclim$site==s & is.na(expclim$target),"temptreat"]) 
  for(t in tmiss){
    t.use <- unique(expclim[expclim$site==s & expclim$temptreat==t & !is.na(expclim$target),"target"]) 
    expclim[expclim$site==s & expclim$temptreat==t & is.na(expclim$target),"target"] <- t.use
  }
}
summary(expclim[is.na(expclim$target),])
expclim[is.na(expclim$target), "target"] <- "unknown" # List unreported expclim that aren't controls as unknwon
expclim$target <- as.factor(expclim$target) # make this a readable factor
summary(expclim)



# Making some color ramps for graphing
expclim$target <- factor(expclim$target, levels=c("0", "1", "1.5", "2", "2.5", "2.7","3", "3.5", "4", "4.5", "5", "5.5"))
#                   0          1               1.5           2      2.5       2.7     3                3.5        4            4.5,      5,           5.5
# colors.target <- c("black", "purple", "mediumpurple", "mediumorchid", "plum3","plum2", "palevioletred", "salmon1", "orange", "coral2", "orangered", "red")
colors.target <- data.frame(target = c("0",     "1",      "1.5",          "2",            "2.5",  "2.7",   "3",             "3.5",     "4",      "4.5",    "5",         "5.5"),
                            color  = c("black", "purple", "mediumpurple", "mediumorchid", "plum3","plum2", "palevioletred", "salmon1", "orange", "coral2", "orangered", "red"))
# Ordering target & temptreat2 so non-warmed are next to each other & first
expclim$temptreat2 <- factor(expclim$temptreat2, levels=c("ambient", "control", paste(1:9)))
colors.temptreat2 <- data.frame(temptreat2 = c("ambient", "control", "1",            "2",            "3",             "4",      "5",         "6",      "7",      "8",         "9"),
                                color      =  c("gray30", "gray50",  "mediumpurple", "mediumorchid", "palevioletred", "salmon", "goldenrod", "orange", "coral2", "orangered", "red"))
                     


# For when 1-degree drops out for some reason
colors.target2 <- c("black", "purple", "mediumpurple", "plum3", "plum2","palevioletred", "salmon1", "orange", "coral2", "orangered", "red")

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
# Expoloratory Boxplot graphing by season
# --------------------
{
expclim[expclim$doy>=doy.start[3] & expclim$doy<=doy.start[5],"season"] <- "spring"
expclim[expclim$doy>=doy.start[6] & expclim$doy<=doy.start[8],"season"] <- "summer"
expclim$season <- as.factor(expclim$season)
summary(expclim)

png("../figures/Exploratory_Boxplot_SoilMoist_Warming_Season.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!is.na(expclim$season) & !is.na(expclim$soilmois1),]) +
  facet_wrap(~site, scales="free_x") +
  geom_boxplot(aes(x=season, y=soilmois1, fill=temptreat2)) +
  scale_y_continuous(name="Daily Mean Soil Moisture") +
  scale_fill_manual(values=as.vector(colors.temptreat2[colors.temptreat2$temptreat2 %in% unique(expclim[!is.na(expclim$season) & !is.na(expclim$soilmois1),"temptreat2"]),"color"])) +
  theme_bw()
dev.off()


png("../figures/Exploratory_Boxplot_AirTempMax_Warming_Season.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!is.na(expclim$season) & !is.na(expclim$airtemp_max),]) +
  facet_wrap(~site, scales="free_x") +
  geom_boxplot(aes(x=season, y=airtemp_max, fill=temptreat2)) +
  scale_y_continuous(name="Daily Max Air/Surface Temp") +
  scale_fill_manual(values=as.vector(colors.temptreat2[colors.temptreat2$temptreat2 %in% unique(expclim[!is.na(expclim$season) & !is.na(expclim$airtemp_max),"temptreat2"]),"color"])) +
  theme_bw()
dev.off()

png("../figures/Exploratory_Boxplot_SoilTempMax_Warming_Season.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!is.na(expclim$season) & !is.na(expclim$soiltemp1_max),]) +
  facet_wrap(~site, scales="free_x") +
  geom_boxplot(aes(x=season, y=soiltemp1_max, fill=temptreat2)) +
  scale_y_continuous(name="Daily Max Soil Temp") +
  scale_fill_manual(values=colors.temptreat2) +
  theme_bw()
dev.off()

ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
  geom_point(aes(x=doy, y=airtemp_max, color=site), size=0.05, alpha=0.5) +
  # scale_fill_manual(values=colors.temptreat2) +
  theme_bw()

png("../figures/Exploratory_Scatter_Tairmax_Tsoilmax.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
  geom_point(aes(x=airtemp_max, y=soiltemp1_max, color=site), size=0.05, alpha=0.25) + 
  theme_bw()
#   stat_smooth(aes(x=airtemp_max, y=soiltemp1_max, color=site, fill=site), method="lm")
dev.off()

png("../figures/Exploratory_LM_Tairmax_Tsoilmax.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
#   geom_point(aes(x=airtemp_max, y=soiltemp1_max, color=site), size=0.05, alpha=0.25)
  stat_smooth(aes(x=airtemp_max, y=soiltemp1_max, color=site, fill=site), method="lm") +
  theme_bw()
dev.off()

ggplot(data=expclim[!expclim$preciptreat2=="- precip" & !is.na(expclim$airtemp_max),]) +
  facet_wrap( ~ site) +
  #   geom_point(aes(x=airtemp_max, y=soiltemp1_max, color=site), size=0.05, alpha=0.25)
  stat_smooth(aes(x=airtemp_max, y=soiltemp1_max, color=temptreat, fill=temptreat), method="lm")


png("../figures/Exploratory_Scatter_Tsoilmax_SoilMoist.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
  geom_point(aes(x=soiltemp1_max, y=soilmois1, color=site), size=0.05, alpha=0.25) +
#   stat_smooth(aes(x=airtemp_max, y=soiltemp1_max, color=site, fill=site), method="lm") +
  theme_bw()
dev.off()

png("../figures/Exploratory_LM_Tsoilmax_SoilMoist.png", height=6, width=6, units="in", res=180)
ggplot(data=expclim[!expclim$preciptreat2=="- precip",]) +
  facet_grid(temptreat2 ~ preciptreat2) +
  #   geom_point(aes(x=airtemp_max, y=soiltemp1_max, color=site), size=0.05, alpha=0.25)
  stat_smooth(aes(x=soiltemp1_max, y=soilmois1, color=site, fill=site), method="lm") +
  theme_bw()
dev.off()
}
# --------------------


# --------------------
# Exploratory graphing of time series
# --------------------
vars.clim <- c("AGtemp_mean", "BGtemp_mean", 
               "soilmois1", "soilmois2")

expclim.agg <- aggregate(expclim[,vars.clim], 
                         by=expclim[,c("site", "temptreat2", "target", "year", "doy", "year.frac")], 
                         FUN=mean, na.rm=T)
expclim.agg[,paste0(vars.clim, ".sd")] <- aggregate(expclim[,vars.clim], 
                                                    by=expclim[,c("site", "temptreat2", "target", "year", "doy", "year.frac")], 
                                                    FUN=sd, na.rm=T)[,vars.clim]

summary(expclim.agg)

expclim.agg <- expclim.agg[order(expclim.agg$site, expclim.agg$temptreat2, expclim.agg$year.frac),]
unique(expclim.agg$target)

expclim.agg$target <- factor(expclim.agg$target, levels=c("0", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.17", "4.5", "5", "5.5", "200", "600", "1000", "unknown", NA))


# png("../figures/Exploratory_SoilMoist_byTarget.png", height=8, width=10, units="in", res=180)
# ggplot(data=expclim.agg[!is.na(expclim.agg$soilmois1),]) +
#   facet_wrap(~site, scales="free_x") +
#   geom_line(aes(x=year.frac, y=soilmois1, color=target), size=0.5) +
#   # geom_line(data=expclim.agg[!is.na(expclim.agg$soilmois1) & expclim.agg$target<100,], aes(x=year.frac, y=soilmois1, color=target), size=0.5) +
#   # geom_line(data=expclim.agg[!is.na(expclim.agg$soilmois1) & expclim.agg$target<100,], aes(x=year.frac, y=soilmois1, color=target), size=0.5) +
#   ggtitle("Soil Moisture by Treatment Through Time") +
#   scale_color_manual(values=colors.target2) + # target2 because no 1-degree warming data
#   theme_bw()
# # dev.off()
# 
# # png("../figures/Exploratory_SoilTemp1_byTarget.png", height=8, width=10, units="in", res=180)
# ggplot(data=expclim.agg[!is.na(expclim.agg$soiltemp1_max),]) +
#   facet_wrap(~site, scales="free_x") +
# #   geom_ribbon(aes(x=year.frac, ymin=soiltemp1_max-soiltemp1_max.sd, ymax=soiltemp1_max+soiltemp1_max.sd, fill=temptreat3), alpha=0.5) +
#   geom_line(aes(x=year.frac, y=soiltemp1_max, color=target), size=0.5) +
#   ggtitle("Soil Temperature by Treatment Through Time") +
#   scale_color_manual(values=colors.target) + 
#   theme_bw()
# # dev.off()
# 
# # png("../figures/Exploratory_AGtemp_max_byTarget.png", height=8, width=10, units="in", res=180)
# ggplot(data=expclim.agg[!is.na(expclim.agg$AGtemp_max),]) +
#   facet_wrap(~site, scales="free_x") +
# #   geom_ribbon(aes(x=year.frac, ymin=airtemp_max-airtemp_max.sd, ymax=airtemp_max+airtemp_max.sd, fill=temptreat3), alpha=0.5) +
#   geom_line(aes(x=year.frac, y=AGtemp_max, color=target), size=0.5) +
#   ggtitle("Air Temperature by Treatment Through Time")+
#   scale_color_manual(values=colors.target2) +
#   theme_bw()
# # dev.off()


# ggplot(data=expclim.agg[expclim.agg$site=="ellison",]) +
#   facet_wrap(~temptreat2, scales="free_x") +
#   #   geom_ribbon(aes(x=year.frac, ymin=airtemp_max-airtemp_max.sd, ymax=airtemp_max+airtemp_max.sd, fill=temptreat3), alpha=0.5) +
#   geom_line(aes(x=year.frac, y=AGtemp_max, color=target), size=0.5) +
#   ggtitle("Air Temperature by Treatment Through Time")+
#   scale_color_manual(values=colors.target2) 
# 
# ggplot(data=expclim.agg[expclim.agg$site=="sherry",]) +
#   facet_wrap(~temptreat3, scales="free_x") +
#   #   geom_ribbon(aes(x=year.frac, ymin=airtemp_max-airtemp_max.sd, ymax=airtemp_max+airtemp_max.sd, fill=temptreat3), alpha=0.5) +
#   geom_line(aes(x=year.frac, y=airtemp_max, color=temptreat3), size=0.5) +
#   ggtitle("Air Temperature by Treatment Through Time")

expclim.agg2a <- aggregate(expclim[,vars.clim], 
                         by=expclim[,c("site","target", "plot", "doy")], 
                         FUN=mean, na.rm=T)
# expclim.agg2a[,paste0(vars.clim, ".sd")] <- aggregate(expclim[,vars.clim], 
#                                                     by=expclim[,c("site", "temptreat3", "plot", "doy")], 
#                                                     FUN=sd)[,vars.clim]
summary(expclim.agg2a)


expclim.agg2b <- aggregate(expclim.agg2a[,vars.clim], 
                           by=expclim.agg2a[,c("site","target", "doy")], 
                           FUN=mean, na.rm=T)
expclim.agg2b[,paste0(vars.clim, ".sd")] <- aggregate(expclim.agg2a[,vars.clim], 
                                                      by=expclim.agg2a[,c("site","target", "doy")], 
                                                      FUN=sd, na.rm=T)[,vars.clim]

summary(expclim.agg2b)

expclim.agg2b$target <- factor(expclim.agg2b$target, levels=c("0", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.17", "4.5", "5", "5.5", "200", "600", "1000", "unknown", NA))


png("../figures/Exploratory_SoilMoist1_byTarget.png", height=8, width=10, units="in", res=180)
ggplot(data=expclim.agg2b[,]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=soilmois1-soilmois1.sd, ymax=soilmois1+soilmois1.sd, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=soilmois1, color=target), size=0.5) +
  ggtitle("Soil Moisture by Treatment Through Time")  +
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(expclim.agg2b$target), "color"])) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(expclim.agg2b$target), "color"])) + 
  theme_bw()
dev.off()

png("../figures/Exploratory_SoilTempMean_byTarget.png", height=8, width=10, units="in", res=180)
ggplot(data=expclim.agg2b[,]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=BGtemp_mean-BGtemp_mean.sd, ymax=BGtemp_mean+BGtemp_mean.sd, fill=target), alpha=0.5) +
  geom_line(aes(x=doy, y=BGtemp_mean, color=target), size=0.5) +
  ggtitle("Soil Temperature by Treatment Through Time") +
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(expclim.agg2b$target), "color"])) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(expclim.agg2b$target), "color"])) + 
  theme_bw()
dev.off()

png("../figures/Exploratory_AGtempMean_byTarget.png", height=8, width=10, units="in", res=180)
ggplot(data=expclim.agg2b[,]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=AGtemp_mean-AGtemp_mean.sd, ymax=AGtemp_mean+AGtemp_mean.sd, fill=target), alpha=0.5) +
  geom_line(aes(x=doy, y=AGtemp_mean, color=target), size=0.5) +
  ggtitle("Air Temperature by Treatment Through Time")+
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(expclim.agg2b$target), "color"])) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(expclim.agg2b$target), "color"])) + 
  theme_bw()
dev.off()
# --------------------


# --------------------
# Graphing the differences through time
# --------------------
vars.clim <- c("AGtemp_mean", "BGtemp_mean", 
               "soilmois1")
summary(expclim)

# Using the mean of everything with no warming
# Find the baseline for each year to calculate the deviations
#  -- will aggregate across years first when going to graph
expclim.base <- aggregate(expclim[expclim$target=="0",vars.clim], 
                         by=expclim[expclim$target=="0",c("site", "year", "doy")], 
                         FUN=mean, na.rm=T)
names(expclim.base)[which(names(expclim.base) %in% vars.clim)] <- paste0(vars.clim, ".base")
summary(expclim.base)

# Finding the mean annual temp etc to add to graphs 
day.means <- aggregate(expclim.base[,paste0(vars.clim, ".base")],
                           by=expclim.base[,c("site", "doy")],
                           FUN=mean, na.rm=T)
summary(day.means)

site.means <- aggregate(day.means[,paste0(vars.clim, ".base")],
                        by=list(day.means[,c("site")]),
                        FUN=mean, na.rm=T)
names(site.means) <- c("site", paste0(vars.clim, ".ann"))
summary(site.means)

ggplot(data=expclim.base[,]) +
  facet_wrap(~site, scales="fixed") +
#   geom_ribbon(aes(x=doy, ymin=soiltemp1_max-soiltemp1_max.sd, ymax=soiltemp1_max+soiltemp1_max.sd, fill=temptreat3), alpha=0.5) +
  geom_line(aes(x=doy, y=BGtemp_mean.base, color=as.factor(year)), size=0.5) +
  ggtitle("Untreated Soil Temperature Through Time")

# Merge the baseline reference temp into the expclim dataframe
expclim2 <- merge(expclim, expclim.base, all.x=T)
summary(expclim2)

expclim2 <- merge(expclim2, site.means, all.x=T)
summary(expclim2)

# Calculate the deviations
expclim2[,paste0(vars.clim, ".dev")] <- expclim2[,vars.clim] - expclim2[,paste0(vars.clim, ".base")]
for(v in vars.clim){
  expclim2[is.na(expclim2[,paste0(v, ".base")]),paste0(v, ".dev")] <- NA
}
summary(expclim2)
dim(expclim); dim(expclim2)


# Finding the mean deviation for each across years
# Going by target warming rather than the temptreatment level
expclim.agg3a <- aggregate(expclim2[,c(paste0(vars.clim, ".base"), paste0(vars.clim, ".dev"), paste0(vars.clim, ".ann"))], 
                           by=expclim2[,c("site", "target", "plot", "doy")], 
                           FUN=mean, na.rm=T)
summary(expclim.agg3a)

# Getting the mean & sd by across treatments
agg.dev <- aggregate(expclim.agg3a[,c(paste0(vars.clim, ".base"), paste0(vars.clim, ".dev"), paste0(vars.clim, ".ann"))], 
                           by=expclim.agg3a[,c("site", "target", "doy")], 
                           FUN=mean, na.rm=T)
agg.dev[,paste0(vars.clim, ".dev.lo")] <- aggregate(expclim.agg3a[,paste0(vars.clim, ".dev")], 
                                                      by=expclim.agg3a[,c("site", "target", "doy")], 
                                                      FUN=quantile, 0.025, na.rm=T)[,paste0(vars.clim, ".dev")]
agg.dev[,paste0(vars.clim, ".dev.hi")] <- aggregate(expclim.agg3a[,paste0(vars.clim, ".dev")], 
                                                    by=expclim.agg3a[,c("site", "target", "doy")], 
                                                    FUN=quantile, 0.975, na.rm=T)[,paste0(vars.clim, ".dev")]

summary(agg.dev)

agg.dev.graph <- agg.dev

# Just trimming things to the CIs to make things easier
agg.dev.graph[agg.dev.graph$soilmois1.dev.lo<= min(agg.dev.graph$soilmois1.dev, na.rm=T) & !is.na(agg.dev.graph$soilmois1.dev.lo),"soilmois1.dev.lo"] <- min(agg.dev.graph$soilmois1.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$soilmois1.dev.hi>= max(agg.dev.graph$soilmois1.dev, na.rm=T) & !is.na(agg.dev.graph$soilmois1.dev.hi),"soilmois1.dev.hi"] <- max(agg.dev.graph$soilmois1.dev, na.rm=T)
summary(agg.dev.graph)

# Ordering things by mean annual soil temp
site.means
agg.dev.graph$site <- factor(agg.dev.graph$site, levels=site.means[order(site.means$soilmois1.ann),"site"])
# identifying which figures we want to graph
sites.graph<-unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.ann) & !agg.dev.graph$site=="exp08","site"])

# summary(agg.dev.graph[agg.dev.graph$site=="exp05",])

png("../figures/Exploratory_TimeSeries_SoilMoist_Deviation.png", height=4.5, width=9, units="in", res=180)
ggplot(data=agg.dev.graph[agg.dev.graph$site %in% sites.graph & !is.na(agg.dev.graph$soilmois1.dev),]) +
  facet_wrap(~site, scales="fixed", ncol=5) +
  geom_ribbon(aes(x=doy, ymin=soilmois1.dev.lo, ymax=soilmois1.dev.hi, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=soilmois1.dev, color=target), size=0.5) +
  geom_text(data=site.means[site.means$site %in% sites.graph,], x=325, y=0.12, aes(label=str_pad(round(soilmois1.ann,2), 4,side="right", pad="0")), hjust="right", fontface="bold") +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed (m3 H2O/mg3 soil)") +
  ggtitle("Daily Mean Soil Moisture Difference") +
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),"target"]), "color"]), 
                     name=expression(paste("target warming " ^"o", "C"))) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),"target"]), "color"]), 
                    name=expression(paste("target warming " ^"o", "C"))) + 
  theme_bw()
dev.off()


png("../figures/Exploratory_TimeSeries_SoilMoist_Deviation_AnnMeans.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=soilmois1.dev.lo+soilmois1.ann, ymax=soilmois1.dev.hi+soilmois1.ann, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=soilmois1.dev+soilmois1.ann, color=target), size=0.5) +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed (m3 H2O/mg3 soil ??)") +
  ggtitle("Daily Mean Soil Moisture Difference") +
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),"target"]), "color"])) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),"target"]), "color"])) + 
  theme_bw()
dev.off()

agg.dev.graph[agg.dev.graph$BGtemp_mean.dev.lo<= min(agg.dev.graph$BGtemp_mean.dev, na.rm=T) & !is.na(agg.dev.graph$BGtemp_mean.dev.lo),"BGtemp_mean.dev.lo"] <- min(agg.dev.graph$BGtemp_mean.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$BGtemp_mean.dev.hi>= max(agg.dev.graph$BGtemp_mean.dev, na.rm=T)  & !is.na(agg.dev.graph$BGtemp_mean.dev.hi),"BGtemp_mean.dev.hi"] <- max(agg.dev.graph$BGtemp_mean.dev, na.rm=T)
summary(agg.dev.graph)

# Ordering things by mean annual soil temp
site.means
agg.dev.graph$site <- factor(agg.dev.graph$site, levels=site.means[order(site.means$BGtemp_mean.ann),"site"])
# identifying which figures we want to graph
sites.graph<-unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"site"])

# exp12 has somethign weird with its soil temperature -- maybe a lot of missing days at a certain point, so that may need to get split up
summary(agg.dev.graph[agg.dev.graph$site=="exp12" & is.na(agg.dev.graph$BGtemp_mean.base),])
unique(agg.dev.graph[agg.dev.graph$site=="exp12" & is.na(agg.dev.graph$BGtemp_mean.base),"doy"])


png("../figures/Exploratory_TimeSeries_SoilTemp1Mean_Deviation.png", height=4.5, width=9, units="in", res=180)
ggplot(data=agg.dev.graph[agg.dev.graph$site %in% sites.graph ,]) +
  facet_wrap(~site, scales="fixed", ncol=5) +
  geom_ribbon(aes(x=doy, ymin=BGtemp_mean.dev.lo, ymax=BGtemp_mean.dev.hi, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=BGtemp_mean.dev, color=target), size=0.5) +
  geom_hline(aes(yintercept=as.numeric(paste(target)), color=target), linetype="dashed") +
  geom_text(data=site.means[site.means$site %in% sites.graph,], x=325, y=7, aes(label=round(BGtemp_mean.ann,1)), hjust="right", fontface="bold") +
  scale_x_continuous(expand=c(0,0), name="day of year") +
  scale_y_continuous(expand=c(0,0), limits=range(agg.dev.graph[,c("BGtemp_mean.dev.lo", "BGtemp_mean.dev.hi")], na.rm=T), name="diff from non-warmed (degrees C)") +
  ggtitle("Daily Mean Soil Temperature Difference")+
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"target"]), "color"]), 
                     name=expression(paste("target warming " ^"o", "C"))) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"target"]), "color"]), 
                    name=expression(paste("target warming " ^"o", "C"))) + 
  theme_bw()
dev.off()

png("../figures/Exploratory_TimeSeries_SoilTemp1Mean_Deviation_AnnMeans.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=BGtemp_mean.dev.lo+BGtemp_mean.ann, ymax=BGtemp_mean.dev.hi+BGtemp_mean.ann, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=BGtemp_mean.dev+BGtemp_mean.ann, color=target), size=0.5) +
  geom_hline(aes(yintercept=as.numeric(paste(target))+BGtemp_mean.ann, color=target), linetype="dashed") +
  scale_x_continuous(expand=c(0,0), name="day of year") +
  scale_y_continuous(expand=c(0,0), limits=range(agg.dev.graph[,c("BGtemp_mean.dev.lo", "BGtemp_mean.dev.hi")]+agg.dev.graph$BGtemp_mean.ann, na.rm=T), name="diff from non-warmed (degrees C)") +
  ggtitle("Daily Mean Soil Temperature Difference")+
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"target"]), "color"]), 
                     name=expression(paste("target warming " ^"o", "C"))) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"target"]), "color"]), 
                    name=expression(paste("target warming " ^"o", "C"))) + 
  theme_bw()
dev.off()

agg.dev.graph[agg.dev.graph$AGtemp_mean.dev.lo<= min(agg.dev.graph$AGtemp_mean.dev, na.rm=T) & !is.na(agg.dev.graph$AGtemp_mean.dev.lo),"AGtemp_mean.dev.lo"] <- min(agg.dev.graph$AGtemp_mean.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$AGtemp_mean.dev.hi>= max(agg.dev.graph$AGtemp_mean.dev, na.rm=T)  & !is.na(agg.dev.graph$AGtemp_mean.dev.hi),"AGtemp_mean.dev.hi"] <- max(agg.dev.graph$AGtemp_mean.dev, na.rm=T)
summary(agg.dev.graph)

png("../figures/Exploratory_TimeSeries_AGTempMean_Deviation.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$AGtemp_mean.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=AGtemp_mean.dev.lo, ymax=AGtemp_mean.dev.hi, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=AGtemp_mean.dev, color=target), size=0.5) +
  geom_hline(aes(yintercept=as.numeric(paste(target)), color=target), linetype="dashed") +
  geom_text(data=site.means[site.means$site %in% unique(agg.dev.graph[!is.na(agg.dev.graph$AGtemp_mean.dev),"site"]),], x=335, y=7, aes(label=round(AGtemp_mean.ann,1)), hjust="right", fontface="bold") +
  scale_x_continuous(expand=c(0,0), name="day of year") +
  scale_y_continuous(expand=c(0,0), limits=range(agg.dev.graph[,c("AGtemp_mean.dev.lo", "AGtemp_mean.dev.hi")], na.rm=T), name="diff from non-warmed (degrees C)") +
  ggtitle("Daily Mean Aboveground Temperature Difference")+
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$AGtemp_mean.dev),"target"]), "color"])) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$AGtemp_mean.dev),"target"]), "color"])) + 
  theme_bw()
dev.off()


png("../figures/Exploratory_TimeSeries_AGTempMean_Deviation_AnnMeans.png", height=10, width=10, units="in", res=180)
ggplot(data=agg.dev.graph[!is.na(agg.dev.graph$AGtemp_mean.dev),]) +
  facet_wrap(~site, scales="fixed") +
  geom_ribbon(aes(x=doy, ymin=AGtemp_mean.dev.lo+AGtemp_mean.ann, ymax=AGtemp_mean.dev.hi+AGtemp_mean.ann, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=AGtemp_mean.dev+AGtemp_mean.ann, color=target), size=0.5) +
  geom_hline(aes(yintercept=as.numeric(paste(target))+AGtemp_mean.ann, color=target), linetype="dashed") +
  # geom_text(data=site.means[site.means$site %in% unique(agg.dev.graph[!is.na(agg.dev.graph$AGtemp_mean.dev),"site"]),], x=335, y=7, aes(label=round(AGtemp_mean.ann,1)), hjust="right", fontface="bold") +
  scale_x_continuous(expand=c(0,0), name="day of year") +
  scale_y_continuous(expand=c(0,0), limits=range(agg.dev.graph[,c("AGtemp_mean.dev.lo", "AGtemp_mean.dev.hi")]+agg.dev.graph$AGtemp_mean.ann, na.rm=T), name="diff from non-warmed (degrees C)") +
  ggtitle("Daily Mean Aboveground Temperature Difference")+
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$AGtemp_mean.dev),"target"]), "color"])) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$AGtemp_mean.dev),"target"]), "color"])) + 
  theme_bw()
dev.off()

# --------------------


# # --------------------
# # Comparing soil moisture and (air) temperature
# # --------------------
# 
# expclim<-read.csv("/home/miriam/Documents/Harvard/PhenologyWorkshop_2016/radcliffe/Analyses/expclim.csv")
# 
# #Figure out what doy is the start of the month (i.e. Jan 1, Feb 1, etc.)
# dpm   <- c(31,28,31,30,31,30,31,31,30,31,30,31) #days per month
# # dpm.l <- c(31,29,31,30,31,30,31,31,30,31,30,31) #leap year days per month
# doy.start <- vector(length=12)
# doy.start[1] <- 0
# for(i in 2:length(dpm)){
#   doy.start[i] <- doy.start[i-1] + dpm[i]
# }
# 
# #Add a factor column to expclim designating spring, summer, or NA
# expclim[which(expclim$doy>=doy.start[3] & expclim$doy<doy.start[6]),"season"] <- "spring" 
# expclim[which(expclim$doy>=doy.start[6] & expclim$doy<doy.start[9]),"season"] <- "summer"
# expclim$season <- as.factor(expclim$season)
# 
# #Add columns useful for designating temp/precip treatments & doy
# expclim$temptreat2 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham"), "ambient", "warming"))
# expclim$temptreat3 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham"), "0", paste(expclim$temptreat)))
# expclim$preciptreat2 <- as.factor(ifelse(expclim$preciptreat=="1", "+ precip", ifelse(expclim$preciptreat=="-1", "- precip", "ambient")))
# expclim$preciptreat2 <- as.factor(ifelse(is.na(expclim$preciptreat2), "ambient", paste(expclim$preciptreat2)))
# expclim <- expclim[!is.na(expclim$doy),] #remove NAs
# expclim$year.frac <- expclim$year + expclim$doy/366
# 
# #Figure out climate variable values for control/sham/outside plots for each site, year, doy
# vars.clim <- c("airtemp_min", "airtemp_max", 
#                "soiltemp1_min", "soiltemp2_min", 
#                "soiltemp1_max", "soiltemp2_max", 
#                "soilmois1", "soilmois2")
# 
# expclim.base <- aggregate(expclim[expclim$temptreat3=="0",vars.clim], 
#                              by=expclim[expclim$temptreat3=="0",c("site", "year", "doy")], 
#                              FUN=mean, na.rm=T)
# names(expclim.base)[which(names(expclim.base) %in% vars.clim)] <- paste0(vars.clim, ".base")
# 
# #Put the control data and the original expclim data together
# expclim2 <- merge(expclim, expclim.base, all.x=T)
# 
# #Add columns for difference between control and warmed
# expclim2$soiltempdiff_min<-expclim2$soiltemp1_min-expclim2$soiltemp1_min.base
# expclim2$soiltempdiff_max<-expclim2$soiltemp1_max-expclim2$soiltemp1_max.base
# 
# #(A) Soil moisture by temperature treatment (discrete)
# expclim3<-subset(expclim2, !is.na(expclim2$soilmois1) & !is.na(expclim2$temptreat))
# ggplot(expclim3[which(expclim3$soilmois1<5),],aes(x=temptreat,y=soilmois1,fill=site))+
#   scale_x_discrete(name="Temperature treatment")+ scale_y_continuous(name="Soil Moisture (depth 1)")+
#   facet_grid(season~site, scales="free")+
#   geom_boxplot()
# 
# #(B) Soil moisture by temperature difference from control (continuous)
# #Get warming on the x axis:
# ####Christy code:
# expclim3<-subset(expclim2, !is.na(expclim2$soilmois1) & !is.na(expclim2$soiltempdiff_min)) #could change to max
# 
# ggplot(expclim3[which(expclim3$soilmois1<5),],aes(x=soiltempdiff_min,y=soilmois1,col=temptreat3))+ #could change to max
#   scale_x_continuous(name="Plot temp - mean(ctrl temp for same site, year, DOY)")+ 
#   scale_y_continuous(name="Soil Moisture (depth 1)")+
#   facet_grid(season~site, scales="free")+
#   geom_point()
# 
# #(C) Add in precipitation treatments
# 
# #(D) Can I put a line on graph from B for average fbb?
# 
# #(E) Model
# 
# #(F) Split up model by... phenology? precip?
# 

