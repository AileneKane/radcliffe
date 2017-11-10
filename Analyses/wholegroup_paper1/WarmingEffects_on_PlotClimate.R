# Analyzing the effects of warming & preciptiation on soil temperature and precipitation
# This is a cleaner version of ExpClim_Comparison.R that corresponds more clearly to 
# analyses and figures in the whole group paper
# Christy Rollinson, crollinson@mortonarb.org

# Workflow:
# 1. Read in the raw data & recode treatments to a binary treatment code
# 2. Adding in detail about the target warming level
# 3. Calculating deviations from the appropriate control (takes into account precip treatment)
# 4. Aggregating & graphing warming effects


# ----------------
# 1. Read in the raw data & recode treatments to a binary treatment code
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


# Recoding to binary treatments (yes/no)
expclim$temptreat2 <- as.factor(ifelse(expclim$temptreat=="0", "control", paste(expclim$temptreat)))
expclim$temptreat3 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham", "ambient"), "ambient", "warming"))
expclim$preciptreat2 <- as.factor(ifelse(expclim$preciptreat=="1", "+ precip", ifelse(expclim$preciptreat=="-1", "- precip", "0")))
expclim$preciptreat2 <- as.factor(ifelse(is.na(expclim$preciptreat2), "0", paste(expclim$preciptreat2)))

# Getting mean aboveground and soil temp1 if not already reported
# -- mean didn't work in the ifelse statement -- don't know why, but the sum/2 seems to work just fine.  
expclim$AGtemp_mean <- rowMeans(expclim[,c("AGtemp_max", "AGtemp_min")], na.rm=F)
expclim$BGtemp_mean <- ifelse(is.na(expclim$soiltemp1_mean), sum(expclim[,c("soiltemp1_max", "soiltemp1_min")], na.rm=F)/2, expclim$soiltemp1_mean) 
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

# ----------------


# ----------------
# 2. Adding in detail about the target warming level
# ----------------
targets <- read.csv("../treats_detail.csv", na.strings=c("NA"))

# Merge the targets into the expclim file
expclim <- merge(expclim, targets, all.x=T)
summary(expclim)
summary(expclim$target)

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
# ----------------


# ----------------
# 3. Calculating deviations from the appropriate control
# ----------------
vars.clim <- c("AGtemp_mean", "BGtemp_mean", 
               "soilmois1")

# Using the mean of everything with no warming
# Find the baseline for each year to calculate the deviations
#  -- will aggregate across years first when going to graph
expclim.base <- aggregate(expclim[expclim$target=="0",vars.clim], 
                          by=expclim[expclim$target=="0",c("site", "year", "doy", "preciptreat2")], 
                          FUN=mean, na.rm=T)
names(expclim.base)[which(names(expclim.base) %in% vars.clim)] <- paste0(vars.clim, ".base")
summary(expclim.base)


# Finding the mean annual temp etc to add to graphs 
# Note: This is only based off of temp + precip control temperatures
day.means <- aggregate(expclim.base[expclim.base$preciptreat2==0,paste0(vars.clim, ".base")],
                       by=expclim.base[expclim.base$preciptreat2==0,c("site", "doy")],
                       FUN=mean, na.rm=T)
summary(day.means)

site.means <- aggregate(day.means[,paste0(vars.clim, ".base")],
                        by=list(day.means[,c("site")]),
                        FUN=mean, na.rm=T)
names(site.means) <- c("site", paste0(vars.clim, ".ann"))
summary(site.means)


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
# ----------------


# ----------------
# 4. Aggregating & graphing warming effects
#    4.a. Warming-only
#    4.b. All treatments, referenced to precip
# ----------------
library(ggplot2); library(stringr)

colors.target <- data.frame(target = c("0",     "1",      "1.5",          "2",            "2.5",  "2.7",   "3",             "3.5",     "4",      "4.5",    "5",         "5.5"),
                            color  = c("black", "purple", "mediumpurple", "mediumorchid", "plum3","plum2", "palevioletred", "salmon1", "orange", "coral2", "orangered", "red"))
# For when 1-degree drops out for some reason
colors.target2 <- c("black", "purple", "mediumpurple", "plum3", "plum2","palevioletred", "salmon1", "orange", "coral2", "orangered", "red")

# ---------
# 4.a Finding the mean deviation for each across years
# ---------
# Going by target warming rather than the temptreatment level
expclim.agg <- aggregate(expclim2[expclim2$preciptreat2==0,c(paste0(vars.clim, ".base"), paste0(vars.clim, ".dev"), paste0(vars.clim, ".ann"))], 
                           by=expclim2[expclim2$preciptreat2==0,c("site", "target", "plot", "doy")], 
                           FUN=mean, na.rm=T)
summary(expclim.agg)

# Getting the mean & sd by across treatments
agg.dev <- aggregate(expclim.agg[,c(paste0(vars.clim, ".base"), paste0(vars.clim, ".dev"), paste0(vars.clim, ".ann"))], 
                     by=expclim.agg[,c("site", "target", "doy")], 
                     FUN=mean, na.rm=T)
agg.dev[,paste0(vars.clim, ".dev.lo")] <- aggregate(expclim.agg[,paste0(vars.clim, ".dev")], 
                                                    by=expclim.agg[,c("site", "target", "doy")], 
                                                    FUN=quantile, 0.025, na.rm=T)[,paste0(vars.clim, ".dev")]
agg.dev[,paste0(vars.clim, ".dev.hi")] <- aggregate(expclim.agg[,paste0(vars.clim, ".dev")], 
                                                    by=expclim.agg[,c("site", "target", "doy")], 
                                                    FUN=quantile, 0.975, na.rm=T)[,paste0(vars.clim, ".dev")]

summary(agg.dev)

agg.dev.graph <- agg.dev

# Just trimming things to the CIs to make things easier
agg.dev.graph[agg.dev.graph$soilmois1.dev.lo<= min(agg.dev.graph$soilmois1.dev, na.rm=T) & !is.na(agg.dev.graph$soilmois1.dev.lo),"soilmois1.dev.lo"] <- min(agg.dev.graph$soilmois1.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$soilmois1.dev.hi>= max(agg.dev.graph$soilmois1.dev, na.rm=T) & !is.na(agg.dev.graph$soilmois1.dev.hi),"soilmois1.dev.hi"] <- max(agg.dev.graph$soilmois1.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$BGtemp_mean.dev.lo<= min(agg.dev.graph$BGtemp_mean.dev, na.rm=T) & !is.na(agg.dev.graph$BGtemp_mean.dev.lo),"BGtemp_mean.dev.lo"] <- min(agg.dev.graph$BGtemp_mean.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$BGtemp_mean.dev.hi>= max(agg.dev.graph$BGtemp_mean.dev, na.rm=T)  & !is.na(agg.dev.graph$BGtemp_mean.dev.hi),"BGtemp_mean.dev.hi"] <- max(agg.dev.graph$BGtemp_mean.dev, na.rm=T)

# summary(agg.dev.graph)

# Ordering things by mean annual soil temp
site.means
agg.dev.graph$site <- factor(agg.dev.graph$site, levels=site.means[order(site.means$soilmois1.ann),"site"])
# identifying which figures we want to graph
sites.graph<-unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.ann) & !agg.dev.graph$site=="exp08","site"])


png("../figures/WarmingEffects_TimeSeries_SoilMoist_Deviation_NoPrecip.png", height=9, width=9, units="in", res=180)
ggplot(data=agg.dev.graph[agg.dev.graph$site %in% sites.graph & !is.na(agg.dev.graph$soilmois1.dev),]) +
  facet_wrap(~site, scales="fixed", ncol=3) +
  geom_ribbon(aes(x=doy, ymin=soilmois1.dev.lo*100, ymax=soilmois1.dev.hi*100, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=soilmois1.dev*100, color=target), size=0.5) +
  geom_text(data=site.means[site.means$site %in% sites.graph,], x=325, y=0.12*100, aes(label=str_pad(round(soilmois1.ann,2), 4,side="right", pad="0")), hjust="right", fontface="bold") +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed (% soil moisture)") +
  ggtitle("Daily Mean Soil Moisture Difference") +
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),"target"]), "color"]), 
                     name=expression(paste("target warming " ^"o", "C"))) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),"target"]), "color"]), 
                    name=expression(paste("target warming " ^"o", "C"))) + 
  theme_bw()
dev.off()


# Re-Ordering things by mean annual soil temp
site.means
agg.dev.graph$site <- factor(agg.dev.graph$site, levels=site.means[order(site.means$BGtemp_mean.ann),"site"])
# identifying which figures we want to graph
sites.graph<-unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"site"])

# exp12 has somethign weird with its soil temperature -- maybe a lot of missing days at a certain point, so that may need to get split up
summary(agg.dev.graph[agg.dev.graph$site=="exp12" & is.na(agg.dev.graph$BGtemp_mean.base),])
unique(agg.dev.graph[agg.dev.graph$site=="exp12" & is.na(agg.dev.graph$BGtemp_mean.base),"doy"])


png("../figures/WarmingEffects_TimeSeries_SoilTemp1Mean_Deviation_NoPrecip.png", height=4.5, width=9, units="in", res=180)
ggplot(data=agg.dev.graph[agg.dev.graph$site %in% sites.graph ,]) +
  facet_wrap(~site, scales="fixed", ncol=5) +
  geom_ribbon(aes(x=doy, ymin=BGtemp_mean.dev.lo, ymax=BGtemp_mean.dev.hi, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=BGtemp_mean.dev, color=target), size=0.5) +
  geom_hline(aes(yintercept=as.numeric(paste(target)), color=target), linetype="dashed") +
  geom_text(data=site.means[site.means$site %in% sites.graph,], x=325, y=7, aes(label=round(BGtemp_mean.ann,1)), hjust="right", fontface="bold") +
  scale_x_continuous(expand=c(0,0), name="day of year") +
  scale_y_continuous(expand=c(0,0), limits=range(agg.dev.graph[,c("BGtemp_mean.dev.lo", "BGtemp_mean.dev.hi")], na.rm=T), name=expression(paste("diff from non-warmed (" ^"o", "C)"))) +
  ggtitle("Daily Mean Soil Temperature Difference")+
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"target"]), "color"]), 
                     name=expression(paste("target warming " ^"o", "C"))) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"target"]), "color"]), 
                    name=expression(paste("target warming " ^"o", "C"))) + 
  theme_bw()
dev.off()
# ---------


# ---------
# 4.a Finding the mean deviation for each across years
# ---------
# Going by target warming rather than the temptreatment level
expclim.agg <- aggregate(expclim2[,c(paste0(vars.clim, ".base"), paste0(vars.clim, ".dev"), paste0(vars.clim, ".ann"))], 
                         by=expclim2[,c("site", "target", "plot", "doy")], 
                         FUN=mean, na.rm=T)
summary(expclim.agg)

# Getting the mean & sd by across treatments
agg.dev <- aggregate(expclim.agg[,c(paste0(vars.clim, ".base"), paste0(vars.clim, ".dev"), paste0(vars.clim, ".ann"))], 
                     by=expclim.agg[,c("site", "target", "doy")], 
                     FUN=mean, na.rm=T)
agg.dev[,paste0(vars.clim, ".dev.lo")] <- aggregate(expclim.agg[,paste0(vars.clim, ".dev")], 
                                                    by=expclim.agg[,c("site", "target", "doy")], 
                                                    FUN=quantile, 0.025, na.rm=T)[,paste0(vars.clim, ".dev")]
agg.dev[,paste0(vars.clim, ".dev.hi")] <- aggregate(expclim.agg[,paste0(vars.clim, ".dev")], 
                                                    by=expclim.agg[,c("site", "target", "doy")], 
                                                    FUN=quantile, 0.975, na.rm=T)[,paste0(vars.clim, ".dev")]

summary(agg.dev)

agg.dev.graph <- agg.dev

# Just trimming things to the CIs to make things easier
agg.dev.graph[agg.dev.graph$soilmois1.dev.lo<= min(agg.dev.graph$soilmois1.dev, na.rm=T) & !is.na(agg.dev.graph$soilmois1.dev.lo),"soilmois1.dev.lo"] <- min(agg.dev.graph$soilmois1.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$soilmois1.dev.hi>= max(agg.dev.graph$soilmois1.dev, na.rm=T) & !is.na(agg.dev.graph$soilmois1.dev.hi),"soilmois1.dev.hi"] <- max(agg.dev.graph$soilmois1.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$BGtemp_mean.dev.lo<= min(agg.dev.graph$BGtemp_mean.dev, na.rm=T) & !is.na(agg.dev.graph$BGtemp_mean.dev.lo),"BGtemp_mean.dev.lo"] <- min(agg.dev.graph$BGtemp_mean.dev, na.rm=T)
agg.dev.graph[agg.dev.graph$BGtemp_mean.dev.hi>= max(agg.dev.graph$BGtemp_mean.dev, na.rm=T)  & !is.na(agg.dev.graph$BGtemp_mean.dev.hi),"BGtemp_mean.dev.hi"] <- max(agg.dev.graph$BGtemp_mean.dev, na.rm=T)
# summary(agg.dev.graph)

# Ordering things by mean annual soil temp
site.means
agg.dev.graph$site <- factor(agg.dev.graph$site, levels=site.means[order(site.means$soilmois1.ann),"site"])
# identifying which figures we want to graph
sites.graph<-unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.ann) & !agg.dev.graph$site=="exp08","site"])


png("../figures/WarmingEffects_TimeSeries_SoilMoist_Deviation.png", height=9, width=9, units="in", res=180)
ggplot(data=agg.dev.graph[agg.dev.graph$site %in% sites.graph & !is.na(agg.dev.graph$soilmois1.dev),]) +
  facet_wrap(~site, scales="fixed", ncol=3) +
  geom_ribbon(aes(x=doy, ymin=soilmois1.dev.lo, ymax=soilmois1.dev.hi, fill=target), alpha=0.3) +
  geom_line(aes(x=doy, y=soilmois1.dev, color=target), size=0.5) +
  geom_text(data=site.means[site.means$site %in% sites.graph,], x=325, y=0.12, aes(label=str_pad(round(soilmois1.ann,2), 4,side="right", pad="0")), hjust="right", fontface="bold") +
  scale_y_continuous(expand=c(0,0), name="diff from non-warmed (VWC)") +
  ggtitle("Daily Mean Soil Moisture Difference") +
  scale_color_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),"target"]), "color"]), 
                     name=expression(paste("target warming " ^"o", "C"))) + 
  scale_fill_manual(values=as.vector(colors.target[colors.target$target %in% unique(agg.dev.graph[!is.na(agg.dev.graph$soilmois1.dev),"target"]), "color"]), 
                    name=expression(paste("target warming " ^"o", "C"))) + 
  theme_bw()
dev.off()


# Re-Ordering things by mean annual soil temp
site.means
agg.dev.graph$site <- factor(agg.dev.graph$site, levels=site.means[order(site.means$BGtemp_mean.ann),"site"])
# identifying which figures we want to graph
sites.graph<-unique(agg.dev.graph[!is.na(agg.dev.graph$BGtemp_mean.dev),"site"])

# exp12 has somethign weird with its soil temperature -- maybe a lot of missing days at a certain point, so that may need to get split up
summary(agg.dev.graph[agg.dev.graph$site=="exp12" & is.na(agg.dev.graph$BGtemp_mean.base),])
unique(agg.dev.graph[agg.dev.graph$site=="exp12" & is.na(agg.dev.graph$BGtemp_mean.base),"doy"])


png("../figures/WarmingEffects_TimeSeries_SoilTemp1Mean_Deviation.png", height=4.5, width=9, units="in", res=180)
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
# ---------

# ----------------
