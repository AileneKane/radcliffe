# Reading in the experimental Climate data 
library(ggplot2)

treats <- read.csv("treats.csv")
summary(treats)

vars.fac <- c("site", "temptreat", "preciptreat", "plot")
expclim <- read.csv("expclim.csv")
expclim$temptreat2 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham"), "ambient", "warming"))
expclim$temptreat3 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham"), "0", paste(expclim$temptreat)))
expclim$preciptreat2 <- as.factor(ifelse(expclim$preciptreat=="1", "+ precip", ifelse(expclim$preciptreat=="-1", "- precip", "ambient")))
expclim$preciptreat2 <- as.factor(ifelse(is.na(expclim$preciptreat2), "ambient", paste(expclim$preciptreat2)))
expclim <- expclim[!is.na(expclim$doy),]
summary(expclim)

dpm   <- c(31,28,31,30,31,30,31,31,30,31,30,31) #days per month
# dpm.l <- c(31,29,31,30,31,30,31,31,30,31,30,31) #leap year days per month
doy.start <- vector(length=12)
doy.start[1] <- 0
for(i in 2:length(dpm)){
  doy.start[i] <- doy.start[i-1] + dpm[i]
}
doy.start

expclim[expclim$doy>=doy.start[3] & expclim$doy<=doy.start[5],"season"] <- "spring"
expclim[expclim$doy>=doy.start[6] & expclim$doy<=doy.start[8],"season"] <- "summer"
expclim$season <- as.factor(expclim$season)
summary(expclim)


ggplot(data=expclim[,]) +
  facet_wrap(~site, scales="free_x") +
  geom_boxplot(aes(x=temptreat, y=soilmois1))

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
