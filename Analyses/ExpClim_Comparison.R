# Reading in the experimental Climate data 
library(ggplot2)

treats <- read.csv("treats.csv")
summary(treats)

vars.fac <- c("site", "temptreat", "preciptreat", "plot")
expclim <- read.csv("expclim.csv")
expclim$temptreat2 <- as.factor(ifelse(expclim$temptreat %in% c("0", "outside", "sham"), "ambient", "warming"))
expclim$preciptreat2 <- as.factor(ifelse(expclim$preciptreat=="1", "+ precip", ifelse(expclim$preciptreat=="-1", "- precip", "ambient")))
expclim$preciptreat2 <- as.factor(ifelse(is.na(expclim$preciptreat2), "ambient", paste(expclim$preciptreat2)))
summary(expclim)

# dpm   <- c(31,28,31,30,31,30,31,31,30,31,30,31) #days per month
# dpm.l <- c(31,29,31,30,31,30,31,31,30,31,30,31) #leap year days per month
summary(expclim)


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
