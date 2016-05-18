# Graphing BACE sites

# ------------------------------------------
# Load Libraries & Datasets
# ------------------------------------------
library(ggplot2)

# Read in experimental met data 
expclim <- read.csv("expclim.csv")
# expclim <- expclim[!is.na(expclim$doy),] # get rid of data that has no day of year (bc it's unusable)
expclim$year.frac <- expclim$year + expclim$doy/366 # Add a continuous time variable that is fractional years
summary(expclim)

bace.plots <- data.frame(plot=unique(expclim[expclim$site=="bace", "plot"]))
for(i in 1:nrow(bace.plots)){
  bace.plots[i,"temptreat"] <- unique(expclim[expclim$site=="bace" & expclim$plot==bace.plots[i,"plot"], "temptreat"])
  bace.plots[i,"preciptreat"] <- unique(expclim[expclim$site=="bace" & expclim$plot==bace.plots[i,"plot"], "preciptreat"])
}

png("Figures/BACE_CanTmax_plots_by_year_label_temptreat.png")
ggplot(data=expclim[expclim$site=="bace" & expclim$cantemp_max<100 & !is.na(expclim$cantemp_max),]) +
  facet_wrap(~plot) +
  geom_line(aes(x=doy, y=cantemp_max, color=as.factor(year))) +
  geom_text(data=bace.plots, aes(x=10, y=50, label=temptreat))
dev.off()

png("Figures/BACE_Soil1Tmax_plots_by_year_label_temptreat.png")
ggplot(data=expclim[expclim$site=="bace" & expclim$cantemp_max<100 & !is.na(expclim$cantemp_max),]) +
  facet_wrap(~plot) +
  geom_line(aes(x=doy, y=soiltemp1_max, color=as.factor(year))) +
  geom_text(data=bace.plots, aes(x=10, y=50, label=temptreat))
dev.off()

png("Figures/BACE_CanTmax_plots_doy.png")
ggplot(data=expclim[expclim$site=="bace" & expclim$cantemp_max<100 & !is.na(expclim$cantemp_max),]) +
  facet_wrap(~plot) +
  geom_line(aes(x=year.frac, y=cantemp_max, color=as.factor(temptreat)))# +
#   geom_text(data=bace.plots, aes(x=10, y=50, label=temptreat))
dev.off()

plot(expclim[expclim$site=="bace" & expclim$plot==10 & expclim$year==2012,"cantemp_max"])
length(expclim[expclim$site=="bace" & expclim$plot==10 & expclim$year==2012,"cantemp_max"])
plot(expclim[expclim$site=="bace" & expclim$plot==1  & expclim$year==2012,"cantemp_max"])
length(expclim[expclim$site=="bace" & expclim$plot==1  & expclim$year==2012,"cantemp_max"])
