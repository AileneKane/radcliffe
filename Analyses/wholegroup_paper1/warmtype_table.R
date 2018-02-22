#Code to make new table comparing mean effects of OTCs with mean effects 
#of different active warming methods, by season
#Started by Ailene February 9, 2018

#Data from Bokhorst at all: Mean OTC effect (mean OTC temperature minus mean control/ambient temperature) on air, 
#soil surface and soil temperatures (±1 SE) across study sites (n = 7–17, see ESM 1–3). The range indicates the lower and upper seasonal OTC effects recorded
#Mean OTC effects and range of effects from Bokhorst et al
season<-c("annual","spring","summer","autumn","winter")#could just have annual values
otcef_air<-c("0.8 (0.1)","0.5 (0.3)","0.9 (0.1)","0.7 (0.2)","1.0 (0.4)")
otcef_soilsurf<-c("0.9 (0.1))","0.6 (0.2)","1.4 (0.1)","1.0 (0.1)","0.5 (0.3)")
otcef_soil<-c("0.8 (0.3))","0.5 (0.3)","0.9 (0.2)","0.7 (0.3)","0.6 (0.3)")
otcran_air<-c("0.5–1.3","-0.8-1.4","0.3–1.5","0.2–1.5","0.1–2.6")
otcran_soilsurf<-c("0.4–1.4","-0.5-1.5","0.5–2.1","0.2–1.6","-0.9-1.4")
otcran_soil<-c("-0.1-3.9","-0.4-3.7","-0.2-3.3","-0.1-3.4","-0.3-2.9")
otctab<-cbind(season,otcef_air, otcef_soilsurf,otcef_soil,otcran_air,otcran_soilsurf,otcran_soil)

otcann<-otctab[1,]#just the annual values
#add annual data from c3e


