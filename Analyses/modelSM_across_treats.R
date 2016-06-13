#######################################################################################################
### GOAL: ANALYSIS OF SOIL MOISTURE DIFFERENCES ACROSS TREATMENTS
#######################################################################################################
### M. Johnston
### For the "Predicting Future Springs" Radcliffe workshop

###Resource citations:
# Faraway, J.J. 2006. Extending the Linear Model with R. Chapman & Hall/CRC.
# Bates, D.M. 2010. lme4: Mixed Effects Modeling with R. Springer.
# Bolker, B. 2013. Comment on Stack Exchange: 
    #http://stats.stackexchange.com/questions/79360/mixed-effects-model-with-nesting

###What are the treatments? 
    #temperature treatment, precip treatment, combinations of temp & precip, ambient/ambient
    #Note: for all these analysis, I am considering control, sham, and ambient to be the same
#######################################################################################################

### (0) R environment prep & data import -------------------------------------------------- 
#Load libraries, set directory
library(ggplot2)
library(lme4)
dir<-"/home/miriam/RadcliffePheno/Analyses"

#Read in experimental phenology data
setwd(dir)
#eg<-read.csv("expclim_gapfill.csv") #This is the gap-filled data (from Christy); it doesn't include gap-filled soil moisture
expclim <- read.csv("expclim.csv") #This includes soil moisture, is not gap-filled
expsiteinfo<-read.csv("expsiteinfo.csv") #This look-up file includes the actual temp/precip treatments

### (A) Data clean-up ----------------------------------------------------------------------

#Remove data for which I don't know year or doy
expclim<-expclim[-which(is.na(expclim$year)),]
expclim<-expclim[-which(is.na(expclim$doy)),]
#Remove unknown column "alltreat" - not in readme file
expclim<-expclim[,-which(colnames(expclim)=="alltreat")]
#Rename Cleland in expsiteinfo to Jasper to match expclim
expsiteinfo$Site<-as.character(expsiteinfo$Site)
expsiteinfo[10,1]<-"jasper"
expsiteinfo$Site<-as.factor(expsiteinfo$Site)
#Rename preciptreat_.1 to preciptreat_-1 for easier coding later
colnames(expsiteinfo)[37]<-"preciptreat_-1"

### (B) Add useful columns of data ------------------------------------------------------

#Figure out what doy is the start of the month (i.e. Jan 1, Feb 1, etc.)
dpm   <- c(31,28,31,30,31,30,31,31,30,31,30,31) #days per month
dpm.l <- c(31,29,31,30,31,30,31,31,30,31,30,31) #leap year days per month. leap years = 1992, 1996, 2000, 2004, 2008, 2012
doy.start <- vector(length=12)
doy.start.l <- vector(length=12)
doy.start[1] <- 0
for(i in 2:length(dpm)){
  doy.start[i] <- doy.start[i-1] + dpm[i]
}
doy.start.l[1] <- 0
for(i in 2:length(dpm.l)){
  doy.start.l[i] <- doy.start.l[i-1] + dpm.l[i]
}

#Add a factor column designating season
  #Non-leapyears
expclim.nl<-subset(expclim,expclim$year!=1992 & expclim$year!=1996 & expclim$year!=2000 &
                           expclim$year!=2004 & expclim$year!=2008 & expclim$year!=2012)
expclim.nl[which(expclim.nl$doy>=doy.start[3] & expclim.nl$doy<doy.start[6]),"season"] <- "spring" 
expclim.nl[which(expclim.nl$doy>=doy.start[6] & expclim.nl$doy<doy.start[9]),"season"] <- "summer"
expclim.nl[which(expclim.nl$doy>=doy.start[9] & expclim.nl$doy<doy.start[12]),"season"] <- "fall"
  #Leapyears
expclim.l<-subset(expclim,expclim$year==1992 | expclim$year==1996 | expclim$year==2000 |
                          expclim$year==2004 | expclim$year==2008 | expclim$year==2012)
expclim.l[which(expclim.l$doy>=doy.start[3] & expclim.l$doy<doy.start[6]),"season"] <- "spring" 
expclim.l[which(expclim.l$doy>=doy.start[6] & expclim.l$doy<doy.start[9]),"season"] <- "summer"
expclim.l[which(expclim.l$doy>=doy.start[9] & expclim.l$doy<doy.start[12]),"season"] <- "fall"
  #Puttling them together
expclim<-rbind(expclim.l,expclim.nl)
  #Adding wintertime
expclim$season<-as.character(expclim$season)
expclim[which(is.na(expclim$season)),ncol(expclim)]<-"winter"
expclim$season<-as.factor(expclim$season)

#Add fractional doy
expclim$year.frac <- expclim$year + expclim$doy/366

#Add numerical designation of temp and precip treatments, basically combining 0s
expclim$temptreat_num <- as.factor(ifelse(expclim$temptreat %in% c("0", "ambient"), "0", paste(expclim$temptreat))) #temp 0 or levels
expclim$preciptreat_num <- as.factor(ifelse(is.na(expclim$preciptreat) | expclim$preciptreat=="ambient" | expclim$preciptreat=="0", "0", paste(expclim$preciptreat))) #precip 0 or levels

#Add a single identifier for treatment
expclim$tottreat<-paste(expclim$temptreat_num,".",expclim$preciptreat_num,sep="") #This may have been what 'alltreat' was...

#Add intended temp & precip treatments, depths at which moisture and temp were measured
df=expclim[FALSE,]
sites<-unique(expclim$site)
for(j in 1:length(sites)){
  sub<-subset(expclim,expclim$site==sites[j])
  info<-subset(expsiteinfo,expsiteinfo$Site==sites[j])
  uniqT<-unique(sub$temptreat_num)
  uniqP<-unique(sub$preciptreat_num)
  sub$Tdepth1<-info$Tsoildepth1_cm
  sub$Tdepth2<-info$Tsoildepth2_cm
  sub$Mdepth1<-info$Msoildepth1_cm
  sub$Mdepth2<-info$Msoildepth2_cm
  for(h in 1:length(uniqT)){
    sub2<-subset(sub,temptreat_num==uniqT[h])
    if(uniqT[h]==0){sub2$Tval<-0}
    if(uniqT[h]!=0){
      sub2$Tval<-info[,which(colnames(info)==paste("temptreat_",uniqT[h],sep=""))]
    }
    for(g in 1:length(uniqP)){
      sub3<-subset(sub2,preciptreat_num==uniqP[g])
      if(nrow(sub3)!=0){
        if(uniqP[g]==0){
          sub3$Pval<-0
        }
        if(uniqP[g]==1){
          sub3$Pval<-info[,which(colnames(info)==paste("preciptreat_",uniqP[g],sep=""))]
        }
        if(uniqP[g]==-1){ #do this separately because I have to make sure the negative sign shows up
          sub3$Pval<-paste("-",info[,which(colnames(info)==paste("preciptreat_",uniqP[g],sep=""))],sep="")
        } 
        df<-rbind(df,sub3)}
    }
  }
  print(sites[j])
}

#Checking to make sure that all temptreat_num and preciptreat_num correspond with a single treatment --looks good!
ggplot(aes(x=temptreat_num,y=Tval),data=df)+facet_wrap(~site,scales="free")+geom_point()+
  theme(strip.text.x = element_text(size = 15))
ggplot(aes(x=preciptreat_num,y=Pval),data=df)+facet_wrap(~site,scales="free")+geom_point()+
  theme(strip.text.x = element_text(size = 15))
#Note: Tval = target warming treatment, Pval = target precipitation treatment, 
#Tdepth = depth at which soil temp was measured, Mdepth = depth at which soil moisture was measured

### (C) Create a dataframe showing what the treatments were meant to be by site, for reference--------------------

sites<-unique(df$site)
treatments<-data.frame(site=NA,temptreat=NA,preciptreat=NA,Tval=NA,Pval=NA, nrecords=NA)
for(j in 1:length(sites)){
  sub<-subset(df,df$site==sites[j])
  temptreat<-unique(sub$temptreat_num)
  preciptreat<-unique(sub$preciptreat_num)
  for(m in 1:length(temptreat)){
    for(k in 1:length(preciptreat)){
      sub_sub<-sub[which(sub$temptreat_num==temptreat[m] & sub$preciptreat_num==preciptreat[k]),]
      treatments<-rbind(treatments,data.frame(site=sites[j],temptreat=temptreat[m],
                                              preciptreat=preciptreat[k],
                                              Tval=sub_sub$Tval[1],Pval=sub_sub$Pval[1],
                                              nrecords=nrow(sub_sub)))
    }
  }
} 
treatments<-treatments[-1,] #This is all the treatments at each site, both numeric codes and what they code for.

### (D) Graphing: Exploration of what the treatments actually look like (as opposed to targets)-----------------

#Don't worry about the error messages here, it's because there are a lot of NA values for these measurements
  #Temperature things:
ggplot(aes(x=as.factor(Tval),y=airtemp_min),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))
ggplot(aes(x=as.factor(Tval),y=airtemp_max),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))

ggplot(aes(x=as.factor(Tval),y=soiltemp1_min),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))
ggplot(aes(x=as.factor(Tval),y=soiltemp1_max),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))

ggplot(aes(x=as.factor(Tval),y=soiltemp2_min),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))
ggplot(aes(x=as.factor(Tval),y=soiltemp2_max),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))

ggplot(aes(x=as.factor(Tval),y=cantemp_min),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))
ggplot(aes(x=as.factor(Tval),y=cantemp_max),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))

ggplot(aes(x=as.factor(Tval),y=surftemp_min),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))
ggplot(aes(x=as.factor(Tval),y=surftemp_max),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))

ggplot(aes(x=as.factor(Tval),y=soiltemp1_mean),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))
ggplot(aes(x=as.factor(Tval),y=soiltemp2_mean),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))

  #Precipitation things:
ggplot(aes(x=as.factor(Pval),y=soilmois1),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))
ggplot(aes(x=as.factor(Pval),y=soilmois2),data=df)+facet_wrap(~site,scales="free")+geom_boxplot()+
  theme(strip.text.x = element_text(size = 15))

#What I learned from these graphs: treatments must be considered continuous, not categorical

### (E) Preliminary test/exploration models--------------------------------

#(I) Dependent variable
  #soilmois1 (only chuine includes soilmois2)
#(II) Independent variables
  #-airtemp (min, max); soiltemp (min, max, potentially mean); precip treatment (factor?), interactions (focus on 2-way)
  #-others available were not measured at enough sites
  #-choose to keep Pval as a factor because I want to try an interaction between precipitation & temperature, but I expect that the
    #interaction will be nonlinear (i.e. at high temp low precip and low temp high precip soilmois will both yield a mid-level temp*precip,
    #but are expected to yield very low SM and very high SM, accordingly). 
#(III) Important sources of other variation: 
  #Temporal 
    #-seasonal variability [+as.factor(season)]
    #-time trend [+year.frac] - actually don't do this, because it's collinear with doy & year
      #assume, instead, that this issue is dealt with using year and doy separately, though there is a bit of a trend:
        dat<-df[which(!is.na(df$soilmois1)),]
        plot( tapply(df$soilmois1, df$year, function(x) mean(x, na.rm=T)) ) #this is all data with soilmois
    #-random yearly variability (not specifically a trend) [+as.factor(year)]
    #-autocorrelation of observations, which were taken at different time intervals depending on the site [+doy]
  #Spatial
    #-site random effect
    #-block random effect - 5 sites have blocks
    #-plot random effect
      #Original thought: I don't think I want to model this out because there is only one treatment per plot, so plot and treatment are confounded
      #actually: there ARE multiple "treatments" per plot if I'm considering the treatments to be continuous. So maybe I should include.
      #but: what using plot as a random effect is essentially doing is making the relationship between soil moisture and treatment
        #conditional on plot. I don't want to do this, because the variation within a plot will be very small. 
      #Conclusion: don't have plot as random effect.
#(IV) Other notes/things to keep in mind
  #Given the variation in actual treatments (despite the targets), I think they must be considered continuous rather than categorical.
  #I'm not using gap filled data. I think that the gap filling is more for people who are interested in accumulating variables over time.
  #What to do abbout missing independent variables? I'm going to omit, for now. 
    #But: can I assume that data are missing at random? If not, how do I deal with that?
    #For starters, check where missingness of covariates is correlated with soilmoist1:
        dat<-df[which(!is.na(df$soilmois1)),] #Must have the independent variable
          #missing air temp data (note that min/max are the same or essentially the same in terms of missingness)
            dat$ATemp.missing<-ifelse(is.na(dat$airtemp_min),"yes","no")
            ggplot(aes(x=ATemp.missing,y=soilmois1),data=dat)+facet_wrap(~site,scales="free")+geom_boxplot()+
              theme(strip.text.x = element_text(size = 15))
          #missing soiltemp data 
            dat$STemp.missing<-ifelse(is.na(dat$soiltemp1_min),"yes","no")
            ggplot(aes(x=STemp.missing,y=soilmois1),data=dat)+facet_wrap(~site,scales="free")+geom_boxplot()+
              theme(strip.text.x = element_text(size = 15))
        #these look okay - missing data seems comparable in terms of soilmois to non-missing. Carry on.
  #I am not currently accounting for the fact that soil moisture was measured at different depths depending on the site... 
    #but I think this is taken care of in the random effect for site
  #bace has temperatures measured in watts as opposed to degreesC. This is not okay, because it will change the point estimates for model coefficients.
    #For now, bace gets its own separate model and is omitted from the total model. But perhaps we should convert the watts to degreesC
  #Should I include non-linear effects? Check to see if the independent variables are linear with soil moisture:
    #If not, I may need to transform....
        plot(moddat$soilmois1,moddat$airtemp_min) #not non-linear, but basically just a cloud
        plot(moddat$soilmois1,moddat$airtemp_max) #not non-linear, but basically just a cloud
        plot(moddat$soilmois1,moddat$soiltemp1_min) #something funky going on here - measurement error, 
          #many points seem bounded by 0... do I need to deal with this?
          #does not meet homoskedasticity assumption
          summary(moddat[which(moddat$soiltemp1_min<0),]) #6 out of 9 sites have temps that go below 0
       plot(moddat$soilmois1,moddat$soiltemp1_max) #does not meet homoskedasticity assumption at all
       plot(moddat$soilmois1,moddat$Pval) #meh, sure
      #So my main issue is heteroskadasticity in soiltemp variables. Note that this doesn't bias coefficient
        #estimates, but it does make the SEs incorrect. See if a simple transformation will do the trick:
        plot(moddat$soilmois1,log(moddat$soiltemp1_min)) #terrible! Plus there are some negatives so this isn't really legit
        plot(moddat$soilmois1,log(moddat$soiltemp1_max)) #terrible again! Plus there are some negatives
     #If the heteroscedasticity is a result of underlying groups (sites?), maybe it's okay because I use site as a RE
        par(mfrow=c(2,3))
        for(i in 1:length(unique(moddat$site))){
        sub<-moddat[which(moddat$site==unique(moddat$site)[i]),]
          plot(sub$soilmois1,sub$soiltemp1_min,main=as.character(unique(moddat$site)[i]))
           } #error is because chuine, farnsworth, jasper have no soil temp data
       #These actually look pretty okay in terms of heteroscadasticity, except for ellison.
        #I think this is fine.
        for(i in 1:length(unique(moddat$site))){
         sub<-moddat[which(moddat$site==unique(moddat$site)[i]),]
          plot(sub$soilmois1,sub$soiltemp1_max,main=as.character(unique(moddat$site)[i]))
        } #again except for ellison, looks pretty okay.
    
#---BLOCKED DATA FIRST---# aka modeling discovery, because I ultimately don't want to just use the sites with blocks.
dfB<-df[which(!is.na(df$block)),] #only blocked data
dfB2<-dfB[which(dfB$site!="bace"),] #remove bace because its temperture is in watts
dfB3<-dfB2[which(!is.na(dfB2$soilmois1)),] #must have the dependent variable
dat<-dfB3 #Just in case I want to do more selection later, so I don't have to rewrite code below
 
  #Trim dataframe to complete cases of all possible variables that might be included
moddat<-dat[,which(colnames(dat) %in% c("site","block","plot","year","doy","airtemp_min","airtemp_max",
                                          "soiltemp1_min","soiltemp1_max","soiltemp1_mean","season","soilmois1",
                                          "year.frac","Pval"))]
dim(moddat[complete.cases(moddat),]) #nrow=0
dim(moddat[which(!is.na(moddat$airtemp_max) & !is.na(moddat$soiltemp1_max)),]) #nrow=0
  
  #Different trimming. site, block, plot, year, doy, year.frac, Pval, soilmois1 are complete. Try removing soiltemp_mean
moddat<-dat[,which(colnames(dat) %in% c("site","block","plot","year","doy","airtemp_min","airtemp_max",
                                        "soiltemp1_min","soiltemp1_max","season","soilmois1",
                                        "year.frac","Pval"))]
dim(moddat[complete.cases(moddat),]) #nrow= 0
dim(moddat[which(!is.na(moddat$airtemp_max) & !is.na(moddat$soiltemp1_max)),]) #nrow=0

  #Ugh, how do I deal with the missing data??
summary(moddat)#It looks like the main culprit is airtemp_min/max.
  #Note: I want to keep some temperature variable and some precipitation variable in the model, 
    #because these are the "treatments" I'm testing the effects of
    #or, poentially, I could model these effects separately (see a future section)
moddat<-dat[,which(colnames(dat) %in% c("site","block","plot","year","doy",
                                        "soiltemp1_min","soiltemp1_max","soiltemp1_mean","season","soilmois1",
                                        "year.frac","Pval"))]

dim(moddat[complete.cases(moddat),]) #nrow=14998 (out of an original 26600). This only has one site (force)
moddat_complete<-moddat[complete.cases(moddat),]

  #So I don't need a random effect of site because it's only one site. 
  #[The models that follow are before the decision not to use plot as a random effect]
mod<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+soiltemp1_mean+as.factor(Pval)+(1|plot), data=moddat_complete)
  #Error: fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
  #May mean that one or more varialbes is not linearly independent, according to the interwebs...
mod<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+as.factor(Pval)+(1|plot), data=moddat_complete)   #getting rid of soiltemp_mean fixed it
  #OH. soiltemp mean is just the average of min and max. Remove forever more.
  #Model about is still fairly meaningless because of all the temporal correlation structures. Add them in here:
mod<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+as.factor(Pval)+as.factor(season)+year.frac+ as.factor(year)+doy+(1|plot), data=moddat_complete) 
  #Error: fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
  #Error: Dropping columns failed to produce full column rank design matrix
  #Try dropping doy: (CJ says try dropping year.frac instead -- as.factor(year) will take some of the longterm trend into consideration,
    #specifying the trend would be biased anyway if it weren't linear. Do this for the next set of models)
mod<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+as.factor(Pval)+as.factor(season)+year.frac+as.factor(year)+(1|plot), data=moddat_complete) 
summary(mod)
  #What about adding year as a random instead of a fixed effect? (CJ says fixed effects easier to interpret, suggests not using year as random)
moddat_complete$year.f<-as.factor(moddat_complete$year)
mod<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+as.factor(Pval)+as.factor(season)+year.frac+(1|year.f)+(1|plot), data=moddat_complete) 
summary(mod) #...not really sure where to go with this, and it's just one site, so moving on. Get rid of plot as random with the next set of models

#What I learned with the blocked data analysis:
#-don't just use blocked data, I end up with only one site
#-ditch soiltemp_mean
#-ditch year.frac

#---More model testing/learning, IGNORING THE FACT THAT SOME SITES HAVE BLOCKS---#
dfAll<-df[which(!is.na(df$soilmois1)),] #must have the dependent variable
dfAll<-dfAll[which(dfAll$site!="bace"),] #because of the watts issue
dat<-dfAll

moddat<-dat[,which(colnames(dat) %in% c("site","block","plot","year","doy","airtemp_min","airtemp_max",
                                        "soiltemp1_min","soiltemp1_max","season","soilmois1",
                                        "Pval"))]
summary(moddat)
#What's the overlap in airtemp and soiltemp measurements?
length(which(!is.na(moddat$airtemp_max) & !is.na(moddat$soiltemp1_max))) #109349 (vs. 150157 -- not too bad)
moddat_complete<-moddat[which(!is.na(moddat$airtemp_max) & !is.na(moddat$soiltemp1_max)),] #This is good 

#Model time!
mod1<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+airtemp_min+airtemp_max+as.factor(Pval)+ #fixed effects
            as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
            (1|site), data=moddat_complete)  #random effects
summary(mod1)
plot(mod1) #meh.

  #Testing how/if to nest random effects:
mod2<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+airtemp_min+airtemp_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site)+(1|plot), data=moddat_complete)  #random effects
summary(mod2)
plot(mod2)
   #same as mod 2, but nest plot in site -- is there a difference?
mod3<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+airtemp_min+airtemp_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site/plot), data=moddat_complete)  #random effects
summary(mod3)
plot(mod3)
anova(mod1,mod2,mod3)
#Note re. precip: only one of the sites in this subset which has airtemp & soiltemp (marchin) has a precipitation treatment
#So: I may have to do SEPARATE models testing the effects of temperature and the effects of precipitation.
#Re. nested random effects (site & plot, if I were to use plot as a RE):
  #Bates [http://lme4.r-forge.r-project.org/lMMwR/lrgprt.pdf] suggests I can treat these the same as non-nested, 
  #Ben Bolker says code as (1|larger/smaller) -
  #it does make a little bit of difference (see mod2 and mod 3, above, but I sorta think I'll trust Ben Bolker.
  #But hopefully it's irrelevant because I've decided not to use plot as a random effect

#TO TRY:
#-temperature difference from control, as opposed to plain ol' temperature
  #but is there an obvious control/treatment correspondence?
  #also, if I did this I would use only the treated plots in the modeling, not the control plots
  #previous iteration of something like this called the control the mean of all untreated plots (by site) - 
    #see code snippet at end of this script
  #Decided not to do this.
#-separate models testing the effects of temperature and the effects of precipitation treatment on soil moisture.
  #Sites that have both temp and moisture treatments: bace, chuine, force, jasper, sherry. And bace is usually disqualified.
  #Done, below
#-including interaction effects
  #Tried (failed), below

### (F) Modeling soil moisture differences across treatments --------------------------------------------

dfAll<-df[which(!is.na(df$soilmois1)),] #must have the dependent variable
dfbace<-dfAll[which(dfAll$site=="bace"),] #Bace modeling separately - use cantemp min/max, soiltemp1 AND soiltemp2, Pval as a factor
dfAll<-dfAll[which(dfAll$site!="bace"),] #because of the watts issue
dat<-dfAll
moddat<-dat[,which(colnames(dat) %in% c("site","year","doy","airtemp_min","airtemp_max",
                                        "soiltemp1_min","soiltemp1_max","season","soilmois1",
                                        "Pval"))] #nrow=150157

#Note: if do REML=FALSE, get the AIC, BIC. Otherwise REML criterion is the only fit statistic
  #For model comparison purposes, fit with ML (see Bates p.8, Faraway p.156)
  #Later, once chosen a model, refit with REML (?) - I want to check Zuur on this one.
#Note: These models are fit with different datasets because of missing information! 
  #Does this make them incomparable? Because the data seem to be missing at random with relation to
  #soilmois, I'm going to add a section at the end fitting all the models with the same dataset (must be the 
  #one that includes the lest missing data) for comparison purposes, choose a model structure based on that 
  #comparison, and then the final model will include all possible data given the chosen fixed effects.

##(I): air temp solo
d1<-moddat[which(!is.na(moddat$airtemp_max)),]
dim(d1) #111915
mod1<-lmer(soilmois1~airtemp_min+airtemp_max+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d1)  #random effects
summary(mod1)
plot(mod1) #actually looks halfway decent
#prmod1<-profile(mod1) #use REML=FALSE for model. Profiling (Bates p. 16) This is not really tenable for so much data, it takes forever. 
#plot(prmod1) #Plus, this has way too many har-to-interpret plots.

##(II): soil temp solo
d2<-moddat[which(!is.na(moddat$soiltemp1_max)),]
dim(d2) #127872
mod2<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+ #fixed effects
               as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
               (1|site), data=d2)  #random effects
summary(mod2)
plot(mod2) #this one isn't so good...

##(III): precip solo
d3<-moddat[which(!is.na(moddat$Pval)),]
dim(d3) #150157
mod3<-lmer(soilmois1~as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d3)  #random effects
summary(mod3)
plot(mod3) #Also crappy - striped, presumably because Pval is a factor here

##(IV): air temp & soil temp
d4<-moddat[which(!is.na(moddat$airtemp_max) & !is.na(moddat$soiltemp1_max)),]
dim(d4) #109347
mod4<-lmer(soilmois1~airtemp_min+ airtemp_max+soiltemp1_min+soiltemp1_max+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d4)  #random effects
summary(mod4)
plot(mod4)

##(V): air temp & precip
d5<-moddat[which(!is.na(moddat$airtemp_max) & !is.na(moddat$Pval)),]
dim(d5) #111915
mod5<-lmer(soilmois1~airtemp_min+ airtemp_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d5)  #random effects
summary(mod5)
plot(mod5)

##(VI): soil temp & precip
d6<-moddat[which(!is.na(moddat$soiltemp1_max) & !is.na(moddat$Pval)),]
dim(d6) #127872
mod6<-lmer(soilmois1~soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d6)  #random effects
summary(mod6)
plot(mod6)

##(VII): airtemp & soil temp & precip
d7<-moddat[which(!is.na(moddat$soiltemp1_max) & !is.na(moddat$airtemp_max) & !is.na(moddat$Pval)),]
dim(d7) #109349
mod7<-lmer(soilmois1~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7)  #random effects
summary(mod7)
plot(mod7)

##Interactions
  #Including Pval*airtemp and Pval*soiltemp
mod8<-lmer(soilmois1~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+
             airtemp_max*Pval + airtemp_min*Pval + soiltemp1_max*Pval + soiltemp1_min*Pval + #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7)  #random effects
  #Error: fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
  #Fiddle with interactions in mod8 to determine that:
    #Removing no single interaction fixes the deficiency
    #Removing both soiltemp or both airtemp interactions does not fix the deficiency
    #Removing one of each (soiltemp, airtemp) also doesn't fix it (?)
    #keeping only one interaction still doesn't fix it (?) - give up on interactions then??

##Models to compare, which are fit with the same (complete) dataset (=d7 from above), REML=FALSE

mod1.b<-lmer(soilmois1~airtemp_min+airtemp_max+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7,REML=FALSE)  #random effects

mod2.b<-lmer(soilmois1~soiltemp1_min+soiltemp1_max+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7,REML=FALSE)  #random effects

mod3.b<-lmer(soilmois1~as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7,REML=FALSE)  #random effects

mod4.b<-lmer(soilmois1~airtemp_min+ airtemp_max+soiltemp1_min+soiltemp1_max+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7,REML=FALSE)  #random effects

mod5.b<-lmer(soilmois1~airtemp_min+ airtemp_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7,REML=FALSE)  #random effects

mod6.b<-lmer(soilmois1~soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7,REML=FALSE)  #random effects

mod7.b<-lmer(soilmois1~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7,REML=FALSE)  #random effects

#Same as mod7.b, but with REML (because of comparison on Faraway p.164):
mod7.c<-lmer(soilmois1~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
               as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
               (1|site), data=d7)  

#NO RANDOM EFFECTS:
mod8.b<-lm(soilmois1~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year), #controlling for temporal variation
             data=d7) 

#What I have now:
  #-Models (mod1-7) that all have the same random effect of site and the same controls for temporal variation,
    #with different inclusion of temperature and precipitation fixed effects, with as much data as possible
    #fit with REML
  #-Models (mod1.b-7.b) that are like mod1-7 except that the all use the most complete dataframe (d7), 
    #fit with ML (so I can do the model comparisons)
  #-One null model with all fixed effect but without the site random effect, just to see... (mod8.a)
  #-None of these models have temp*precip interactions, because all of those interactions yield rank deficiency.

#Outstanding issues/things to do:
  #-Most of these models seem to have issues with a small group of residuals, from ellison - check this out.
  #-I'm using a dataframe with the incorrect chuine precipitation coding. 
    #It's just treating some of the chuine Pval factors as -NA instead of 70 (there's an issue on GitHub for this).
  #-Including any interaction with Pval yields rank deficiency. Why? Because Pval is a factor?
  #-Picking the best model
   #I want to keep the random effect and the temporal variation stuff regardless, just to be conservative
    #What effect does keeping extraneous stuff in the model (i.e. a random effect, if it's not needed) have?
  #-Should I refit with REML after model comparison? I think yes...
  #-Separate model for bace (who also uses canopy temperature)? - Ask Aileen whether she wants this.

### (G) Model Comparison & assessment--------------------------------------------------------------------

#First, look at whether the random effects are necessary (Faraway p.164):
  #Log-likelihood test:
2*(logLik(mod7.c)-logLik(mod8.b,REML=TRUE))
  #Parametric bootstrap: I think this is correct given Faraway, but I'm not exactly sure
lrstatf<-numeric(1000)
for(i in 1:100){
  rsoilmois<-unlist(simulate(mod8.b))
  mod8.br<-lm(rsoilmois~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
               as.factor(season)+doy+as.factor(year), #controlling for temporal variation
             data=d7)
  mod7.cr<-lmer(rsoilmois~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
                 as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
                 (1|site), data=d7) 
  lrstatf[i]<-2*(logLik(mod7.cr)-logLik(mod8.br,REML=TRUE))
  print(i)
} #This takes a really long time - but 100 isn't that many iterations
  #Computation of p-value
mean(lrstatf>2*(logLik(mod7.c)-logLik(mod8.b,REML=TRUE))) #0
  #Conclusion: random effect is super significant (?). Regardless, I would want to keep it anyway
    #because there are so many differences between the sites (where/when vars were measured, env conditions...)

#Next, given inclusion of the site RE, compare the fixed effects in the models (Faraway p.163):
  #Could use parametric bootstrap approach as above, but it takes forever with this much data
  #Instead, use the maximum-likelihood ratio method (models must be nested for this!)
anova(mod7.b,mod1.b) #larger model preferred
anova(mod7.b,mod2.b) #larger model preferred
anova(mod7.b,mod3.b) #larger model preferred
anova(mod7.b,mod4.b) #larger model preferred
anova(mod7.b,mod5.b) #larger model preferred
anova(mod7.b,mod6.b) #larger model preferred

#Double-checking that all the fixd effects in mod7.b are significant:
anova(mod7.b) #probably yes, given ridiculously high F values, though P values are not printed

#Diagnostic plots with the analog of the chosen model
  #(which, in the case of mod7.b = mod7, which should be basically the same because we used data d7,
  #except it will be fit with ML):
par(mfrow=c(1,2))
qqnorm(resid(mod7))
plot(fitted(mod7),resid(mod7),xlab="Fitted",ylab="Resid");abline(0,0)
  #these look pretty reasonable except for a few outliers...
  #Are the outliers those ellison points?
d7[which(resid(mod7)>.28),] #yes, they are.
  #Just to see: what if I get rid of those outliers?
d7_testomit<-d7[-which(resid(mod7)>.28),]
  mod7.d<-lmer(soilmois1~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
             as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
             (1|site), data=d7_testomit)  #random effects
qqnorm(resid(mod7.d))
plot(fitted(mod7.d),resid(mod7.d),xlab="fitted",ylab="Resid")
abline(0,0) #This is damn good. So what's with those ellison data? Are they reliable?

#Get complete information from the ellison outliers...
  #This is how I got the dataframe:
dfAll<-df[which(!is.na(df$soilmois1)),] #must have the dependent variable
dfAll<-dfAll[which(dfAll$site!="bace"),] #because of the watts issue
dat<-dfAll
moddat<-dat[,which(colnames(dat) %in% c("site","year","doy","airtemp_min","airtemp_max",
                                        "soiltemp1_min","soiltemp1_max","season","soilmois1",
                                        "Pval"))] #nrow=150157
d7<-moddat[which(!is.na(moddat$soiltemp1_max) & !is.na(moddat$airtemp_max) & !is.na(moddat$Pval)),]
  #So now, just use dat, not moddat, so I can get more of the covariates
    #(except I don't actually need to plot any of them, since I see below that this issue
    #is the soil moisture values):
d7_full<-dat[which(!is.na(dat$soiltemp1_max) & !is.na(dat$airtemp_max) & !is.na(dat$Pval)),]
outliers<-d7_full[which(resid(mod7)>.28),] #This is just 27 points out of 109349!
  #Are these weird points? Put plot values in context with full ellison data vales:
par(mfrow=c(2,3))
plot(d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="year.frac")],
     d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="airtemp_min")])
points(outliers$year.frac,outliers$airtemp_min,col="red")
plot(d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="year.frac")],
     d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="airtemp_max")])
points(outliers$year.frac,outliers$airtemp_max,col="red")
plot(d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="year.frac")],
     d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="soiltemp1_min")])
points(outliers$year.frac,outliers$soiltemp1_min,col="red")
plot(d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="year.frac")],
     d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="soiltemp1_max")])
points(outliers$year.frac,outliers$soiltemp1_max,col="red")
plot(d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="year.frac")],
     d7_full[which(d7_full$site=="ellison"),which(names(d7_full)=="soilmois1")])
points(outliers$year.frac,outliers$soilmois1,col="red")
#Not plotting Pval because ellison has no precip treatment
#So these points are very much outliers in soil moisture values.
  #Are those good measurements? This is a judegment call, but it seems reasonable to remove them.
  #What do the other sites look like in terms of soil moisture?
sites<-unique(dat$site) #Note that there are so few sites here because we stipulate
  #that we need both soil and air temp data.
par(mfrow=c(3,3))
for(i in 1:length(sites)){
plot(dat[which(dat$site==sites[i]),which(names(dat)=="year.frac")],
     dat[which(dat$site==sites[i]),which(names(dat)=="soilmois1")],
     main=sites[i])
  if(sites[i]=="ellison"){
    points(outliers$year.frac,outliers$soilmois1,col="red")
 }}
#The ellison outliers are the highest soil moisture measurement at any site, except force.
  #The force data don't screw up the model because there is no airtemp data, so it gets omitted.
#Decision: remove soil moisture outliers from ellison -- anything>5 SD from the mean seems reasonable:
par(mfrow=c(1,1))
el<-dat[which(dat$site=="ellison"),]
plot(el$year.frac,el$soilmois1)
abline(a=mean(el$soilmois1),b=0,col="red") #meean
abline(a=mean(el$soilmois1)+sd(el$soilmois1),b=0,col="blue") #+1 SD
abline(a=mean(el$soilmois1)-sd(el$soilmois1),b=0,col="blue") #-1 SD
abline(a=mean(el$soilmois1)+2*sd(el$soilmois1),b=0,col="green") #+2 SD
abline(a=mean(el$soilmois1)-2*sd(el$soilmois1),b=0,col="green") #-2 SD
abline(a=mean(el$soilmois1)+6*sd(el$soilmois1),b=0,col="purple") #+6 SD
abline(a=mean(el$soilmois1)-6*sd(el$soilmois1),b=0,col="purple") #-6 SD

#Refit the model without these outliers - I could even be more conservative, I think, but 
  #Apparenctly soil VWC at field capacity can be ~0.40 for clay soils.
el6SD<-mean(el$soilmois1)+6*sd(el$soilmois1)
d7_outomit<-d7[-(which(d7$site=="ellison" & d7$soilmois1>el6SD)),]
mod7_outomit<-lmer(soilmois1~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+ #fixed effects
               as.factor(season)+doy+as.factor(year)+ #controlling for temporal variation
               (1|site), data=d7_outomit)  #random effects
###^^^^^^^THIS IS MY FINAL MODEL RIGHT NOW.^^^^^^^^^^^^^^^

  #Graphical model assessment:
par(mfrow=c(1,2))
qqnorm(resid(mod7_outomit))
plot(fitted(mod7_outomit),resid(mod7_outomit),xlab="fitted",ylab="Resid")
abline(0,0,col="red")
  #Numerical summary (yay, coefficients make sense!):
summary(mod7_outomit)

#Finally, is there a way to tell how good this model is overall (rather than just in comparison to other models)? 
    #Fit on a portion of the data & predict the other portion many times and see how good predictions are:
mederr<-vector(length=100) #median error of each iteration
pcorr<-vector(length=100)  #pearson correlation between real and predicted for each iteration
for(k in 1:100){
  samp<-sample(x=nrow(d7_outomit),size=nrow(d7_outomit)*.3) #sample 30%
  test<-d7_outomit[samp,] #test set of data
  SMreal<-test$soilmois1 #real SM of the test set
  test<-test[,-which(names(d7_outomit)=="soilmois1")]#test set without the response variable
  train<-d7_outomit[-samp,] #training set of data
  fit<-lmer(soilmois1~airtemp_min+airtemp_max+soiltemp1_min+ soiltemp1_max+as.factor(Pval)+
              as.factor(season)+doy+as.factor(year)+ 
              (1|site), data=train)
  pred<-predict(fit,test) #predicted SM of the test set
  mederr[k]<-median(abs(pred-SMreal)) #median absolute error
  pcorr[k]<-cor(pred,SMreal,method="pearson")
    #correlation between real SM and predicted SM
  print(k)
}

par(mfrow=c(1,2))
plot(boxplot(mederr,main="Median Errors \nfor 100 simulations",ylab="median(abs(pred-SMreal)"))
plot(boxplot(pcorr,main="Pearson correlation between obs and pred \nfor 100 simulations",ylab="cor(pred,SMreal,method='pearson')"))

#Just a spot check to make sure that the relationship between observed & predicted SM is in fact linear:
plot(pred,SMreal) #it's linear -- pearson correlation is okay
  abline(0,1,col="red") #hmmm, not the best but not TOO too bad...
    #since the slope of the cloud is closer to 1 when we use all the data (see below), does this suggest overfitting?
  data.pred<-d7_outomit[,-which(names(d7_outomit)=="soilmois1")] #data on which model was trained, without repsonse variable
  full.pred<-predict(mod7_outomit,data.pred)
  plot(d7_outomit$soilmois1,full.pred)
    abline(0,1,col="red")

### (H) Plots for model visualization------------------------------------------------------------------------

#What effect does each predictor variable really have on soil moisture, according to the model?
#In this section, I compare plots of modeled soil moisture when there is "real" data vs. when one or more
  #of the variables is held at its mean or mean +/- 2 SD.
    
data.pred<-d7_outomit[,-which(names(d7_outomit)=="soilmois1")] #data on which model was trained, w/o repsonse variable
  #Given all predictor vars as is, here's what the model predicts:
full.pred<-predict(mod7_outomit,data.pred)

  #Functions for setting predictors to their mean or their mean +/- 2SD & repredicting:
meanpred<-function(data,model,variable){
  data[,which(names(data)==variable)]<-mean(data[,which(names(data)==variable)])
  pred<-predict(model,data)
  return(pred)}
sd2pred<-function(data,model,variable){
  data[,which(names(data)==variable)]<-mean(data[,which(names(data)==variable)])+
    2*(sd((data[,which(names(data)==variable)])))
  pred<-predict(model,data)
  return(pred)}
sd_2pred<-function(data,model,variable){
  data[,which(names(data)==variable)]<-mean(data[,which(names(data)==variable)])-
    2*(sd((data[,which(names(data)==variable)])))
  pred<-predict(model,data)
  return(pred)}

#Predicting:
ATmin_mean.pred<-meanpred(data.pred,mod7_outomit,"airtemp_min") #airtemp_min is at mean
ATmin_2sd.pred<-sd2pred(data.pred,mod7_outomit,"airtemp_min") #air temp_min is at mean+2SD
ATmin_neg2sd.pred<-sd_2pred(data.pred,mod7_outomit,"airtemp_min") #air temp_min is at mean-2SD

ATmax_mean.pred<-meanpred(data.pred,mod7_outomit,"airtemp_max")
ATmax_2sd.pred<-sd2pred(data.pred,mod7_outomit,"airtemp_max")
ATmax_neg2sd.pred<-sd_2pred(data.pred,mod7_outomit,"airtemp_max")

STmin_mean.pred<-meanpred(data.pred,mod7_outomit,"soiltemp1_min")
STmin_2sd.pred<-sd2pred(data.pred,mod7_outomit,"soiltemp1_min")
STmin_neg2sd.pred<-sd_2pred(data.pred,mod7_outomit,"soiltemp1_min")

STmax_mean.pred<-meanpred(data.pred,mod7_outomit,"soiltemp1_max")
STmax_2sd.pred<-sd2pred(data.pred,mod7_outomit,"soiltemp1_max")
STmax_neg2sd.pred<-sd_2pred(data.pred,mod7_outomit,"soiltemp1_max")

data.pred1<-data.pred
data.pred1$Pval<-0
Pval0.pred<-predict(mod7_outomit,data.pred1)
  
  #set both air temp vars to means:
data.pred1<-data.pred
data.pred1$airtemp_min<-mean(data.pred1$airtemp_min)
data.pred1$airtemp_max<-mean(data.pred1$airtemp_max)
  ATmean.pred<-predict(mod7_outomit,data.pred1)
  
  #set both soil temp vars to means:
data.pred1<-data.pred
data.pred1$soiltemp1_min<-mean(data.pred1$soiltemp1_min)
data.pred1$soiltemp1_max<-mean(data.pred1$soiltemp1_max)
  STmean.pred<-predict(mod7_outomit,data.pred1)

#Plot the difference between predictions:
  #zoom in on 1 year for plots because otherwise it's too difficult to interpret (too much data)
  #and/or: just plot every thousandth point
data.pred$year.frac <- data.pred$year + data.pred$doy/366 #put the year.frac column back in for plotting purposes
  
  #air temp
dfplot<-data.frame(stack(data.frame(ATmin_mean.pred,ATmin_2sd.pred,ATmin_neg2sd.pred,full.pred)),rep(data.pred$year.frac,4))
names(dfplot)<-c("soilmois","prediction","year.frac")
ggplot(dfplot[which(substr(dfplot$year.frac,1,4)==2011),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_point()+ggtitle("airtemp_min manipulations\n2011 only")
ggplot(dfplot[seq(1,nrow(dfplot),1000),],
        aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("airtemp_min manipulations\n1000th points")


dfplot<-data.frame(stack(data.frame(ATmax_mean.pred,ATmax_2sd.pred,ATmax_neg2sd.pred,full.pred)),rep(data.pred$year.frac,4))
names(dfplot)<-c("soilmois","prediction","year.frac")
ggplot(dfplot[which(substr(dfplot$year.frac,1,4)==2011),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("airtemp_max manipulations\n2011 only")
ggplot(dfplot[seq(1,nrow(dfplot),1000),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("airtemp_max manipulations\n1000th points")
 
  #soil temp
dfplot<-data.frame(stack(data.frame(STmin_mean.pred,STmin_2sd.pred,STmin_neg2sd.pred,full.pred)),rep(data.pred$year.frac,4))
names(dfplot)<-c("soilmois","prediction","year.frac")
ggplot(dfplot[which(substr(dfplot$year.frac,1,4)==2011),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("soiltemp1_min manipulations\n2011 only")
ggplot(dfplot[seq(1,nrow(dfplot),1000),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("soiltemp1_min manipulations\n1000th points")

dfplot<-data.frame(stack(data.frame(STmax_mean.pred,STmax_2sd.pred,STmax_neg2sd.pred,full.pred)),rep(data.pred$year.frac,4))
names(dfplot)<-c("soilmois","prediction","year.frac")
ggplot(dfplot[which(substr(dfplot$year.frac,1,4)==2011),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("soiltemp1_max manipulations\n 2011 only")
ggplot(dfplot[seq(1,nrow(dfplot),1000),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("soiltemp1_max manipulations\n1000th points")
  
  #Pval, air, soil
dfplot<-data.frame(stack(data.frame(Pval0.pred,ATmean.pred,STmean.pred,full.pred)),rep(data.pred$year.frac,4))
names(dfplot)<-c("soilmois","prediction","year.frac")
ggplot(dfplot[which(substr(dfplot$year.frac,1,4)==2011),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("Variables at mean\n2011 only")
ggplot(dfplot[seq(1,nrow(dfplot),1000),],
       aes(x=year.frac,y=soilmois,col=as.factor(prediction)))+geom_line()+ggtitle("Variables at mean\n1000th points")

### (I) Not used: A bit of previous code--------------------------------------------------------------------

#Figure out climate variable values for control/sham/outside plots for each site, year, doy 
vars.clim <- c("airtemp_min", "airtemp_max", 
               "soiltemp1_min", "soiltemp2_min", 
               "soiltemp1_max", "soiltemp2_max", 
               "soilmois1", "soilmois2")

expclim.control <- aggregate(expclim[expclim$temptreat3=="0",vars.clim], 
                             by=expclim[expclim$temptreat3=="0",c("site", "year", "doy")], 
                             FUN=mean, na.rm=T)
names(expclim.control)[which(names(expclim.control) %in% vars.clim)] <- paste0(vars.clim, ".control")

#Put the control data and the original expclim data together
expclim2 <- merge(expclim, expclim.control, all.x=T)

#Add columns for difference between control and warmed
expclim2$soiltempdiff_min<-expclim2$soiltemp1_min-expclim2$soiltemp1_min.control
expclim2$soiltempdiff_max<-expclim2$soiltemp1_max-expclim2$soiltemp1_max.control


