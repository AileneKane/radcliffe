### Started 4 January 2015 ##
### By Ailene Ettinger ###
setwd("~/GitHub/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(reshape)
library(zoo)
##Want to get several files:
#1. Phenology data: site, plot, event (phen event),year, doy,genus,species,Temp_treat, Precip_treat, onetempchange, precipchange, precipchangetyp
#2. Species names: site,genus,species,scrub,genus.prescrub,species.prescrub,ipni
#3. Study info: DatasetID  Year  medium	Temp_treat	onetempchange	Precip_treat	precipchange	precipchangetyp
#4. Temperature data
#5. Precipitation/moisture data

###Below code is from Lizzi'es NECTAR database 
# Species binomials etc. extractions #
sppexpr <- "^([^ ]*) *([^ ]*) *([^ ]*) *$"
sppexprcrazy <- "^([^ ]*) ?([^ ]*)? ?(var. |subsp. | )?(.*)?"

common.cols.derived <- c("site", "plot", "event", "doy", "family",
    "genus", "species", "scrub", "varetc", "cult")

common.cols.raw <- c("site", "plot", "event", "year", "doy", "date",
    "genus", "species", "scrub", "varetc", "cult")

common.cols.taxo <- c("genus", "species", "scrub", "genus.prescrub",
    "species.prescrub")

#
# Helper functions
#

subsetEarliest <- function(dat, by, datevar="date") {
    groups <- do.call("paste", dat[by])
    do.call("rbind", lapply(split(dat, groups),
        function(x) x[which.min(x[[datevar]]),]))
}

seasonalize <- function(date, start.mon) {
    if (!start.mon %in% 1:12) stop("start.mon must be 1-12, dummy")
    year.range <- range(as.numeric(format(date, "%Y")), na.rm=TRUE)
    years <- seq(year.range[1]-1, year.range[2])
    breaks <- c(as.Date(paste(years, start.mon, 1, sep="-")),
        as.Date(paste(year.range[2]+1, start.mon, 1, sep="-"))-1)
    cut(date, breaks=breaks, labels=paste(years, "+", sep=""))
}

space2na <- function(x) {
    if (!is.character(x)) {
        stop("x must be a character vector")
    }
    x[grepl("^[.[:space:]]*$", x)] <- NA
    x
}

stripwhite <- function(x) {
    if (!is.character(x)) {
        stop("x must be a character vector")
    }
    sub("^ *([^ ]*) *$", "\\1", x)
}

names.lu <- unique(read.csv("taxonscrub.csv", colClasses="character"))
names.lu[] <- lapply(names.lu, function(x) space2na(stripwhite(x)))
taxoscrub <- function(dat, sitename) {
    dat$scrub <- 0
    dat$genus.prescrub <- dat$genus
    dat$species.prescrub <- dat$species
    dat$genus <- space2na(stripwhite(dat$genus))
    dat$species <- space2na(stripwhite(dat$species))
    if (!sitename %in% names.lu[["site"]]) {
        message("Not scrubbing site '", sitename, "'.")
    } else {
        tax <- subset(names.lu, site==sitename)
        key <- paste(tax$genus, tax$species)
        ind <- match(paste(dat$genus, dat$species), key)
        dat[!is.na(ind),c("genus", "species", "scrub")] <-
            tax[na.omit(ind),c("scrub.genus", "scrub.species",
            "scrub_round")]
    }
    return(dat)
}

#
# Dataset processing functions
#

#########################
####### Raw data ########
#########################

# make list to store all the derived dataset cleaning functions
clean.rawobs <- list()


clean.rawobs$fitter <- function(filename, path) 
  {#file="Fitter_data.csv",path="Observations/Raw/"

    ## Fitter ##
    ## Data type: FFD ##
    ## Notes: Contact: Public data ##
    file <- file.path(path, filename)
    fitter1 <- read.csv(file, skip=1, check.names=FALSE, header=TRUE)
    names(fitter1)[names(fitter1) == ""] <- "genusspecies"
    fitter <- melt(fitter1, id.var=c("genusspecies"))
    names(fitter)[names(fitter) == "variable"] <- "year"
    fitter$year <- as.numeric(as.character(fitter$year))
    names(fitter)[names(fitter) == "value"] <- "notdoy"
    fitter$date <- as.Date(paste(fitter$year, fitter$notdoy, sep="-"),
         format="%Y-%d-%b")
    fitter$doy <- as.numeric(format(fitter$date, "%j"))
    fitter <- fitter[!is.na(fitter$doy),]
    #sppexpr <- "^([^ ]*) *([^ ]*) *([^ ]*) *$"
    #genus <- sub(sppexpr, "\\1", fitter$genusspecies)
    #species <- sub(sppexpr, "\\2", fitter$genusspecies)
    #varetc <- sub(sppexpr, "\\4", fitter$genusspecies)
    sppexpr <- "^([^ ]*) ?((x|sect.)? ?[^ ]*)? ?(var. |subsp. | )?(.*)?"
    fitter$genus <- sub(sppexpr, "\\1", fitter$genusspecies, perl=TRUE)
    fitter$species <- sub(sppexpr, "\\2", fitter$genusspecies, perl=TRUE)
    fitter$varetc <- sub(sppexpr, "\\4", fitter$genusspecies, perl=TRUE)
    fitter <- taxoscrub(fitter, "fitter")
    # Add columns to match for rbind #
    fitter$site <- "fitter"
    fitter$plot <- NA
    fitter$cult <- NA
    fitter <- subsetEarliest(fitter, c("plot", "genus", "species",
        "year"))

    fitter$event <- "ffd"

    fitterrb <- subset(fitter, select=common.cols.raw)
    row.names(fitterrb) <- NULL
    return(fitterrb)
}


clean.rawobs$harvard <- function(filename="hf003-03-spring.csv",
    path="/Users/aileneettinger/GitHub/radcliffe/Observations/Raw") {
    ## Harvard Forest ##
    ## Data type: Regular monitoring of multiple events ##
    ## I only entered one of the 4 possible files, in particular,
    ## there are also fall phenology data. ##
    file <- file.path(path, filename)
    harvard <- read.csv(file, header=TRUE)
    names(harvard)[names(harvard) == "julian"] <- "doy"
    harvard <- harvard[!is.na(harvard$doy),]
    splist <- read.csv(file.path(path, "specieslist_bylizzie.csv"),
        header=TRUE)
    harvard$spcode <- sub("-.*", "", harvard$tree.id)
    harvard <- merge(harvard, splist, by.x="spcode", by.y="code",
        all.x=TRUE)
    harvard$genus <- sub(sppexpr, "\\1", harvard$genusspecies)
    harvard$species <- sub(sppexpr, "\\2", harvard$genusspecies)
    harvard <- taxoscrub(harvard, "harvard")
    names(harvard)[names(harvard) == "date"] <- "date2"
    harvard$date <- as.Date(harvard$date2, "%m/%d/%Y")
    harvard$year <- format(harvard$date, "%Y")

    # bbd = for each species*yr, the first time that BBRK > 0
    bbd <- subset(harvard, bbrk > 0)
    bbd <- subsetEarliest(bbd, c("genus", "species", "year"))
    bbd <- cbind(bbd, event="bbd")

    # ffd = for each species*yr, the first time that FOPN > 0
    ffd <- subset(harvard, fopn > 0)
    ffd <- subsetEarliest(ffd, c("genus", "species", "year"))
    ffd <- cbind(ffd, event="ffd")

    harvardrb <- rbind(bbd, ffd)

    # Add and adjust columns to match for rbind #
    harvardrb$site <- "harvard"
    harvardrb$plot <- NA
    harvardrb$cult <- NA
    harvardrb$varetc <- NA

    harvardrb <- subset(harvardrb, select=common.cols.raw)
    row.names(harvardrb) <- NULL

    return(harvardrb)
}


clean.rawobs$hubbard <- function(filename="phn.txt", path="/Users/aileneettinger/GitHub/radcliffe/Observations/Raw") {

    ## Hubbard Brook ##
    ## Data type: Regular monitoring of multiple events I think ##
    ## Missing data is coded as -9.0 ##
    ## Re-label numerical events to char descrips and then flip, also get species names from common ##
    file <- file.path(path, filename)
    hubbard1 <- read.csv(file, check.names=FALSE, header=TRUE,
      na.strings="-9.0")
    hubbard1 <- subset(hubbard1, SEASON=="SPRING")
    indivs <- c("1B", "6T", "4B", "4T", "5B", "5T", "7B", "7T")
    hubbard1$budburst <- rowSums(hubbard1[indivs]>=2)>0
    hubbard1$leafout <- rowSums(hubbard1[indivs]>=4)>0
    
    hubbard1$genus <- NA_character_
    hubbard1$genus[hubbard1$SPECIES=="American_Beech"] <- "Fagus"
    hubbard1$genus[hubbard1$SPECIES=="Sugar_Maple"] <- "Acer"
    hubbard1$genus[hubbard1$SPECIES=="Yellow_Birch"] <- "Betula"
    hubbard1$species <- NA_character_
    hubbard1$species[hubbard1$SPECIES=="American_Beech"] <- "grandifolia"
    hubbard1$species[hubbard1$SPECIES=="Sugar_Maple"] <- "saccharum"
    hubbard1$species[hubbard1$SPECIES=="Yellow_Birch"] <- "alleghaniensis"
    hubbard1 <- taxoscrub(hubbard1, "hubbard")
    hubbard1$Date <- as.Date(hubbard1$Date)
    names(hubbard1)[names(hubbard1) == "Date"] <- "date"
    hubbard1$year <- format(hubbard1$date, "%Y")
    
    hubbard1$doy <- as.numeric(format(hubbard1$date, "%j"))
    hubbard1 <- hubbard1[!is.na(hubbard1$doy),]

    # bbd = for each species*yr, the first time its buds have burst
    hubbardbbd <- subset(hubbard1, budburst)
    hubbardbbd  <- subsetEarliest(hubbard1, c("genus", "species", "year"),
        datevar="doy")
    hubbardbbd1 <- cbind(hubbardbbd, event="bbd")
    
    # lod = for each species*yr, the first time its buds have burst
    hubbardlod <- subset(hubbard1, leafout)
    hubbardlod <- subsetEarliest(hubbard1, c("genus", "species", "year"),
                              datevar="doy")
    hubbardlod1 <- cbind(hubbardlod, event="lod")
    
    hubbard<-rbind(hubbardbbd1,hubbardlod1)
    hubbard$site <- "hubbard"
    hubbard$plot <- NA
    hubbard$varetc <- NA
    hubbard$cult <- NA
    hubbardrb <- subset(hubbard, select=common.cols.raw)
    row.names(hubbardrb) <- NULL

    return(hubbard)
}

clean.rawobs$konza <- function(filename="Konza_PlantPhenology.csv", path=".") {
  
    ## Konza ##
    ## Data type: FFD ##
    file <- file.path(path, filename)
    konza1 <- read.csv(file, skip=1, check.names=FALSE, header=TRUE)
    names(konza1)[1] <- "common name"
    names(konza1)[2] <- "genusspecies"
    names(konza1)[3] <- "observer_2015"
    names(konza1)[4] <- "plot"
    names(konza1)[20] <- "comments"
    names(konza1)[21] <- "planttype"
    konza2 <- subset(konza1, select=c("common name","genusspecies","2015","2014","2013","2012","2011","2010","2009",
        "2008",  "2007","2006","2005" , "2004","2003" , "2002","2001"))
    konza <- melt(konza2, id.var=c("common name", "genusspecies"))
    names(konza)[names(konza) == "variable"] <- "year"
    konza$year <- as.numeric(as.character(konza$year))
    names(konza)[names(konza) == "value"] <- "Date"

    sppexpr <- "^([^ ]*) ?([^ ]*)? ?(var. |subsp. | )?(.*)?"
    konza$genus <-sub(sppexpr, "\\1", konza$genusspecies)
    konza$species <- sub(sppexpr, "\\2", konza$genusspecies)
    konza <- taxoscrub(konza, "konza")
    konza$Date <- as.character(konza$Date)
    konza$Date[konza$Date=="8/14 04 "] <- "8/14/2004"
    konza$Date[konza$Date=="05 21 07 "] <- "5/21/2007"
    konza$Date[konza$Date=="05 24 07 "] <- "5/24/2007"
    konza$Date[konza$Date=="06 17 06 "] <- "6/17/2006"
    konza$Date[konza$Date=="09//07 "|konza$Date=="  "] <- NA
    konza$date <- as.Date(konza$Date, "%m/%d/%Y")
    konza$doy <- as.numeric(format(konza$date, "%j"))
    konza <- konza[!is.na(konza$doy),]
    # Add columns to match for rbind #
    konza$site <- "konza"
    konza$plot <- NA
    konza$varetc <- NA
    konza$cult <- NA
    konza$event <- "ffd"

    konzarb <- subset(konza, select=common.cols.raw)

    return(konzarb)
}



clean.rawobs$niwot <- function(filename="itexphen.mw.data.csv", path=".") {

    ## Niwot ##
    ## Data type: Regular monitoring of multiple events ##
    ## Empty cells mean lack of event ##
    ## ADD column names ##
    file <- file.path(path, filename)
    niwot <- read.csv(file,header=TRUE)
    colnames(niwot)[2]<-"plot"
    niwot$species<-NA
    niwot$genus<-NA
    niwot[grep("AR",niwot$plant_id),]$genus<-"Acomastylis"
    niwot[grep("AR",niwot$plant_id),]$species<-"rossii"
    niwot[grep("BB",niwot$plant_id),]$genus<-"Bistorta"
    niwot[grep("BB",niwot$plant_id),]$species<-"bistortoides"
    niwot1<-niwot[grep("EP",niwot$plot),]
    niwot2<-niwot[substr(niwot$plot,4,4)=="C",]
    niwot3<-rbind(niwot1,niwot2)
    niwot4<-subset(niwot3, select=c("year","plot","genus","species","X1st_leaf","X1st_flower","X1st_flower_open","X1st_petal_shed","last_petal_shed","X1st_seed_disp","X1st_red_leaf"))
    niwot5<-reshape(niwot4,varying = list(names(niwot4)[5:11]), direction = "long", v.names = c("doy"), times = names(niwot4)[5:11])
    niwot5$event<-NA
    niwot5[niwot5$time=="X1st_leaf",]$event<-"bbd"
    niwot5[niwot5$time=="X1st_flower_open",]$event<-"ffd"
    niwot5[niwot5$time=="X1st_seed_disp",]$event<-"sd"
    niwot6<-niwot5[which(niwot5$event=="bbd"|niwot5$event=="ffd"|niwot5$event=="sd"),]
    niwot6$date <- as.Date(paste(niwot6$year, niwot6$doy), format="%Y %j")
    niwot6$site <- "niwot"
    niwot6$varetc <- NA
    niwot6$cult <- NA
    niwot <- taxoscrub(niwot6, "niwot")
    niwotrb <- subset( niwot, select=common.cols.raw)            
    rownames(niwotrb)<-NULL  
    return(niwotrb)
}

clean.rawobs$mikesell <- function(filename="Mikesell_pheno_1883_1912.csv",
    spkey="Mikesell_species_key.csv", path=".") {
 
    # Mikesell #
    # NA is 999 #
    mikey <- read.csv(file.path(path, filename), header=TRUE,
        na.strings="999")
    mikeysp <- read.csv(file.path(path, spkey), header=TRUE)
    mikesell <- merge(mikey, mikeysp, by.x="sp._code", by.y="Sp..Code")
    mikesell$genus <- sub(sppexpr, "\\1", mikesell$Plant.Name)
    mikesell$species <- sub(sppexpr, "\\2", mikesell$Plant.Name)
    mikesell <- taxoscrub(mikesell, "mikesell")
    mikesell$varetc <- sub(sppexpr, "\\3", mikesell$Plant.Name)
    # remove columns that we don't need (and that we don't want to
    # participate in the melt)
    mikesell$sp._code <- NULL
    mikesell$Plant.Name <- NULL
    mikesell$genus.prescrub <- NULL
    mikesell$species.prescrub <- NULL
    mikemelt <- melt(mikesell, id.var=c("year", "genus", "species",
        "scrub", "varetc"))
    lookupmike <- c("bud_burst"="bbd", "first_leaf"="lud",
        "full_leaf "="lod")
    mikemelt$event <- unname(lookupmike[mikemelt$variable])
    mikemelt$doy <- mikemelt$value
    mikemelt <- mikemelt[!is.na(mikemelt$doy),]
    mikemelt$site <- "mikesell"
    mikemelt$date <- as.Date(paste(mikemelt$year, mikemelt$doy), format="%Y %j")
    mikemelt$plot <- NA
    mikemelt$cult <- NA
    mikerb <- subset(mikemelt, select=common.cols.raw)
    return(mikerb)
}

##Concord data from Richard Primack
clean.rawobs$concord <- function(filename="Miller-Rushing_Primack_Concord_phenology_data_complete.csv", path=".") {
  concord <- read.csv(file.path(path, filename), skip=35,header=TRUE)
  colnames(concord)[1]<-"genus"
  colnames(concord)[2]<-"species"
  concord2<-reshape(concord,varying = list(names(concord)[16:46]), direction = "long", v.names = c("doy"), times = names(concord)[16:46])
  concord2$site<-"concord"
  concord2$plot<-NA
  concord2$event<-"ffd"
  concord2$year <- gsub("X","", concord2$time) 
  concord2$date <- as.Date(paste(concord2$year, concord2$doy, sep="-"),format="%Y-%d-%b")            
  concord2$varetc <- NA
  concord2$cult <- NA
  concord3 <- taxoscrub(concord2, "concord")
  concordrb <- subset(concord3, select=common.cols.raw)
  return(concordrb)
}
###Mohonk data from Ben Cook (directly from NECTAR)
clean.rawobs$mohonk <- function(filename="mohonk.csv", path=".") {
  mohonk <- read.csv(file.path(path, filename),header=TRUE)
  colnames(mohonk)[2]<-"site"
  colnames(mohonk)[3]<-"genus"
  colnames(mohonk)[4]<-"species"
  colnames(mohonk)[5]<-"event"
  colnames(mohonk)[6]<-"year"
  colnames(mohonk)[7]<-"doy"
  mohonk$date <- as.Date(paste(mohonk$year, mohonk$doy, sep="-"),format="%Y-%d-%b")            
  mohonk$plot<-NA
  mohonk$varetc <- NA
  mohonk$cult <- NA
  mohonk2 <- taxoscrub(mohonk, "mohonk")
  mohonkdrb <- subset(mohonk2, select=common.cols.raw)
  return(mohonkdrb)
}

###Marsham data  (directly from NECTAR)
clean.rawobs$marsham <- function(filename="marsham.csv", path=raw.data.dir) {
  marsham <- read.csv(file.path(path, filename),header=TRUE)
  colnames(marsham)[2]<-"site"
  colnames(marsham)[3]<-"genus"
  colnames(marsham)[4]<-"species"
  colnames(marsham)[5]<-"event"
  colnames(marsham)[6]<-"year"
  colnames(marsham)[7]<-"doy"
  marsham$date <- as.Date(paste(marsham$year, marsham$doy, sep="-"),format="%Y-%d-%b")            
  marsham$plot<-NA
  marsham$varetc <- NA
  marsham$cult <- NA
  marsham2 <- taxoscrub(marsham, "mohonk")
  marshamdrb <- subset(marsham2, select=common.cols.raw)
  return(marshamdrb)
}
#
###Fargo data  (directly from NECTAR)
clean.rawobs$fargo <- function(filename="fargo.csv", path=raw.data.dir) {
  fargo <- read.csv(file.path(path, filename),header=TRUE)
  colnames(fargo)[2]<-"site"
  colnames(fargo)[3]<-"genus"
  colnames(fargo)[4]<-"species"
  colnames(fargo)[5]<-"event"
  colnames(fargo)[6]<-"year"
  colnames(fargo)[7]<-"doy"
  fargo$date <- as.Date(paste(fargo$year, fargo$doy, sep="-"),format="%Y-%d-%b")            
  fargo$plot<-NA
  fargo$varetc <- NA
  fargo$cult <- NA
  fargo2 <- taxoscrub(fargo, "fargo")
  fargorb <- subset(fargo2, select=common.cols.raw)
  return(marshamdrb)
}


# Produce cleaned raw data
#

raw.data.dir <- "Observations/Raw"
cleandata.rawobs$fitter <- clean.rawobs$fitter(file="Fitter_data.csv",path="Observations/Raw/")
cleandata.rawobs$harvard <- clean.rawobs$harvard(path=raw.data.dir)
cleandata.rawobs$hubbard <- clean.rawobs$hubbard(path=raw.data.dir)
cleandata.rawobs$konza <- clean.rawobs$konza(path=raw.data.dir)
cleandata.rawobs$niwot <- clean.rawobs$niwot(path=raw.data.dir)
cleandata.rawobs$mikesell <- clean.rawobs$mikesell(path=raw.data.dir)
cleandata.rawobs$concord<-clean.rawobs$concord(path=raw.data.dir)
cleandata.rawobs$mohonk<-clean.rawobs$mohonk(path=raw.data.dir)
cleandata.rawobs$marsham<-clean.rawobs$marsham(path=raw.data.dir)

obsphendb <- do.call("rbind", cleandata.rawobs)
row.names(obsphendb) <- NULL
write.csv(obsphendb, "radmeeting/obspheno.csv", row.names=FALSE)

stop()



#
# Taxonomy
#

taxonomy <- list()

raw.data.dir <- "Observations/Raw"

taxonomy$fitter <- clean.raw$fitter(path=raw.data.dir, names.only=TRUE)
taxonomy$harvard <- clean.raw$harvard(path=raw.data.dir, names.only=TRUE)
taxonomy$hubbard <- clean.raw$hubbard(path=raw.data.dir, names.only=TRUE)
taxonomy$luquillo <- clean.raw$luquillo(path=raw.data.dir, names.only=TRUE)
taxonomy$konza <- clean.raw$konza(path=raw.data.dir, names.only=TRUE)
taxonomy$niwot <- clean.raw$niwot(path=raw.data.dir, names.only=TRUE)
taxonomy$mikesell <- clean.raw$mikesell(path=raw.data.dir, names.only=TRUE)

pnames <- unique(do.call("rbind", lapply(names(taxonomy),
                                         function(site) data.frame(site=site, taxonomy[[site]]))))
pheno.species <- tolower(paste(pnames$genus, pnames$species))

ipni <- read.csv("TaxonScrubber/TS_AllTaxa.csv")
ipni.species <- tolower(paste(ipni$Genus, ipni$Species))

ipni.na <- ipni[is.na(ipni$accepted) | ipni$accepted==0, ]
ipni.a <- ipni[!is.na(ipni$accepted) & ipni$accepted==1, ]

pnames.inIPNI <- pnames[pheno.species %in% ipni.species,]
pheno.species.inIPNI <- tolower(paste(pnames.inIPNI$genus,
                                      pnames.inIPNI$species))
pnames.a <- pnames.inIPNI[pheno.species.inIPNI %in%
                            tolower(paste(ipni.a$Genus, ipni.a$Species)), ]
pnames.na <- pnames.inIPNI[!pheno.species.inIPNI %in%
                             tolower(paste(ipni.a$Genus, ipni.a$Species)), ]
pnames.noIPNI <- pnames[!pheno.species %in% ipni.species,]

all.pheno.species <- rbind(data.frame(pnames.a, ipni="accepted"),
                           data.frame(pnames.na, ipni="not accepted"),
                           data.frame(pnames.noIPNI, ipni="unmatched"))

stop("Stopping here...")
write.csv(pnames.a, "knb/nectarnames_IPNIaccepted.csv", row.names=FALSE, quote=FALSE)
write.csv(pnames.na, "knb/nectarnames_IPNInotaccepted.csv", row.names=FALSE, quote=FALSE)
write.csv(pnames.noIPNI, "knb/nectarnames_IPNIunmatched.csv", row.names=FALSE, quote=FALSE)
write.csv(all.pheno.species, "knb/nectarnames_all.csv", row.names=FALSE, quote=FALSE)

