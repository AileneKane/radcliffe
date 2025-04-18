### Started 4 January 2015 ##
### By Ailene Ettinger ###
setwd("~/GitHub/radcliffe") # setwd("~/Documents/git/projects/meta_ep2/radcliffe")
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(reshape)

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

names.lu <- unique(read.csv("Analyses/cleanspp/taxonscrub.csv", colClasses="character"))
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
clean.rawobs<- list()

clean.rawobs$fitter <- function(filename, path) 
  {
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
    path="Data/Observations/Raw") {
    ## Harvard Forest ##
    ## Data type: Regular monitoring of multiple events ##
    ## I only entered one of the 4 possible files, in particular,
    ## there are also fall phenology data. ##
    file <- file.path(path, filename)
    harvard <- read.csv(file, header=TRUE)
    names(harvard)[names(harvard) == "julian"] <- "doy"
    #harvard <- harvard[!is.na(harvard$doy),]
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


clean.rawobs$hubbard <- function(filename="phn.txt", path="Data/Observations/Raw") {

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

    return(hubbardrb)
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



clean.rawobs$niwot <- function(filename="itexphen.mw.data.csv", path=raw.data.dir) {

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
    niwot6<-niwot6[-grep("<",niwot6$doy),]#for now, remove all sites with "<"
    niwot6<-niwot6[-which(niwot6$doy=="A"),]
    niwot6<-niwot6[-which(niwot6$doy=="NaN"),]
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
  concord2$date <- as.Date(paste(concord2$year, concord2$doy, sep="-"),format="%Y-%j")            
  concord2$varetc <- NA
  concord2$cult <- NA
  concord3 <- taxoscrub(concord2, "concord")
  concord3<-concord3[!is.na(concord3$genus),]
  concord3<-concord3[!is.na(concord3$doy),]
  concord3<-concord3[-which(concord3$doy=="236226"),]
  concord3<-concord3[-which(concord3$doy==""),]
  concord3<-concord3[-which(concord3$doy=="#REF!"),]
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
  mohonk$date <- as.Date(paste(mohonk$year, mohonk$doy, sep="-"),format="%Y-%j")            
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
  marsham$date <- as.Date(paste(marsham$year, marsham$doy, sep="-"),format="%Y-%j")            
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
  fargo$date <- as.Date(paste(fargo$year, fargo$doy, sep="-"),format="%Y-%j")            
  fargo$plot<-NA
  fargo$varetc <- NA
  fargo$cult <- NA
  fargo2 <- taxoscrub(fargo, "fargo")
  fargorb <- subset(fargo2, select=common.cols.raw)
  return(fargorb)
}

###Washdc data  (directly from NECTAR)
clean.rawobs$washdc <- function(filename="washdc.csv", path=raw.data.dir) {
  washdc <- read.csv(file.path(path, filename),header=TRUE)
  colnames(washdc)[2]<-"site"
  colnames(washdc)[3]<-"genus"
  colnames(washdc)[4]<-"species"
  colnames(washdc)[5]<-"event"
  colnames(washdc)[6]<-"year"
  washdc$doy<-washdc[,7]+1
  washdc$date <- as.Date(paste(washdc$year, washdc$doy, sep="-"),format="%Y-%j")            
  washdc$plot<-NA
  washdc$varetc <- NA
  washdc$cult <- NA
  washdc2 <- taxoscrub(washdc, "washdc")
  washdcrb <- subset(washdc2, select=common.cols.raw)
  return(washdcrb)
}
###Bolmgren data
clean.rawobs$bolmgren <- function(filename="gunnar_copy2ailene_151126.csv", path=raw.data.dir) {
  bolmgren <- read.csv(file.path(path, filename),header=TRUE)
  bolmgren$year<-seq(from=1934, to=2006, by=1)
  bolmgren2<-reshape(bolmgren,varying = list(names(bolmgren)[1:25]), direction = "long", v.names = c("doy"), times = names(bolmgren)[1:25])
  bolmgren2$site<-"bolmgren"
  gensp <- strsplit(as.character(bolmgren2$time),'_') 
  gensp<-do.call(rbind, gensp)
  bolmgren3<-cbind(bolmgren2,gensp[,1:2])
  colnames(bolmgren3)[6]<-"genus"
  bolmgren3$genus<-as.character(bolmgren3$genus)
  colnames(bolmgren3)[7]<-"species"
  bolmgren3$event<-"ffd"
  bolmgren3$date <- as.Date(paste(bolmgren3$year, bolmgren3$doy, sep="-"),format="%Y-%j")            
  bolmgren3$plot<-NA
  bolmgren3$varetc <- NA
  bolmgren3$cult <- NA
  bolmgren3<-bolmgren3[!is.na(bolmgren3$doy),]
  bolmgren3 <- taxoscrub(bolmgren3, "bolmgren")
  bolmgrenrb <- subset(bolmgren3, select=common.cols.raw)
  rownames(bolmgrenrb)<-NULL  
  return(bolmgrenrb)
}

###Gothic data: these data are not quite complete- Dr. Jane E. Ogilvie will send final datafile when it is complete
clean.rawobs$gothic <- function(filename="gothicphenology1973-2015_March2016.csv", path=raw.data.dir) {
  gothic <- read.csv(file.path(path, filename),header=TRUE)
  gothic$site<-"gothic"
  colnames(gothic)[3]<-"genus.species"
  gensp <- strsplit(as.character(gothic$genus.species),' ') 
  gensp<-do.call(rbind, gensp)
  gothic<-cbind(gothic,gensp[,1:2])
  colnames(gothic)[8]<-"genus"
  colnames(gothic)[9]<-"species"
  gothic$event<-"ffd"
  gothic$date <- as.Date(paste(gothic$year, gothic$doy, sep="-"),format="%Y-%j")            
  gothic$varetc <- NA
  gothic$cult <- NA
  gothic <- taxoscrub(gothic, "gothic")
  gothic<-gothic[!gothic$genus=="unknforb",]
  gothic<-gothic[!gothic$genus=="unkngrass",]
  gothic<-gothic[!is.na(gothic$doy),]
  gothicrb <- subset(gothic, select=common.cols.raw)
  return(gothicrb)
}


###UWM data from Mark Schwartz
clean.rawobs$uwm <- function(filename="UWMFS_HFP_Pheno_2000.csv", path=raw.data.dir) {
  files<-c("UWMFS_HFP_Pheno_2000.csv","UWMFS_HFP_Pheno_2001.csv","UWMFS_HFP_Pheno_2002.csv","UWMFS_HFP_Pheno_2003.csv","UWMFS_HFP_Pheno_2004.csv","UWMFS_HFP_Pheno_2005.csv","UWMFS_HFP_Pheno_2006.csv","UWMFS_HFP_Pheno_2007.csv","UWMFS_HFP_Pheno_2008.csv","UWMFS_HFP_Pheno_2009.csv","UWMFS_HFP_Pheno_2010.csv","UWMFS_HFP_Pheno_2011.csv","UWMFS_HFP_Pheno_2012.csv","UWMFS_HFP_Pheno_2013.csv","UWMFS_HFP_Pheno_2014.csv","UWMFS_HFP_Pheno_2015.csv")
  years<-seq(from=2000,to=2015, by=1)
  uwm<-NA
  for(i in 1:16){
    filename<-files[i]
    uwm1 <- read.csv(file.path(path, filename),header=TRUE)
    uwm1<-uwm1[,1:7]
    names(uwm1)<-c("Code","Latin.name","Common.name","N","bbm50doy","L75mdoy","L95mdoy")
    uwm1$year<-paste(years[i])
    uwm<-rbind(uwm,uwm1)
  }
  uwm<-uwm[uwm$Code!='',]
  uwm<-uwm[-1,]
  uwm$site<-"uwm"
  gensp <- strsplit(as.character(uwm$Latin.name),' ') 
  gensp<-do.call(rbind, gensp)
  uwm<-cbind(uwm,gensp[,1:2])
  colnames(uwm)[10]<-"genus"
  colnames(uwm)[11]<-"species"
  uwm2<-reshape(uwm,varying = list(names(uwm)[5:7]), direction = "long", v.names = c("doy"), times = names(uwm)[5:7])
  colnames(uwm2)[9]<-"event"
  uwm2$event[uwm2$event == "bbm50doy"] <- "bbd"
  uwm2$event[uwm2$event == "L95mdoy"] <- "lod"
  uwm2$event[uwm2$event == "L75mdoy"] <- "lud"
  uwm2$date <- as.Date(paste(uwm2$year, uwm2$doy, sep="-"),format="%Y-%j")            
  uwm2$varetc <- NA
  uwm2$cult <- NA
  uwm2$plot <- NA
  uwm2$site <-"uwm"
  uwm2<-uwm2[-which(uwm2$doy=="999"),]
  uwm2 <- taxoscrub(uwm2, "uwm")
  uwmrb <- subset(uwm2, select=common.cols.raw)
  rownames(uwmrb)<-NULL  
  return(uwmrb)
}


###Rousi data on Betula pendula; these data begin when the first date of budburst occurred. 
clean.rawobs$rousi <- function(filename="Rousi_BDbud_1997_2005.csv", path=raw.data.dir) {
  rousi <- read.csv(file.path(path, filename),skip=1,header=T)
  rousi$site<-"rousi"
  colnames(rousi)[5]<-"doy"
  rousi$event<-"bbd"
  rousi$date <- as.Date(paste(rousi$year, rousi$doy, sep="-"),format="%Y-%j")            
  rousi$plot<- NA
  rousi$varetc <- NA
  rousi$cult <- NA
  rousi$genus <- "Betula"
  rousi$species <- "pendula"
  rousi2<-rousi[rousi$total>0,]#remove rows for which indiviudals did not burst their buds
  rousi2 <- taxoscrub(rousi2, "rousi")
  rousirb <- subset(rousi2, select=common.cols.raw)
  return(rousirb)
}
###Rousi data on Betula pendula and B. pubescens flowering; these data begin when the first date of budburst occurred. 
clean.rawobs$rousifl <- function(filename="rousi_betula_flowering.csv", path=raw.data.dir) {
  rousifl <- read.csv(file.path(path, filename),skip=2,header=T)
  rousifl<-rousifl[-1,]
  rousifl$site<-"rousi"
  rousifl$genus <- "Betula"
  rousifl$species <- "pendula"
  colnames(rousifl)[4]<-"sp"
  rousifl[rousifl$sp==2,]$species <- "pubescens"
  colnames(rousifl)[5]<-"doy"
  rousifl<-rousifl[-which(rousifl$doy==""),]
  rousifl$event<-"ffd"#use just female flowers for now?
  rousifl$date <- as.Date(paste(rousifl$year, rousifl$doy, sep="-"),format="%Y-%j")            
  rousifl$plot<- NA
  rousifl$varetc <- NA
  rousifl$cult <- NA  
  rousifl <- taxoscrub(rousifl, "rousi")
  rousiflrb <- subset(rousifl, select=common.cols.raw)
  return(rousiflrb)
}

###Bock et al data on flowering of hundreds of species on the island of guernsey; these data begin when the first date of budburst occurred. 
clean.rawobs$bock <- function(filename="Guernsey_data_for_Ailene.csv", path=raw.data.dir) {
  bock <- read.csv(file.path(path, filename),header=T)
  bock$date <- as.Date(paste(bock$Month, bock$Day,bock$Year, sep="-"),format="%m-%d-%Y")            
  bock$doy <- format(bock$date, "%j")
  ffd<-aggregate(x=bock$doy, by=list(bock$Plant,bock$Year), FUN=min,na.rm=F)
  colnames(ffd)<-c("genus.sp","year","doy")
  ffd$site<-"bock"
  ffd$event<-"ffd"
  ffd$plot<- NA
  gensp <- strsplit(as.character(ffd$genus.sp),' ') 
  gensp<-do.call(rbind, gensp)
  bock<-cbind(ffd,gensp[,1:4])
  colnames(bock)[11]<-"genus"
  colnames(bock)[12]<-"species"
  
  bock[which(bock$Day==52),]$Day<-22#fix typo in day column
  bock<- taxoscrub(bock, "bock")
  bock<- subset(bock, select=common.cols.raw)
  return(bock)
}
# Produce cleaned raw data
#
# make list to store all the derived dataset cleaning functions
cleandata.rawobs <- list()
raw.data.dir <- "Data/Observations/Raw"
cleandata.rawobs$fitter <- clean.rawobs$fitter(file="Fitter_data.csv",path=raw.data.dir)
cleandata.rawobs$harvard <- clean.rawobs$harvard(path=raw.data.dir)
cleandata.rawobs$hubbard <- clean.rawobs$hubbard(path=raw.data.dir)
cleandata.rawobs$konza <- clean.rawobs$konza(path=raw.data.dir)
cleandata.rawobs$niwot <- clean.rawobs$niwot(path=raw.data.dir)
cleandata.rawobs$mikesell <- clean.rawobs$mikesell(path=raw.data.dir)
cleandata.rawobs$concord<-clean.rawobs$concord(path=raw.data.dir)
cleandata.rawobs$mohonk<-clean.rawobs$mohonk(path=raw.data.dir)
cleandata.rawobs$marsham<-clean.rawobs$marsham(path=raw.data.dir)
cleandata.rawobs$fargo<-clean.rawobs$fargo(path=raw.data.dir)
cleandata.rawobs$washdc<-clean.rawobs$washdc(path=raw.data.dir)
cleandata.rawobs$bolmgren<-clean.rawobs$bolmgren(path=raw.data.dir)
cleandata.rawobs$gothic<-clean.rawobs$gothic(path=raw.data.dir)
cleandata.rawobs$uwm<-clean.rawobs$uwm(path=raw.data.dir)
cleandata.rawobs$rousi<-clean.rawobs$rousi(path=raw.data.dir)
cleandata.rawobs$rousifl<-clean.rawobs$rousifl(path=raw.data.dir)
cleandata.rawobs$bock<-clean.rawobs$bock(path=raw.data.dir)

obsphendb <- do.call("rbind", cleandata.rawobs)
row.names(obsphendb) <- NULL
obsphendb<-obsphendb[!obsphendb$genus=="na",]#look at these- some rows from konza and washdc have doys...why na?
obsphendb<-obsphendb[!is.na(obsphendb$site),]#look at these-why NA?
dim(obsphendb)
#244682  rows
##check some other things...
unique(obsphendb$site)
sort(unique(obsphendb$year))
sort(unique(as.numeric(obsphendb$doy)))
sort(unique(obsphendb$genus))
sort(unique(obsphendb$species))
obsphendb[which(obsphendb$species=="sect."),]$species<-"sp."#bolmgren
obsphendb[which(obsphendb$species=="sect. Erythrosperma"),]$species<-"sp."#fitter
obsphendb[which(obsphendb$species=="na"),]$species<-"sp."#fargo
sort(unique(obsphendb$species))
unique(obsphendb$scrub)
unique(obsphendb$cult)
#variety and cult not used; remove
obsphendb2<-obsphendb[,-9]
obsphendb2<-obsphendb2[,-9]
obsphendb2<-obsphendb2[,-9]
write.csv(obsphendb2, "Analyses/obspheno.csv", row.names=FALSE, eol="\r\n")
head(obsphendb)
unique(obsphendb$site)#15 sites
unique(obsphendb$event)#6 events
sort(unique(obsphendb$genus))
sort(unique(paste(obsphendb$genus,obsphendb$species, sep=".")))
unique(obsphendb$doy)
#questions: what to do with things like "rush sp" ribes sp? keep or get rid of?
#questions: what to do with things like "<178" for doy- remove or use number without< (for now removed- just for niwot and chuine
specieslist<-sort(unique(paste(obsphendb$genus,obsphendb$species, sep=".")))
write.csv(specieslist,"obs_splist.csv")
