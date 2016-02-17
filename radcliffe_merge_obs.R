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


clean.rawobs$fitter <- function(filename="Fitter_data.csv", path="Observations/Raw/",
    names.only=FALSE) {

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
    if (names.only) return(fitter[common.cols.taxo])
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
    path="/Users/aileneettinger/GitHub/radcliffe/Observations/Raw", names.only=FALSE) {
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
    if (names.only) return(harvard[common.cols.taxo])
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


clean.rawobs$hubbard <- function(filename="phn.txt", path="/Users/aileneettinger/GitHub/radcliffe/Observations/Raw",
    names.only=FALSE) {

    ## Hubbard Brook ##
    ## Data type: Regular monitoring of multiple events I think ##
    ## Missing data is coded as -9.0 ##
    ## Re-label numerical events to char descrips and then flip, also get species names from common ##
    file <- file.path(path, filename)
    hubbard1 <- read.csv(file, check.names=FALSE, header=TRUE,
      na.strings="-9.0")
    hubbard1 <- subset(hubbard1, SEASON=="SPRING")
    indivs <- c("1B", "6T", "4B", "4T", "5B", "5T", "7B", "7T")
    hubbard1$isinleaf <- rowSums(hubbard1[indivs]>=2)>0
    hubbard1$genus <- NA_character_
    hubbard1$genus[hubbard1$SPECIES=="American_Beech"] <- "Fagus"
    hubbard1$genus[hubbard1$SPECIES=="Sugar_Maple"] <- "Acer"
    hubbard1$genus[hubbard1$SPECIES=="Yellow_Birch"] <- "Betula"
    hubbard1$species <- NA_character_
    hubbard1$species[hubbard1$SPECIES=="American_Beech"] <- "grandifolia"
    hubbard1$species[hubbard1$SPECIES=="Sugar_Maple"] <- "saccharum"
    hubbard1$species[hubbard1$SPECIES=="Yellow_Birch"] <- "alleghaniensis"
    hubbard1 <- taxoscrub(hubbard1, "hubbard")
    if (names.only) return(hubbard1[common.cols.taxo])

    hubbard1$Date <- as.Date(hubbard1$Date)
    names(hubbard1)[names(hubbard1) == "Date"] <- "date"
    hubbard1$year <- format(hubbard1$date, "%Y")
    
    hubbard1$doy <- as.numeric(format(hubbard1$date, "%j"))
    hubbard1 <- hubbard1[!is.na(hubbard1$doy),]

    # ffd = for each species*yr, the first time it's in leaf
    hubbard <- subset(hubbard1, isinleaf)
    hubbard <- subsetEarliest(hubbard, c("genus", "species", "year"),
        datevar="doy")
    hubbard <- cbind(hubbard, event="fld")

    hubbard$site <- "hubbard"
    hubbard$plot <- NA
    hubbard$varetc <- NA
    hubbard$cult <- NA
    hubbard <- subset(hubbard, select=common.cols.raw)
    row.names(hubbard) <- NULL

    return(hubbard)
}

clean.raw$konza <- function(filename="Konza_PlantPhenology.csv", path=".",
    names.only=FALSE) {
  
    ## Konza ##
    ## Data type: FFD ##
    ## I tossed out a lot of stuff while fixing this up, observer, site etc. ##
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
    if (names.only) return(konza[common.cols.taxo])
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

    konza <- subset(konza, select=common.cols.raw)

    return(konza)
}


## Luquillo ##
## Data type: Basket data ##
## Change code to character field (phenological event) ##
## Empty baskets are NA (double-check in pdf) if no species otherwise (w/species) -- WTF? ##
clean.raw$luquillo <- function(path=".", names.only=FALSE) {
    luqbisley <- read.csv(file.path(path, "Lfdp1-BisleyPhenology.txt"),
        header=TRUE)
    luqbisley$plot <- "bisley"
    luqverde1 <- read.csv(file.path(path, "Lfdp1-ElVerdePhenology.txt"),
        header=TRUE)
    luqverde1$plot <- "verde1"
    luqverde2 <- read.csv(file.path(path, "Lfdp2-ElVerdePhenology.txt"),
        header=TRUE)
    luqverde2$plot <- "verde2"
    # Species list #
    luqspecies1 <- read.delim(file.path(path, "Luquillo_species_JD.txt"),
        header=TRUE, na.strings="na")
    luqspecies1$spcode <- luqspecies1$SPECIES
    luqspecies <- subset(luqspecies1, select=c("spcode","SPECIES", "Genus",
        "Species"))
    names(luqspecies)[names(luqspecies) == "Genus"] <- "genus"
    names(luqspecies)[names(luqspecies) == "Species"] <- "species"
    luqspecies$SPECIES <- luqspecies$spcode
    luqspecies <- aggregate(luqspecies["spcode"],
        luqspecies[c("genus", "species", "SPECIES")], FUN=length)
    luq1 <- rbind(luqbisley, luqverde1, luqverde2)
    luq <- merge(luq1, luqspecies, by="SPECIES", all.x=TRUE)
    luq <- taxoscrub(luq, "luquillo")
    if (names.only) return(luq[common.cols.taxo])
#    lkupluq <- c("1"="flower"," 2 "="aborted fruit", "3"="immature fruit",
#        "4"="mature fruit","5"="seed","6"="pedicel of fruit",
#        ""="empty basket") # not using by good to remember
    luq$date <- as.Date(luq$DATE, format="%m/%d/%y")
    luq$doy <- format(luq$date, "%j")
    luq <- luq[!is.na(luq$doy),]
    luq$doy <- as.numeric(luq$doy)
    luq$year <- format(luq$date, "%Y")
    luq$site <- "luquillo"
    luq$varetc <- NA
    luq$cult <- NA

    # ffd = for each species*yr*plot, the first time that CODE==1
    ffd <- subset(luq, CODE==1)
    ##jr: changing this to use plot, not site
    ffd <- subsetEarliest(ffd, c("genus", "species", "year", "plot"))
    ffd <- cbind(ffd, event="ffdbasket")
    ffd$scrub <- NA
    luqrb <- subset(ffd, select=common.cols.raw)
    row.names(luqrb) <- NULL

    return(luqrb)

}


clean.raw$niwot <- function(filename="Walker_IndPhenology.txt", path=".",
    names.only=FALSE) {

    ## Niwot ##
    ## Data type: Regular monitoring of multiple events ##
    ## Empty cells mean lack of event ##
    ## ADD column names ##
    file <- file.path(path, filename)
    niwot <- read.csv(file, skip=199, nrow=17051,
        header=FALSE, col.names=c("year", "spcode", "plot", "quadrat",
        "coordinates", "plantID", "doy", "plantheight", "longestleaflength",
        "numleaves", "numinflor", "numbuds", "numflowers", "numdevfruits",
        "numaturefruits"), colClasses=c(rep(NA, 15)) )
    niwot <- niwot[!is.na(niwot$doy),]
    splistniwot <- data.frame(cbind(spcode=c("ACOROS", "BISBIS", "BISVIV"),
        genus=c("Acomastylis", "Bistorta", "Bistorta"), species=c("rossii",
        "bistortoides", "vivipara")))
    niwot <- merge(niwot, splistniwot, by="spcode", all.x=TRUE)
    niwot$date <- as.Date(paste(niwot$year, niwot$doy), format="%Y %j")
    niwot <- taxoscrub(niwot, "niwot")
    if (names.only) return(niwot[common.cols.taxo])
    niwot$site <- "niwot"
    niwot$varetc <- NA
    niwot$cult <- NA
    niwot$year <- format(niwot$date, "%Y")

    niwot$isflowering <- !is.na(niwot$numflowers) & niwot$numflowers>0
    niwot$isinleaf <- !is.na(niwot$numleaves) & niwot$numleaves>0

    # ffd = for each species*yr*plot, the first time flowering
    ffd <- subset(niwot, isflowering)
    ffd <- subsetEarliest(ffd, c("genus", "species", "year", "plot"))
    ffd <- cbind(ffd, event="ffd")

    # fld = for each species*yr*plot, the first time in leaf
    fld <- subset(niwot, isinleaf)
    fld <- subsetEarliest(fld, c("genus", "species", "year", "plot"))
    fld <- cbind(fld, event="fld")

    niwotrb <- rbind(ffd, fld)
    niwotrb <- subset(niwotrb, select=common.cols.raw)
    row.names(niwotrb) <- NULL 

    return(niwotrb)
}

clean.raw$sevcore <- function(splistfile="sev048_1991_meta.dbf",
    datafile="sev137_data.dbf", path=".", names.only=FALSE) {

    ## Sevilleta ##
    ## Data type: Regular monitoring of multiple events ##

    ## Species codes ##
    sevsplist <- read.table(file.path(path, splistfile), skip=315,
        nrow=821, sep=" ", header=FALSE,  col.names=c("spcode", "genus",
        "species", "variety"), comment.char="\\")
    ## Core site phenology ##
    ## Missing data is -999 ##
    sev <- read.csv(file.path(path, datafile), skip=8, nrow=291435,
        comment.char="\\", header=FALSE, col.names=c("moyr","Date",
        "observer", "site", "rodentweb", "spcode","obsnum", "foliage",
        "reprocond", "blah", "blah"), colClasses=c(rep(NA, 9),
        rep("NULL", 2)))
    lookupsevf <- c("N"="new green foliage", "O"="old green foliage only",
        "B"="brown leaves only", "Z"="no leaves")
    lookupsevr <- c("FL"="newflower", "FR"="newfruits",
        "FF"="newfruitsflowers", "Z"="nofruitsorflower",
        "B"="onlybuds", "BFL"="budsandflowers", "BFR"="budsandfruits")
    sev$foliage <- unname(lookupsevf[sev$foliage])
    sev$reprocond <- unname(lookupsevr[sev$reprocond])
    sev <- merge(sev, sevsplist, by="spcode", all.x=TRUE)
    sev <- taxoscrub(sev, "sev")
    if (names.only) return(sev[common.cols.taxo])
    sev$date <- as.Date(sev$Date, format="%m/%d/%Y")
    sev$year <- format(sev$date, "%Y")
    sev$doy <- as.numeric(format(sev$date, "%j"))
    sev <- sev[!is.na(sev$doy),]

    # sev "site" is really what we call "plot"
    sev$plot <- sev$site 
    sev$site <- "sevcore"

    sev$isinleaf <- !is.na(sev$foliage) & sev$foliage=="new green foliage"
    fld <- subset(sev, isinleaf)
    fld <- subsetEarliest(fld, c("plot", "genus", "species", "year"))
    fld <- cbind(fld, event="fld")

    fld$varetc <- NA
    fld$cult <- NA
    sevrb <- subset(fld, select=common.cols.raw)
    row.names(sevrb) <- NULL

    return(sevrb)
}

clean.raw$sevtrans <- function(splistfile="sev048_1991_meta.dbf",
    transect.files = c("sev048_1995_data.dbf", "sev048_1994_data.dbf",
    "sev048_1993_data.dbf", "sev048_1991_data.dbf"), path=".",
    names.only=FALSE) {

    ## Phenology transects ##

    ## Species codes ##
    sevsplist <- read.table(file.path(path, splistfile), skip=315,
        nrow=821, sep=" ", header=FALSE,  col.names=c("spcode", "genus",
        "species", "variety"), comment.char="\\")

    col.names <- c("weirddate", "maybeobserver", "plot", "spcode",
        "germinating", "perenating", "vegetating", "budding", "flowering",
        "fruiting", "dispersing", "dormant", "senescing", "dead", "notpresent")
    sev95 <- read.table(file.path(path, transect.files[1]), skip=1568,
        nrow=4532, header=FALSE, comment.char="\\", col.names=col.names)
    sev94 <- read.table(file.path(path, transect.files[2]), skip=1568,
        nrow=4532, header=FALSE, comment.char="\\", col.names=col.names)
    sev93 <- read.table(file.path(path, transect.files[3]), skip=1420,
        nrow=5163, header=FALSE, comment.char="\\", col.names=col.names)
    # no data in Sev 1992 file, it's in 1991 file #
    sev912 <- read.table(file.path(path, transect.files[4]), skip=1245,
        nrow=8199, header=FALSE, comment.char="\\", col.names=col.names)
    sevtrans <- rbind(sev95, sev94, sev93, sev912)
    sevtrans <- merge(sevtrans, sevsplist, by="spcode", all.x=TRUE)
    sevtrans <- taxoscrub(sevtrans, "sev")
    if (names.only) return(sevtrans[common.cols.taxo])
    sevtrans$date <- as.Date(as.character(sevtrans$weirddate),
        format="%Y%m%d")
    sevtrans$year <- format(sevtrans$date, "%Y")
    sevtrans$doy <- as.numeric(format(sevtrans$date, "%j"))
    sevtrans <- sevtrans[!is.na(sevtrans$doy),]
    sevtrans$site <- "sevtrans"
    sevtrans$varetc <- NA
    sevtrans$cult <- NA

    sevtrans$isflowering <- sevtrans$flowering=="y"
    sevtrans$isinleaf <- (sevtrans$germinating=="y" |
        sevtrans$perenating=="y" | sevtrans$vegetating=="y")

    # ffd = for each species*yr*plot, the first time flowering
    ffd <- subset(sevtrans, isflowering)
    ffd <- subsetEarliest(ffd, c("plot", "genus", "species", "year"))
    ffd <- cbind(ffd, event="ffd")

    # fld = for each species*yr*plot, the first time in leaf
    fld <- subset(sevtrans, isinleaf)
    fld <- subsetEarliest(fld, c("plot", "genus", "species", "year"))
    fld <- cbind(fld, event="fld")

    sevtransrb <- rbind(ffd, fld)
    sevtransrb <- subset(sevtransrb, select=common.cols.raw)
    row.names(sevtransrb) <- NULL

    return(sevtransrb)

}


clean.raw$mikesell <- function(filename="Mikesell_pheno_1883_1912.csv",
    spkey="Mikesell_species_key.csv", path=".", names.only=FALSE) {
 
    # Mikesell #
    # NA is 999 #
    mikey <- read.csv(file.path(path, filename), header=TRUE,
        na.strings="999")
    mikeysp <- read.csv(file.path(path, spkey), header=TRUE)
    mikesell <- merge(mikey, mikeysp, by.x="sp._code", by.y="Sp..Code")
    mikesell$genus <- sub(sppexpr, "\\1", mikesell$Plant.Name)
    mikesell$species <- sub(sppexpr, "\\2", mikesell$Plant.Name)
    mikesell <- taxoscrub(mikesell, "mikesell")
    if (names.only) return(mikesell[common.cols.taxo])
    mikesell$varetc <- sub(sppexpr, "\\3", mikesell$Plant.Name)
    # remove columns that we don't need (and that we don't want to
    # participate in the melt)
    mikesell$sp._code <- NULL
    mikesell$Plant.Name <- NULL
    mikesell$genus.prescrub <- NULL
    mikesell$species.prescrub <- NULL

    mikemelt <- melt(mikesell, id.var=c("year", "genus", "species",
        "scrub", "varetc"))
    lookupmike <- c("bud_burst"="firstleafbud", "first_leaf"="firstleaf",
        "full_leaf "="fullleaf")
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
taxonomy$sevtrans <- clean.raw$sevtrans(path=raw.data.dir, names.only=TRUE)
taxonomy$sevcore <- clean.raw$sevcore(path=raw.data.dir, names.only=TRUE)
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


#
# Produce cleaned raw data
#

raw.data.dir <- "Observations/Raw"
clean.rawobs <- list()
clean.rawobs$fitter <- clean.rawobs$fitter(path=raw.data.dir)
clean.rawobs$harvard <- clean.rawobs$harvard(path=raw.data.dir)
clean.rawobs$ <- clean.rawobs$hubbard(path=raw.data.dir)

cleandata.raw$konza <- clean.raw$konza(path=raw.data.dir)
cleandata.raw$luquillo <- clean.raw$luquillo(path=raw.data.dir)
cleandata.raw$niwot <- clean.raw$niwot(path=raw.data.dir)
cleandata.raw$sevcore <- clean.raw$sevcore(path=raw.data.dir)
cleandata.raw$sevtrans <- clean.raw$sevtrans(path=raw.data.dir)
cleandata.raw$mikesell <- clean.raw$mikesell(path=raw.data.dir)


###
ffdfld <- do.call("rbind", cleandata.raw)
row.names(ffdfld) <- NULL
write.csv(ffdfld, "knb/nectarpheno_raw.csv", row.names=FALSE)

stop()
