### Started 18 April 2016 ###
### By Lizzie ###

## Code taken from merge.R and radcliffe_merge_obs.R ##
## This file is similar to the harvard chunks of the aforementioned code #
## save that it takes the first date BY INDIVIDUAL, then calculates a mean #
## first event date (bbd and ffd) ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

library(reshape)
library(zoo)

setwd("~/Documents/git/projects/meta_ep2/radcliffe")

# Species binomials etc. extractions #
sppexpr <- "^([^ ]*) *([^ ]*) *([^ ]*) *$"
sppexprcrazy <- "^([^ ]*) ?([^ ]*)? ?(var. |subsp. | )?(.*)?"
# sub(sppexpr, "\\1", column)


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

subsetInd <- function(dat, by, flowervar="value") {
    groups <- do.call("paste", dat[by])
    do.call("rbind", lapply(split(dat, groups),
        function(x) x[which.max(x[[flowervar]]),]))
}

getprecision <- function(date) {
    unique.sorted.dates <- sort(unique(date))
    z <- zoo(as.numeric(unique.sorted.dates), unique.sorted.dates)
    d <- na.omit(diff(z))
    as.vector(d)[match(date, index(d))]
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

clean.raw <- list()

clean.raw$harvard <- function(filename="hf003-03-spring.csv",
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
    bbd <- subsetEarliest(bbd, c("genus", "species", "year", "tree.id"))
    bbd <- cbind(bbd, event="bbd")
    bbd.agg <- aggregate(bbd[c("doy", "date")], bbd[c("event",
        "year", "genus", "species", "scrub")],
        FUN=mean)

    # ffd = for each species*yr, the first time that FOPN > 0
    ffd <- subset(harvard, fopn > 0)
    ffd <- subsetEarliest(ffd, c("genus", "species", "year", "tree.id"))
    ffd <- cbind(ffd, event="ffd")
    ffd.agg <- aggregate(ffd[c("doy", "date")], ffd[c("event",
       "year", "genus", "species", "scrub")],
        FUN=mean)

    harvard <- rbind(bbd.agg, ffd.agg)

    # Add and adjust columns to match for rbind #
    harvard$site <- "harvard"
    harvard$plot <- NA
    harvard$cult <- NA
    harvard$varetc <- NA

    harvard <- subset(harvard, select=common.cols.raw)
    row.names(harvard) <- NULL

    return(harvard)
}


#
# Produce cleaned raw data
#

raw.data.dir <- "Data/Observations/Raw"
cleandata.raw <- list()
cleandata.raw$harvard <- clean.raw$harvard(path=raw.data.dir)
ffdfld <- do.call("rbind", cleandata.raw)

write.csv(ffdfld, "Analyses/output/harvard_firstsbyind.csv", row.names=FALSE)
