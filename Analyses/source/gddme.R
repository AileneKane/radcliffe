### Started 5 April 2016 ###
### By Lizzie (for now), other authors welcome! ###


##
## some f(x)s to help calculate GDD
##

## this f(x) makes a column which zeroes out all data
# below your threshold temperature

makethreshold.data <- function(dater, temp.col, thresh){ 
    ifelse(dater[[temp.col]]>thresh,
       (dater[[temp.col]]-thresh), 0)
  }

## this f(x) adds up gdd
## requires data ordered by doy (I do this in the loop below)
## this f(x) returns the value while treating NA as zeroes
# needstartdate is when you require data to start that year, otherwise it returns NA
# for example, 5 means you need data that starts before 5 January to actually count
makegdd.data.skipNA <- function(dater, gdd.col, doy.col, startdate, needstartdate){
     saveme <- c()
     for(i in 1:nrow(dater)){
     # deal with cases where the data start after Jan 1
     if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
     else
     # deal with cases where the entire column is NA
     if (sum(is.na(dater[[gdd.col]]))==length(dater[[gdd.col]])) saveme[i] <- NA
     else
     # okay, finally calculate the GDD
     if (dater[[doy.col]][i]==startdate) saveme[i] <- (dater[[gdd.col]][i])
     else
     # if a cell is NA, just add 0 instead of the cell
     if (is.na(dater[[gdd.col]][i])) saveme[i] <- (0+saveme[i-1])
     else
     saveme[i] <- (dater[[gdd.col]][i]+saveme[i-1])
 }
 return(saveme)
}

 
countNA.pergdd <- function(dater, gdd.col, doy.col, startdate, needstartdate){
     saveme <- c()
     for(i in 1:nrow(dater)){
     dater <- dater[order(as.numeric(dater[[doy.col]])),]
     if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
     else 
     if  (sum(is.na(dater[[gdd.col]]))==length(dater[[gdd.col]])) saveme[i] <- NA
     else
     saveme[i] <- sum(is.na(dater[[gdd.col]][which(dater[[doy.col]]<(i+1))]))
     }
 return(saveme)
}



##
## f(x)s that I never got to run
## I tried to count NA and make cumulative GDD in one f(x)
##

makegdd.data.skipNA.arghh <- function(dater, gdd.col, doy.col, startdate, needstartdate){
     saveme <- c()
     nacount <- c()
     for(i in 1:nrow(dater)){
     if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
     else 
     if (is.na(unique(dater[[gdd.col]][i]))==TRUE) saveme[i] <- NA
     else
         
     # subsetme <- dater[which(dater[[doy.col]]<(i+1)),]
     # nacount[i] <- sum(is.na(subsetme[[gdd.col]]))
  
     if (is.na(dater[[gdd.col]][i])==TRUE) saveme[i] <- (0+saveme[i-1])
     else
     if (dater[[doy.col]][i]==startdate) saveme[i] <- (dater[[gdd.col]][i])
     else
     saveme[i] <- (dater[[gdd.col]][i]+saveme[i-1])
     nacount[i] <- (sum(is.na(subsetme[[gdd.col]][i]))+nacount[i-1])
 }
 return(cbind(saveme, nacount))
}
