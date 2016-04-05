# Reformatting Observational Climate to match the format for experiments


# Load the experimental climate as an example
expclim <- read.csv("expclim.csv")
names(expclim)

# Set the file path for the observational climate
obs.dir <- "../Observations//Temp"

files.tmax <- dir(obs.dir, c("tmax_", ".csv"))
files.tmin <- dir(obs.dir, c("tmin_", ".csv"))

# Get a list of our sites
sites <- substr(files.tmax, 6, nchar(files.tmax)-4)

# Grabbing the data by site
for(s in sites){
  print(paste0("processing: ", s))
  tmax.raw <- read.csv(file.path(obs.dir, paste0("tmax_", s, ".csv")))
  tmin.raw <- read.csv(file.path(obs.dir, paste0("tmin_", s, ".csv")))
  
  temp <- stack(tmax.raw[,2:ncol(tmax.raw)]) # creating a 2-D data frame with temp & doy
  names(temp) <- c("airtemp_max", "doy") # giving it better names
  temp$doy <- as.numeric(substr(temp$doy,2,nchar(paste(temp$doy)))) # getting rid of the X in doy
  temp$year <- tmax.raw[,1] # adding in year
  temp$site <- as.factor(s)
  summary(temp)
  
  # Tmin & tmax don't always line up, so we're doing it separately and merging
  temp2 <- stack(tmin.raw[,2:ncol(tmin.raw)]) # adding in min air temp
  names(temp2) <- c("airtemp_min", "doy") # giving it better names
  temp2$doy <- as.numeric(substr(temp2$doy,2,nchar(paste(temp2$doy)))) # getting rid of the X in doy
  temp2$year <- tmin.raw[,1] # adding in year
  temp2$site <- as.factor(s)

  # Merging things together
  temp3 <- merge(temp, temp2, all.x=T, all.y=T)
  
  if(s == sites[1]){
    temp.clean <- temp3
  } else {
#     row.names(temp) <- (nrow(temp.clean)+1):(nrow(temp.clean)+nrow(temp))
    temp.clean <- rbind(temp.clean, temp3)
  }
}

# saving as a .csv file
write.csv(temp.clean, "obsclim.csv", row.names=F, eol="\r\n")