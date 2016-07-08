#Cleaning species lists: Preparing for the phenology workshop 4-6 April 2016
#Code written by M. Johnston (mjohnston@g.harvard.edu), 23 March 2016
#Edited 30 March 2016
#Please refer to Workflow.txt for details re. when to run each code snippet below

##########################################################################
##########################################################################
#(1) Read in the data

dir = '/home/miriam/Documents/Harvard/PhenologyWorkshop_2016/'
filename = 'obs_splist_Site'
ext = '.csv'
data<-read.csv(paste(dir, filename, ext, sep=""))

##########################################################################
#(2) Export a file in the correct format for the ITIS database

###Getting data into the format for which this code was written:
data$site<-as.character(data$site); data$species<-as.character(data$species)
df<-data.frame(do.call('rbind',strsplit(data$species,"\\.")))
data<-data.frame(data$site,df)
names(data)<-c("site","Genus","Species")
data<-data[with(data,order(Genus)),] #alphabetize by genus

#Next, getting data into format for ITIS:
spp<-data.frame(paste(data$Genus,data$Species,sep=" "))
colnames(spp)<-"name"
write.csv(spp,paste(dir,filename,"_format",".txt", sep = ""),row.names=FALSE, quote=FALSE)

##########################################################################
#(3) Export a file in the format that we're eventually looking for

data_new<-data.frame(data, data$Genus, data$Species)
colnames(data_new)<-c("Site","orig.Genus","orig.Species","clean.Genus","clean.Species")

data_new$clean.Species[which(data_new$orig.Species=="spp")]<-NA #Change spp to NA
data_new$clean.Species[which(data_new$orig.Species=="sp")]<-NA
data_new$clean.Species[which(data_new$orig.Species=="Unknown")]<-NA #Change unknown to NA in Species column
data_new$clean.Genus[which(data_new$orig.Genus=="Unknown")]<-NA #Change unknown to NA in Genus column

write.csv(data_new,paste(dir,filename,"_toclean",ext,sep=""),row.names=FALSE)

##########################################################################
#(4) Go to ITIS database, hand correct spelling mistakes & replace with accepted names

##########################################################################
#(5) Import new, cleaned spreadsheet, format for double-checking in ITIS. Iterate if necessary.
#Note: iteration involves both this chunk of code and ITIS, so it is done by hand

data<-read.csv(paste(dir,filename,"_toclean",ext,sep=""))
spp<-data.frame(paste(data$clean.Genus,data$clean.Species,sep=" "))
colnames(spp)<-"name"
write.csv(spp,paste(dir,filename,"_check",".txt", sep = ""),row.names=FALSE, quote=FALSE)

##########################################################################
#(6) Check for repeat entries
#if different sites have the same clean.Genus and clean.Species, leave alone (?)
#if a single site has >1 of the same clean.Genus and clean.Species, collapse (?)

data<-read.csv(paste(dir,filename,"_toclean",ext,sep=""))

#For testing only (creates duplicates)
#data[2,]<-data[3,]
#data[4,1]<-"test1"

#RUN ONLY ONE of (a), (b), or (c):

#(a)Duplicates removed from the database
data<-data[!duplicated(data[,c(1,4:5)]),] 

#(b)Duplicates (not including first instance) labeled
#data[,6]<-duplicated(data[,c(1,4:5)])
#names(data)[6]<-"duplicate"

#(c)All duplicates (including the first instance) labeled
#dup1<-duplicated(data[,c(1,4:5)])
#dup2<-duplicated(data[,c(1,4:5)],fromLast=TRUE)
#data[,6]<-dup1|dup2 #If either is TRUE, choose TRUE
#names(data[6])<-"duplicate"

#(6b) Figure out how much was changed
#For summary only (counts edits)
#data<-read.csv(paste(dir,filename,"_toclean",ext,sep=""),stringsAsFactors=FALSE)
#dat1<-data[,2:3]
#names(dat1)<-c("a","b")
#dat2<-data[,4:5]
#names(dat2)<-c("a","b")
#dat3<-vector()
#for (i in 1: nrow(dat1)){dat3[i]<-identical(dat1[i,],dat2[i,])}
#sum(dat3)

##########################################################################
#(7) Write out the final cleaned file

write.csv(data,paste(dir,filename,"_final",ext,sep=""),row.names=FALSE)


         