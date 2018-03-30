#Get some stats on the ME3C database:
setwd("/Users/aileneettinger/git/radcliffe/")



expclim <- read.csv("Analyses/expclim.csv", header=TRUE)
#count up how many years each experiment has data
exp.yr<-table(expclim$site,expclim$year)
exp.yr<-as.data.frame(as.matrix(cbind(rownames(exp.yr),exp.yr)))
colnames(exp.yr)[1]<-"site"
exp.yr$numyears<-0
for(i in 1:dim(exp.yr)[1]){
  exp.yr$numyears[i]<-length(which(exp.yr[i,2:24]!=0))
}
sum(exp.yr$numyears)#59
dim(exp.yr)
expclim$year.doy<-paste(expclim$year,expclim$doy,sep=".")
#count up how many days each experiment has data
exp.day<-table(expclim$site,expclim$year.doy)
exp.day<-as.data.frame(as.matrix(cbind(rownames(exp.day),exp.day)))
colnames(exp.day)[1]<-"site"
exp.day$numdays<-0
for(i in 1:dim(exp.day)[1]){
  exp.day$numdays[i]<-length(which(exp.day[i,2:6056]!=0))
}
sum(exp.day$numdays)
dim(exp.day)