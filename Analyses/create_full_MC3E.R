#R code to merge daily experimental climate with treatment detail 
#to create full MC3E Database
#Contact Ailene Ettinger for questions
#ailene.ettinger@gmail.com

expclim<-read.csv("expclim.csv", header=T)
treats<-read.csv("treats_detail.csv", header=T)

expclim2<-full_join(treats,expclim, by=c("site", "block", "plot","temptreat","preciptreat"), match="first")
