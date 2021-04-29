#read in life form data from usda plants, format, and put together

trees<-read.table("Data/Other/treelist_usda.txt", header=TRUE, sep=",")
head(tree)
trees$form<-"tree"

shrubs<-read.table("Data/Other/shrublist_usda.txt", header=TRUE, sep=",")
head(shrubs)
shrubs$form<-"shrub"

forbs<-read.table("Data/Other/forblist_usda.txt", header=TRUE, sep=",")
head(forbs)
forbs$form<-"forb"

grass<-read.table("Data/Other/graminoidlist_usda.txt", header=TRUE, sep=",")
head(grass)
grass$form<-"grass"

lfall<-rbind(trees,shrubs,forbs,grass)

lfall$sp.name<-gsub(lfall$Scientific.Name,pattern=" ", replacement=".", fixed=TRUE)
lf<-subset(lfall,select=c(sp.name,form))
write.csv(lf,"Analyses/output/lifeform.csv", row.names = FALSE)
