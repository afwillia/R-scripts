#move to correct dir
setwd("~/Dropbox/nfl_data")

#make list of files to load
doc.list <- list.files("~/Dropbox/nfl_data", pattern= "*.csv")
#make list of DFs
doc.read.list <- lapply(doc.list, read.csv)
#give each DF a name
names(doc.read.list) <- doc.list

##do this for all weeks rush and rec and pass-
##import all files for rush into one list then rbind into one DF

