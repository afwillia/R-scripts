#move to rush directory
setwd("~/Dropbox/nfl_data/rush_data") 

#make list of DFs
rush.doc.list <- list.files("~/Dropbox/nfl_data/rush_data", pattern= "*.csv") #make list of files to load
rush.doc.read.list <- lapply(rush.doc.list, read.csv) 
#give each DF a name
names(rush.doc.read.list) <- rush.doc.list  

#should make one big df
data.cat <- do.call(rbind, rush.doc.read.list) 

#Get rid of headers within body
data.clean <- subset(data.cat, data.cat$Rk != "Rk")


