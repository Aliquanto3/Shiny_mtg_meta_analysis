#Get the data from the source for the dates
dfModern=readRDS(paste(RDSDir,"Modern_treated.rds",sep="/"))

dfPioneer=readRDS(paste(RDSDir,"Pioneer_treated.rds",sep="/"))

dfPauper=readRDS(paste(RDSDir,"Pauper_treated.rds",sep="/"))

dfLegacy=readRDS(paste(RDSDir,"Legacy_treated.rds",sep="/"))

#Get the card data
cardDataSub=getCardData(CardFile)
