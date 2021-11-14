#Message when neither Preliminaries nor Challenges are selected
noEventType = "\nNo type of event selected.\n"

#Corresponding plot
plotNoEventType = ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label = noEventType) + 
  theme_void()

#Corresponding table
tabNoEventType=matrix(data = noEventType)

#Message when there is no row in the generated dataset, likely because 
#the dates were written wrong
emptyDf = "\nThe data set is empty, \n check the dates.\n"

#Corresponding plot
plotEmptyDf = ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label = emptyDf) + 
  theme_void()

#Corresponding table
tabEmptyDf=matrix(data = emptyDf)

#Message when there is no row in the generated dataset, likely because 
#no archetype was selected
emptyArch = "\nPlease select an archetype.\n"
tabEmptyArch=matrix(data = emptyArch)