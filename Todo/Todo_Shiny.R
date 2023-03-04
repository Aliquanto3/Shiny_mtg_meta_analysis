#In the functions file
###########################################
# #returns a dataframe containing statistics for each card in the data: names, 
# #number of copies per deck/overall/on average, number of decks, presence  
# #board = "Mainboard" or "Sideboard" or "Allboards"
# #TODO: merge MD and SB ("All")
# CardsStatsGetter = function(df,board){
#   
#   #GET THE NAMES OF EACH DIFFERENT CARD
#   cardList=list()
#   for (i in 1:nrow(df)){
#     cardList[i]=list(df[[board]][[i]]$CardName)
#   }
#   CardsNames=unique(unlist(cardList))
#   
#   #NUMBER OF DIFFERENT CARDS
#   NCards=length(CardsNames)
#   
#   CardsCounts=rep(0,NCards)
#   DecksCounts=rep(0,NCards)
#   WinsCounts=rep(0,NCards)
#   MatchesCounts=rep(0,NCards)
#   WinrateAverage=rep(0,NCards)
#   Artists=rep(0,NCards)
#   Colors=rep(0,NCards)
#   CMC=rep(0,NCards)
#   Types=rep(0,NCards)
#   FirstSet=rep(0,NCards)
#   Archetypes=rep(0,NCards)
#   for (i in 1:NCards){
#     print(i)
#     
#     #DOES NOT HANDLE WEAR//TEAR (SPLIT CARDS) VERY WELL!
#     if (CardsNames[i] %in% cardDataSub$name){
#       cardDataSubi=cardDataSub[cardDataSub$name==CardsNames[i],]
#     }else if (CardsNames[i] %in% cardDataSub$faceName){
#       cardDataSubi=cardDataSub[cardDataSub$faceName==CardsNames[i],]
#     }
#     cardDataSubi=cardDataSubi[cardDataSubi$isReprint==0,][1,]
#     
#     Artists[i]=cardDataSubi$artist
#     
#     Colors[i]=cardDataSubi$colors
#     
#     CMC[i]=cardDataSubi$convertedManaCost
#     
#     Types[i]=cardDataSubi$types
#     
#     FirstSet[i]=cardDataSubi$setCode
#     
#     #GET THE NUMBER OF COPIES OF EACH DIFFERENT CARD
#     cardCountsi=rep(0,nrow(df))
#     for (j in 1:nrow(df)){
#       if(length(which(df[[board]][[j]]$CardName==CardsNames[i]))>0){
#         
#         cardCountsi[j]=sum(df[[board]][[j]][which(
#           df[[board]][[j]]$CardName==CardsNames[i]),]$Count)
#       }
#     }
#     CardsCounts[i]=sum(cardCountsi)
#     
#     #GET THE NUMBER OF DECKS WHERE EACH CARD WAS PLAYED
#     deckCountsi=rep(0,nrow(df))
#     for (j in 1:nrow(df)){
#       if(CardsNames[i] %in% df[[board]][[j]]$CardName){
#         deckCountsi[j]=1
#       }
#     }
#     DecksCounts[i]=sum(deckCountsi)
#     
#     #GET THE NUMBER OF WINS OF THE CARD
#     winCountsi=rep(0,nrow(df))
#     for (j in 1:nrow(df)){
#       if(CardsNames[i] %in% df[[board]][[j]]$CardName){
#         winCountsi[j]=sum((df$Points[j] + df$T8Points[j])/3)
#       }
#     }
#     WinsCounts[i]=sum(winCountsi, na.rm = TRUE)
#     
#     #GET THE NUMBER OF MATCHES OF THE CARD
#     matchCountsi=rep(0,nrow(df))
#     for (j in 1:nrow(df)){
#       if(CardsNames[i] %in% df[[board]][[j]]$CardName){
#         matchCountsi[j]=sum((df$NRounds[j] + df$T8Matches[j]))
#       }
#     }
#     MatchesCounts[i]=sum(matchCountsi, na.rm = TRUE)
#     
#     #CI WITH BINOMIAL TEST
#     WinrateAverage[i]=as.numeric(format(round(
#       binom.test(WinsCounts[i], MatchesCounts[i], p=0.5,
#                  alternative="two.sided", conf.level=0.95)$estimate*100,1), 
#       nsmall = 1))
#     
#     #GET THE LIST OF ARCHETYPES WHERE THE CARD IS PLAYED
#     archetypesi=list()
#     for (j in 1:nrow(df)){
#       if(CardsNames[i] %in% df[[board]][[j]]$CardName){
#         archetypesi[j]=df$Archetype$Archetype[j]
#       }
#     }
#     archetypesiNames=names(table(unlist(archetypesi), exclude = NULL))
#     archetypesiCount=as.vector(table(unlist(archetypesi), exclude = NULL))
#     archetypesiString=""
#     for (k in 1:length(archetypesiNames)){
#       if(!is.null(archetypesiNames[k])){
#         archetypesiString=paste(archetypesiString,archetypesiCount[k],
#                                 archetypesiNames[k],"\n",sep=" ")
#       }
#     }
#     Archetypes[i]=archetypesiString
#   }
#   
#   #ASSOCIATE THE NAMES OF EACH DIFFERENT CARD WITH THEIR TOTAL NUMBER OF COPIES 
#   #AND THE NUMBER OF DECKS PLAYING THEM
#   Cards=setNames(data.frame(CardsNames,CardsCounts,DecksCounts,WinsCounts,
#                             MatchesCounts,WinrateAverage,Winrate95Min,
#                             Winrate95Max,Archetypes,URL,Artists,Colors,
#                             CMC,Types,FirstSet), 
#                  c("CardNames", "CardCount", "DeckCount", "WinCount", 
#                    "MatchCount","WinrateAverage","Winrate95Min",
#                    "Winrate95Max","Archetypes","URL","Artists","Colors",
#                    "CMC","Types","FirstSet"))
#   
#   #TOTAL NUMBER OF CARDS PLAYED IN THE EVENTS
#   NbTotalCards=sum(Cards$CardCount)
#   
#   #GET THE % PRESENCE OF EACH CARD OUT OF ALL THE CARDS PLAYED
#   Cards$CardCountOutTotalCards=as.numeric(format(round(
#     Cards$CardCount*100/NbTotalCards,1), nsmall = 1))
#   
#   #GET THE % OF DECKS PLAYING EACH CARD
#   Cards$DeckCountOutTotalDecks=as.numeric(format(round(
#     Cards$DeckCount*100/nrow(df),1), nsmall = 1))
#   
#   #GET THE AVERAGE NUMBER OF COPIES OF EACH CARD
#   Cards$AverageCopies=as.numeric(format(round(
#     Cards$CardCount/DecksCounts,1), nsmall = 1))
#   
#   return(Cards)
# }
# 
# MDStats=CardsStatsGetter(df,"Mainboard")
# SBStats=CardsStatsGetter(df,"Sideboard")
###########################################




#At the end of the server.R file
##################################
# #Return a table with the MD card data
# output$MDTable = renderDataTable({
#   #Copy the first loaded data
#   dataset=df
#   
#   #Keep only results of the chosen format
#   dataset=dataset[grep(pattern = input$mdFormat,x=dataset$Tournament),]
#   
#   #Keep only the data between the selected dates
#   dataset=dataset[dataset$Date >= input$mdDates1 &
#                     dataset$Date < input$mdDates2,]
#   
#   #Keep only data of the chosen archetype
#   dataset=dataset[dataset$Archetype$Archetype==input$mdArchetype,]
#   
#   #Keep only the most recent results with the most points
#   if(input$mdOnlyBest){
#     dataset=dataset[dataset$Points+dataset$T8Points==
#                       max(dataset$Points+dataset$T8Points),]
#     dataset=dataset[dataset$Date==max(dataset$Date),]
#   }
#   
#   #Provide the right name to the type of chosen events
#   #and filter to keep only the selected events
#   EventType="Official Competitions"
#   if (!input$mdPrelims & !input$mdChallenges){
#     dataset=NA
#     EventType=NA
#   }else if (input$mdPrelims & !input$mdChallenges){
#     dataset=dataset[grep("Preliminary", dataset$Tournament), ]
#     EventType="Preliminaries"
#   }else if (!input$mdPrelims & input$mdChallenges){
#     MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
#     dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
#     EventType="Major Official Events"
#   }
#   
#   #Return a table with a message if there is no event in the selection
#   if (is.na(EventType)){
#     decklistsDf=as.data.frame(tabNoEventType)
#     
#     #Return a table with a message if there is no row in the data frame
#   } else if (nrow(dataset)==0){
#     decklistsDf=as.data.frame(tabEmptyDf)
#     
#     #Get the table of results for each decklist
#   } else {
#     #Display a table with all the cards of the archetype and their average counts
#     if (input$mdDisplay=="Breakdown"){
#       
#       decklistsDf=archAverageData(dataset) #TODO: update the function
#       
#       #Display the URL of the archetype
#     }else if (input$mdDisplay=="URL"){
#       #PRINT WINRATES OF ALL THE MD CARDS
#       CardResultsMD = MDStats[order(-MDStats$DeckCount),]
#       
#       CardResults2CSV=CardResultsMD
#       CardResults2CSV[,"Archetypes"]=NA
#       CardResults2CSV[,"URL"]=NA
#       for (i in 1:length(CardResults2CSV$CardName)){
#         CardResults2CSV$Archetypes[i]=paste(unlist(
#           CardResultsMD$Archetypes[i]),collapse = "; ")
#         
#         CardResults2CSV$URL[i]=paste(unlist(CardResultsMD$URL[i]),
#                                      collapse = "; ")
#       }
#       
#       CardResultsMD=CardResults2CSV
#     }
#   }
#   CardResultsMD
# })
# 
# 
# #Downloadable CSV of the MD card data
# output$MDTableDL = downloadHandler(
#   
#   filename = function() {
#     #Provide the right name to the type of chosen events
#     #and filter to keep only the selected events
#     EventType="Official_Competitions"
#     if (!input$mdPrelims & !input$mdChallenges){
#       dataset=NA
#       EventType=NA
#     }else if (input$mdPrelims & !input$mdChallenges){
#       dataset=dataset[grep("Preliminary", dataset$Tournament), ]
#       EventType="Preliminaries"
#     }else if (!input$mdPrelims & input$mdChallenges){
#       MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
#       dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
#       EventType="Major_Official_Events"
#     }
#     
#     nameWoExt=paste("MTGO",input$mdFormat,EventType,"MD_cards_data_from", 
#                     input$mdDates1,"to", input$mdDates2, sep = "_")
#     
#     paste(nameWoExt,".csv",sep="")
#   },
#   content = function(file) {
#     
#     #Copy the first loaded data
#     dataset=df
#     
#     #Keep only results of the chosen format
#     dataset=dataset[grep(pattern = input$mdFormat,x=dataset$Tournament),]
#     
#     #Keep only the data between the selected dates
#     dataset=dataset[dataset$Date >= input$mdDates1 &
#                       dataset$Date < input$mdDates2,]
#     
#     #Keep only data of the chosen archetype
#     dataset=dataset[dataset$Archetype$Archetype==input$mdArchetype,]
#     
#     #Keep only the most recent results with the most points
#     if(input$mdOnlyBest){
#       dataset=dataset[dataset$Points+dataset$T8Points==
#                         max(dataset$Points+dataset$T8Points),]
#       dataset=dataset[dataset$Date==max(dataset$Date),]
#     }
#     
#     #Provide the right name to the type of chosen events
#     #and filter to keep only the selected events
#     EventType="Official Competitions"
#     if (!input$mdPrelims & !input$mdChallenges){
#       dataset=NA
#       EventType=NA
#     }else if (input$mdPrelims & !input$mdChallenges){
#       dataset=dataset[grep("Preliminary", dataset$Tournament), ]
#       EventType="Preliminaries"
#     }else if (!input$mdPrelims & input$mdChallenges){
#       MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
#       dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
#       EventType="Major Official Events"
#     }
#     
#     #Return a table with a message if there is no event in the selection
#     if (is.na(EventType)){
#       decklistsDf=as.data.frame(tabNoEventType)
#       
#       #Return a table with a message if there is no row in the data frame
#     } else if (nrow(dataset)==0){
#       decklistsDf=as.data.frame(tabEmptyDf)
#       
#       #Get the table of results for each decklist
#     } else {
#       #Display a table with all the cards of the archetype and their average counts
#       if (input$mdDisplay=="Breakdown"){
#         
#         decklistsDf=archAverageData(dataset) #TODO: update the function
#         
#         #Display the URL of the archetype
#       }else if (input$mdDisplay=="URL"){
#         #PRINT WINRATES OF ALL THE MD CARDS
#         CardResultsMD = MDStats[order(-MDStats$DeckCount),]
#         
#         CardResults2CSV=CardResultsMD
#         CardResults2CSV[,"Archetypes"]=NA
#         CardResults2CSV[,"URL"]=NA
#         for (i in 1:length(CardResults2CSV$CardName)){
#           CardResults2CSV$Archetypes[i]=paste(unlist(
#             CardResultsMD$Archetypes[i]),collapse = "; ")
#           
#           CardResults2CSV$URL[i]=paste(unlist(CardResultsMD$URL[i]),
#                                        collapse = "; ")
#         }
#         
#         CardResultsMD=CardResults2CSV
#       }
#     }
#     
#     write.csv(CardResultsMD, file, row.names = TRUE)
#   }
#   
# )
# 
# #Return a table with the SB card data
# output$SBTable = renderDataTable({
#   #Copy the first loaded data
#   dataset=df
#   
#   #Keep only results of the chosen format
#   dataset=dataset[grep(pattern = input$sbFormat,x=dataset$Tournament),]
#   
#   #Keep only the data between the selected dates
#   dataset=dataset[dataset$Date >= input$sbDates1 &
#                     dataset$Date < input$sbDates2,]
#   
#   #Keep only data of the chosen archetype
#   dataset=dataset[dataset$Archetype$Archetype==input$sbArchetype,]
#   
#   #Keep only the most recent results with the most points
#   if(input$sbOnlyBest){
#     dataset=dataset[dataset$Points+dataset$T8Points==
#                       max(dataset$Points+dataset$T8Points),]
#     dataset=dataset[dataset$Date==max(dataset$Date),]
#   }
#   
#   #Provide the right name to the type of chosen events
#   #and filter to keep only the selected events
#   EventType="Official Competitions"
#   if (!input$sbPrelims & !input$sbChallenges){
#     dataset=NA
#     EventType=NA
#   }else if (input$sbPrelims & !input$sbChallenges){
#     dataset=dataset[grep("Preliminary", dataset$Tournament), ]
#     EventType="Preliminaries"
#   }else if (!input$sbPrelims & input$sbChallenges){
#     MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
#     dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
#     EventType="Major Official Events"
#   }
#   
#   #Return a table with a message if there is no event in the selection
#   if (is.na(EventType)){
#     decklistsDf=as.data.frame(tabNoEventType)
#     
#     #Return a table with a message if there is no row in the data frame
#   } else if (nrow(dataset)==0){
#     decklistsDf=as.data.frame(tabEmptyDf)
#     
#     #Get the table of results for each decklist
#   } else {
#     #Display a table with all the cards of the archetype and their average counts
#     if (input$sbDisplay=="Breakdown"){
#       
#       decklistsDf=archAverageData(dataset) #TODO: update the function
#       
#       #Display the URL of the archetype
#     }else if (input$sbDisplay=="URL"){
#       #PRINT WINRATES OF ALL THE SB CARDS
#       CardResultsSB = SBStats[order(-SBStats$DeckCount),]
#       
#       CardResults2CSV=CardResultsSB
#       CardResults2CSV[,"Archetypes"]=NA
#       CardResults2CSV[,"URL"]=NA
#       for (i in 1:length(CardResults2CSV$CardName)){
#         CardResults2CSV$Archetypes[i]=paste(unlist(
#           CardResultsSB$Archetypes[i]),collapse = "; ")
#         
#         CardResults2CSV$URL[i]=paste(unlist(CardResultsSB$URL[i]),
#                                      collapse = "; ")
#       }
#       
#       CardResultsSB=CardResults2CSV
#     }
#   }
#   CardResultsSB
# })
# 
# #Downloadable CSV of the SB card data
# output$SBTableDL = downloadHandler(
#   
#   filename = function() {
#     #Provide the right name to the type of chosen events
#     #and filter to keep only the selected events
#     EventType="Official_Competitions"
#     if (!input$sbPrelims & !input$sbChallenges){
#       dataset=NA
#       EventType=NA
#     }else if (input$sbPrelims & !input$sbChallenges){
#       dataset=dataset[grep("Preliminary", dataset$Tournament), ]
#       EventType="Preliminaries"
#     }else if (!input$sbPrelims & input$sbChallenges){
#       MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
#       dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
#       EventType="Major_Official_Events"
#     }
#     
#     nameWoExt=paste("MTGO",input$sbFormat,EventType,"SB_cards_data_from", 
#                     input$sbDates1,"to", input$sbDates2, sep = "_")
#     
#     paste(nameWoExt,".csv",sep="")
#   },
#   content = function(file) {
#     
#     #Copy the first loaded data
#     dataset=df
#     
#     #Keep only results of the chosen format
#     dataset=dataset[grep(pattern = input$sbFormat,x=dataset$Tournament),]
#     
#     #Keep only the data between the selected dates
#     dataset=dataset[dataset$Date >= input$sbDates1 &
#                       dataset$Date < input$sbDates2,]
#     
#     #Keep only data of the chosen archetype
#     dataset=dataset[dataset$Archetype$Archetype==input$sbArchetype,]
#     
#     #Keep only the most recent results with the most points
#     if(input$sbOnlyBest){
#       dataset=dataset[dataset$Points+dataset$T8Points==
#                         max(dataset$Points+dataset$T8Points),]
#       dataset=dataset[dataset$Date==max(dataset$Date),]
#     }
#     
#     #Provide the right name to the type of chosen events
#     #and filter to keep only the selected events
#     EventType="Official Competitions"
#     if (!input$sbPrelims & !input$sbChallenges){
#       dataset=NA
#       EventType=NA
#     }else if (input$sbPrelims & !input$sbChallenges){
#       dataset=dataset[grep("Preliminary", dataset$Tournament), ]
#       EventType="Preliminaries"
#     }else if (!input$sbPrelims & input$sbChallenges){
#       MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
#       dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
#       EventType="Major Official Events"
#     }
#     
#     #Return a table with a message if there is no event in the selection
#     if (is.na(EventType)){
#       decklistsDf=as.data.frame(tabNoEventType)
#       
#       #Return a table with a message if there is no row in the data frame
#     } else if (nrow(dataset)==0){
#       decklistsDf=as.data.frame(tabEmptyDf)
#       
#       #Get the table of results for each decklist
#     } else {
#       #Display a table with all the cards of the archetype and their average counts
#       if (input$sbDisplay=="Breakdown"){
#         
#         decklistsDf=archAverageData(dataset) #TODO: update the function
#         
#         #Display the URL of the archetype
#       }else if (input$sbDisplay=="URL"){
#         #PRINT WINRATES OF ALL THE SB CARDS
#         CardResultsSB = SBStats[order(-SBStats$DeckCount),]
#         
#         CardResults2CSV=CardResultsSB
#         CardResults2CSV[,"Archetypes"]=NA
#         CardResults2CSV[,"URL"]=NA
#         for (i in 1:length(CardResults2CSV$CardName)){
#           CardResults2CSV$Archetypes[i]=paste(unlist(
#             CardResultsSB$Archetypes[i]),collapse = "; ")
#           
#           CardResults2CSV$URL[i]=paste(unlist(CardResultsSB$URL[i]),
#                                        collapse = "; ")
#         }
#         
#         CardResultsSB=CardResults2CSV
#       }
#     }
#     
#     write.csv(CardResultsSB, file, row.names = TRUE)
#   }
#   
# )
#####################################

