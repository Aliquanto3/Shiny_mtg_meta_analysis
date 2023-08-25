
#Generate the outputs
function(input, output, session) {
  
  #Plot the presence as a pie chart or histogram of each archetype
  output$plotPresence = renderGirafe({
    #Copy the first loaded data for the chosen format
    if (input$presenceFormat=="Modern"){
      dataset=dfModern
    }else if (input$presenceFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$presenceFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$presenceFormat=="Legacy"){
    dataset=dfLegacy
    }
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$presenceDates1 &
                      dataset$Date <= input$presenceDates2,]
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$presencePrelims & !input$presenceChallenges & !input$presencePaper){
      dataset=NA
      EventType=NA
    }else if (input$presencePrelims & !input$presenceChallenges & !input$presencePaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$presencePrelims & input$presenceChallenges & !input$presencePaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$presencePrelims & !input$presenceChallenges & input$presencePaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    #Plot text if there is no event in the selection
    if (is.na(EventType)){
      p = plotNoEventType
      
      #Plot text if there is no row in the data frame
    } else if (nrow(dataset)==0){
      p = plotEmptyDf
      
      #Plot a pie chart of the presence
    } else if (input$presenceGraph=="Pie Chart"){
      p=metagame_pie_chart(dataset,input$presenceShare,input$presenceType,
                           as.Date(input$presenceDates1, format="%Y-%m-%d", 
                                   origin ="1970-01-01"),
                           as.Date(input$presenceDates2, format="%Y-%m-%d", 
                                   origin ="1970-01-01"),
                           EventType,input$presenceFormat)
      
      #Plot a histogram of the presence
    } else if (input$presenceGraph=="Histogram"){
      p=metagame_box_plot(dataset,input$presenceShare,input$presenceType,
                          as.Date(input$presenceDates1, format="%Y-%m-%d", 
                                  origin ="1970-01-01"),
                          as.Date(input$presenceDates2, format="%Y-%m-%d", 
                                  origin ="1970-01-01"),
                          EventType,input$presenceFormat)
    }

    #Return the graph to be plotted
    print(p)

  })
  
  #Plot the winrate of the most played decks with CI
  output$plotWinrate = renderPlot({
    
    #Copy the first loaded data for the chosen format
    if (input$winrateFormat=="Modern"){
      dataset=dfModern
    }else if (input$winrateFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$winrateFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$winrateFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$winrateDates1 &
                      dataset$Date <= input$winrateDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$winratePrelims & !input$winrateChallenges & !input$winratePaper){
      dataset=NA
      EventType=NA
    }else if (input$winratePrelims & !input$winrateChallenges & !input$winratePaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$winratePrelims & input$winrateChallenges & !input$winratePaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$winratePrelims & !input$winrateChallenges & input$winratePaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Plot text if there is no event in the selection
    if (is.na(EventType)){
      p = plotNoEventType
      
      #Plot text if there is no row in the data frame
    } else if (nrow(dataset)==0){
      p = plotEmptyDf
      
      #Plot a chart of the winrate
    } else {
      
      metric_df=metric_points_archetypes(dataset)
      
      arch_ranked=archetypes_ranking(metric_df,
                                     as.Date(input$winrateDates1, format="%Y-%m-%d", 
                                             origin ="1970-01-01"),
                                     as.Date(input$winrateDates2, format="%Y-%m-%d", 
                                             origin ="1970-01-01"), 1, 1)
      
      p=winrates_graph(df=dataset,arch_ranked=arch_ranked,presence="Matches",
                       HistShare=input$winrateShare,
                       beginning=as.Date(input$winrateDates1, format="%Y-%m-%d", 
                               origin ="1970-01-01"),
                       end=as.Date(input$winrateDates2, format="%Y-%m-%d", 
                               origin ="1970-01-01"),
                       EventType=EventType)
      
      
    }
    
    #Return the graph to be plotted
    print(p)
    
  }, height=800)
  
  #Plot the representation of the win rate depending on presence for all decks
  output$plotWvpf = renderGirafe({
    
    #Copy the first loaded data for the chosen format
    if (input$wvpfFormat=="Modern"){
      dataset=dfModern
    }else if (input$wvpfFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wvpfFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wvpfFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wvpfDates1 &
                      dataset$Date <= input$wvpfDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$wvpfPrelims & !input$wvpfChallenges & !input$wvpfPaper){
      dataset=NA
      EventType=NA
    }else if (input$wvpfPrelims & !input$wvpfChallenges & !input$wvpfPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$wvpfPrelims & input$wvpfChallenges & !input$wvpfPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$wvpfPrelims & !input$wvpfChallenges & input$wvpfPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
        
    #Plot text if there is no event in the selection
    if (is.na(EventType)){
      p=girafe(ggobj = plotNoEventType )
      
      #Plot text if there is no event in the selection
    } else if (nrow(dataset)==0){
      p=girafe(ggobj = plotEmptyDf )
      
      #Get the win rate data by archetype and create a graph displaying
      #win rate based on presence
    } else {
      metric_df=metric_points_archetypes(dataset)
      
      p = metric_graph(metric_df,input$wvpfPresence,input$diameters,input$diam_ratio,
                       as.Date(input$wvpfDates1, format="%Y-%m-%d", origin ="1970-01-01"),
                       as.Date(input$wvpfDates2, format="%Y-%m-%d", origin ="1970-01-01"),
                       input$isLog,input$wvpfShare,EventType,input$wvpfFormat)
    }
    
    #Return the graph to be plotted
    print(p)
    
  })
  
  #Plot the representation of the win rate depending on presence for the most
  #present archetypes, with clustering
  output$plotWvpmp = renderGirafe({
    
    #Copy the first loaded data for the chosen format
    if (input$wvpmpFormat=="Modern"){
      dataset=dfModern
    }else if (input$wvpmpFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wvpmpFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wvpmpFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wvpmpDates1 &
                      dataset$Date <= input$wvpmpDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$wvpmpPrelims & !input$wvpmpChallenges & !input$wvpmpPaper){
      dataset=NA
      EventType=NA
    }else if (input$wvpmpPrelims & !input$wvpmpChallenges & !input$wvpmpPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$wvpmpPrelims & input$wvpmpChallenges & !input$wvpmpPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$wvpmpPrelims & !input$wvpmpChallenges & input$wvpmpPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Plot text if there is no event in the selection
    if (is.na(EventType)){
      p=girafe(ggobj = plotNoEventType )
      
      #Plot text if there is no row in the data frame
    } else if (nrow(dataset)==0){
      p=girafe(ggobj = plotEmptyDf )
      
      #Get the win rate data by archetype and create a graph displaying
      #win rate based on presence (clustering them, only for the most present)
    } else {
      metric_df=metric_points_archetypes(dataset)
      
      p = kmeans_arch(metric_df,input$wvpmpNoClusters,30,50,"Hartigan-Wong",
                      as.Date(input$wvpmpDates1, format="%Y-%m-%d", origin ="1970-01-01"),
                      as.Date(input$wvpmpDates2, format="%Y-%m-%d", origin ="1970-01-01")
                      ,TRUE,TRUE,EventType,input$wvpmpFormat)

    }
    
    #Return the graph to be plotted
    print(p)
    
  })
  
  #Plot the result of the combination of win rate and presence to rank decks
  output$plotLc = renderGirafe({
    
    #Copy the first loaded data for the chosen format
    if (input$lcFormat=="Modern"){
      dataset=dfModern
    }else if (input$lcFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$lcFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$lcFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$lcDates1 &
                      dataset$Date <= input$lcDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$lcPrelims & !input$lcChallenges & !input$lcPaper){
      dataset=NA
      EventType=NA
    }else if (input$lcPrelims & !input$lcChallenges & !input$lcPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$lcPrelims & input$lcChallenges & !input$lcPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$lcPrelims & !input$lcChallenges & input$lcPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Plot text if there is no event in the selection
    if (is.na(EventType)){
      p=plotNoEventType
      
      #Plot text if there is no row in the data frame
    } else if (nrow(dataset)==0){
      p=plotEmptyDf
      
      #Plot the result of the combination of win rate and presence (linear)
    } else {
      metric_df=metric_points_archetypes(dataset)
      
      # metric_df_sub = metric_df[
      #   metric_df$TotalMatches > input$lcHistShare*sum(metric_df$TotalMatches), ]
      
      metric_df_log_matches=metric_df
      
      metric_df_log_matches$TotalMatches=
        log(metric_df_log_matches$TotalMatches)
      
      arch_ranked_sub=archetypes_ranking(metric_df_log_matches,
                                         as.Date(input$lcDates1, format="%Y-%m-%d", origin ="1970-01-01"),
                                         as.Date(input$lcDates2, format="%Y-%m-%d", origin ="1970-01-01"),
                                         input$lcPresence_Weight, input$lcPPR_Weight)
      
      p = log_comb_graph(df=dataset,arch_ranked=arch_ranked_sub,
                         beginning=as.Date(input$lcDates1, format="%Y-%m-%d", origin ="1970-01-01"),
                         end=as.Date(input$lcDates2, format="%Y-%m-%d", origin ="1970-01-01"),
                         EventType=EventType,HistShare=input$lcHistShare, input$lcPresence_Weight, 
                         input$lcPPR_Weight,input$presenceFormat)
    }
    
    #Return the graph to be plotted
    print(p)
    
  })
  
  #Return a table with the characteristics of each archetype
  output$archetypeTable = DT::renderDataTable({
    
    #Copy the first loaded data for the chosen format
    if (input$dtFormat=="Modern"){
      dataset=dfModern
    }else if (input$dtFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$dtFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$dtFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$dtDates1 &
                      dataset$Date <= input$dtDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$dtPrelims & !input$dtChallenges & !input$dtPaper){
      dataset=NA
      EventType=NA
    }else if (input$dtPrelims & !input$dtChallenges & !input$dtPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$dtPrelims & input$dtChallenges & !input$dtPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$dtPrelims & !input$dtChallenges & input$dtPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Return a table with a message if there is no event in the selection
    if (is.na(EventType)){
      metric_df=as.data.frame(tabNoEventType)
      
      #Return a table with a message if there is no row in the data frame
    } else if (nrow(dataset)==0){
      metric_df=as.data.frame(tabEmptyDf)
      
      #Get the table of results for each archetype
    } else {
      metric_df=metric_points_archetypes(dataset)
      colnames(metric_df) = c('Archetype Name', 
                              paste('Number of copies (out of',sum(metric_df$NoCopies),'copies)'), 
                              paste('Number of different players (out of',sum(metric_df$NoPlayers),'players)'), 
                             paste('Number of matches played (out of',sum(metric_df$TotalMatches),'matches)'),
                             'Actual winrate',
                             'Upper bound of the 95% confidence interval on the winrate',
                             'Lower bound of the 95% confidence interval on the winrate',
                             'Average number of matches per player')
      metric_df[['Actual winrate']]=metric_df[['Actual winrate']]
      metric_df[['Upper bound of the 95% confidence interval on the winrate']]=
        metric_df[['Upper bound of the 95% confidence interval on the winrate']]
      metric_df[['Lower bound of the 95% confidence interval on the winrate']]=
        metric_df[['Lower bound of the 95% confidence interval on the winrate']]
    }
    
    metric_df
    
  })
  
  #Downloadable CSV of the archetype data
  output$archetypeTableDL = downloadHandler(
    
    filename = function() {
      #Provide the right name to the type of chosen events
      #and filter to keep only the selected events
      EventType="Official Competitions"
      if (!input$dtPrelims & !input$dtChallenges & !input$dtPaper){
        dataset=NA
        EventType=NA
      }else if (input$dtPrelims & !input$dtChallenges & !input$dtPaper){
        dataset=dataset[grep("Preliminary", dataset$Tournament), ]
        EventType="Preliminaries"
      }else if (!input$dtPrelims & input$dtChallenges & !input$dtPaper){
        MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
        dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
        EventType="Major Official Events"
      }else if (!input$dtPrelims & !input$dtChallenges & input$dtPaper){
        PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
        dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
        EventType="Major Official Paper Events"
      }
      
      nameWoExt=paste("MTGO",input$dtFormat,EventType,"archetype_data_from", 
                      input$dtDates1,"to", input$dtDates2, sep = "_")
      
      paste(nameWoExt,".csv",sep="")
    },
    content = function(file) {
      
      #Copy the first loaded data for the chosen format
      if (input$dtFormat=="Modern"){
        dataset=dfModern
      }else if (input$dtFormat=="Pioneer"){
        dataset=dfPioneer
      }else if (input$dtFormat=="Pauper"){
        dataset=dfPauper
      }else if (input$dtFormat=="Legacy"){
        dataset=dfLegacy
      }
      
      #Provide the right name to the type of chosen events
      #and filter to keep only the selected events
      EventType="Official Competitions"
      if (!input$dtPrelims & !input$dtChallenges & !input$dtPaper){
        dataset=NA
        EventType=NA
      }else if (input$dtPrelims & !input$dtChallenges & !input$dtPaper){
        dataset=dataset[grep("Preliminary", dataset$Tournament), ]
        EventType="Preliminaries"
      }else if (!input$dtPrelims & input$dtChallenges & !input$dtPaper){
        MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
        dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
        EventType="Major Official Events"
      }else if (!input$dtPrelims & !input$dtChallenges & input$dtPaper){
        PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
        dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
        EventType="Major Official Paper Events"
      }
      
      #Return a table with a message if there is no event in the selection
      if (is.na(EventType)){
        metric_df=as.data.frame(tabNoEventType)
        
        #Return a table with a message if there is no row in the data frame
      } else if (nrow(dataset)==0){
        metric_df=as.data.frame(tabEmptyDf)
        
        #Get the table of results for each archetype
      } else {
        metric_df=metric_points_archetypes(dataset)
      }
      
      write.csv(metric_df, file, row.names = TRUE)
    }
    
  )
  
  #Data of the events
  output$eventInfo = renderText({
    
    #Copy the first loaded data for the chosen format
    if (input$eiFormat=="Modern"){
      dataset=dfModern
    }else if (input$eiFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$eiFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$eiFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$eiDates1 &
                      dataset$Date <= input$eiDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$eiPrelims & !input$eiChallenges & !input$eiPaper){
      dataset=NA
      EventType=NA
    }else if (input$eiPrelims & !input$eiChallenges & !input$eiPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$eiPrelims & input$eiChallenges & !input$eiPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$eiPrelims & !input$eiChallenges & input$eiPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Return a message if there is no event in the selection
    if (is.na(EventType)){
      eventInfo=noEventType
      
      #Return a message if there is no row in the data frame
    } else if (nrow(dataset)==0){
      eventInfo=emptyDf
      
      #Get the table of results for each archetype
    } else {
      
      nbDecks=length(dataset$AnchorUri)
      nbDiffPlayers=length(unique(dataset$Player))
      nbExactArch=length(unique(dataset$Archetype$Archetype))
      nbSuperArch=length(unique(dataset$Archetype$SuperArchetype))
      totNbRounds=sum(dataset$NRounds)+sum(dataset$T8Matches)
      avgNbRounds=as.numeric(format(round(sum(dataset$NRounds)/length(dataset$AnchorUri),2),nsmall=2))
      minNbRounds=min(dataset$NRounds)
      maxNbRounds=max(dataset$NRounds)
      nbEvents=length(unique(dataset$TournamentName))
      
      eventInfo=paste("QUICK ANALYSIS OF THE DATA", 
                      "\nBeginning: ", input$eiDates1,
                      "\nEnd: ", input$eiDates2,
                      "\nFormat: ", input$eiFormat,
                      "\nType of MTGO events: ", EventType,
                      "\nNumber of decks in the data: ",nbDecks,
                      "\nNumber of different players in the data: ",nbDiffPlayers,
                      "\nNumber of exact archetypes in the data: ",nbExactArch,
                      "\nNumber of rounds played in the data (with top8): ",totNbRounds,
                      "\nAverage number of rounds in the data (w/o top8): ",avgNbRounds,
                      "\nMinimum number of rounds in the data (w/o top8): ",minNbRounds,
                      "\nMaximum number of rounds in the data (w/o top8): ",maxNbRounds,
                      "\nNumber of events in the data: ", nbEvents,
                      sep="")
    }
    
    eventInfo
    
    
  })
  
  #URL of the events
  output$eventURL = DT::renderDataTable({
    
    #Copy the first loaded data for the chosen format
    if (input$euFormat=="Modern"){
      dataset=dfModern
    }else if (input$euFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$euFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$euFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$euDates1 &
                      dataset$Date <= input$euDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$euPrelims & !input$euChallenges & !input$euPaper){
      dataset=NA
      EventType=NA
    }else if (input$euPrelims & !input$euChallenges & !input$euPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$euPrelims & input$euChallenges & !input$euPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$euPrelims & !input$euChallenges & input$euPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Return a table with a message if there is no event in the selection
    if (is.na(EventType)){
      coveredEvents=as.data.frame(tabNoEventType)
      
      #Return a table with a message if there is no row in the data frame
    } else if (nrow(dataset)==0){
      coveredEvents=as.data.frame(tabEmptyDf)
      
      #Get the table of results for each archetype
    } else {
      nbEvents=length(unique(dataset$TournamentName))
      
      coveredEvents=setNames(data.frame(matrix(ncol=2, nrow=nbEvents)), 
                             c("TournamentName", "URL"))
      coveredEvents$TournamentName=unique(dataset$TournamentName)
      for (i in 1:nbEvents){
        coveredEvents$URL[i]=dataset[dataset$TournamentName==
                                       coveredEvents$TournamentName[i],]$AnchorUri[1]
        coveredEvents$URL[i]<-gsub("#.*","",coveredEvents$URL[i])
        
        coveredEvents$TournamentName[i]<-paste('<a href="',coveredEvents$URL[i],
                                               '" target="_blank" class="btn btn-primary">',
                                               coveredEvents$TournamentName[i],'</a>',sep="")
        }
    }
    
    coveredEvents
  }, escape = FALSE)
  
  #Creates an input with the list of archetypes corresponding to other inputs
  output$dlUiArchetypes <- renderUI({
    #Copy the first loaded data for the chosen format
    if (input$dlFormat=="Modern"){
      dataset=dfModern
    }else if (input$dlFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$dlFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$dlFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$dlDates1 &
                      dataset$Date <= input$dlDates2,]
    
    selectInput(inputId = "dlArchetype", 
                label = h4("Archetype"), 
                choices = sort(as.character(dataset$Archetype$Archetype)))
  })
  
  #Return a table with the characteristics of each decklist
  output$decklistTable = DT::renderDataTable({
    
    #Copy the first loaded data for the chosen format
    if (input$dlFormat=="Modern"){
      dataset=dfModern
    }else if (input$dlFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$dlFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$dlFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$dlDates1 &
                      dataset$Date <= input$dlDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$dlPrelims & !input$dlChallenges & !input$dlPaper){
      dataset=NA
      EventType=NA
    }else if (input$dlPrelims & !input$dlChallenges & !input$dlPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$dlPrelims & input$dlChallenges & !input$dlPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$dlPrelims & !input$dlChallenges & input$dlPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Return a table with a message if there is no event in the selection
    if (is.na(EventType)){
      decklistsDf=as.data.frame(tabNoEventType)
      
      #Return a table with a message if the dataset is empty
    } else if (nrow(dataset)==0){
      decklistsDf=as.data.frame(tabEmptyDf)
      
      #Return a message if there is no archetype selected
    } else if (is.null(input$dlArchetype) || input$dlArchetype==""){
      decklistsDf=as.data.frame(emptyArch)
      
      #Get the table of results for each decklist
    } else {
      #Display a table with all the cards of the archetype and their average counts
      if (input$dlDisplay=="Breakdown"){
        #Keep only data of the chosen archetype
        dataset=dataset[dataset$Archetype$Archetype==input$dlArchetype,]
        
        #Keep only the most recent results with the most points
        if(input$dlOnlyBest){
          dataset=dataset[dataset$Points+dataset$T8Points==
                            max(dataset$Points+dataset$T8Points),]
          dataset=dataset[dataset$Date==max(dataset$Date),]
        }
        
        decklistsDf=archAverageData(dataset,cardDataSub)
        
        for (i in 3:6){
          decklistsDf[,i]=as.numeric(decklistsDf[,i])
        }
        
        #Display the URL of the archetype
      }else if (input$dlDisplay=="URL"){
        #Keep only data of the chosen archetype
        dataset=dataset[dataset$Archetype$Archetype==input$dlArchetype,]
        
        #Keep only the most recent results with the most points
        if(input$dlOnlyBest){
          dataset=dataset[dataset$Points+dataset$T8Points==
                            max(dataset$Points+dataset$T8Points),]
          dataset=dataset[dataset$Date==max(dataset$Date),]
        }
        
        nbLists=nrow(dataset)
        
        decklistsDf=setNames(data.frame(matrix(ncol=3, nrow=nbLists)), 
                             c("TournamentName", "PlayerName", "URL"))
        decklistsDf$URL=dataset$AnchorUri
        decklistsDf$PlayerName=dataset$Player
        decklistsDf$TournamentName=dataset$TournamentName
        for (i in 1:nbLists){
          
          decklistsDf$TournamentName[i]<-paste('<a href="',decklistsDf$URL[i],
                                               '" target="_blank" class="btn btn-primary">',
                                               decklistsDf$TournamentName[i],'</a>',sep="")
        }
      }
    }
    
    DT::datatable(decklistsDf, escape = FALSE)
    
  }, escape = FALSE)
  
  #Downloadable CSV of the decklist data
  output$decklistTableDL = downloadHandler(

    filename = function() {
      #Provide the right name to the type of chosen events
      #and filter to keep only the selected events
      EventType="Official Competitions"
      if (!input$dlPrelims & !input$dlChallenges & !input$dlPaper){
        dataset=NA
        EventType=NA
      }else if (input$dlPrelims & !input$dlChallenges & !input$dlPaper){
        dataset=dataset[grep("Preliminary", dataset$Tournament), ]
        EventType="Preliminaries"
      }else if (!input$dlPrelims & input$dlChallenges & !input$dlPaper){
        MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
        dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
        EventType="Major Official Events"
      }else if (!input$dlPrelims & !input$dlChallenges & input$dlPaper){
        PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
        dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
        EventType="Major Official Paper Events"
      }
      
      nameWoExt=paste("MTGO",input$dlFormat,EventType,"decklist_data_from", 
                      input$dlDates1,"to", input$dlDates2, sep = "_")
      
      paste(nameWoExt,".csv",sep="")
    },
    content = function(file) {
      
      #Copy the first loaded data for the chosen format
      if (input$dlFormat=="Modern"){
        dataset=dfModern
      }else if (input$dlFormat=="Pioneer"){
        dataset=dfPioneer
      }else if (input$dlFormat=="Pauper"){
        dataset=dfPauper
      }else if (input$dlFormat=="Legacy"){
        dataset=dfLegacy
      }
      
      #Keep only the data between the selected dates
      dataset=dataset[dataset$Date >= input$dlDates1 &
                        dataset$Date <= input$dlDates2,]
      
      #Keep only data of the chosen archetype
      dataset=dataset[dataset$Archetype$Archetype==input$dlArchetype,]
      
      #Keep only the most recent results with the most points
      if(input$dlOnlyBest){
        dataset=dataset[dataset$Points+dataset$T8Points==
                          max(dataset$Points+dataset$T8Points),]
        dataset=dataset[dataset$Date==max(dataset$Date),]
      }
      
      #Provide the right name to the type of chosen events
      #and filter to keep only the selected events
      EventType="Official Competitions"
      if (!input$dlPrelims & !input$dlChallenges & !input$dlPaper){
        dataset=NA
        EventType=NA
      }else if (input$dlPrelims & !input$dlChallenges & !input$dlPaper){
        dataset=dataset[grep("Preliminary", dataset$Tournament), ]
        EventType="Preliminaries"
      }else if (!input$dlPrelims & input$dlChallenges & !input$dlPaper){
        MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
        dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
        EventType="Major Official Events"
      }else if (!input$dlPrelims & !input$dlChallenges & input$dlPaper){
        PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
        dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
        EventType="Major Official Paper Events"
      }
      
      #Return a table with a message if there is no event in the selection
      if (is.na(EventType)){
        decklistsDf=as.data.frame(tabNoEventType)
        
        #Return a table with a message if there is no row in the data frame
      } else if (nrow(dataset)==0){
        decklistsDf=as.data.frame(tabEmptyDf)
        
        #Get the table of results for each decklist
      } else {
        #Display a table with all the cards of the archetype and their average counts
        if (input$dlDisplay=="Breakdown"){
          
          decklistsDf=archAverageData(dataset) #TODO: update the function
          
          #Display the URL of the archetype
        }else if (input$dlDisplay=="URL"){
          nbEvents=length(unique(dataset$TournamentName))
          
          decklistsDf=setNames(data.frame(matrix(ncol=3, nrow=nbEvents)), 
                               c("TournamentName", "PlayerName", "URL"))
          decklistsDf$TournamentName=unique(dataset$TournamentName)
          for (i in 1:nbEvents){
            decklistsDf$URL[i]=dataset[dataset$TournamentName==
                                         decklistsDf$TournamentName[i],]$AnchorUri[1]
            
            decklistsDf$PlayerName[i]=dataset$Player[i]
            
            decklistsDf$TournamentName[i]<-paste('<a href="',decklistsDf$URL[i],
                                                 '" target="_blank" class="btn btn-primary">',
                                                 decklistsDf$TournamentName[i],'</a>',sep="")
          }
        }
      }
      
      write.csv(decklistsDf, file, row.names = TRUE)
    }
    
  )
  
  wrcMDUiArchetypesChoices <- reactive({
    #Copy the first loaded data for the chosen format
    if (input$wrcMDFormat=="Modern"){
      dataset=dfModern
    }else if (input$wrcMDFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wrcMDFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wrcMDFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wrcMDDates1 &
                      dataset$Date <= input$wrcMDDates2,]
    
    sort(as.character(dataset$Archetype$Archetype))
  })
  
  #Creates an input with the list of archetypes corresponding to other inputs
  output$wrcMDUiArchetypes <- renderUI({
    
    selectInput(inputId = "wrcMDArchetype", 
                label = h4("Archetype"), 
                choices = c("Select" = "",wrcMDUiArchetypesChoices()))
  })
  
  wrcMDUiCardsChoices <- reactive({
    #Copy the first loaded data for the chosen format
    if (input$wrcMDFormat=="Modern"){
      dataset=dfModern
    }else if (input$wrcMDFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wrcMDFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wrcMDFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wrcMDDates1 &
                      dataset$Date <= input$wrcMDDates2,]
    
    #Keep only data of the chosen archetype
    dataset=dataset[dataset$Archetype$Archetype==input$wrcMDArchetype,]
    
    sort(unique(c(unlist(dataset$MainboardCardNames))))
  })
  
  #Creates an input with the list of cards corresponding to other inputs
  output$wrcMDUiCards <- renderUI({
    
    selectizeInput(inputId = "wrcMDCard", 
                label = h4("Card"), 
                choices = c("Select" = "",wrcMDUiCardsChoices()))
  })
  
  #Archetype data depending on number of copies of cards in MD
  output$winratecomparisonMDText = renderText({
    
    #Copy the first loaded data for the chosen format
    if (input$wrcMDFormat=="Modern"){
      dataset=dfModern
    }else if (input$wrcMDFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wrcMDFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wrcMDFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wrcMDDates1 &
                      dataset$Date <= input$wrcMDDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$wrcMDPrelims & !input$wrcMDChallenges & !input$wrcMDPaper){
      dataset=NA
      EventType=NA
    }else if (input$wrcMDPrelims & !input$wrcMDChallenges & !input$wrcMDPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$wrcMDPrelims & input$wrcMDChallenges & !input$wrcMDPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$wrcMDPrelims & !input$wrcMDChallenges & input$wrcMDPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Return a message if there is no event in the selection
    if (is.na(EventType)){
      winrateComparisonMD=noEventType
      
      #Return a message if there is no row in the data frame
    } else if (nrow(dataset)==0){
      winrateComparisonMD=emptyDf
      
      #Return a message if there is no archetype selected
    } else if (is.null(input$wrcMDArchetype) || input$wrcMDArchetype==""){
      winrateComparisonMD="Please select an archetype."
      
      #Return a message if there is no card selected
    } else if (is.null(input$wrcMDCard) || input$wrcMDCard==""){
      winrateComparisonMD="Please select a card."
      
      #Get the table of results for each archetype
    } else {
      
      ArchetypeName=input$wrcMDArchetype
      CardToLookFor=input$wrcMDCard
      ArchetypeDf=dataset[dataset$Archetype$Archetype==ArchetypeName,]
      
      #Stats without the card
      ArchetypeWoTheCard=ArchetypeDf[-grep(CardToLookFor,ArchetypeDf$Mainboard),]
      if (sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)==0){
        WinrateWoTheCard=0
      }else{
        WinrateWoTheCard=sum(ArchetypeWoTheCard$Points+ArchetypeWoTheCard$T8Points)/
          3/sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)
      } 
      WoTheCard=paste("The winrate of ", ArchetypeName,
                      " decks <strong>without</strong> ", CardToLookFor," is <strong>",
                      format(round(WinrateWoTheCard*100, 2),nsmall = 2)," %</strong> for ",
                      nrow(ArchetypeWoTheCard)," decklists between ", input$wrcMDDates1,
                      " and ", input$wrcMDDates2,".<br><br>",sep = "")
      
      WithTheCard=c()
      
      #Stats with any number of the card when it is present
      ArchetypeWithTheCard=ArchetypeDf[grep(CardToLookFor,ArchetypeDf$Mainboard),]
      for (i in 1:4){
        ArchetypeWithTheCardi=ArchetypeWithTheCard[0,]
        
        #Goes over each row of the dataframe
        for (j in 1:nrow(ArchetypeWithTheCard)){
          
          #Check all the cards in that row
          for (k in 1:length(ArchetypeWithTheCard$MainboardCardNames[[j]])){
            
            if (ArchetypeWithTheCard$MainboardCardNames[[j]][k]==
                CardToLookFor 
                && 
                ArchetypeWithTheCard$MainboardCardCounts[[j]][k]==
                i){
              
              ArchetypeWithTheCardi=rbind(ArchetypeWithTheCardi,
                                          ArchetypeWithTheCard[j,])
              
            }
          }
          
          if (sum(ArchetypeWithTheCardi$NRounds+ArchetypeWithTheCardi$T8Matches)==0){
            WinrateWithTheCardi=0
          }else{
            WinrateWithTheCardi=sum(ArchetypeWithTheCardi$Points+ArchetypeWithTheCardi$T8Points)/
              3/sum(ArchetypeWithTheCardi$NRounds+ArchetypeWithTheCardi$T8Matches)
          }
        }
        
        WithTheCardi=paste("The winrate of ", ArchetypeName," decks <strong> with ", 
                           i," </strong> ", CardToLookFor," is <strong>",
                           format(round(WinrateWithTheCardi*100, 2), nsmall = 2),
                           " %</strong> for ",nrow(ArchetypeWithTheCardi),
                           " decklists between ", input$wrcMDDates1," and ", 
                           input$wrcMDDates2,".<br><br>",sep = "")
        WithTheCard=c(WithTheCard,WithTheCardi)
        
      }
      
      #Get the ID on the card picture to fetch on Scryfall
      if (CardToLookFor %in% cardDataSub$name){
        scryfallId=cardDataSub[cardDataSub$name==CardToLookFor,]$scryfallId[1]
      }else if (CardToLookFor %in% cardDataSub$faceName){
        scryfallId=cardDataSub[cardDataSub$faceName==CardToLookFor,]$scryfallId[1]
      }
      picSrc=paste("https://api.scryfall.com/cards/",scryfallId,"?format=image",sep="")
      
      #Magic card size: 63x88mm
      picDisplay=c('<img src="',picSrc,'" width="315" height="440">')
      #picLink=paste("<a href=",picSrc,">Card picture</a>",sep="")
      
      WithTheCard=c(WoTheCard,WithTheCard)
      
      winrateComparisonMD=paste(WithTheCard,collapse='\n')
      # winrateComparisonMD=paste(winrateComparisonMD,picDisplay,sep='\n',collapse='\n')
      # winrateComparisonMD=picDisplay
    }
    
    winrateComparisonMD
    
    
  })
  
  #Archetype data depending on number of copies of cards in MD
  output$wcMDPicture = renderText({
    
    #Copy the first loaded data for the chosen format
    if (input$wrcMDFormat=="Modern"){
      dataset=dfModern
    }else if (input$wrcMDFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wrcMDFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wrcMDFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wrcMDDates1 &
                      dataset$Date <= input$wrcMDDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$wrcMDPrelims & !input$wrcMDChallenges & !input$wrcMDPaper){
      dataset=NA
      EventType=NA
    }else if (input$wrcMDPrelims & !input$wrcMDChallenges & !input$wrcMDPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$wrcMDPrelims & input$wrcMDChallenges & !input$wrcMDPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$wrcMDPrelims & !input$wrcMDChallenges & input$wrcMDPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Return a message if there is no event in the selection
    if (is.na(EventType)){
      wcMDPicture=""
      
      #Return a message if there is no row in the data frame
    } else if (nrow(dataset)==0){
      wcMDPicture=""
      
      #Return a message if there is no archetype selected
    } else if (is.null(input$wrcMDArchetype) || input$wrcMDArchetype==""){
      wcMDPicture=""
      
      #Return a message if there is no card selected
    } else if (is.null(input$wrcMDCard) || input$wrcMDCard==""){
      wcMDPicture=""
      
      #Get the table of results for each archetype
    } else {
      
      ArchetypeName=input$wrcMDArchetype
      CardToLookFor=input$wrcMDCard
      ArchetypeDf=dataset[dataset$Archetype$Archetype==ArchetypeName,]
      
      #Stats without the card
      ArchetypeWoTheCard=ArchetypeDf[-grep(CardToLookFor,ArchetypeDf$Mainboard),]
      if (sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)==0){
        WinrateWoTheCard=0
      }else{
        WinrateWoTheCard=sum(ArchetypeWoTheCard$Points+ArchetypeWoTheCard$T8Points)/
          3/sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)
      } 
      WoTheCard=paste("The winrate of ", ArchetypeName," decks <strong>without</strong> ", 
                      CardToLookFor," is <strong>",format(round(WinrateWoTheCard*100, 2), 
                                                          nsmall = 2)," %</strong> for ",
                      nrow(ArchetypeWoTheCard)," decklists between ", input$wrcMDDates1,
                      " and ", input$wrcMDDates2,".<br><br>",sep = "")
      
      WithTheCard=c()
      
      #Stats with any number of the card when it is present
      ArchetypeWithTheCard=ArchetypeDf[grep(CardToLookFor,ArchetypeDf$Mainboard),]
      for (i in 1:4){
        ArchetypeWithTheCardi=ArchetypeWithTheCard[0,]
        
        #Goes over each row of the dataframe
        for (j in 1:nrow(ArchetypeWithTheCard)){
          
          #Check all the cards in that row
          for (k in 1:length(ArchetypeWithTheCard$MainboardCardNames[[j]])){
            
            if (ArchetypeWithTheCard$MainboardCardNames[[j]][k]==
                CardToLookFor 
                && 
                ArchetypeWithTheCard$MainboardCardCounts[[j]][k]==
                i){
              
              ArchetypeWithTheCardi=rbind(ArchetypeWithTheCardi,
                                          ArchetypeWithTheCard[j,])
              
            }
          }
          
          if (sum(ArchetypeWithTheCardi$NRounds+ArchetypeWithTheCardi$T8Matches)==0){
            WinrateWithTheCardi=0
          }else{
            WinrateWithTheCardi=sum(ArchetypeWithTheCardi$Points+ArchetypeWithTheCardi$T8Points)/
              3/sum(ArchetypeWithTheCardi$NRounds+ArchetypeWithTheCardi$T8Matches)
          }
        }
        
        WithTheCardi=paste("The winrate of ", ArchetypeName," decks <strong> with ", 
                           i," </strong> ", CardToLookFor," is <strong>",
                           format(round(WinrateWithTheCardi*100, 2), nsmall = 2),
                           " %</strong> for ",nrow(ArchetypeWithTheCardi),
                           " decklists between ", input$wrcMDDates1," and ", 
                           input$wrcMDDates2,".<br><br>",sep = "")
        WithTheCard=c(WithTheCard,WithTheCardi)
        
      }
      
      
      #Get the ID on the card picture to fetch on Scryfall
      if (CardToLookFor %in% cardDataSub$name){
        scryfallId=cardDataSub[cardDataSub$name==CardToLookFor,]$scryfallId[1]
      }else if (CardToLookFor %in% cardDataSub$faceName){
        scryfallId=cardDataSub[cardDataSub$faceName==CardToLookFor,]$scryfallId[1]
      }
      picSrc=paste("https://api.scryfall.com/cards/",scryfallId,"?format=image",sep="")
      
      #Magic card size: 63x88mm
      picDisplay=c('<img src="',picSrc,'" width="315" height="440">')
      wcMDPicture=picDisplay
    }
    
    wcMDPicture
    
  })
  
  wrcSBUiArchetypesChoices <- reactive({
    #Copy the first loaded data for the chosen format
    if (input$wrcSBFormat=="Modern"){
      dataset=dfModern
    }else if (input$wrcSBFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wrcSBFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wrcSBFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wrcSBDates1 &
                      dataset$Date <= input$wrcSBDates2,]
    
    sort(as.character(dataset$Archetype$Archetype))
  })
  
  #Creates an input with the list of archetypes corresponding to other inputs
  output$wrcSBUiArchetypes <- renderUI({
    
    selectInput(inputId = "wrcSBArchetype", 
                label = h4("Archetype"), 
                choices = c("Select" = "",wrcSBUiArchetypesChoices()))
  })
  
  wrcSBUiCardsChoices <- reactive({
    #Copy the first loaded data for the chosen format
    if (input$wrcSBFormat=="Modern"){
      dataset=dfModern
    }else if (input$wrcSBFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wrcSBFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wrcSBFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wrcSBDates1 &
                      dataset$Date <= input$wrcSBDates2,]
    
    #Keep only data of the chosen archetype
    dataset=dataset[dataset$Archetype$Archetype==input$wrcSBArchetype,]
    
    sort(unique(c(unlist(dataset$SideboardCardNames))))
  })
  
  #Creates an input with the list of cards corresponding to other inputs
  output$wrcSBUiCards <- renderUI({
    
    selectizeInput(inputId = "wrcSBCard", 
                   label = h4("Card"), 
                   choices = c("Select" = "",wrcSBUiCardsChoices()))
  })
  
  #Archetype data depending on number of copies of cards in SB
  output$winratecomparisonSBText = renderText({
    
    #Copy the first loaded data for the chosen format
    if (input$wrcSBFormat=="Modern"){
      dataset=dfModern
    }else if (input$wrcSBFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wrcSBFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wrcSBFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wrcSBDates1 &
                      dataset$Date <= input$wrcSBDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$wrcSBPrelims & !input$wrcSBChallenges & !input$wrcSBPaper){
      dataset=NA
      EventType=NA
    }else if (input$wrcSBPrelims & !input$wrcSBChallenges & !input$wrcSBPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$wrcSBPrelims & input$wrcSBChallenges & !input$wrcSBPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$wrcSBPrelims & !input$wrcSBChallenges & input$wrcSBPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Return a message if there is no event in the selection
    if (is.na(EventType)){
      winrateComparisonSB=noEventType
      
      #Return a message if there is no row in the data frame
    } else if (nrow(dataset)==0){
      winrateComparisonSB=emptyDf
      
      #Return a message if there is no archetype selected
    } else if (is.null(input$wrcSBArchetype) || input$wrcSBArchetype==""){
      winrateComparisonSB="Please select an archetype."
      
      #Return a message if there is no card selected
    } else if (is.null(input$wrcSBCard) || input$wrcSBCard==""){
      winrateComparisonSB="Please select a card."
      
      #Get the table of results for each archetype
    } else {
      
      ArchetypeName=input$wrcSBArchetype
      CardToLookFor=input$wrcSBCard
      ArchetypeDf=dataset[dataset$Archetype$Archetype==ArchetypeName,]
      
      #Stats without the card
      ArchetypeWoTheCard=ArchetypeDf[-grep(CardToLookFor,ArchetypeDf$Sideboard),]
      if (sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)==0){
        WinrateWoTheCard=0
      }else{
        WinrateWoTheCard=sum(ArchetypeWoTheCard$Points+ArchetypeWoTheCard$T8Points)/
          3/sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)
      } 
      WoTheCard=paste("The winrate of ", ArchetypeName," decks <strong>without</strong> ", 
                      CardToLookFor," is <strong>",format(round(WinrateWoTheCard*100, 2), 
                                                          nsmall = 2)," %</strong> for ",
                      nrow(ArchetypeWoTheCard)," decklists between ", input$wrcSBDates1,
                      " and ", input$wrcSBDates2,".<br><br>",sep = "")
      
      WithTheCard=c()
      
      #Stats with any number of the card when it is present
      ArchetypeWithTheCard=ArchetypeDf[grep(CardToLookFor,ArchetypeDf$Sideboard),]
      for (i in 1:4){
        ArchetypeWithTheCardi=ArchetypeWithTheCard[0,]
        
        #Goes over each row of the dataframe
        for (j in 1:nrow(ArchetypeWithTheCard)){
          
          #Check all the cards in that row
          for (k in 1:length(ArchetypeWithTheCard$SideboardCardNames[[j]])){
            
            if (ArchetypeWithTheCard$SideboardCardNames[[j]][k]==
                CardToLookFor 
                && 
                ArchetypeWithTheCard$SideboardCardCounts[[j]][k]==
                i){
              
              ArchetypeWithTheCardi=rbind(ArchetypeWithTheCardi,
                                          ArchetypeWithTheCard[j,])
              
            }
          }
          
          if (sum(ArchetypeWithTheCardi$NRounds+ArchetypeWithTheCardi$T8Matches)==0){
            WinrateWithTheCardi=0
          }else{
            WinrateWithTheCardi=sum(ArchetypeWithTheCardi$Points+ArchetypeWithTheCardi$T8Points)/
              3/sum(ArchetypeWithTheCardi$NRounds+ArchetypeWithTheCardi$T8Matches)
          }
        }
        
        WithTheCardi=paste("The winrate of ", ArchetypeName," decks <strong> with ", 
                           i," </strong> ", CardToLookFor," is <strong>",
                           format(round(WinrateWithTheCardi*100, 2), nsmall = 2),
                           " %</strong> for ",nrow(ArchetypeWithTheCardi),
                           " decklists between ", input$wrcSBDates1," and ", 
                           input$wrcSBDates2,".<br><br>",sep = "")
        WithTheCard=c(WithTheCard,WithTheCardi)
        
      }
      
      
      #Get the ID on the card picture to fetch on Scryfall
      if (CardToLookFor %in% cardDataSub$name){
        scryfallId=cardDataSub[cardDataSub$name==CardToLookFor,]$scryfallId[1]
      }else if (CardToLookFor %in% cardDataSub$faceName){
        scryfallId=cardDataSub[cardDataSub$faceName==CardToLookFor,]$scryfallId[1]
      }
      picSrc=paste("https://api.scryfall.com/cards/",scryfallId,"?format=image",sep="")
      
      #Magic card size: 63x88mm
      picDisplay=c('<img src="',picSrc,'" width="315" height="440">')
      #picLink=paste("<a href=",picSrc,">Card picture</a>",sep="")
      
      WithTheCard=c(WoTheCard,WithTheCard)
      
      winrateComparisonSB=paste(WithTheCard,collapse='\n')
      # winrateComparisonSB=paste(winrateComparisonSB,picDisplay,sep='\n',collapse='\n')
      # winrateComparisonSB=picDisplay
    }
    
    winrateComparisonSB
    
  })
  
  #Archetype data depending on number of copies of cards in SB
  output$wcSBPicture = renderText({
    
    #Copy the first loaded data for the chosen format
    if (input$wrcSBFormat=="Modern"){
      dataset=dfModern
    }else if (input$wrcSBFormat=="Pioneer"){
      dataset=dfPioneer
    }else if (input$wrcSBFormat=="Pauper"){
      dataset=dfPauper
    }else if (input$wrcSBFormat=="Legacy"){
      dataset=dfLegacy
    }
    
    #Keep only the data between the selected dates
    dataset=dataset[dataset$Date >= input$wrcSBDates1 &
                      dataset$Date <= input$wrcSBDates2,]
    
    #Provide the right name to the type of chosen events
    #and filter to keep only the selected events
    EventType="Official Competitions"
    if (!input$wrcSBPrelims & !input$wrcSBChallenges & !input$wrcSBPaper){
      dataset=NA
      EventType=NA
    }else if (input$wrcSBPrelims & !input$wrcSBChallenges & !input$wrcSBPaper){
      dataset=dataset[grep("Preliminary", dataset$Tournament), ]
      EventType="Preliminaries"
    }else if (!input$wrcSBPrelims & input$wrcSBChallenges & !input$wrcSBPaper){
      MajEvents = c("Challenge","Champ","Showcase","Premier","Qualifier","MOCS")
      dataset=dataset[grep(paste(MajEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Events"
    }else if (!input$wrcSBPrelims & !input$wrcSBChallenges & input$wrcSBPaper){
      PapEvents = c("20k","10k","5k","2k","1k","Pro Tour","LMS","LEC","RCQ","Re-CQ")
      dataset=dataset[grep(paste(PapEvents,collapse="|"), dataset$Tournament), ]
      EventType="Major Official Paper Events"
    }
    
    #Return a message if there is no event in the selection
    if (is.na(EventType)){
      wcSBPicture=""
      
      #Return a message if there is no row in the data frame
    } else if (nrow(dataset)==0){
      wcSBPicture=""
      
      #Return a message if there is no archetype selected
    } else if (is.null(input$wrcSBArchetype) || input$wrcSBArchetype==""){
      wcSBPicture=""
      
      #Return a message if there is no card selected
    } else if (is.null(input$wrcSBCard) || input$wrcSBCard==""){
      wcSBPicture=""
      
      #Get the table of results for each archetype
    } else {
      
      ArchetypeName=input$wrcSBArchetype
      CardToLookFor=input$wrcSBCard
      ArchetypeDf=dataset[dataset$Archetype$Archetype==ArchetypeName,]
      
      #Stats without the card
      ArchetypeWoTheCard=ArchetypeDf[-grep(CardToLookFor,ArchetypeDf$Sideboard),]
      if (sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)==0){
        WinrateWoTheCard=0
      }else{
        WinrateWoTheCard=sum(ArchetypeWoTheCard$Points+ArchetypeWoTheCard$T8Points)/
          3/sum(ArchetypeWoTheCard$NRounds+ArchetypeWoTheCard$T8Matches)
      } 
      WoTheCard=paste("The winrate of ", ArchetypeName," decks <strong>without</strong> ", 
                      CardToLookFor," is <strong>",format(round(WinrateWoTheCard*100, 2), 
                                                          nsmall = 2)," %</strong> for ",
                      nrow(ArchetypeWoTheCard)," decklists between ", input$wrcSBDates1,
                      " and ", input$wrcSBDates2,".<br><br>",sep = "")
      
      WithTheCard=c()
      
      #Stats with any number of the card when it is present
      ArchetypeWithTheCard=ArchetypeDf[grep(CardToLookFor,ArchetypeDf$Sideboard),]
      for (i in 1:4){
        ArchetypeWithTheCardi=ArchetypeWithTheCard[0,]
        
        #Goes over each row of the dataframe
        for (j in 1:nrow(ArchetypeWithTheCard)){
          
          #Check all the cards in that row
          for (k in 1:length(ArchetypeWithTheCard$SideboardCardNames[[j]])){
            
            if (ArchetypeWithTheCard$SideboardCardNames[[j]][k]==
                CardToLookFor 
                && 
                ArchetypeWithTheCard$SideboardCardCounts[[j]][k]==
                i){
              
              ArchetypeWithTheCardi=rbind(ArchetypeWithTheCardi,
                                          ArchetypeWithTheCard[j,])
              
            }
          }
          
          if (sum(ArchetypeWithTheCardi$NRounds+ArchetypeWithTheCardi$T8Matches)==0){
            WinrateWithTheCardi=0
          }else{
            WinrateWithTheCardi=sum(ArchetypeWithTheCardi$Points+ArchetypeWithTheCardi$T8Points)/
              3/sum(ArchetypeWithTheCardi$NRounds+ArchetypeWithTheCardi$T8Matches)
          }
        }
        
        WithTheCardi=paste("The winrate of ", ArchetypeName," decks <strong> with ", 
                           i," </strong> ", CardToLookFor," is <strong>",
                           format(round(WinrateWithTheCardi*100, 2), nsmall = 2),
                           " %</strong> for ",nrow(ArchetypeWithTheCardi),
                           " decklists between ", input$wrcSBDates1," and ", 
                           input$wrcSBDates2,".<br><br>",sep = "")
        WithTheCard=c(WithTheCard,WithTheCardi)
        
      }
      
      
      #Get the ID on the card picture to fetch on Scryfall
      if (CardToLookFor %in% cardDataSub$name){
        scryfallId=cardDataSub[cardDataSub$name==CardToLookFor,]$scryfallId[1]
      }else if (CardToLookFor %in% cardDataSub$faceName){
        scryfallId=cardDataSub[cardDataSub$faceName==CardToLookFor,]$scryfallId[1]
      }
      picSrc=paste("https://api.scryfall.com/cards/",scryfallId,"?format=image",sep="")
      
      #Magic card size: 63x88mm
      picDisplay=c('<img src="',picSrc,'" width="315" height="440">')
      wcSBPicture=picDisplay
    }
    
    wcSBPicture
    
  })

}
