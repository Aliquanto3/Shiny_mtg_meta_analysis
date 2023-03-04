#LIST ALL THE DIFFERENT ARCHETYPES IN THE DATA
generate_archetype_list = function(df){
  #CREATE A DATAFRAME CONTAINING THE LIST OF ARCHETYPES
  arch_list=data.frame(unique(df$Archetype$Archetype))
  names(arch_list) = c("Archetype")
  return(arch_list)
}

#COMPUTES THE Share OF EACH ARCHETYPE IN THE DATA
#presence CAN BE EITHER "Copies", "Players" or "Matches"
generate_metagame_data = function(df,graph_share,presence){
  
  graph_share=as.numeric(graph_share)
  arch_list=generate_archetype_list(df)
  
  #ADD THE PRESENCE OF EACH ARCHETYPE IN THE DATA
  arch_list$Presence=rep(0,length(arch_list$Archetype))
  for (i in 1:length(arch_list$Presence)){
    arch_id=which(df$Archetype$Archetype==arch_list$Archetype[i])
    if (presence=="Copies"){
      #NUMBER OF COPIES
      arch_list$Presence[i]=length(arch_id)
    }else if (presence=="Players"){
      #NUMBER OF PLAYERS
      arch_list$Presence[i]=length(unique(df[arch_id,]$Player))
    }else if (presence=="Matches"){
      #NUMBER OF ROUNDS PLAYED
      arch_list$Presence[i]=sum(df[arch_id,]$NRounds,df[arch_id,]$T8Matches)
    }
  }
  
  #FOR EASIER READING OF THE GRAPHS, AGGREGATE ALL THE ARCHETYPES ACCOUNTING FOR 
  #LESS THAN graph_share% OF THE DATA
  graph_perc=graph_share/100*sum(arch_list$Presence)
  arch_list_vis=arch_list[arch_list$Presence >= graph_perc, ]
  #arch_list_vis=arch_list_vis[order(arch_list_vis$Archetype, decreasing = TRUE),]
  arch_list_vis=arrange(arch_list_vis,desc(Presence))
  
  #ADD AN "OTHER" CATEGORY CONTAINING THE SUM OF COPIES OF ARCHETYPES UNDER X%
  sum_others=sum(arch_list[arch_list$Presence < graph_perc, ]$Presence)
  otherName=paste("Other (each <",graph_share,"%)",sep="")
  arch_list_vis=rbind(arch_list_vis,c(otherName, sum_others))
  #arch_list_vis=arch_list_vis[order(arch_list_vis$Archetype),]
  
  arch_list_vis$Presence=as.numeric(arch_list_vis$Presence)
  arch_list_vis$Share=as.numeric(format(round(arch_list_vis$Presence/
                                                sum(arch_list_vis$Presence)*100,
                                              1), nsmall = 1))
  
  arch_list_vis$Archetype = reorder(arch_list_vis$Archetype, 
                                    as.numeric(arch_list_vis$Presence))
  arch_list_vis$Archetype = relevel(arch_list_vis$Archetype, otherName)
  
  arch_list_vis$Archetype=fct_rev(arch_list_vis$Archetype)
  
  arch_list_vis <- arch_list_vis %>%
    mutate(
      Tooltip_Text = paste0(Archetype,": ",Share,"%")
    )
  
  return(arch_list_vis)
}

#COMPUTES A NAME FOR THE HISTOGRAM AND THE PIE CHART
#presence CAN BE EITHER "Copies", "Players" or "Matches"
generate_metagame_graph_title = function(presence,beginning,end,EventType,mtgFormat){
  MetaGraphTitle=paste("Proportion of archetypes in MTGO",mtgFormat, 
                       EventType,"between\n", beginning, "and", end,
                       "based on number of", presence,sep = " ")
  return(MetaGraphTitle)
}

#GENERATE A PIE CHART BASED ON DATA IN DF
#presence CAN BE EITHER "Copies", "Players" or "Matches"
metagame_pie_chart = function(df,PieShare,presence,beginning,end,EventType,mtgFormat){
  
  #CHANGE THE NUMBER FOR THE PROPORTION OF THE "OTHERS" CATEGORY HERE
  df_gen=generate_metagame_data(df,PieShare,presence)
  
  metagame_chart = ggplot(df_gen, aes(x="", -Share, fill = Archetype, tooltip = Tooltip_Text)) + 
    geom_col_interactive(width = 1, size = 1, color = "white") + 
    coord_polar("y", start=0) + 
    geom_text(aes(label = paste0(Share, "%"), x = 1.3), 
              position = position_stack(vjust = 0.5),  size=2) +
    labs(x = NULL, y = NULL, fill = NULL, subtitle = "by Anael Yahi",
         title = generate_metagame_graph_title(
           presence,beginning,end,EventType,mtgFormat)) + 
    guides(color = FALSE, size = FALSE) +
    scale_color_gradient(low="red", high="blue") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "#111111",size = 9),
          plot.subtitle = element_text(hjust = 0.5,size = 6),
          legend.text = element_text(size = 6))
  
  # return(metagame_chart)
  
  metagame_girafe = girafe(ggobj = metagame_chart)

  return(metagame_girafe)
    
  # df2 <- df_gen %>%
  #   mutate(csum = rev(cumsum(rev(-Share))),
  #          pos = -Share/2 + lead(csum, 1),
  #          pos = if_else(is.na(pos), -Share/2, pos))
  
  # df <- df_gen %>% 
  #   mutate(end = 2 * pi * cumsum(-Share)/sum(-Share),
  #          start = lag(end, default = 0),
  #          middle = 0.5 * (start + end),
  #          hjust = ifelse(middle > pi, 1, 0),
  #          vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  # graph=ggplot(df_gen, aes(x=1, y=rev(Share), fill = Archetype)) + 
  #   geom_bar(color = "black", stat = "identity") + 
  #   coord_polar(theta = "y") + 
  #   geom_text(aes(label = paste0(rev(Share), "%")), 
  #              position = position_stack(vjust = 0.5)) +
  #   labs(x = NULL, y = NULL, fill = NULL, subtitle = "by Anael Yahi",
  #        title = generate_metagame_graph_title(
  #          presence,beginning,end,EventType,mtgFormat)) + 
  #   guides(color = FALSE, size = FALSE) +
  #   #scale_color_gradient(low="red", high="blue") +
  #   theme_classic() +
  #   theme(axis.line = element_blank(),
  #         axis.text.y = element_blank(),
  #         axis.text.x = element_text(colour='black',size = 12,face="bold"),
  #         axis.title=element_blank(),
  #         axis.ticks = element_blank(),
  #         plot.title = element_text(hjust = 0.5, color = "#111111",size = 20),
  #         plot.subtitle = element_text(hjust = 0.5,size = 18),
  #         legend.position = "none")
    
  # graph = graph + geom_label_repel(data = df2,
  #                    aes(y = pos, label = paste0(Archetype, ": ",Share, "%")),
  #                    size = 4.5, nudge_x = 1, show.legend = FALSE)
  
  # graph = graph + guides(fill = guide_legend(title = "Archetype")) + 
  #   scale_y_continuous(breaks = df2$pos, labels = df_gen$Archetype) +
  #   geom_text(aes(label = paste0(Share, "%")), 
  #             position = position_stack(vjust = 0.5))
  
  # graph = graph + 
  #   scale_y_continuous(breaks=(cumsum(df_gen$Share) - df_gen$Share / 2), 
  #                      labels= df_gen$Archetype)
  # 
  # graph
  
}

#GENERATE A BOX PLOT BASED ON DATA IN DF
#presence CAN BE EITHER "Copies", "Players" or "Matches"
metagame_box_plot = function(df,histShare,presence,beginning,end,EventType,mtgFormat){
  
  #GET THE DATA FOR ALL ARCHETYPES HAVING A META Share ABOVE HistShare
  df_gen=generate_metagame_data(df,histShare,presence)
  
  #GENERATE A TITLE FOR THE BOXPLOT
  boxplot_title=paste(generate_metagame_graph_title(
    presence,beginning,end,EventType,mtgFormat),"-",
    df_gen[grep("Other",df_gen$Archetype),]$Archetype, sep=" ")
  
  #THIS GRAPH DOESN'T DISPLAY THE "Other" CATEGORY
  df_gen=df_gen[!grepl("Other",df_gen$Archetype),]
  
  #REORDER ARCHETYPES BY ASCENDING PRESENCE
  df_gen$Archetype = reorder(df_gen$Archetype, as.numeric(df_gen$Presence))
  
  # for (i in 1:nrow(df_gen)){
  #   picName=paste(gsub(" ","_",df_gen$Archetype[i]),".JPG",sep = "")
  #   picPath=paste("Pics",picName,sep = "/")
  #   pic=readJPEG(picPath, native = TRUE)
  #   df_gen$image[i]=pic
  # }
  
  #plot is much clearer
  metagame_chart=ggplot(df_gen, aes(x=Archetype, y=as.numeric(Share), fill=Archetype, tooltip = Tooltip_Text)) +
    geom_text(aes(label = paste0(Share, "%")), hjust = -0.2, size = 1.5) +
    geom_col_interactive() + theme_minimal() + guides(fill = FALSE) +
    labs(x = NULL, y = "Presence (%)", fill = NULL, 
         title = boxplot_title, subtitle = "by Anael Yahi", size = 8) + 
    scale_color_gradient(low="blue", high="red") +
    #scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
    theme(axis.text.x  = element_text(size=6),
          axis.text.y  = element_text(size=6),
          plot.title = element_text(hjust = 0.5, color = "#111111",size = 9),
          plot.subtitle = element_text(hjust = 0.5,size = 6),
          legend.text = element_text(size = 6)) +
    coord_flip()
  
  # i=1
  # picName=paste(gsub(" ","_",df_gen$Archetype[i]),".JPG",sep = "")
  # picPath=paste("Pics",picName,sep = "/")
  # 
  # pic=readJPEG(picPath, native = TRUE)
  # 
  # graph2 = metagame_chart + inset_element(p = pic, 
  #                               left = 0.05,
  #                               bottom = 0.9,
  #                               right = 0.2,
  #                               top = 1)
  # return(metagame_chart)
  
  metagame_girafe = girafe(ggobj = metagame_chart)

  return(metagame_girafe)
}

#PLOT OF THE AVERAGE WINRATE FOR THE MOST POPULAR ARCHETYPES
#presence CAN BE EITHER "Copies", "Players" or "Matches"
winrates_graph = function(df,arch_ranked,presence,HistShare,beginning,end,EventType){
  
  #GET ONLY THE DECKS APPEARING THE MOST IN THE DATA
  if (presence=="Copies"){
    #KEEP ONLY THE DECK WITH THE MOST COPIES
    presence_min=HistShare/100*length(df$Archetype$Archetype)
    arch_most_played=arch_ranked[arch_ranked$NCopies>=presence_min,]
  }else if (presence=="Players"){
    #KEEP ONLY THE DECK WITH THE MOST PLAYERS
    presence_min=HistShare/100*length(unique(df$Player))
    arch_most_played=arch_ranked[arch_ranked$NPlayers>=presence_min,]
  }else if (presence=="Matches"){
    #KEEP ONLY THE DECK WITH THE MOST MATCHES
    presence_min=HistShare/100*(sum(df$NRounds)+sum(df$T8Matches))
    arch_most_played=arch_ranked[arch_ranked$TotalMatches>=presence_min,]
  }
  
  #REORDER ARCHETYPES BY ASCENDING AVERAGE WINRATE
  arch_most_played$Archetype = reorder(arch_most_played$Archetype, 
                                       as.numeric(arch_most_played$WinrateAverage))
  #PLOT THE AVERAGE WINRATE AND THE CONFIDENCE INTERVALS
  y_label_winrate="Winrates of the most popular archetypes (%)"
  graph_title_winrate=paste(
    "Confidence intervals on the winrates of the most present archetypes ", 
    "( at least ",HistShare,"% of the ",presence,") between ", beginning, 
    " and ", end, " in MTGO ", EventType,sep="")
  
  ggplot(arch_most_played, aes(x=Archetype, y=WinrateAverage)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(WinrateAverage,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y=y_label_winrate, title=graph_title_winrate,
         subtitle="Red lines for the average of the bounds of the CI
Green line for the average of the computed winrate
by Anael Yahi")+
    geom_errorbar(aes(ymax = Winrate95Max, ymin = Winrate95Min)) + 
    geom_hline(yintercept = mean(arch_most_played$WinrateAverage), 
               color="green", linetype="dashed", size=1)+ 
    geom_hline(yintercept = mean(arch_most_played$Winrate95Min), 
               color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = mean(arch_most_played$Winrate95Max), 
               color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
}


#LIST ALL THE DIFFERENT PLAYERS IN THE DATA
generate_player_list = function(df){
  #CREATE A DATAFRAME CONTAINING THE LIST OF ARCHETYPES
  play_list=data.frame(unique(df$Player))
  names(play_list)[1] = c("PlayerS")
  return(play_list)
}

#FILL IN METRIC POINTS IN A PLAYER DATA FRAME
metric_points_players = function(df){
  #GET THE LIST OF THE DIFFERENT PLAYERS IN THE DATA
  metric_df_players=generate_player_list(df)
  
  metric_df_players$NAppearances=rep(0,length(metric_df_players$PlayerS))
  metric_df_players$TotalMatches=rep(0,length(metric_df_players$PlayerS))
  metric_df_players$WinrateAverage=rep(0,length(metric_df_players$PlayerS))
  metric_df_players$Winrate95Min=rep(0,length(metric_df_players$PlayerS))
  metric_df_players$Winrate95Max=rep(0,length(metric_df_players$PlayerS))
  metric_df_players$TotalPoints=rep(0,length(metric_df_players$PlayerS))
  metric_df_players$ArchetypeNames=rep(0,length(metric_df_players$PlayerS))
  metric_df_players$ArchetypeCounts=rep(0,length(metric_df_players$PlayerS))
  metric_df_players$URL=rep(0,length(metric_df_players$PlayerS))
  for (i in 1:length(metric_df_players$PlayerS)){
    #POSITION OF THE CORRESPONDING EXACT OR SUPER ARCHETYPE IN THE DATA
    play_identification=which(df$Player==metric_df_players$PlayerS[i])
    df3=df[play_identification,]
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    metric_df_players$NAppearances[i]=length(play_identification)
    #LIST OF DIFFERENT ARCHETYPES THAT PLAYER PLAYED
    metric_df_players$ArchetypeNames[i]=list(unique(df3$Archetype$Archetype))
    #paste(unique(df3$Archetype$Archetype),collapse=",")
    arch_count=c()
    for(j in 1:length(metric_df_players$ArchetypeNames[[i]])){
      arch_count[j]=length(df3[df3$Archetype$Archetype==metric_df_players$
                                 ArchetypeNames[[i]][[j]],]$AnchorUri)
    }
    metric_df_players$ArchetypeCounts[i]=list(arch_count)
    
    metric_df_players$URL[i]=list(unique(df3$AnchorUri))
    #paste(df3$AnchorUri,collapse=",")
    
    #NUMBER OF MATCHES PLAYED BY THAT ARCHETYPE IN THE DATA
    metric_df_players$TotalMatches[i]=sum(df3$NRounds,
                                          df3$T8Matches)
    #NUMBER OF WINS OF THAT ARCHETYPE
    total_wins_arch=sum((df3$Points + df3$T8Points)/3)
    #NUMBER OF MATCHES OF THAT ARCHETYPE
    total_matches_arch=sum(df3$NRounds + df3$T8Matches)
    metric_df_players$TotalPoints[i]=total_wins_arch*3
    
    #95% CONFIDENCE INTERVALS OF THE WINRATE
    #EFFECTIVE WINRATE IN THE DATA
    metric_df_players$WinrateAverage[i]=binom.test(total_wins_arch, total_matches_arch, 
                                                   p=0.5,alternative="two.sided", 
                                                   conf.level=0.95)$estimate
    #LOWER BOUND OF THE MEASURED WINRATE             
    metric_df_players$Winrate95Min[i]=binom.test(total_wins_arch, total_matches_arch, 
                                                 p=0.5,alternative="two.sided", 
                                                 conf.level=0.95)$conf.int[1]
    #UPPER BOUND OF THE MEASURED WINRATE 
    metric_df_players$Winrate95Max[i]=binom.test(total_wins_arch, total_matches_arch, 
                                                 p=0.5,alternative="two.sided", 
                                                 conf.level=0.95)$conf.int[2]
  }
  metric_df_players=arrange(metric_df_players,desc(TotalPoints))
  
  return(metric_df_players)
}

#FILL IN METRIC POINTS IN AN ARCHETYPES DATA FRAME
metric_points_archetypes = function(df){
  #GET THE LIST OF THE DIFFERENT ARCHETYPES IN THE DATA
  metric_df=generate_archetype_list(df)
  players_df=metric_points_players(df)
  
  metric_df$NoCopies=rep(0,length(metric_df$Archetype))
  metric_df$NoPlayers=rep(0,length(metric_df$Archetype))
  metric_df$TotalMatches=rep(0,length(metric_df$Archetype))
  metric_df$WinrateAverage=rep(0,length(metric_df$Archetype))
  metric_df$Winrate95Min=rep(0,length(metric_df$Archetype))
  metric_df$Winrate95Max=rep(0,length(metric_df$Archetype))
  metric_df$MatchesPerPlayer=rep(0,length(metric_df$Archetype))
  metric_df$Tooltip_Text=rep(0,length(metric_df$Archetype))
  #metric_df$WinrateArchetypeOutPlayers=rep(0,length(metric_df$Archetype))
  
  for (i in 1:length(metric_df$Archetype)){
    #POSITION OF THE CORRESPONDING EXACT OR SUPER ARCHETYPE IN THE DATA
    arch_identification=which(df$Archetype$Archetype==metric_df$Archetype[i])
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    metric_df$NoCopies[i]=length(arch_identification)
    #NUMBER OF DIFFERENT PLAYERS PLAYING THAT DECK
    metric_df$NoPlayers[i]=length(unique(df[arch_identification,]$Player))
    #NUMBER OF MATCHES PLAYED BY THAT ARCHETYPE IN THE DATA
    metric_df$TotalMatches[i]=sum(df[arch_identification,]$NRounds,
                                  df[arch_identification,]$T8Matches)
    #NUMBER OF MATCHES PER PLAYER - THE HIGHER, THE MORE A PLAYER wiTH THAT DECK
    #APPEARED IN THE RESULTS
    metric_df$MatchesPerPlayer[i]=signif(metric_df$TotalMatches[i]/
      metric_df$NoPlayers[i],2)
    
    #NUMBER OF WINS OF THAT ARCHETYPE
    total_wins_arch=sum((df$Points[arch_identification] + 
                           df$T8Points[arch_identification])/3)
    #NUMBER OF MATCHES OF THAT ARCHETYPE
    total_matches_arch=sum(df$NRounds[arch_identification] + 
                             df$T8Matches[arch_identification])
    
    #95% CONFIDENCE INTERVALS OF THE WINRATE
    #EFFECTIVE WINRATE IN THE DATA
    metric_df$WinrateAverage[i]=signif(
      binom.test(total_wins_arch, total_matches_arch,p=0.5,alternative="two.sided", 
                 conf.level=0.95)$estimate, 4)*100
    #LOWER BOUND OF THE "TRUE" WINRATE             
    metric_df$Winrate95Min[i]=signif(
      binom.test(total_wins_arch, total_matches_arch, p=0.5,alternative="two.sided", 
                 conf.level=0.95)$conf.int[1], 4)*100
    #UPPER BOUND OF THE "TRUE" WINRATE 
    metric_df$Winrate95Max[i]=signif(
      binom.test(total_wins_arch, total_matches_arch, p=0.5,alternative="two.sided", 
                 conf.level=0.95)$conf.int[2], 4)*100
    
    # #WINRATE OF THAT ARCHETYPE DIVIDED BY THE AVERAGE WINRATE OF ITS PILOTS
    # metric_df$WinrateArchetypeOutPlayers[i]=format(round(
    #   metric_df$WinrateAverage[i]/mean(players_df[grep(metric_df$Archetype[i],
    #                        players_df$ArchetypeNames),]$WinrateAverage), 2),nsmall = 2)
    
    metric_df$Tooltip_Text[i]=paste(metric_df$Archetype[i],": ",
                                    round(metric_df$NoCopies[i]/sum(metric_df$NoCopies)*100, digits = 2),
                                    "% share / ",
                                    metric_df$WinrateAverage[i],"% win rate",sep="")
  }
  
  total_matches=sum(metric_df$NoCopies)
  metric_df <- metric_df %>%
    mutate(
      paste(metric_df$Archetype,":\n",
            round(metric_df$NoCopies/total_matches*100, digits = 2),
            "% match share\n ",
            metric_df$WinrateAverage,"% win rate",sep="")
    )
  
  return(metric_df)
}

#PROVIDE A GRAPH FOR A METRIC DATAFRAME DISPLAYING WINRATES DEPENDING ON
#PRESENCE, WHICH IS HIGHLIGHTED BY THE DIAMETERS OF ANOTHER TYPE OF PRESENCE
#presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
#isLog is a boolean
#only_best is a boolean
metric_graph = function(metric_df,presence,diameters,diam_ratio,beginning,end,
                        isLog,HistShare,EventType,mtgFormat) {
  beginning=as.Date(beginning,format="%Y-%m-%d", origin ="1970-01-01")
  end=as.Date(end,format="%Y-%m-%d", origin ="1970-01-01")
  
  diam_ratio=as.numeric(diam_ratio)
  
  #GENERATES THE LABELS
  if (presence=="Copies"){
    x_label="Total number of copies of each archetype (%)"
  }else if (presence=="Players"){
    x_label="Total number of different players for each archetype (%)"
  }else if (presence=="Matches"){
    x_label="Total number of matches played by each archetype (%)"
  }
  y_label="Average winrate of each archetype (%)"
  graph_title=paste("Winrates depending on presence: archetypes ", 
                    "between", beginning, "and", end, "in MTGO", mtgFormat, EventType,sep = " ")
  graph_subtitle=paste("Circle diameters depending on",diameters,"\nby Anael Yahi",sep=" ")
  
  #GENERATES THE GRAPH - FIRST THE VALUES OF PRESENCE
  if (presence=="Copies"){
    metric_df$NoCopies=
      metric_df$NoCopies/sum(metric_df$NoCopies)*100
    metric_plot=ggplot(metric_df, aes(NoCopies, WinrateAverage))
    #USE THE UNFILTERED DATAFRAME FOR THE VALUES OF THE TIERS
    avg_presence=mean(metric_df$NoCopies)
    std_presence=sd(metric_df$NoCopies)
  }else if (presence=="Players"){
    metric_df$NoPlayers=
      metric_df$NoPlayers/sum(metric_df$NoPlayers)*100
    metric_plot=ggplot(metric_df, aes(NoPlayers, WinrateAverage))
    avg_presence=mean(metric_df$NoPlayers)
    std_presence=sd(metric_df$NoPlayers)
  }else if (presence=="Matches"){
    metric_df$TotalMatches=
      metric_df$TotalMatches/sum(metric_df$TotalMatches)*100
    metric_plot=ggplot(metric_df, aes(TotalMatches, WinrateAverage))
    avg_presence=mean(metric_df$TotalMatches)
    std_presence=sd(metric_df$TotalMatches)
  }
  
  if (diameters=="Copies"){
    metric_plot=metric_plot + 
      geom_point_interactive(aes(color = Archetype, tooltip = Tooltip_Text, data_id = Archetype), 
                             size=metric_df$NoCopies*diam_ratio, show.legend = FALSE)
  }else if (diameters=="Players"){
    metric_plot=metric_plot + 
      geom_point_interactive(aes(color = Archetype, tooltip = Tooltip_Text, data_id = Archetype), 
                             size=metric_df$NoPlayers*diam_ratio, show.legend = FALSE)
  }else if (diameters=="Matches"){
    metric_plot=metric_plot + 
      geom_point_interactive(aes(color = Archetype, tooltip = Tooltip_Text, data_id = Archetype), 
                             size=metric_df$TotalMatches*diam_ratio, show.legend = FALSE)
  }
  
  metric_plot=metric_plot + coord_cartesian() + theme_bw(base_size = 6) + 
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) + 
    geom_text_repel(aes(label=Archetype),hjust=0, vjust=0,point.padding = NA,
                    size = 2, nudge_x = 0.1, direction = "y",
                    segment.size=0.2,segment.color="grey",segment.alpha=0.4) 
  
  #TIERS BASED ON MEAN + N * STANDARD DEVIATION OF PRESENCE, N={0,1,2,3}
  metric_plot=metric_plot + geom_vline(xintercept = avg_presence, linetype="dotted",
                                       color = "purple", size=0.5) +
    geom_text(aes(x=avg_presence, label="Tiers 2.5\n", y=
                    max(WinrateAverage)*0.99), colour="purple",
              angle=0, size=3) +
    geom_text(aes(x=avg_presence, label="\nPresence mean", y=
                    max(WinrateAverage)*0.99), colour="grey",
              angle=0, size=2) +
    geom_vline(xintercept = avg_presence + std_presence, linetype="dotted",
               color = "blue", size=0.5) +
    geom_text(aes(x=avg_presence + std_presence, label="Tiers 2\n",
                  y=max(WinrateAverage)*0.98), colour="blue",
              angle=0, size=3) +
    geom_text(aes(x=avg_presence + std_presence, label="\nMean + 1*sd",
                  y=max(WinrateAverage)*0.98), colour="grey",
              angle=0, size=2) +
    geom_vline(xintercept = avg_presence + 2*std_presence, linetype="dotted",
               color = "dark green", size=0.5) +
    geom_text(aes(x=avg_presence + 2*std_presence, label="Tiers 1.5\n",
                  y=max(WinrateAverage)*0.97), colour="dark green",
              angle=0, size=3) +
    geom_text(aes(x=avg_presence + 2*std_presence, label="\nMean + 2*sd",
                  y=max(WinrateAverage)*0.97), colour="grey",
              angle=0, size=2) +
    geom_vline(xintercept = avg_presence + 3*std_presence, linetype="dotted",
               color = "orange", size=0.5) +
    geom_text(aes(x=avg_presence + 3*std_presence, label="Tiers 1\n",
                  y=max(WinrateAverage)*0.96), colour="orange",
              angle=0, size=3) +
    geom_text(aes(x=avg_presence + 3*std_presence, label="\nMean + 3*sd",
                  y=max(WinrateAverage)*0.96), colour="grey",
              angle=0, size=2) +
    geom_vline(xintercept = avg_presence + 4*std_presence, linetype="dotted",
               color = "red", size=0.5) +
    geom_text(aes(x=avg_presence + 4*std_presence, label="Tiers 0\n",
                  y=max(WinrateAverage)*0.95), colour="red",
              angle=0, size=3) +
    geom_text(aes(x=avg_presence + 4*std_presence, label="\nMean + 4*sd",
                  y=max(WinrateAverage)*0.95), colour="grey",
              angle=0, size=2)
  
  if (isLog){
    metric_plot=metric_plot + scale_x_continuous(trans = 'log10',
                                                 limits = c(HistShare,NA))
  }else{
    metric_plot=metric_plot+xlim(HistShare,NA)
  }
  
  metric_girafe = girafe(ggobj = metric_plot)
  
  return(metric_girafe)
  
}

#COMBINES THE RATIOS OF POINTS PER ROUND AND NUMBER OF COPIES FOR EACH
#ARCHETYPE, THEN PROVIDES A Rank BASED ON THAT
#THE NEW METRIC OBTAINED THAT WAY IS NORMALIZED TO BE BETWEEN 0 AND 1
#ALSO IMPLEMENTS THE VS META SCORE 
#https://www.vicioussyndicate.com/vs-meta-score-new-metric-measuring-archetypes-standing-meta/
archetypes_ranking = function(metric_df,beginning,end,Presence_Weight,PPR_Weight){
  
  beginning=as.Date(beginning,format="%Y-%m-%d", origin ="1970-01-01")
  end=as.Date(end,format="%Y-%m-%d", origin ="1970-01-01")
  
  #Make the values of both metrics start at 0 by substracting the minimum value
  metric_df_start_at_0=metric_df
  metric_df_start_at_0$TotalMatches = 
    metric_df_start_at_0$TotalMatches - min(metric_df_start_at_0$TotalMatches)
  metric_df_start_at_0$WinrateAverage = 
    metric_df_start_at_0$WinrateAverage - min(metric_df_start_at_0$WinrateAverage)
  
  #Make the values of both metrics go up to 1 by dividing by the maximum value
  metric_df_between_0_and_1 = metric_df_start_at_0
  metric_df_between_0_and_1$TotalMatches = 
    metric_df_between_0_and_1$TotalMatches / max(metric_df_between_0_and_1$TotalMatches)
  metric_df_between_0_and_1$WinrateAverage = 
    metric_df_between_0_and_1$WinrateAverage / max(metric_df_between_0_and_1$WinrateAverage)
  
  #We now have normalized metrics we can sum
  metric_df_normalized=metric_df_between_0_and_1
  metric_df_normalized$NormalizedSum = 
    Presence_Weight * metric_df_normalized$TotalMatches +
    PPR_Weight * metric_df_normalized$WinrateAverage
  
  #Also divide by the weights of each metric, so that even if we have a deck 
  #with the maximum value for each metric, i.e. 1 after normalization, 
  #we also find 1 as the value for the mark granted to the deck by the sum
  metric_df_normalized$NormalizedSum = 
    metric_df_normalized$NormalizedSum / (Presence_Weight+PPR_Weight)
  
  #Re-affect those new values to the original dataframe
  metric_df$NormalizedPresence=metric_df_normalized$TotalMatches
  metric_df$NormalizedWinrate=metric_df_normalized$WinrateAverage
  metric_df$NormalizedSum=metric_df_normalized$NormalizedSum
  
  #Order by putting the highest value of the normalized sum first
  metric_df = metric_df[order(-metric_df_normalized$NormalizedSum),]
  
  #Provide a rank based on the place in the reordered data frame
  metric_df$Rank=seq(from=1,to=nrow(metric_df_normalized),by=1)
  
  # #Computation of the VS Meta Score by the Vicious Syndicate
  # #Source: https://www.vicioussyndicate.com/drr/vs-meta-score/ 
  # metric_df$VSMetaScore=metric_df$NormalizedSum
  # MetaPeak=c(max(metric_df$TotalMatches) - min(metric_df$TotalMatches) /
  #              max(metric_df$TotalMatches),
  #            max(metric_df$WinrateAverage) - min(metric_df$WinrateAverage) /
  #              max(metric_df$WinrateAverage))
  # 
  # for (i in 1:length(metric_df$VSMetaScore)){
  #   ArchCoord=c((metric_df$TotalMatches[i]-min(metric_df$TotalMatches)) /
  #                 max(metric_df$TotalMatches),
  #               (metric_df$WinrateAverage[i]-min(metric_df$WinrateAverage)) /
  #                 max(metric_df$WinrateAverage))
  #   
  #   metric_df$VSMetaScore[i]=pointDistance(MetaPeak, ArchCoord, lonlat=FALSE)
  # }
  return(metric_df)
}

#SORT THE ARCHETYPES IN CLUSTERS BASED ON PRESENCE AND WINRATE
kmeans_arch = function (metric_df,k,iter,init,algo,beginning,end,
                        count_wr,only_best,EventType,mtgFormat){
  df_elim=select(metric_df, TotalMatches, WinrateAverage, Archetype, NoPlayers)
  if(only_best){
    df_elim=df_elim[df_elim$TotalMatches>mean(df_elim$TotalMatches),]
  }
  df_elim$Presence=df_elim$TotalMatches/(sum(df_elim$TotalMatches))*100
  if (count_wr){
    df_kde=select(df_elim, Presence, WinrateAverage)
  }else{
    df_kde=select(df_elim, Presence)
  }
  
  set.seed(123)
  res.km=kmeans(scale(df_kde), k, iter.max = iter, nstart = init, 
                algorithm = algo)
  df_kde$CLUSTER=factor(res.km$cluster)
  df_kde$Archetype=df_elim$Archetype
  df_kde$NoPlayers=df_elim$NoPlayers
  df_kde$WinrateAverage=df_elim$WinrateAverage
  
  x_label="Presence"
  y_label="Winrate"
  graph_title=paste("Winrates depending on presence: archetypes ", 
                    "between", beginning, "and", end, "in MTGO",mtgFormat, 
                    EventType,sep = " ")
  graph_subtitle=paste("Clustered in",k,"categories with",algo,"algorithm
  Circle diameters depending on number of Players
  Minimum presence to appear (average):",round(mean(df_elim$TotalMatches)/100,digits=3),"%
by Anael Yahi",sep = " ")
  gg_point = ggplot(data = df_kde,  mapping = aes(x = Presence, y = WinrateAverage, 
                                                  colour = CLUSTER)) + 
    coord_cartesian() + theme_bw(base_size = 6) + 
    scale_x_continuous(trans = 'log10') + scale_fill_locuszoom() +
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) +
    geom_text_repel(aes(label=Archetype),hjust=0, vjust=0,point.padding = NA,
                    size = 2, nudge_x = 0.1, direction = "y", show.legend = FALSE,
                    segment.size=0.2,segment.alpha=0.4) + 
    geom_point_interactive(aes(size=3*NoPlayers, tooltip = Tooltip_Text, 
                               data_id = Archetype),show.legend = FALSE)
  
  girafe(ggobj = gg_point)
}

#PLOT THE REPARTITION FOR THE A LOGARITHMIC COMBINATION OF THE PRESENCE AND 
#WINRATES FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
log_comb_graph = function(df,arch_ranked,beginning,end,EventType,HistShare,
                          Presence_Weight,PPR_Weight,mtgFormat){
  HistShare=as.numeric(HistShare)
  presence_min=HistShare/100*(sum(df$NRounds)+sum(df$T8Matches))
  arch_ranked_sub_2=arch_ranked[arch_ranked$TotalMatches>=log(presence_min),]
  
  meanData=mean(arch_ranked_sub_2$NormalizedSum*100)
  sdData=sd(arch_ranked_sub_2$NormalizedSum*100)
  
  arch_ranked_sub_2$Archetype=reorder(arch_ranked_sub_2$Archetype,
                                      arch_ranked_sub_2$NormalizedSum)
  
  titleLinearComb=paste("Combination of the metrics for the most popular archetypes
At least ",HistShare,"% of presence - Linear winrate, logarithmic presence
Presence Weight = ",Presence_Weight, " / Winrate weight = ",PPR_Weight,  "
Between ",beginning," and ",end," in MTGO ", mtgFormat, " ",EventType,sep="")
  
  gg_point = ggplot(arch_ranked_sub_2, aes(x=Archetype, y=NormalizedSum*100)) + 
    theme_classic() + 
    geom_point_interactive(aes(
      size=4, tooltip = Tooltip_Text,data_id = Archetype),
      show.legend = FALSE,color="sky blue") +  
    geom_text_repel(aes(label=format(round(NormalizedSum*100,1), nsmall = 1)),
                    hjust=1, vjust=1,point.padding = NA,color="dark grey",
                    size = 2, nudge_x = 0.1, direction = "y", show.legend = FALSE,
                    segment.size=0.2,segment.alpha=0.4)+ 
    labs(x=NULL, y="Value of the linear combination metric", title=titleLinearComb,
         subtitle="Green line for the average of the metrics linear combination
Red lines for the average +/- a standard deviation
by Anael Yahi")+
    
    geom_hline(yintercept = meanData-3*sdData, color="#004CA3", linetype="dotted", size=0.5) +
    geom_text(aes(x=1, y=meanData-3*sdData,label="Tiers 3\n"), colour="#004CA3",angle=0, size=3) +
    geom_text(aes(x=1, y=meanData-3*sdData-0.5,label="Mean - 3*SD"), colour="grey",angle=0, size=2) + 
    
    geom_hline(yintercept = meanData-2*sdData, color="#8A51A5", linetype="dotted", size=0.5) +
    geom_text(aes(x=1, y=meanData-2*sdData,label="Tiers 2.5\n"), colour="#8A51A5",angle=0, size=3) + 
    geom_text(aes(x=1, y=meanData-2*sdData-0.5,label="Mean - 2*SD"), colour="grey",angle=0, size=2) +
    
    geom_hline(yintercept = meanData-sdData, color="#CB5E99", linetype="dotted", size=0.5) +
    geom_text(aes(x=1, y=meanData-sdData,label="Tiers 2\n"), colour="#CB5E99",angle=0, size=3) +
    geom_text(aes(x=1, y=meanData-sdData-0.5,label="Mean - SD"), colour="grey",angle=0, size=2) +
    
    geom_hline(yintercept = meanData, color="#F47B89", linetype="dotted", size=0.5)+
    geom_text(aes(x=1, y=meanData,label="Tiers 1.5\n"), colour="#F47B89",angle=0, size=3) +
    geom_text(aes(x=1, y=meanData-0.5,label="Mean"), colour="grey",angle=0, size=2) +
    
    geom_hline(yintercept = meanData+sdData, color="#FFA47E", linetype="dotted", size=0.5)+ 
    geom_text(aes(x=1, y=meanData+sdData,label="Tiers 1\n"), colour="#FFA47E",angle=0, size=3) +
    geom_text(aes(x=1, y=meanData+sdData-0.5,label="Mean + SD"), colour="grey",angle=0, size=2) +
    
    geom_hline(yintercept = meanData+2*sdData, color="#FFD286", linetype="dotted", size=0.5)+ 
    geom_text(aes(x=1, y=meanData+2*sdData,label="Tiers 0.5\n"), colour="#FFD286",angle=0, size=3) +
    geom_text(aes(x=1, y=meanData+2*sdData-0.5,label="Mean + 2*SD"), colour="grey",angle=0, size=2) +
    
    geom_hline(yintercept = meanData+3*sdData, color="green", linetype="dotted", size=0.5)+ 
    geom_text(aes(x=1, y=meanData+3*sdData,label="Tiers 0\n"), colour="green",angle=0, size=3) +
    geom_text(aes(x=1, y=meanData+3*sdData-0.5,label="Mean + 3*SD"), colour="grey",angle=0, size=2) +
    
    theme_bw(base_size = 6) + scale_x_discrete(guide = guide_axis(n.dodge=2))
  
  girafe(ggobj = gg_point)
}

#COMPUTES THE AVERAGE CHARACTERISTICS OF THE ARCHETYPE
#AVERAGE RATIO OF EACH TYPE OF CARD, AVERAGE NUMBER OF EACH CARD IN MD/SB...
archAverageData = function(dataset,cardDataSub){
  # #Get all the different cards
  # cardsList=sort(unique(c(unlist(dataset$Mainboard),
  #                  unlist(dataset$Sideboard))))
  # #Remove the card counts from the list
  # cardsList=cardsList[!grepl("^[0-9]{1,2}$", cardsList)]
  #With the additional colums created in generate_df
  cardsList=sort(unique(c(unlist(dataset$MainboardCardNames),
                          unlist(dataset$SideboardCardNames))))
  averageCards=setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                        c("Card Picture","Card Name","Average MD Count if present", 
                          "Average SB Count if present", "Proportion Of MD (%)",
                          "Proportion Of SB (%)","Types"))
  imgSrc='<img src="'
  imgDim='" width="74.125" height="110"></img>'
  
  #Iterate over each card to find their data
  for (i in 1:length(cardsList)){
    cardNamei=cardsList[i]
    
    #Ajouter "unique" ?
    if (cardNamei %in% cardDataSub$name){
      cardTypei=unique(cardDataSub[cardDataSub$name==cardNamei,]$types)
      scryfallIdi=cardDataSub[cardDataSub$name==cardNamei,]$scryfallId[1]
    }else if (cardNamei %in% cardDataSub$faceName){
      cardTypei=unique(cardDataSub[cardDataSub$faceName==cardNamei,]$types)
      scryfallIdi=cardDataSub[cardDataSub$faceName==cardNamei,]$scryfallId[1]
    }
    
    picSrci=paste('https://api.scryfall.com/cards/',scryfallIdi,'?format=image',sep="")
    
    #Magic card size: 63x88mm
    picDisplayi=paste(imgSrc,picSrci,imgDim,sep='')
    
    AvgMdCounti=0
    AvgSbCounti=0
    ProportionOfMDi=0
    ProportionOfSBi=0
    
    for (j in 1:nrow(dataset)){
      if (!is.na(match(cardNamei,dataset$MainboardCardNames[[j]]))){
        AvgMdCounti=AvgMdCounti+dataset$MainboardCardCounts[[j]][match(
          cardNamei, dataset$MainboardCardNames[[j]])]
        
        ProportionOfMDi=ProportionOfMDi+1
      }
      
      if (!is.na(match(cardNamei,dataset$SideboardCardNames[[j]]))){
        AvgSbCounti=AvgSbCounti+dataset$SideboardCardCounts[[j]][match(
          cardNamei, dataset$SideboardCardNames[[j]])]
        
        ProportionOfSBi=ProportionOfSBi+1
      }
    }
    
    AvgMdCounti=as.numeric(format(round(AvgMdCounti/ProportionOfMDi,2),
                                  nsmall = 2))
    AvgSbCounti=as.numeric(format(round(AvgSbCounti/ProportionOfSBi,2),
                                  nsmall = 2))
    ProportionOfMDi=as.numeric(format(round(ProportionOfMDi/nrow(dataset),2),
                                      nsmall = 2))*100
    ProportionOfSBi=as.numeric(format(round(ProportionOfSBi/nrow(dataset),2),
                                      nsmall = 2))*100
    
    
    #Add a line to the result dataframe with the data for the iterated card
    averageCards[i,]=c(picDisplayi,cardNamei,AvgMdCounti,AvgSbCounti,
                       ProportionOfMDi,ProportionOfSBi,cardTypei)
  }
  
  return(averageCards)
}

#GET THE USEFUL DATA FROM ALL THE CARDS IN THE GAME
getCardData = function(CardFile){
  #DATA FROM: https://mtgjson.com/downloads/all-files/
  #IMPORT ALL THE DATA FOR ALL THE CARDS IN THE GAME
  cardData=read.csv(CardFile,sep=",",header=T)
  #KEEP ONLY RELEVANT INFORMATION AND REMOVE DUPLICATES - CARDS PRINTED 
  #MULTIPLE TIMES
  cardDataSub=unique(subset(cardData,select=c(
    colors,convertedManaCost,faceName,layout,manaCost,name,subtypes,supertypes,
    type,types,isReprint,setCode,artist,scryfallId)))
  return(cardDataSub)
}