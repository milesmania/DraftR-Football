## Load File or Setup New ###########################################  
if(file.exists(draftFile)){
  load(draftFile)
}else{
  playersTakenCount <- 0
  StartPickTime <- Sys.time()# + 4*60
  if(length(teams) == 0){
    dfDraft <- playersAvail <- rosters <- availPlayers <- playersTaken <- draftResults <- draftForecast <- seasonProjection <- NULL
  }else{
    playersAvail <- unique(ff$pId)
    playerLevels <- c("",playersAvail)
    availPlayers <- factor(rep("",nRounds),levels=playerLevels)
    #ff$pId <- factor(ff$pId,levels=playerLevels)
    playersTaken <- character()
    
    dfAvail <- ff
    
    dfDraft <- data.frame(matrix("",length(availPlayers),length(teams)), stringsAsFactors = F) #data.frame(matrix(availPlayers,length(availPlayers),length(teams)))
    colnames(dfDraft) <- teams
    
    rosters <- data.frame(matrix("",length(availPlayers),length(teams)), stringsAsFactors = F)
    colnames(rosters) <- teams; rownames(rosters) <- rosterPositions[1:nrow(rosters)]; #rosters[,] <- ""
    
    draftResults <- data.frame('Overall'=integer(),'Round'=integer(),'Team'=character(),'Pick'=character(), stringsAsFactors = F)
    n <- 0
    for(x in 1:nRounds){#x=1
      if(IsOdd(x)){
        roundOrder <- 1:length(teams)
      }else{
        roundOrder <- length(teams):1
      }
      for(t in roundOrder){#t=1
        n <- n + 1
        draftResults[n,'Overall'] <- n
        draftResults[n,'Round'] <- x
        draftResults[n,'Team'] <- teams[t]
      }
    }
    draftResults$Pick <- ""
    draftForecast <- forecastDraft(draftResults,ff)
    
    seasonProjection <- leagueProjection(ff,draftForecast,starterPositions)
    
    #Traded Draft Picks
    tradedPicks <- getTradedPicks(leagueId) %>% filter(season == Year(Sys.Date()))
    dR <- sapply(1:nrow(tradedPicks),function(x){
      tPick <- tradedPicks[x,]
      which(draftResults$Round == tPick$round & draftResults$Team == tPick$prevowner)
    })
    if(length(dR)>0){
      for(tP in dR){#tP=dR[1]
        dPick <- draftResults[tP,]
        tPick <- tradedPicks[tradedPicks$round == dPick$Round & tradedPicks$prevowner == dPick$Team,]
        if(nrow(tPick)==1){
          draftResults[tP,'Team'] <- tPick$newowner
          print(paste('Traded Pick in Round ',dPick$Round,'to',tPick$newowner,'from',tPick$prevowner))
        }
      }
      
    }
    
    save(dfDraft,teams,playersAvail,dfAvail,rosters,availPlayers,playersTaken,draftResults,draftForecast,seasonProjection,playersTakenCount,StartPickTime, file = draftFile) 
  }
}