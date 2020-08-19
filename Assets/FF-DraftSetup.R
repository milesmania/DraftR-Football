## Load File or Setup New ###########################################  
if(file.exists(draftFile)){
  load(draftFile)
}else{
  playersAvail <- unique(ff$pId)
  playerLevels <- c("",playersAvail)
  availPlayers = factor(rep("",nRounds),levels=playerLevels)
  #ff$pId <- factor(ff$pId,levels=playerLevels)
  playersTaken <- character()
  
  dfDraft = data.frame(matrix("",length(availPlayers),length(teams)), stringsAsFactors = F) #data.frame(matrix(availPlayers,length(availPlayers),length(teams)))
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
  # if(draftResults[96,]$Team == "Miles") draftResults[96,]$Team <- "Nate"
  # if(draftResults[59,]$Team == "Nate") draftResults[59,]$Team <- "Miles"
  
  StartPickTime <- Sys.time()# + 4*60
  playersTakenCount <- 0
  
  save(dfDraft,teams,playersAvail,rosters,availPlayers,playersTaken,draftResults,draftForecast,seasonProjection,playersTakenCount,StartPickTime, file = draftFile)
}