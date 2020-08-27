
## User Functions ###########################################

#grid.newpage();grid.draw(roundupGraph(ffDT <- ff,pos <- "RB",nPlayers=75)) #pos <- "QB"
roundupGraph <- function(ffDT,pos,playersTaken=NULL,nPlayers=25,yVal="pos_rank",xVal="points",showTaken=TRUE,
                         showRisk=FALSE) {
  ffDT <- ffDT[grepl(pos,ffDT$pos),]
  
  if(!is.null(playersTaken)){
    ffTaken <- ffDT[ffDT$pId %in% playersTaken,]
    if(nrow(ffTaken)>0){
      ffTaken$yMin <- ffTaken[,yVal]-.5
      ffTaken$yMax <- ffTaken[,yVal]+.5
      if(!showTaken) ffDT <- ffDT[!(ffDT$pId %in% ffTaken$pId),]
    } 
  }
  ffDT <- head(ffDT,nPlayers)
  ffDT$tier <- factor(ffDT$tier)
  switch(xVal,"points"={xMin<-"lower";xMax<-"upper"},
         {xMin <- paste0(xVal,"Low"); xMax <- paste0(xVal,"High")})
  xlimit<- max(ffDT[,xMax],na.rm=TRUE)+3
  ffDT$xValRnd <- round(ffDT[,xVal],0)
  ffDT$riskRnd <- round(ffDT$risk,0)
  p <- ggplot(ffDT, aes_string(x=xVal, y=yVal, color="tier")) 
  p <- p +geom_errorbarh(aes_string(xmin=xMin,xmax=xMax),height=.3)+
    geom_point(size=5,color="white")
  p <- p + geom_text(aes_string(x=xVal, label="xValRnd"),size=3,show.legend = FALSE)+
    geom_text(aes(x=xlimit,label=pId),
              hjust=1.2, angle=(0), size=3,show.legend = FALSE)
  
  if(showRisk) p <- p + geom_text(aes_string(x=xMax,label="riskRnd"),color="red",fontface="bold",
                                  hjust=-0.2, angle=(0), size=3,show.legend = FALSE)
  
  if(!is.null(playersTaken) & showTaken){
    if(nrow(ffTaken)>0){
      ffTaken$yMin <- ffTaken[,yVal]-.5
      ffTaken$yMax <- ffTaken[,yVal]+.5
      p <- p + geom_rect(data=ffTaken,aes(ymin=yMin,ymax=yMax),xmin=0,xmax=xlimit,alpha=0.5,fill="grey",color="grey")
    } 
  }
  
  p <- p + theme_minimal()+
    theme(
      #plot.background = element_blank(),
      #panel.grid.major.x = element_line(color="grey"),
      #panel.grid.minor.y = element_line(color="grey"),
      #panel.border=element_rect(color="grey",fill=NA),
      #panel.background = element_blank(),
      legend.position = "none" #c(0.95, 0.1)
    ) + scale_y_reverse()+
    ylab(yVal) + xlab(paste("Median",xVal,"Projection")) +
    labs(title = paste(pos,"Position Tier",xVal,"Projections"))+
    coord_cartesian(xlim =c(0,xlimit)) #+scale_color_colorblind()
  #p
  p
}
#dForcast <- forecastDraft(draftResults,ff)
#dForcast <- forecastDraft(draftResults,ff)
forecastDraft <- function(draftResults,ff){
  if(is.null(draftResults)) return(NULL)
  dForcast <- draftResults
  dForcast$Selected <- ifelse(dForcast$Pick!="","selected","forecast")
  dForcast$ForcastComment <- NA
  rForecast <- dForcast$Selected!="selected"
  dFF <- ff[!(ff$pId %in% dForcast[!rForecast,'Pick']),]
  if(any(rForecast)){
    for(rF in dForcast[rForecast,'Overall']){
      #for(rF in 1:55){#rF=56
      dTeam <- dForcast[rF,'Team']
      dPlayers <- subset(dForcast,Pick!="" & Team == dTeam,Pick,drop=T)
      dComments <- ""
      if(length(dPlayers) > 0){
        #Enforce position caps
        if(length(grep("RB$|WR$",dPlayers)) >= 7){
          dComments <- paste(dComments,"7 or more RB/WR;")
          if(length(grep("QB$",dPlayers)) == 0){
            dForcast[rF,'Pick'] <- head(dFF[dFF$pos=="QB",'pId'],1)
            dComments <- paste(dComments,"F1 QB:",paste(head(dFF[dFF$pos=="QB",'name'],3),collapse=","))
          }
          else if(length(grep("TE$",dPlayers)) == 0){
            dForcast[rF,'Pick'] <- head(dFF[dFF$pos=="TE",'pId'],1)
            dComments <- paste(dComments,"F1 TE:",paste(head(dFF[dFF$pos=="TE",'name'],3),collapse=","))
          }
        }
        if(length(grep("RB$|WR$",dPlayers)) >= 9){
          dComments <- paste(dComments,"9 or more RB/WR;")
          if(length(grep("QB$",dPlayers)) == 0){
            dForcast[rF,'Pick'] <- head(dFF[dFF$pos=="QB",'pId'],1)
            dComments <- paste(dComments,"F2 QB:",paste(head(dFF[dFF$pos=="QB",'name'],3),collapse=","))
          }
          else if(length(grep("TE$",dPlayers)) == 0){
            dForcast[rF,'Pick'] <- head(dFF[dFF$pos=="TE",'pId'],1)
            dComments <- paste(dComments,"F2 TE:",paste(head(dFF[dFF$pos=="TE",'name'],3),collapse=","))
          }
          else if(length(grep("DST$",dPlayers)) == 0){
            dForcast[rF,'Pick'] <- head(dFF[dFF$pos=="DST",'pId'],1)
            dComments <- paste(dComments,"F2 DST:",paste(head(dFF[dFF$pos=="DST",'name'],3),collapse=","))
          }
          else if(length(grep("K$",dPlayers)) == 0){
            dForcast[rF,'Pick'] <- head(dFF[dFF$pos=="K",'pId'],1)
            dComments <- paste(dComments,"F2 K:",paste(head(dFF[dFF$pos=="K",'name'],3),collapse=","))
          }
        }
        if(dForcast[rF,'Pick'] == "") {
          dRestrict <- character()
          if(length(grep("K$",dPlayers)) >= 1) dRestrict <- c(dRestrict,"K$")
          if(length(grep("DST$",dPlayers)) >= 1) dRestrict <- c(dRestrict,"DST$")
          if(length(grep("QB$",dPlayers)) >= 2) dRestrict <- c(dRestrict,"QB$")
          if(length(grep("TE$",dPlayers)) >= 2) dRestrict <- c(dRestrict,"TE$")
          if(length(grep("RB$",dPlayers)) >= 4) dRestrict <- c(dRestrict,"RB$")
          if(length(grep("WR$",dPlayers)) >= 4) dRestrict <- c(dRestrict,"WR$")
          if(length(dRestrict)>5){
            dComments <- paste(dComments,"All Restricted, Remove Position Player Restrictions:")
            dRestrict <- c("K$","DST$")
          }
          if(length(dRestrict) > 0){
            dRestrict <- paste(dRestrict,collapse = "|")
            if(any(!grepl(dRestrict,dFF$pos))){
              dForcast[rF,'Pick'] <- head(dFF[!grepl(dRestrict,dFF$pos),'pId'],1)
              dComments <- paste(dComments,"Restrict:",gsub("\\$","",dRestrict))
              dComments <- paste(dComments,"FR:",paste(head(dFF[!grepl(dRestrict,dFF$pos),'pId'],5),collapse=","))
            }
          }
        }
      }
      if(dForcast[rF,'Pick'] == ""){
        dForcast[rF,'Pick'] <- head(dFF$pId,1)
        dComments <- paste(dComments,"F:",paste(head(dFF$pId,5),collapse=","))
      }
      dForcast[rF,'ForcastComment']  <- dComments
      dFF <- dFF[dFF$pId != dForcast[rF,'Pick'], ]
    }
    #dForcast[rForecast,'Pick'] <- ff[1:nrow(dForcast[rForecast,]),'pId']
  }
  dForcast <- merge(dForcast,ff, by.x = "Pick", by.y = "pId", all.x=TRUE, sort=FALSE)
  dForcast <- dForcast[,c(2:4,1,5:ncol(dForcast))]
  dForcast <- dForcast[order(dForcast$Overall),]
  return(dForcast)
}

draftTablePopulate <- function(dfDraft,draftForecast){
  dTable <- dfDraft
  dFCast <- draftForecast %>% select(Overall,Round,Team,Pick,team,pos,Selected,ForcastComment) %>% unique()
  
  dComments <- matrix(data=NA,nrow=nrow(dTable),ncol=ncol(dTable))
  for(j in 1:ncol(dTable)){ #j=1
    dFCastCol <- dFCast[dFCast$Team==colnames(dTable)[j],]
    dTable[,j] <- dFCastCol$Pick
    dRowFcast <- dFCastCol$Selected=="forecast"
    #dFCastCol <- dFCastCol[dForecastRows,]
    dPosColor <- sapply(dFCastCol$pos,function(x){switch(x,"QB"="pink","RB"="lightgreen","WR"="lightblue",
                                                         "TE"="orange","DST"="violet","K"="white","white")
    })
    dTable[,j] <- dTable[,j] %>% text_spec(format="html", background = dPosColor, #ifelse(dRowFcast,"white",dPosColor),
                                           angle = ifelse(dRowFcast,10,0), 
                                           #color = ifelse(dRowFcast,dPosColor,"black"), 
                                           bold = !dRowFcast,
                                           font_size = ifelse(dRowFcast,"x-small","small")
                                           #,tooltip = dFCastCol$ForecastComment
                                           #,popover = spec_popover(content = dFCastCol$Selected, trigger = "click", position = "auto")
    )
    #dTable[grepl("RB",dFCastCol$Pick),j] <- dTable[grepl("RB",dFCastCol$Pick),j] %>% cell_spec(background = "blue")
    dComments[dFCast[dFCast$Team==colnames(dTable)[j],"Selected"]=="forecast",j] <- "Forecast"
  }
  
  dTableKable <- kable(dTable,"html", escape = F, row.names = T) %>% kable_styling("striped", full_width = T)
  return(dTableKable)
}

draftChart <- function(dForcast){
  draftPoints <- dForcast %>%
    group_by(Team) %>%
    summarise(actual = sum(points[Selected=="selected"]),
              actualLo = sum(lower[Selected=="selected"]),
              actualHi = sum(upper[Selected=="selected"]),
              forecast = sum(points),
              forecastLo = sum(lower),
              forecastHi = sum(upper), .groups = 'drop')
  draftPoints$Team <- factor(draftPoints$Team)
  p <- ggplot(draftPoints, aes(x=actual, y=forecast, color=Team, fill=Team)) 
  p <- p + geom_rect(aes(xmin=actualLo,xmax=actualHi, ymin=forecastLo,ymax=forecastHi), alpha = 0.5)+
    geom_point(size=5,color="white")
  p <- p + geom_label_repel(aes(label=paste0(Team,"\nActual:",round(actual,0),"\nForecast:",round(forecast,0))),
                            size=3,color="black",fontface="bold",show.legend = FALSE)
  p <- p + theme_fivethirtyeight()+
    theme(legend.position = "none" #c(0.95, 0.1)
    ) + #ylab(yVal) + xlab(paste("Median",xVal,"Projection")) +
    labs(title = paste("Draft Projections after", nrow(dForcast[dForcast$Selected=="selected",]),"picks"))
  p
}

setRoster <- function(draftedPlayers,showForecast=TRUE,
                      rosterPositions = c('QB-1','RB-1','RB-2','WR-1','WR-2','RB|WR-1','TE-1','DST-1','K-1','BE-1','BE-2','BE-3','BE-4','BE-5','BE-6','BE-7','BE-8')
){#draftedPlayers <- draftForecast
  #Set Roster Data
  if(is.null(draftedPlayers)) return(NULL)
  teams <- unique(draftedPlayers$Team)
  rosters <- data.frame(matrix("",length(rosterPositions),length(teams)), stringsAsFactors = F)
  colnames(rosters) <- teams; rownames(rosters) <- rosterPositions; #rosters[,] <- ""
  
  if(!showForecast) draftedPlayers <- subset(draftedPlayers,Selected == "selected")
  for(t in 1:length(teams)){#t=1
    teamPlayers <- draftedPlayers[draftedPlayers$Team == teams[t],]
    teamPlayers <- teamPlayers[order(teamPlayers$points,decreasing = T),]
    if(nrow(teamPlayers) > 0){
      teamDrafted <- character()
      for(tP in 1:nrow(teamPlayers)){#tP=1
        tPos <- paste0(teamPlayers[tP,'pos'],"|BE")
        newPlayer <- paste0(teamPlayers[tP,c('name','pos','team','bye')], collapse = "|")
        if(teamPlayers[tP,"Selected"]=="forecast") newPlayer <- paste0("(",newPlayer,")")
        if(!(newPlayer %in% rosters[,teams[t]]) & any(rosters[grepl(tPos,rownames(rosters)),teams[t]]=="") ){
          availSlots <- rownames(rosters)[grepl(tPos,rownames(rosters)) & rosters[,teams[t]] == ""]
          rosters[availSlots[1],teams[t]] <- newPlayer
          teamDrafted <- c(teamDrafted, newPlayer)
        }
      }
    }
  }
  return(rosters)
}

setRosterKable <- function(draftedPlayers,showForecast=TRUE,
                           rosterPositions = c('QB-1','RB-1','RB-2','WR-1','WR-2','RB|WR-1','TE-1','DST-1','K-1','BE-1','BE-2','BE-3','BE-4','BE-5','BE-6','BE-7','BE-8')
){#draftedPlayers <- draftForecast
  #Set Roster Data
  rosters <- setRoster(draftedPlayers,showForecast,rosterPositions)
  return(rosters)
}

setConfigTxt <- function(configFile="Assets/config.txt"){
  if(!file.exists(configFile)){
    assign("draftId","", envir = .GlobalEnv)
    assign("leagueId","", envir = .GlobalEnv)
    return(NULL)
  }
  key.val<-read.table(configFile, sep="=", col.names=c("key","value"), as.is=c(1,2))
  config <- key.val$value; names(config) <- key.val$key
  for(kV in 1:length(config)){
    assign(names(config)[kV],config[kV], envir = .GlobalEnv)
  }
  return(config)
}

ffDataAvail <- function(dataAvail,dForecast){
  dtF <- datatable(dataAvail[,c('name','pos','team','bye','age')], 
                   options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 25)) %>%
    formatStyle("pos",target = 'row',
                backgroundColor = styleEqual(levels=c("QB","RB","WR","TE","DST","K"),
                                             values=c("pink","lightgreen","lightblue","orange","violet","lightgrey")))
  
  if("Team" %in% names(dForecast)){
    myPlayers <- subset(dForecast,Team==MyTeam & Selected=="forecast","name",drop=T)
    dtF <- dtF %>% 
      formatStyle(columns = "name", border = styleEqual(levels=myPlayers, values=rep('3px dashed red',length(myPlayers))))
  }
  return(dtF)
}

## Sleeper Draft ####
#picksToUpdate <- updateDraftFromSleeper(draftId,draftResults)
#dfDraft <- draftPopulate(picksToUpdate,dfDraft)
updateDraftFromSleeper <- function(draftId,draftResults,allPlayers){#draftId=469304291434164225
  dPlayers <- getDraftFromSleeper(draftId, allPlayers)
  if(nrow(dPlayers) > 0){
    dPlayers$pId <- sapply(1:nrow(dPlayers), function(x){
      if(!is.na(dPlayers[x,'pId'])){
        dPlayers[x,'pId']
      }else{
        if(dPlayers[x,'position'] == "DEF"){
          paste(dPlayers[x,'last_name'], dPlayers[x,'team'], "DST", sep="|")
        }else{
          paste(dPlayers[x,'name'], dPlayers[x,'team'], dPlayers[x,'position'], sep="|")
        }
      }
    })
    unpicked <- draftResults[draftResults$Pick=="","Overall"]
    slPicked <- dPlayers$pick_no
    picksToFill <- slPicked[slPicked %in% unpicked]
    return(dPlayers[dPlayers$pick_no %in% picksToFill,c('round','draft_slot','pId')])
  }
  return(dPlayers)
}

draftPopulate <- function(picksToUpdate,dfDraft){
  if(nrow(picksToUpdate) > 0){
    for(dD in 1:nrow(picksToUpdate)){#dD=1
      dPick <- picksToUpdate[dD,]
      dfDraft[dPick$round,dPick$draft_slot] <- dPick$pId
    }
  }
  return(dfDraft)
}
draftPopulateResults <- function(dfDraft,draftResults){
  for(x in 1:nrow(dfDraft)){#x=11  #Rounds
    for(t in 1:ncol(dfDraft)){#t=10  #Teams
      pick <- draftResults[draftResults$Round == x & draftResults$Team == colnames(dfDraft)[t],"Overall"]
      draftResults[pick, 'Pick'] <- as.character(dfDraft[x,t])
    }
  }
  return(draftResults)
}

updateRostersFromSleeper <- function(leagueId,draftResults){#leagueId=469304291434164224
  sLeague <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",leagueId), flatten = TRUE)
  sUsers <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",leagueId,"/users"), flatten = TRUE)
  sRoster <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",leagueId,"/rosters"), flatten = TRUE)
  sPlayers <- getPlayersFromSleeper() 
  if(nrow(sPlayers) > 0){
    unpicked <- draftResults[draftResults$Pick=="","Overall"]
    slPicked <- sPlayers$pick_no
    picksToFill <- slPicked[slPicked %in% unpicked]
  }
  return(sPlayers[sPlayers$pick_no %in% picksToFill,c('round','draft_slot','pId')])
}

#leagueId=469304291434164224
updatePlayersFromSleeper <- function(pFileName=gsub("Draft","Player",draftFile), ...){
  if(file.exists(pFileName)){
    if(file.info(fffile)$mtime >= Sys.Date()){
      load(pFileName)
      return(allPlayers)
    }
  }
  
  allPlayers <- getPlayersFromSleeper()
  #allPlayers <- updateProjectionsFromSleeper(sPlayers=allPlayers,leagueId)
  save(allPlayers, file = pFileName)
    
  return(allPlayers)
}



#allPlayers <- correctSleeperNames(allPlayers)
correctSleeperNames <- function(sPlayers){
  #sPlayers <- allPlayers
  # sPlayers[sPlayers$player_id == 1408,"name"] <- "LeVeon Bell"
  # sPlayers[sPlayers$team == "JAX","team"] <- "JAC"
  # sPlayers[sPlayers$name == "Juju Smith-Schuster","name"] <- "JuJu Smith-Schuster"
  sPlayers[grepl(" Jr\\.$",sPlayers$name),"name"] <- gsub(" Jr\\.$","",sPlayers[grepl(" Jr\\.$",sPlayers$name),"name"])
  sPlayers[grepl(" Jr$",sPlayers$name),"name"] <- gsub(" Jr$","",sPlayers[grepl(" Jr$",sPlayers$name),"name"])
  sPlayers[grepl(" II$",sPlayers$name),"name"] <- gsub(" II$","",sPlayers[grepl(" II$",sPlayers$name),"name"])
  sPlayers[grepl(" III$",sPlayers$name),"name"] <- gsub(" III$","",sPlayers[grepl(" III$",sPlayers$name),"name"])
  sPlayers[grepl("^DEF$",sPlayers$position),"position"] <- "DST"
  sPlayers[grepl("^FB$",sPlayers$position),"position"] <- "RB"
  # sPlayers[sPlayers$name == "Odell Beckham Jr","name"] <- "Odell Beckham"
  # sPlayers[sPlayers$name == "Mark Ingram II","name"] <- "Mark Ingram"
  # sPlayers[sPlayers$name == "Todd Gurley II","name"] <- "Todd Gurley"
  # sPlayers[sPlayers$name == "Melvin Gordon III","name"] <- "Melvin Gordon"
  # sPlayers[sPlayers$name == "Devante Parker","name"] <- "DeVante Parker"
  sPlayers[sPlayers$name == "D.J. Chark","name"] <- "DJ Chark"
  # sPlayers[sPlayers$name == "CJ Anderson","name"] <- "C.J. Anderson"
  # sPlayers[sPlayers$name == "OJ Howard","name"] <- "O.J. Howard"
  # sPlayers[sPlayers$name == "AJ Green","name"] <- "A.J. Green"
  # sPlayers[sPlayers$name == "Mitch Trubisky","name"] <- "Mitchell Trubisky"
  # sPlayers[sPlayers$name == "Chris Herndon IV","name"] <- "Chris Herndon"
  # sPlayers[sPlayers$player_id == 5230,"name"] <- "Mike Badgley"
  # sPlayers[sPlayers$player_id == 3451,"name"] <- "Kaimi Fairbairn"
  # sPlayers[sPlayers$player_id == 5052,"name"] <- "Ronald Jones"
  return(sPlayers)
}

getNFLSchedule <- function(){
  nflSchedFile <- "Data/NFL-Schedule.csv"
  nflSched <- read.csv(nflSchedFile,stringsAsFactors = F)
  colnames(nflSched)[2:18] <- sapply(1:17,function(x) paste0("Wk",formatC(x, width=2, flag="0")) )
  #nflSched <- apply(nflSched,c(1,2),function(x) gsub("JAX","JAC",x))
  return(nflSched)
}

updateTeamNames <- function(ff){
  oldNFLNames <- c("GBP","KCC","JAC","LVR","NEP","NOS","SFO","TBB","WAS")
  newNFLNames <- c("GB","KC","JAX","LV","NE","NO","SF","TB","WSH")
  ff[ff$team %in% oldNFLNames,'team'] <- newNFLNames[match(ff$team,oldNFLNames,nomatch = 0)]
  return(ff)
}

updateStatNames <- function(ff){
  oldnames <- c('ceiling','floor','points_vor','ceiling_vor','floor_vor','drop_off')
  newnames <- c('upper','lower','vor','vorHigh','vorLow','dropoff')
  oldnamesToReplace <- (oldnames %in% colnames(ff))
  if(any(oldnamesToReplace)){
    oldnames <- oldnames[oldnamesToReplace]; newnames <- newnames[oldnamesToReplace]
    ff <- ff %>% rename_at(vars(oldnames), ~newnames)
  }
  return(ff)
}

## Sleeper Projections ####
#sP <- updateProjectionsFromSleeper(sPlayers,leagueId)
updateProjectionsFromSleeper <- function(sPlayers,leagueId,sYr=2020,wks=1:16){#leagueId=469304291434164224
  for(wk in wks){#wk=3
    sPlayers <- getProjectionsFromSleeper(sPlayers,leagueId,sYr,wk)
  } 
  return(sPlayers)
}

getProjectionsFromSleeper <- function(sPlayers,leagueId,sYr=2020,wk=1,ptsCol="pts_std"){#leagueId=469304291434164224
  sP <- sPlayers
  pCol <- paste0("PtsWk",formatC(wk, width=2, flag="0"))
  sP[,pCol] <- 0
  wkPrj <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/projections/nfl/regular/",sYr,"/",wk), flatten = TRUE) 
  for(i in 1:nrow(sP)){#i=4
    pStats <- unlist(wkPrj[[sP[i,"player_id"]]])
    if(!is.null(pStats)){
      if(ptsCol %in% names(pStats)){
        sP[i,pCol] <- pStats[ptsCol]
      }
    }
  }
  return(sP)
}

leagueProjection <- function(ff,draftForecast,starterPositions,wks=1:16){
  aPlayers <- ff[,c("pId",colnames(ff)[grep("Wk",colnames(ff))])]
  colnames(aPlayers)[1] <- "Pick"
  dForcast <- draftForecast[,1:6]
  dPrj <- merge(dForcast,aPlayers,all.x=TRUE)
  fTeams <- unique(dPrj$Team)
  lPrj <- data.frame("Team" = character(),"Selected" = character(),"Player" = character(),"Pts" = numeric(),"Position" = character(),"Wk" = integer(), stringsAsFactors = F)
  for(wk in wks){#wk=1
    pCol <- paste0("PtsWk",formatC(wk, width=2, flag="0"))
    for(fT in fTeams){#fT="Miles"
      tR <- dPrj[dPrj$Team==fT,]
      tR <- tR[order(tR[,pCol], decreasing = TRUE),]
      for(sP in starterPositions){#sP="QB-1"
        sPP <- unlist(strsplit(sP,"-"))[1]
        tRR <- head(tR[grepl(paste0(sPP,"$"),tR$Pick),],1)
        if(nrow(tRR)==0){
          lPrj[nrow(lPrj)+1,] <- data.frame("Team" = fT,"Selected" = "forecast","Player" = "","Pts" = 0,"Position" = sP,"Wk" = wk, stringsAsFactors = F)
        }else{
          lP <- tRR[1,c("Team","Selected","Pick",pCol)]; colnames(lP)[4] <- "Pts"
          lP$Position <- sP; lP$Wk <- wk
          lPrj <- rbind(lPrj,lP)
          tR <- tR[tR$Pick!=tRR$Pick,]
        }
      }
      if(nrow(tR)>0){
        #SUM TOTAL  POINTS
        lPrj[nrow(lPrj)+1,] <- data.frame("Team" = fT,"Selected" = "forecast","Player" = "TOTAL",
                                          "Pts" = lPrj %>% filter(Team==fT & Wk==wk) %>% summarise(sum(Pts,na.rm=T)),
                                          "Position" = "TOTAL","Wk" = wk, stringsAsFactors = F)
        #Include Bench Players
        lP <- tR[,c("Team","Selected","Pick",pCol)]; colnames(lP)[4] <- "Pts"
        lP$Position <- paste0("BE-",1:nrow(lP)); lP$Wk <- wk
        lPrj <- rbind(lPrj,lP)
        #SUM BENCH POINTS
        lPrj[nrow(lPrj)+1,] <- data.frame("Team" = fT,"Selected" = "forecast","Player" = "BENCH","Pts" = sum(tR[,pCol],na.rm=T),"Position" = "BENCH","Wk" = wk, stringsAsFactors = F)
      }
    }
  }
  return(lPrj)
}

leagueProjection_Plot <- function(lPrj){
  lPrj2 <- subset(lPrj,!grepl("BENCH|TOTAL",Position)) %>% group_by(Team,Wk) %>% summarise(Pts = sum(Pts,na.rm=T), .groups = 'drop')
  p <- ggplot(lPrj2) + geom_line(aes(x=Wk,y=Pts,color=Team)) + theme_fivethirtyeight() +
    labs(title = "Weekly Points Projections")
  
  return(p)
}

leagueProjection_Table <- function(lPrj){
  lPrj2 <- subset(lPrj,!grepl("TOTAL|BE",Position)) %>% group_by(Team,Wk) %>% summarise(Pts = sum(Pts,na.rm=T), .groups = 'drop')
  lPrj3 <- lPrj2 %>% tidyr::spread(Wk,Pts)
  lPrj3$Regular <- sapply(1:nrow(lPrj3), function(x) round(sum(lPrj3[x,2:15], na.rm=T)/14,2))
  lPrj3$Playoff <- sapply(1:nrow(lPrj3), function(x) round(sum(lPrj3[x,16:17], na.rm=T)/2,2))
  lPrj3 <- as.data.frame(lPrj3)
  return(lPrj3)
}

leagueProjection_Kable <- function(lPrj3){
  lPrj3K <- lPrj3 %>% mutate_if(is.numeric, function(x){
    cell_spec(round(x,2), bold = T, color = spec_color(x,end=0.8,option="C",direction=-1),
              font_size = spec_font_size(x))
  }) %>% 
    kable(escape = F, format="html", align = "c", digits = 1) %>%
    kable_styling(c("striped","condensed"))
  return(lPrj3K)
}

leagueProjection_Rank <- function(lPrj3){
  lPrj4 <- lPrj3; lPrj4[,2:16] <- apply(lPrj4[,2:16],c(1,2),as.integer)
  Teams <- lPrj3$Team
  for(i in 2:ncol(lPrj3)){#i=2
    lRnk <- lPrj3[order(lPrj3[,i],decreasing=TRUE),c(1,i)]
    lPrj4[,i] <- sapply(1:nrow(lPrj4), function(x) which(lRnk$Team==lPrj4[x,"Team"]))
  }
  
  return(lPrj4)
}
#leagueProjection_RankKable(leagueProjection_Rank(leagueProjection_Table(lPrj)))
leagueProjection_RankKable <- function(lPrj4){
  lPrj4K <- lPrj4 %>% mutate_if(is.numeric, function(x){
    cell_spec(x, bold = T, color = spec_color(x,end=0.8,option="C"),
              font_size = spec_font_size(x, scale_from=c(12,1)))
  }) %>% 
    kable(escape = F, format="html", align = "c") %>%
    kable_styling(c("striped","condensed"))
  return(lPrj4K)
}

leagueProjection_TeamSeason <- function(lPrj,team){
  lTeam <- subset(lPrj,Team == team) %>% group_by(Position,Wk) %>% summarise(Player = paste(sum(Pts,na.rm=T),"||",Pick), .groups = 'drop')
  lPrj3 <- lPrj2 %>% tidyr::spread(Wk,Pts)
  lPrj3 <- as.data.frame(lPrj3)
  return(lPrj3)
}

#leagueProjection_Week(lPrj,wk)
#lPrj <- leagueProjection(ff, draftForecast,starterPositions)
leagueProjection_Week <- function(lPrj,wk){#wk=3
  positions <- unique(lPrj$Position)
  lWk <- subset(lPrj,Wk == wk) %>% group_by(Position,Team) %>% summarise(Player = paste(round(sum(Pts,na.rm=T),1),"||",Pick), .groups='drop')
  #lWkTotal <- subset(lPrj,Wk == wk & Position != "BENCH") %>% group_by(Team) %>% summarise(TOTAL = paste(sum(Pts,na.rm=T),"|| TOTAL"))
  lWk2 <- lWk %>% tidyr::spread(Team,Player)
  lWk2 <- as.data.frame(lWk2,stringsAsFactors=F); rownames(lWk2) <- lWk2$Position; lWk2 <- lWk2[,2:ncol(lWk2)]
  #lWk2[nrow(lWk2)+1,] <- unlist(sapply(1:ncol(lWk2), function(x) lWkTotal[lWkTotal$Team==colnames(lWk2)[x],"TOTAL"]))
  #rownames(lWk2)[nrow(lWk2)] <- "TOTAL"
  lWk2 <- lWk2[positions,]
  lWk2 <- lWk2[,order(as.numeric(gsub(" \\|\\| TOTAL","",lWk2["TOTAL",])), decreasing = TRUE)]
  return(lWk2)
}
#leagueProjection_WeekKable(leagueProjection_Week(lPrj,wk))
leagueProjection_WeekKable <- function(lWk2){
  lWk2k <- lWk2 %>%
    kable(escape = F, format="html", align = "c") %>%
    kable_styling(c("striped","condensed")) %>% 
      row_spec(which(rownames(lWk2)=="TOTAL"), bold = T, color = "White", background = "Black") %>%
      row_spec(which(rownames(lWk2)=="BENCH"), bold = T, color = "White", background = "Grey")
  return(lWk2k)
}

#dfAvail <- ff; dataAvail = dfAvail[!(dfAvail$pId %in% playersTaken),]
#dPrj <- leagueProjectionplayersAvail(dataAvail,allPlayers)
leagueProjectionplayersAvail <- function(dataAvail,allPlayers){
  aPlayers <- allPlayers[,c("pId",colnames(allPlayers)[grep("Wk",colnames(allPlayers))])]
  colnames(aPlayers)[grep("Wk",colnames(aPlayers))] <- gsub("Pts","",colnames(aPlayers)[grep("Wk",colnames(aPlayers))])
  dAvail <- dataAvail[,c("pId","team","pos","age","bye","points","vor")]
  dPrj <- merge(dAvail,aPlayers,all.x=TRUE, sort=FALSE)
  dPrj$points <- round(dPrj$points,1)
  dPrj$vor <- round(dPrj$vor,1)
  
  nflSched <- getNFLSchedule()
  
  for(wCol in colnames(dPrj)[grep("Wk",colnames(dPrj))]){
    dPrj[,wCol] <- sapply(1:nrow(dPrj),
                          function(x){#x=1
                            tSched <- nflSched[nflSched$TEAM==dPrj[x,"team"],wCol]
                            paste0(dPrj[x,wCol],"|",tSched)
                          })
  }
  return(dPrj)
}
#leagueProjectionplayersAvailKable(leagueProjectionplayersAvail(dataAvail,allPlayers))
leagueProjectionplayersAvailKable <- function(dPrj,pRrows=25){
  dPrj2 <- head(dPrj,25) 
  
  dPrj2k <- dPrj2 %>%
    kable(escape = F, format="html", align = "c") %>%
    kable_styling(c("striped","condensed"),font_size = 8) %>% 
    row_spec(which(dPrj2$pos=="QB"), background = "pink") %>%
    row_spec(which(dPrj2$pos=="RB"), background = "lightgreen") %>%
    row_spec(which(dPrj2$pos=="WR"), background = "lightblue") %>%
    row_spec(which(dPrj2$pos=="TE"), background = "orange") %>%
    row_spec(which(dPrj2$pos=="DST"), background = "violet") %>%
    row_spec(which(dPrj2$pos=="K"), background = "lightgrey") 
  return(dPrj2k)
}

## Sleeper API Functions ####
getDraftFromSleeper <- function(draftId,allPlayers=NULL){
  dPlayers <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/draft/",draftId,"/picks"), flatten = TRUE)
  if(is.null(dPlayers) || length(dPlayers)==0) return(data.frame('round'=integer(),'draft_slot'=integer(),'pId'=character(),stringsAsFactors = F))
  colnames(dPlayers)[colnames(dPlayers) == "metadata.player_id"] <- "metadata.sleeper_id"
  colnames(dPlayers)[grep("metadata",colnames(dPlayers))] <- gsub("metadata.","",colnames(dPlayers)[grep("metadata",colnames(dPlayers))])
  dPlayers$name <- paste(dPlayers$first_name,dPlayers$last_name)#x=1
  dPlayers <- correctSleeperNames(dPlayers)
  
  if(nrow(dPlayers) > 0){
    if(!is.null(allPlayers)){
      dPlayers <- dPlayers %>% left_join(allPlayers[,c("player_id","pId")], by = c("player_id" = "player_id"))
    }
    dPlayers$pId <- sapply(1:nrow(dPlayers), function(x){
      if(!is.na(dPlayers[x,'pId'])){
        dPlayers[x,'pId']
      }else{
        if(dPlayers[x,'position'] == "DEF"){
          paste(dPlayers[x,'last_name'], dPlayers[x,'team'], "DST", sep="|")
        }else{
          paste(dPlayers[x,'name'], dPlayers[x,'team'], dPlayers[x,'position'], sep="|")
        }
      }
    })
  }
  return(dPlayers)
}

getPlayersFromSleeper <- function(){
  nflPlayers <- jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl", flatten = TRUE)
  #nflPlayers[[1]]
  nflPlayersRows <- names(nflPlayers)
  nflPlayerCols <- character()
  for(nR in nflPlayersRows){
    nflColNames <- names(nflPlayers[[nR]])
    nflPlayerCols <- c(nflPlayerCols, nflColNames[!(nflColNames %in% nflPlayerCols)])
  }
  pL <- unlist(nflPlayers[["4034"]])
  pL1 <- unlist(nflPlayers[["3198"]])
  pL <- rbind(pL,pL1)
  pL2 <- as.data.frame(pL, stringsAsFactors=FALSE)
  for(i in 1:ncol(pL2)){#i=1
    if(!is.na(suppressWarnings(any(as.numeric(pL2[,i]))))) pL2[,i] <- as.numeric(pL2[,i])
  }
  for(nC in nflPlayerCols){
    if(!(nC %in% colnames(pL2))){
      pL2[,nC] <- NA
    }
  }
  allPlayers <- pL2[0,]
  for(nI in 1:length(nflPlayersRows)){#nI=1
    nP <- nflPlayersRows[nI]
    pL <- unlist(nflPlayers[[nP]])
    for(i in 1:ncol(allPlayers)){#i=1
      cName <- colnames(allPlayers)[i]
      if(cName %in% names(pL)){
        if(is.na(suppressWarnings(as.numeric(pL[cName])))){
          allPlayers[nI,i] <- pL[cName]
        }else{
          allPlayers[nI,i] <- as.numeric(pL[cName])
        }
      }
    }
  }
  #sPlayers <- correctSleeperNames(allPlayers)
  sPlayers <- allPlayers#[!is.na(allPlayers$position) & !is.na(allPlayers$team),]
  sPlayers$name <- paste(sPlayers$first_name,sPlayers$last_name)#x=1
  sPlayers <- correctSleeperNames(sPlayers)
  sPlayers <- updateTeamNames(sPlayers)
  if(nrow(sPlayers) > 0){
    sPlayers$pId <- sapply(1:nrow(sPlayers), function(x){#x=1
      if(!is.na(sPlayers[x,'position']) && sPlayers[x,'position'] == "DEF"){
        paste(sPlayers[x,'last_name'], sPlayers[x,'team'], "DST", sep="|")
      }else{
        paste(sPlayers[x,'name'], sPlayers[x,'team'], sPlayers[x,'position'], sep="|")
      }
    })
  }
  return(sPlayers)
}

getRostersFromSleeper <- function(leagueId,pFileName=NULL,sPlayers=NULL){
  sUsers <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",leagueId,"/users"), flatten = TRUE)
  sRoster <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",leagueId,"/rosters"), flatten = TRUE)
  
  sPicks <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",leagueId,"/traded_picks"), flatten = TRUE)
  if(is.null(pFileName)) pFileName <- "Data/FFPlayerData.RData"
  if(is.null(sPlayers)) sPlayers <- updatePlayersFromSleeper(pFileName=pFileName,leagueId)
  #sRoster[[5]]
  sRoster$User <- sapply(sRoster$owner_id,function(x) sUsers[sUsers$user_id==x, 'display_name'])
  allRoster <- data.frame(user_id=numeric(),user=character(),player_id=character(),pId=character(),stringsAsFactors = F)
  for(i in 1:nrow(sRoster)){#i=1
    pRoster <- allRoster[0,]
    pPlayer_ids <- unlist(sRoster[i,'players'])
    pPlayers <- sapply(pPlayer_ids,function(x) sPlayers[sPlayers$player_id == x, 'pId'])
    pRoster <- data.frame(user_id=rep(sRoster[i,'owner_id'],length(pPlayers)), 
                          user=rep(sRoster[i,'User'],length(pPlayers)), 
                          player_id = pPlayer_ids, pId = pPlayers,stringsAsFactors = F)
    allRoster <- rbind(allRoster,pRoster)
  }
  
  return(allRoster)
}
#getUsersFromSleeper(leagueId,draftId)
getUsersFromSleeper <- function(leagueId,draftId){
  sUsers <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",leagueId,"/users"), flatten = TRUE)
  if(length(sUsers)==0) return(NULL)
  dDraft <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/draft/",draftId), flatten = TRUE)
  nTeams <- dDraft[["settings"]]$teams
  draftOrder <- sapply(1:nTeams,function(x){#x=1
    draftSpot <- dDraft[["metadata"]][[paste0("slot_name_",x)]]
    if(is.null(draftSpot)) draftSpot <- sUsers[x,'display_name']
    draftSpot
  })
  return(draftOrder)
}
#getUserLeaguesFromSleeper(userId)
getUserLeaguesFromSleeper <- function(userId,seasons=NULL){#userId=339913611239542784
  if(is.null(seasons)) seasons <- Year(Sys.Date())-0:5
  sLeagues <- NULL
  for(s in seasons){#s=2019
    sLeague <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/user/",userId,"/leagues/nfl/",s), flatten = TRUE)
    if(length(sLeague) > 0){
      if(is.null(sLeagues)){
        sLeagues <- sLeague  
      }else{
        if(any(!(colnames(sLeague) %in% colnames(sLeagues)))){
          sLeagues[,colnames(sLeague)[!(colnames(sLeague) %in% colnames(sLeagues))]] <- NA
        }
        if(any(!(colnames(sLeagues) %in% colnames(sLeague)))){
          sLeague[,colnames(sLeagues)[!(colnames(sLeagues) %in% colnames(sLeague))]] <- NA
        }
        sLeagues <- rbind(sLeagues,sLeague)
      }
    } 
  }
  return(sLeagues)
}

getLeagueDraftsFromSleeper <- function(leagueId){#leagueIds<-unique(sLeagues$league_id)
  sDrafts <- NULL
  for(s in leagueId){
    sDraft <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",s,"/drafts/"), flatten = TRUE)
    if(length(sDraft) > 0){
      if(is.null(sDrafts)){
        sDrafts <- sDraft  
      }else{
        if(any(!(colnames(sDraft) %in% colnames(sDrafts)))){
          sDrafts[,colnames(sDraft)[!(colnames(sDraft) %in% colnames(sDrafts))]] <- NA
        }
        if(any(!(colnames(sDrafts) %in% colnames(sDraft)))){
          sDraft[,colnames(sDrafts)[!(colnames(sDrafts) %in% colnames(sDraft))]] <- NA
        }
        sDrafts <- rbind(sDrafts,sDraft)
      }
    } 
  }
  return(sDrafts)
}

getLeagueTransFromSleeper <- function(leagueId, wks = 0:20){#leagueIds<-unique(sLeagues$league_id)
  sTrans <- NULL
  for(w in wks){#w=2
    sTran <- jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/league/",leagueId,"/transactions/",w), flatten = TRUE)
    if(length(sTran) > 0){
      sTran <- sTran[sTran$status=="complete",]
      sTran$Week <- w; sTran$add <- NA; sTran$drop <- NA
      if(any(sTran$type=="trade")){
        sTradesRaw <- sTran[sTran$type=="trade",]
        sTrades <- sTradesRaw[0,]
        sCols <- colnames(sTrades)[grepl("drops|adds",colnames(sTrades))]
        for(sT in 1:nrow(sTradesRaw)){#sT=1
          sRosters <- sTradesRaw[sT,'roster_ids'][[1]]
          for(sR in sRosters){#sR <- sRosters[2]
            sTrade <- sTradesRaw[sT,]
            sTrade$roster_ids <- sR
            aCols <- sCols[which(sapply(sCols,function(x) sTradesRaw[sT,x]==sR))]
            sTrade[,sCols[!(sCols %in% aCols)]] <- NA
            sTrades[nrow(sTrades)+1,] <- sTrade
          }
        }
        if(nrow(sTrades)>0){
          sTranNoTrade <- sTran[!(sTran$transaction_id %in% sTrades$transaction_id),]
          sTran <- rbind(sTranNoTrade,sTrades)
        }
      }
      for(aCol in colnames(sTran)[grepl("drops|adds",colnames(sTran))]){
        aSplit <- strsplit(aCol,'\\.')[[1]]
        aType <- substr(aSplit[1],1,nchar(aSplit[1])-1)
        aPlayerId <- aSplit[2]
        sTran[!is.na(sTran[aCol]),aType] <- aPlayerId
      }
      sTran <- sTran[,!grepl("drops|adds",colnames(sTran))]
      if(is.null(sTrans)){
        sTrans <- sTran  
      }else{
        if(any(!(colnames(sTran) %in% colnames(sTrans)))){
          sTrans[,colnames(sTran)[!(colnames(sTran) %in% colnames(sTrans))]] <- NA
        }
        if(any(!(colnames(sTrans) %in% colnames(sTran)))){
          sTran[,colnames(sTrans)[!(colnames(sTrans) %in% colnames(sTran))]] <- NA
        }
        sTrans <- rbind(sTrans,sTran)
      }
    } 
  }
  return(sTrans)
}

#sKeepers <- getKeeperDraftRound(leagueId = "469304291434164224",ff,writeFile="Data/Keepers.csv", allPlayers=allPlayers)
getKeeperDraftRound <- function(leagueId, ff=NULL, draftId = NULL, writeFile = NULL, allPlayers = NULL){
  #leagueId <- "469304291434164224"
  if(is.null(draftId)) draftId <- getLeagueDraftsFromSleeper(leagueId)$draft_id
  sDraft <- getDraftFromSleeper(draftId, allPlayers)
  sRoster <- getRostersFromSleeper(leagueId)
  sTrans <- getLeagueTransFromSleeper(leagueId)
  sPlayers <- updatePlayersFromSleeper()
  sDrafted <- left_join(sRoster,sDraft[,c("player_id","round")], by=c("player_id"))
  sAdds <- sTrans %>% filter(!grepl("trade|commiss",type)) %>% select(type,Week,add,drop)
  sCommiss <- sTrans %>% filter(grepl("commiss",type)) %>% select(type,Week,add,drop) %>% filter(!(add %in% sAdds$add))
  if(nrow(sCommiss)>0) sAdds <- rbind(sAdds,sCommiss); sAdds <- sAdds[order(sAdds$Week),]
  sTrades <- sTrans %>% filter(type == "trade") %>% select(type,Week,add,drop)
  latestAdd <- sAdds[0,]
  for(sA in nrow(sAdds):1){
    sAdd <- sAdds[sA,]
    if(!(sAdd$add %in% latestAdd$add)) latestAdd[nrow(latestAdd)+1,] <- sAdd
  }
  for(sA in nrow(sTrades):1){
    sAdd <- sTrades[sA,]
    if(!(sAdd$add %in% latestAdd$add)) latestAdd[nrow(latestAdd)+1,] <- sAdd
  }
  latestAdd$player_id <- latestAdd$add
  latestAdd <- latestAdd %>% mutate(transaction=paste0(type," (Wk ",Week,")"))
  sKeepers <- left_join(sDrafted,latestAdd[,c("player_id","transaction")], by=c("player_id"))
  sKeepers <- sKeepers %>% mutate(ACQ=ifelse(!is.na(transaction)&!grepl("trade",transaction),transaction,
                                             paste("DRAFT: ",round,ifelse(is.na(transaction),"",transaction))
                                                    ),
                                  KeeperRound=ifelse(!is.na(transaction)&!grepl("trade",transaction),8,round-1))
  sKeepers$KeeperRound <- pmax(1,sKeepers$KeeperRound)
  if(!is.null(ff)){
    sKeepers <- left_join(sKeepers,ff[,c("pId",colnames(ff)[grepl("adp",colnames(ff))])],by="pId")
  }
  if(!is.null(writeFile)) write.csv(sKeepers,file=writeFile)
  return(sKeepers)
}

## ffanalytics Projections ####
getFFAnalytics_RawData <- function(ffDataFile,pos = c("QB", "RB", "WR", "TE", "K", "DST"), src_weights = NULL,
                                       season = 2020, week = 0, avgType = "robust"){
  if(file.exists(ffDataFile)){
    if(file.info(ffDataFile)$mtime >= Sys.Date()){
      load(ffDataFile)
      return(rawData)
    }
  }
  if(is.null(src_weights)) src_weights <- getFFAnalytics_SrcWeights()
  src <- names(src_weights)[src_weights > 0]
  rawData <- scrape_data(src = src, pos = pos, season = season, week = week)
  save(rawData, file = ffDataFile)
  return(rawData)
}

getFFAnalytics_Projections <- function(data_result,pos = c("QB", "RB", "WR", "TE", "K", "DST"), src_weights = NULL,
                                     season = 2020, week = 0, avgType = "robust"){
  if(is.null(src_weights)) src_weights <- getFFAnalytics_SrcWeights()
  scoring_rules <- getFFAnalytics_ScoringSettings()
  tier_thresholds <- getFFAnalytics_TierThreshold()
  ff_projections_all <- projections_table(data_result = data_result, scoring_rules = scoring_rules, src_weights = src_weights,
                                      tier_thresholds = tier_thresholds)
  ff_projections <- ff_projections_all %>% filter(avg_type == avgType) %>% select(!avg_type) #projection_table <- ff_projections
  ff_projections <- ff_projections %>% add_ecr_override() %>% #add_ecr() %>% 
    add_risk() %>%
    add_adp() %>% #add_adpaav_override(type="ADP") %>% 
    add_aav() #add_adpaav_override(type="AAV")
  ff_projections <- ff_projections %>% add_player_info_override()
  ff_projections <- ff_projections %>% add_player_bye()
  ff_projections <- ff_projections %>% mutate(name = paste(first_name,last_name))
  if("rank" %in% colnames(ff_projections))
    ff_projections <- ff_projections[order(ff_projections$rank),]
  ff_projections$sleeper <- NA
  ff_projections <- updateStatNames(ff_projections)
  ff_projections <- updateTeamNames(ff_projections)
  return(ff_projections)
}

getFFAnalytics_Weekly <- function(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                                       season = 2020, weeks = 1:16, avgType = "robust"){
  src_weights <- getFFAnalytics_SrcWeightsWeekly()
  for(week in weeks){#week=3
    ff_weekly <- getFFAnalytics_Projections(pos = pos, season = season, week = week, src_weights = src_weights, avgType = avgType)
  }
  return(ff_projections)
}

getManualProjections_Weekly <- function(ff, weeks = 1:16){
  nflSched <- getNFLSchedule()
  
  for(wk in weeks){#wk=1
    pCol <- paste0("PtsWk",formatC(wk, width=2, flag="0"))
    ff[,pCol] <- 0
    ff[ff$bye != wk,pCol] <- ff[ff$bye != wk, "points"]/16
  }
  return(ff)
}

getManualProjections_Strength <- function(ff, weeks = 1:16){
  nflSched <- getNFLSchedule()
  ffOff <- getStrengthOffense(ff)
  ffDef <- getStrengthDefense(ff)
  for(wk in weeks){#wk=1
    wkL <- paste0("Wk",formatC(wk, width=2, flag="0"))
    pCol <- paste0("Pts",wkL)
    ff[,pCol] <- 0
    ffNoBye <- which(ff$bye != wk)
    ff[ffNoBye,pCol] <- sapply(1:length(ffNoBye),function(x){#x=1
      fTeam <- ff[x,'team']
      fPos <- ff[x,'pos']
      isDef <- grepl("DST",fPos)
      oTeam <- nflSched[nflSched$TEAM==fTeam,wkL]
      fHome <- grepl("\\@",oTeam)
      oTeam <- gsub("\\@","",oTeam)
      fOff <- ffOff[ffOff$team == fTeam,'strength']; fDef <- ffDef[ffDef$team == fTeam,'strength']
      oOff <- ffOff[ffOff$team == oTeam,'strength']; oDef <- ffDef[ffDef$team == oTeam,'strength']
      fWeight <- ifelse(isDef,mean(c(fDef,oDef,1/fDef,1/oOff)),mean(c(fOff,oOff,1/fOff,1/oDef)))
      fWeight * ff[x, "points"]/17
    }) 
  }
  return(ff)
}

getFFAnalytics_Projections_CSV <- function(fffile, data_result, pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                                       season = 2020, week = 0, avgType = "robust"){
  if(file.exists(fffile)){
    if(file.info(fffile)$mtime >= Sys.Date()){
      ff_projections <- read.csv(fffile,stringsAsFactors = F)
      return(ff_projections)
    }
  }
  ff_projections <- getFFAnalytics_Projections(data_result,pos = pos, season = season, week = week, avgType = avgType)
  write.csv(ff_projections,fffile,row.names = FALSE)
  return(ff_projections)
}

add_adpaav_override <- function(projection_table, sources = c("RTS", "CBS", "ESPN", "Yahoo", "NFL", "FFC"), type="ADP"){
  sources <- match.arg(sources, several.ok = TRUE)
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")
  if (week != 0) {
    warning("ADP data is not available for weekly data", 
            call. = FALSE)
    return(projection_table)
  }
  adp_tbl <- get_adp(sources, type = type)
  varname <- paste0(tolower(type),"_diff")
  projection_table <- left_join(projection_table, adp_tbl, 
                                by = "id") %>% mutate(!!varname := rank - Avg)
  colnames(projection_table)[colnames(projection_table) == "Avg"] <- tolower(type)
  srcCols <- colnames(projection_table) %in% tolower(sources)
  colnames(projection_table)[srcCols] <- sapply(colnames(projection_table)[srcCols],function(x) paste0(tolower(type),".",x)) 
  projection_table %>% `attr<-`(which = "season", season) %>% 
    `attr<-`(which = "week", week) %>% `attr<-`(which = "lg_type", lg_type)
}

add_ecr_override <- function (projection_table) 
{
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week") 
  ecr_pos <- lg_type %>% imap(~scrape_ecr(rank_period = ifelse(week == 0, "draft", "week"), 
                                          position = ifelse(week == 0, "Overall", .y), rank_type = .x)) %>% 
    map(select, id, pos_ecr = avg, sd_ecr = std_dev) %>% 
    bind_rows() %>% distinct(id,.keep_all = TRUE)
  projection_table <- left_join(projection_table, ecr_pos, 
                                by = "id")
  if (week == 0) {
    lg_ov <- ifelse(any(lg_type == "PPR"), "PPR", ifelse(any(lg_type == 
                                                               "Half"), "Half", "Std"))
    ecr_overall <- scrape_ecr(rank_period = "draft", rank_type = lg_ov) %>% 
      select(id, ecr = avg)
    projection_table <- left_join(projection_table, ecr_overall, 
                                  by = "id")
  }
  projection_table %>% `attr<-`(which = "season", season) %>% 
    `attr<-`(which = "week", week) %>% `attr<-`(which = "lg_type", 
                                                lg_type)
}
add_player_info_override <- function (projection_table) 
{
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")
  select(player_table, id, first_name, last_name, team, position, 
         age, exp) %>% inner_join(projection_table, by = c("id" = "id", "position" = "pos")) %>% 
    `attr<-`(which = "season", season) %>% `attr<-`(which = "week", 
                                                    week) %>% `attr<-`(which = "lg_type", lg_type)
}

add_player_bye <- function(projection_table){
  lg_type <- attr(projection_table, "lg_type")
  season <- attr(projection_table, "season")
  week <- attr(projection_table, "week")
  nflSched <- getNFLSchedule()
  colnames(nflSched) <- tolower(colnames(nflSched))
  nflSched$bye <- sapply(1:nrow(nflSched),function(x){#x=1
    byeWk <- colnames(nflSched)[nflSched[x,] == "BYE"]
    bye <- as.integer(gsub("wk","",byeWk))
  })
  select(nflSched, team, bye) %>% inner_join(projection_table, by = "team") %>% 
    `attr<-`(which = "season", season) %>% `attr<-`(which = "week", 
                                                    week) %>% `attr<-`(which = "lg_type", lg_type)
}

getFFAnalytics_SrcWeights <- function(){
  # ffWeights <- c(CBS = 0.344, Yahoo = 0.400, ESPN = 0.329, NFL = 0.329, FFToday = 0.379,
  #                NumberFire = 0.322, FantasyPros = 0.000, FantasySharks = 0.327, FantasyFootballNerd = 0.000,
  #                Walterfootball = 0.281, RTSports = 0.330, FantasyData = 0.428, FleaFlicker = 0.428)
  #NumberFire throws error on projections..
  ffWeights <- c(FantasySharks = 0.327, FantasyData = 0.428, RTSports = 0.330, CBS = 0.344, FFToday = 0.379, Yahoo = 0.400, ESPN = 0.329, NFL = 0.329, 
                 FantasyPros = 0.220, FantasyFootballNerd = 0.220, Walterfootball = 0.281, FleaFlicker = 0.428)
  return(ffWeights)
}

getFFAnalytics_SrcWeightsWeekly <- function(){
  # ffWeights <- c(CBS = 0.344, Yahoo = 0.400, ESPN = 0.329, NFL = 0.329, FFToday = 0.379,
  #                NumberFire = 0.322, FantasyPros = 0.000, FantasySharks = 0.327, FantasyFootballNerd = 0.000,
  #                Walterfootball = 0.281, RTSports = 0.330, FantasyData = 0.428, FleaFlicker = 0.428)
  #NumberFire throws error on projections..
  ffWeights <- c(FantasySharks = 1, CBS = 0, FantasyFootballNerd = 0)
  return(ffWeights)
}

getFFAnalytics_VorBaseline <- function(){
  ffVOR <- c(QB = 13, RB = 35, WR = 36, TE = 13, K = 8, DST = 3, DL = 0, LB = 0, DB = 0)
  return(ffVOR)
}

getFFAnalytics_TierThreshold <- function(){
  ffTiers <- c(QB = 1, RB = 1, WR = 1, TE = 1, K = 1, DST = 0.1, DL = 1, DB = 1, LB = 1)
  return(ffTiers)
}

getFFAnalytics_ScoringSettings <- function(){
  ffScoreSettings <- list(
    pass = list(
      pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
      pass_int = -3, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
      pass_400_yds = 0
    ),
    rush = list(
      all_pos = TRUE,
      rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
      rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
    rec = list(
      all_pos = TRUE,
      rec = 0, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
      rec_150_yds = 0, rec_200_yds = 0
    ),
    misc = list(
      all_pos = TRUE,
      fumbles_lost = -3, fumbles_total = 0,
      sacks = 0, two_pts = 2
    ),
    kick = list(
      xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
      fg_50 = 5.0,  fg_miss = 0.0
    ),
    ret = list(
      all_pos = TRUE,
      return_tds = 6, return_yds = 0
    ),
    idp = list(
      all_pos = TRUE,
      idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
      idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
    ),
    dst = list(
      dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
      dst_blk = 1.5, dst_ret_yds = 0, dst_pts_allowed = 0
    ),
    pts_bracket = list(
      list(threshold = 0, points = 10),
      list(threshold = 6, points = 7),
      list(threshold = 20, points = 4),
      list(threshold = 34, points = 0),
      list(threshold = 99, points = -4)
    )
  )
  return(ffScoreSettings)
}

getTeamRankingDataDownload <- function(download_location,fangraphFile,dataURL,dataFile){
  
  driver <- rsDriver(browser=c("chrome"), chromever="83.0.4103.39", port = 4444L)
  remote_driver <- driver[["client"]] #remote_driver$open()
  
  dataURL <- 'https://www.teamrankings.com/nfl/stat/opponent-rushing-yards-per-game'
  rushYrdsTable <- getTeamRankingDataTable(remote_driver,dataURL)
  
  dataURL <- 'https://www.teamrankings.com/nfl/stat/opponent-passing-yards-per-game'
  passYrdsTable <- getTeamRankingDataTable(remote_driver,dataURL)
  
  remote_driver$close()
  driver$server$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
}

getTeamRankingDataTable <- function(remote_driver, dataURL){
  
  remote_driver$navigate(dataURL)
  
  Sys.sleep(2)
  exportData_Link <- remote_driver$findElement(using = 'id', value = 'DataTables_Table_0')
  webElem <- exportData_Link$getElementAttribute("outerHTML")[[1]]
  table <- readHTMLTable(webElem, header = TRUE, as.data.frame = TRUE)[[1]]
  
  return(table)
}

getStrengthDefense <- function(ff){
  ffDef <- ff[ff$pos == "DST",] %>% select(team,pId,points)
  ffDefMean <- mean(ffDef$points)
  ffDef <- ffDef %>% mutate(strength = points/ffDefMean) 
  return(ffDef)
}

getStrengthOffense <- function(ff){
  ffOff <- ff[ff$pos != "DST",] %>% select(team,pos,pId,points)
  ffQB <- ffOff %>% filter(pos == "QB") %>% group_by(team) %>% top_n(n=1,wt=points) %>% select(team,pId,points) %>% rename_at(vars(c("pId","points")),funs(paste0("QB",c("","Pts"))))
  ffRB <- ffOff %>% filter(pos == "RB") %>% group_by(team) %>% top_n(n=1,wt=points) %>% select(team,pId,points) %>% rename_at(vars(c("pId","points")),funs(paste0("RB",c("","Pts"))))
  ffWR <- ffOff %>% filter(grepl('TE|WR',pos)) %>% group_by(team) %>% top_n(n=1,wt=points) %>% select(team,pId,points) %>% rename_at(vars(c("pId","points")),funs(paste0("WR",c("","Pts"))))
  
  ffOffTotal <- ffQB %>% inner_join(ffRB, by='team') %>% inner_join(ffWR, by='team')
  ffOffTotal <- ffOffTotal %>% mutate(points=sum(QBPts,RBPts,WRPts)) %>% arrange(desc(points)) %>% as.data.frame()
  ffOffMean <- mean(ffOffTotal$points)
  ffOff <- ffOffTotal %>% mutate(strength = points/ffOffMean)
  return(ffOff)
}

updateMissingIds <- function(ff, allPlayers){
  playersMissing <- ff[!(ff$pId %in% allPlayers$pId),]
  if(nrow(playersMissing)>0){
    ffPlayerIds <- playersMissing$pId
    allPlayerIds <- unlist(sapply(1:nrow(playersMissing),function(x){#x=1
      fPlayer <- playersMissing[x,]
      teamPlayers <- allPlayers %>% filter(team == fPlayer$team)
      strFirstNames <- teamPlayers %>% select(first_name) %>% unlist() %>% tm::removePunctuation() %>% tolower()
      strLastNames <- teamPlayers %>% select(last_name) %>% unlist() %>% tm::removePunctuation() %>% tolower()
      fPlayerFirst <- fPlayer$first_name %>% tm::removePunctuation() %>% tolower()
      fPlayerLast <- fPlayer$last_name %>% tm::removePunctuation() %>% tolower()
      resFirst <- stringdist::stringdistmatrix(strFirstNames,fPlayerFirst,method = 'lcs')[,1]
      resLast <- stringdist::stringdistmatrix(strLastNames,fPlayerLast,method = 'lcs')[,1]
      resName <- resFirst + resLast
      aPlayer <- teamPlayers[which(resName == resName %>% min()),'pId']
      aPlayer <- head(aPlayer[!is.na(aPlayer)],1)
      if(length(aPlayer)==0) aPlayer <- NA
      aPlayer
    }))
    names(allPlayerIds) <- ffPlayerIds
    for(aX in 1:length(allPlayerIds)){
      ff[ff$pId == names(allPlayerIds)[aX],'pId'] <- allPlayerIds[aX]
    }
    print(allPlayerIds)
  }
  return(ff)
}
