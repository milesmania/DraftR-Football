
## Refresh Draft Charts ####
refreshCharts <- function(values,output, input, session, ... ){
  #update draft results
  withProgress(message = 'Refreshing Draft Data', value = 0, {
    incProgress(0.1,paste('Refreshing Draft from Sleeper ...'))
    draftResults <- draftPopulateResults(dfDraft,draftResults)
    values$dResult <- draftResults 
    picksToUpdate <- updateDraftFromSleeper(draftId,draftResults,allPlayers)
    
    setProgress(0.2,paste('Populating Draft from Sleeper ...'))
    dfDraft <- values$data
    dfDraft <- draftPopulate(picksToUpdate,dfDraft)
    values$data <- dfDraft
    draftResults <- draftPopulateResults(dfDraft,draftResults)
    
    #Get playerTaken and players available
    setProgress(0.3,paste('Getting Players Taken and Available ...'))
    playersTaken <- as.character(unlist(as.list(dfDraft)))
    playersTaken <- playersTaken[playersTaken != ""]
    if(length(playersTaken) != values$pTakenCount){
      StartPickTime <- Sys.time()# + 4*60
      values$StartPickTime <- StartPickTime
      playersTakenCount <- length(playersTaken)
      values$pTakenCount <- playersTakenCount
    }
    playersAvail <- as.character(unique(ff$pId))
    playersAvail <- playersAvail[!(playersAvail %in% playersTaken)]
    values$pAvail <- playersAvail
    values$pTaken <- playersTaken
    
    dfAvail <- ff
    dfAvail <- dfAvail[!(dfAvail$pId %in% playersTaken),]
    values$dataAvail <- dfAvail
    
    ffDataAvailable <- ffDataAvail(values$dataAvail,values$dForecast)
    output$dataAvail = DT::renderDataTable({
      ffDataAvailable
    })
    
    setProgress(0.4,paste('Forecasting Draft ...'))
    values$dResult <- draftResults 
    draftForecast <- forecastDraft(draftResults,ff)
    values$dForecast <- draftForecast
    
    setProgress(0.6,paste('Updating Available Charts ...'))
    ## Available Chart graphing ####
    output$rbChart <- renderPlot({
      roundupGraph(ff,pos="RB",nPlayers=100,playersTaken = playersTaken, xVal = values$availChartX, yVal = values$availChartY, showTaken=input$chartShowTaken)
    },height = 1000)
    output$wrChart <- renderPlot({
      roundupGraph(ff,pos="WR",nPlayers=100,playersTaken = playersTaken, xVal = values$availChartX, yVal = values$availChartY, showTaken=input$chartShowTaken)
    },height = 1000)
    output$qbChart <- renderPlot({
      roundupGraph(ff,pos="QB",nPlayers=30,playersTaken = playersTaken, xVal = values$availChartX, yVal = values$availChartY, showTaken=input$chartShowTaken)
    },height = 300)
    output$teChart <- renderPlot({
      roundupGraph(ff,pos="TE",nPlayers=30,playersTaken = playersTaken, xVal = values$availChartX, yVal = values$availChartY, showTaken=input$chartShowTaken)
    },height = 300)
    output$dstChart <- renderPlot({
      roundupGraph(ff,pos="DST",nPlayers=30,playersTaken = playersTaken, xVal = values$availChartX, yVal = values$availChartY, showTaken=input$chartShowTaken)
    },height = 300)
    output$kChart <- renderPlot({
      roundupGraph(ff,pos="K",nPlayers=30,playersTaken = playersTaken, xVal = values$availChartX, yVal = values$availChartY, showTaken=input$chartShowTaken)
    },height = 300)
    
    
    ## DataAvail Next Avail Forecast ########################################### 
    incProgress(0.1,paste('Projecting League Players Available ...'))
    pAvailable <- values$dataAvail
    ## Forecast RB's
      pToForecast <- 1:input$nextAvailRB
      rbP <- pAvailable %>% filter(pos == "RB")
      rbAvail <- nextAvailProgress(pToForecast,rbP,values$dResult,sTeam=MyTeam, ...)
      output$dataNextAvailRB = renderText(leagueProjection_Kable(rbAvail))
    ## Forecast QB's
      pToForecast <- 1:input$nextAvailQB
      qbP <- pAvailable %>% filter(pos == "QB")
      qbAvail <- nextAvailProgress(pToForecast,qbP,values$dResult,sTeam=MyTeam, ...)
      output$dataNextAvailQB = renderText(leagueProjection_Kable(qbAvail))
    ## Forecast WR's
      pToForecast <- 1:input$nextAvailWR
      wrP <- pAvailable %>% filter(pos == "WR")
      wrAvail <- nextAvailProgress(pToForecast,wrP,values$dResult,sTeam=MyTeam, ...)
      output$dataNextAvailWR = renderText(leagueProjection_Kable(wrAvail))
    ## Forecast TE's
      pToForecast <- 1:input$nextAvailTE
      teP <- pAvailable %>% filter(pos == "TE")
      teAvail <- nextAvailProgress(pToForecast,teP,values$dResult,sTeam=MyTeam, ...)
      output$dataNextAvailTE = renderText(leagueProjection_Kable(teAvail))
    ## Forecast DST's
      pToForecast <- 1:input$nextAvailDST
      dstP <- pAvailable %>% filter(pos == "DST")
      dstAvail <- nextAvailProgress(pToForecast,dstP,values$dResult,sTeam=MyTeam, ...)
      output$dataNextAvailDST = renderText(leagueProjection_Kable(dstAvail))
    ## Forecast K's
      pToForecast <- 1:input$nextAvailK
      kP <- pAvailable %>% filter(pos == "K")
      kAvail <- nextAvailProgress(pToForecast,kP,values$dResult,sTeam=MyTeam, ...)
      output$dataNextAvailK = renderText(leagueProjection_Kable(kAvail))
    
    save(dfDraft,teams,playersAvail,dfAvail,rosters,availPlayers,playersTaken,draftResults,draftForecast,seasonProjection,playersTakenCount,StartPickTime, file = draftFile)
    
    #EndProgress Messages
  })
  return(values)
}
    ## DataAvail ########################################### 
refreshDataAvail <- function(values,output, input, session, ... ){
  withProgress(message = 'Refreshing Data Available', value = 0, {
    incProgress(0.1,paste('Rendering Data Available Tables ...'))
    output$dataAvailALL = DT::renderDataTable({
      datatable(values$dataAvail, options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 25)) %>%
        formatStyle("pos",target = 'row',
                    backgroundColor = styleEqual(levels=c("QB","RB","WR","TE","DST","K"),
                                                 values=c("pink","lightgreen","lightblue","orange","violet","lightgrey"))) %>%
        formatRound(columns = colnames(values$dataAvail)[8:35])
    })
    
    #EndProgress Messages
  })
  return(values)
}
## DataAvail Next Avail Forecast ########################################### 
refreshDataNextAvailForecast <- function(values,output, input, session, ... ){
  withProgress(message = 'Refreshing Data Available', value = 0, {
    incProgress(0.1,paste('Projecting League Players Available ...'))
    pAvailable <- values$dataAvail
    ## Forecast RB's
    pToForecast <- 1:input$nextAvailRB
    rbP <- pAvailable %>% filter(pos == "RB")
    rbAvail <- nextAvailProgress(pToForecast,rbP,values$dResult,sTeam=MyTeam, ...)
    output$dataNextAvailRB = renderText(leagueProjection_Kable(rbAvail))
    ## Forecast QB's
    pToForecast <- 1:input$nextAvailQB
    qbP <- pAvailable %>% filter(pos == "QB")
    qbAvail <- nextAvailProgress(pToForecast,qbP,values$dResult,sTeam=MyTeam, ...)
    output$dataNextAvailQB = renderText(leagueProjection_Kable(qbAvail))
    ## Forecast WR's
    pToForecast <- 1:input$nextAvailWR
    wrP <- pAvailable %>% filter(pos == "WR")
    wrAvail <- nextAvailProgress(pToForecast,wrP,values$dResult,sTeam=MyTeam, ...)
    output$dataNextAvailWR = renderText(leagueProjection_Kable(wrAvail))
    ## Forecast TE's
    pToForecast <- 1:input$nextAvailTE
    teP <- pAvailable %>% filter(pos == "TE")
    teAvail <- nextAvailProgress(pToForecast,teP,values$dResult,sTeam=MyTeam, ...)
    output$dataNextAvailTE = renderText(leagueProjection_Kable(teAvail))
    ## Forecast DST's
    pToForecast <- 1:input$nextAvailDST
    dstP <- pAvailable %>% filter(pos == "DST")
    dstAvail <- nextAvailProgress(pToForecast,dstP,values$dResult,sTeam=MyTeam, ...)
    output$dataNextAvailDST = renderText(leagueProjection_Kable(dstAvail))
    ## Forecast K's
    pToForecast <- 1:input$nextAvailK
    kP <- pAvailable %>% filter(pos == "K")
    kAvail <- nextAvailProgress(pToForecast,kP,values$dResult,sTeam=MyTeam, ...)
    output$dataNextAvailK = renderText(leagueProjection_Kable(kAvail))
    #EndProgress Messages
  })
  return(values)
}
## DataAvail League Projection  ###########################################
refreshDataLeagueProjection <- function(values,output, input, session, ... ){
  withProgress(message = 'Refreshing Data Available', value = 0, {
    incProgress(0.3,paste('Projecting League Players Available ...'))
    dfAvail <- values$dataAvail
    dPrjAvail <- leagueProjectionplayersAvail(dfAvail)
    output$dataAvailPrjWk = renderText(leagueProjectionplayersAvailKable(dPrjAvail))
    #EndProgress Messages
  })
  return(values)
}
    ## Update Roster ####
refreshRosters <- function(values,output, input, session, ... ){
  withProgress(message = 'Refreshing Draft Rosters', value = 0, {
    incProgress(0.1,paste('Updating Rosters ...'))
    draftForecast <- values$dForecast
    draftResults <- values$dResult
    rosters <- setRoster(draftForecast,showForecast=input$chartShowForecastedRoster)
    values$rosterData <- rosters
    
    output$rosterData = renderTable({rosters}, rownames = TRUE, striped = TRUE, width = "100%")
    
    output$draftTotalChart = renderPlot({draftChart(draftForecast)},height = 500)
    
    ## update next pick timer ####
    nextPick <- head(draftResults[draftResults$Pick=="",],1)
    dTxt <- ""
    if(nrow(nextPick)==0) dTxt <- "DRAFT IS OVER!!!!" else{
      values$nextPick <- nextPick
      dTxt <- paste("<h4>Round",nextPick$Round," Pick",rownames(nextPick),"On the Clock:",nextPick$Team,"</h4>")
      
      picksAway <- head(subset(draftResults,Team==MyTeam & Pick=="","Overall"),1) - nextPick$Overall + 1
      
      if(nextPick$Team!=MyTeam){
        values$picksAway <- picksAway
        dTxt <- paste0(dTxt," <i>Picks until ",MyTeam,": ",values$picksAway,"</i>")
      }
    }
    
    output$nextPick <- renderUI({
      HTML(dTxt)
    })
    
    output$pickTimeElapsed <- renderText({
      invalidateLater(1000, session)
      paste("Time Since Last Pick:", 
            round(difftime(Sys.time(), StartPickTime, units='secs')), 'seconds')
    })
    #EndProgress Messages
  })
  return(values)
}
    
    ## DataForcastALL ########################################### 
refreshDataForecast <- function(values,output, input, session, ... ){
      withProgress(message = 'Refreshing Data Available', value = 0, {
        incProgress(0.1,paste('Rendering Data Forecast Tables ...'))
    output$dataForcastALL = DT::renderDataTable({
      draftForecast <- values$dForecast
      dFall <- draftForecast #values$dForecast
      dFall <- dFall[dFall$Selected == "forecast",c("Overall","Round","Team","Pick","pos","ForcastComment","points","lower","upper","vor")]
      datatable(dFall, options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 25)) %>%
        formatStyle("pos",target = 'row',
                    backgroundColor = styleEqual(levels=c("QB","RB","WR","TE","DST","K"),
                                                 values=c("pink","lightgreen","lightblue","orange","violet","lightgrey"))) %>%
        formatRound(columns = colnames(dFall)[7:ncol(dFall)])
    })
    
    #EndProgress Messages
  })
  return(values)
}

## Refresh Projections ####
refreshProjections <- function(values,output, input, session, ...){
  lPrj <- leagueProjection(ff,values$dForecast,starterPositions)
  values$lPrj <- lPrj
  
  output$leagueProjChart = renderPlotly({leagueProjection_Plot(lPrj)})
  
  output$leagueProjTable = renderText(leagueProjection_Kable(leagueProjection_Table(lPrj)))
  
  output$leagueProjRank = renderText(leagueProjection_RankKable(leagueProjection_Rank(leagueProjection_Table(lPrj))))
  
  output$leagueProjWeek = renderText(leagueProjection_WeekKable(leagueProjection_Week(lPrj,input$leaguePrjWk)))
  return(values)
}

## Hide/Show Tabs ####
tabShowHide <- function(tabId, hideIt = TRUE){
  if(hideIt){
    hideTab(inputId = tabId, target = "Draft")
    hideTab(inputId = tabId, target = "Roster")
    hideTab(inputId = tabId, target = "Roster")
    hideTab(inputId = tabId, target = "Projections")
    hideTab(inputId = tabId, target = "Available Charts")
    hideTab(inputId = tabId, target = "Tables")
  }else{
    showTab(inputId = tabId, target = "Draft")
    showTab(inputId = tabId, target = "Roster")
    showTab(inputId = tabId, target = "Roster")
    showTab(inputId = tabId, target = "Projections")
    showTab(inputId = tabId, target = "Available Charts")
    showTab(inputId = tabId, target = "Tables")
  }
}