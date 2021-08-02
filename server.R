
shinyServer(function(input, output, session) {
  
  ## Reactives ###########################################
  
  ffValues = reactiveValues(data = dfDraft, 
                          pAvail = playersAvail,
                          pTaken = playersTaken,
                          pTakenCount = playersTakenCount,
                          dataAvail = ff,
                          rosterData = rosters,
                          dResult = draftResults,
                          dForecast = draftForecast,
                          lPrj = seasonProjection,
                          availChartX = availChartX,
                          availChartY = availChartY,
                          StartPickTime = StartPickTime,
                          nextPick = head(draftResults[draftResults$Pick=="",],1),
                          picksAway = 0
                          )

  observe({
    #set up autocomplete on drafting table
    # req(input$data)
    # dfDraft <- hot_to_r(input$data)
    # ffValues$data <- dfDraft
    
    #Get playerTaken and players available
    playersTaken <- as.character(unlist(as.list(dfDraft)))
    playersTaken <- playersTaken[playersTaken != ""]
    if(length(playersTaken) != ffValues$pTakenCount){
      ffValues$StartPickTime <- Sys.time()# + 4*60
      playersTakenCount <- length(playersTaken)
      ffValues$pTakenCount <- playersTakenCount
    }
    playersAvail <- as.character(unique(ff$pId))
    playersAvail <- playersAvail[!(playersAvail %in% playersTaken)]
    ffValues$pAvail <- playersAvail
    ffValues$pTaken <- playersTaken
    
    dfAvail <- ff
    ffValues$dataAvail = dfAvail[!(dfAvail$pId %in% playersTaken),]
    
    #update draft results
    draftResults <- draftPopulateResults(ffValues$data,draftResults)
    
    ffValues$dResult <- draftResults 
    ffValues$dForecast <- forecastDraft(draftResults,ff)
    draftForecast <- ffValues$dForecast
    seasonProjection <- ffValues$lPrj
    
    rosters <- setRoster(draftForecast,showForecast=input$chartShowForecastedRoster)
    ffValues$rosterData <- rosters
    ffValues$availChartY <- input$chartY
    ffValues$availChartX <- input$chartX
    
    if(draftId == "" | leagueId == ""){
      tabShowHide(tabId="tabs",hideIt = TRUE)
    }
    
    output$draftForecasted = renderText(
      draftTablePopulate(dfDraft, draftForecast)
    )
  })
  
  ## Refresh draft from sleeper ####
  observeEvent(input$RefreshDraft,{
    withProgress(message = 'Refreshing Draft', value = 0, {
      incProgress(0.1,paste('Refreshing Charts ...'))
      ffValues <- refreshCharts(ffValues,output,input,session,dfDraft,draftResults,draftId,ff,draftFile,allPlayers)
      incProgress(0.2,paste('Refreshing Data ...'))
      ffValues <- refreshDataAvail(ffValues,output,input,session,dfDraft,draftResults,draftId,ff,draftFile)
      incProgress(0.4,paste('Refreshing Rosters ...'))
      ffValues <- refreshRosters(ffValues,output,input,session,dfDraft,draftResults,draftId,ff,draftFile)
      incProgress(0.6,paste('Refreshing Forecast ...'))
      ffValues <- refreshDataForecast(ffValues,output,input,session,dfDraft,draftResults,draftId,ff,draftFile)
    })
  })
  
  ##update Projections ####
  observeEvent(input$RefreshLeaguePrj,{
    withProgress(message = 'Refreshing Projections ', value = 0, {
      incProgress(0.1,paste('Refreshing League Projections ...'))
      ffValues <- refreshProjections(ffValues,output,input,session,ff)
      incProgress(0.3,paste('Refreshing League Projection ...'))
      ffValues <- refreshDataLeagueProjection(ffValues,output,input,session,allPlayers)
    })
  })
  
  ##update Next Available Forecast ####
  observeEvent(input$RefreshNextAvail,{
    withProgress(message = 'Refreshing Projections ', value = 0, {
      incProgress(0.3,paste('Running Next Available Forecast ...'))
      ffValues <- refreshDataNextAvailForecast(ffValues,output,input,session,starterPositions,MyTeam)
    })
  })
  
  ## Save Settings Button ####
  observeEvent(input$saveSettings,{
    withProgress(message = 'Saving Settings', value = 0, {
      incProgress(0.5,paste('Saving Config Settings ...'))
      draftId <- input$draftId
      leagueId <- input$leagueId
      MyTeam <- input$myTeam
      
      if(draftId != "" & leagueId != ""){
        teams <- getUsersFromSleeper(leagueId,draftId)
        if(MyTeam=="") MyTeam <- teams[1]
        names(teams) <- teams
        updateSelectizeInput(session,"myTeam",choices = teams,selected = MyTeam)
        tabShowHide(tabId="tabs",hideIt = FALSE)
      }
      config <- c('draftId'=draftId,'leagueId'=leagueId,'MyTeam'=MyTeam)
      configTxt <- paste(sapply(1:length(config),function(x){paste(names(config[x]),config[x], sep = "=")}),
                         collapse = "\n")
      write(configTxt,file = "Assets/config.txt")
      Sys.sleep(1)
      
      session$reload()
      return()
    })
  })
  
  ## Output updates ########################################### 
  # output$draftData = DT::renderDataTable({
  #   datatable(ffValues$dForecast, options = list(lengthMenu = c(100, 50, 25, 10),
  #                                            columnDefs = list(list(visible = FALSE, targets = 5:ncol(ffValues$dForecast))), 
  #                                            pageLength = 50)) %>%
  #     formatStyle(valueColumns="Selected",target = 'cell',columns = "Pick",color = styleEqual(levels="forecast",values="lightblue"))
  # })
  
})
