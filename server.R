
shinyServer(function(input, output, session) {
  
  ## Reactives ###########################################
  
  values = reactiveValues(data = dfDraft, 
                          pAvail = playersAvail,
                          pTaken = playersTaken,
                          pTakenCount = playersTakenCount,
                          dataAvail = ff,
                          rosterData = rosters,
                          dResult = draftResults,
                          fResult = draftForecast,
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
    # values$data <- dfDraft
    
    #Get playerTaken and players available
    playersTaken <- as.character(unlist(as.list(dfDraft)))
    playersTaken <- playersTaken[playersTaken != ""]
    if(length(playersTaken) != values$pTakenCount){
      values$StartPickTime <- Sys.time()# + 4*60
      playersTakenCount <- length(playersTaken)
      values$pTakenCount <- playersTakenCount
    }
    playersAvail <- as.character(unique(ff$pId))
    playersAvail <- playersAvail[!(playersAvail %in% playersTaken)]
    values$pAvail <- playersAvail
    values$pTaken <- playersTaken
    
    dfAvail <- ff
    values$dataAvail = dfAvail[!(dfAvail$pId %in% playersTaken),]
    
    #update draft results
    draftResults <- draftPopulateResults(dfDraft,draftResults)
    
    values$dResult <- draftResults 
    values$dForecast <- forecastDraft(draftResults,ff)
    draftForecast <- values$dForecast
    seasonProjection <- values$lPrj
    
    rosters <- setRoster(draftForecast,showForecast=input$chartShowForecastedRoster)
    values$rosterData <- rosters
    values$availChartY <- input$chartY
    values$availChartX <- input$chartX
    
    if(draftId == "" | leagueId == ""){
      tabShowHide(tabId="tabs",hideIt = TRUE)
    }
  })
  
  ## Refresh draft from sleeper ####
  observeEvent(input$RefreshDraft,{
    values <- refreshCharts(values,output,input,session,dfDraft,draftResults,draftId,ff,draftFile)
  })
  
  ##update Projections ####
  observeEvent(input$RefreshLeaguePrj,{
    values <- refreshProjections(values,output,input,session,ff)
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
  #   datatable(values$dForecast, options = list(lengthMenu = c(100, 50, 25, 10),
  #                                            columnDefs = list(list(visible = FALSE, targets = 5:ncol(values$dForecast))), 
  #                                            pageLength = 50)) %>%
  #     formatStyle(valueColumns="Selected",target = 'cell',columns = "Pick",color = styleEqual(levels="forecast",values="lightblue"))
  # })
  
})
