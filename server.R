
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
  })
  
  ## Refresh draft from sleeper ####
  observeEvent(input$RefreshDraft,{
    values <- refreshCharts(values,output,input,session,dfDraft,draftResults,draftId,ff,draftFile)
  })
  
  ##update Projections ####
  observeEvent(input$RefreshLeaguePrj,{
    values <- refreshProjections(values,output,input,session,ff)
  })
  
  ## Output updates ########################################### 
  # output$draftData = DT::renderDataTable({
  #   datatable(values$dForecast, options = list(lengthMenu = c(100, 50, 25, 10),
  #                                            columnDefs = list(list(visible = FALSE, targets = 5:ncol(values$dForecast))), 
  #                                            pageLength = 50)) %>%
  #     formatStyle(valueColumns="Selected",target = 'cell',columns = "Pick",color = styleEqual(levels="forecast",values="lightblue"))
  # })
  
  
  
  
})
