
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
                          #lPrj = seasonProjection,
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
    #seasonProjection <- values$lPrj
    
    rosters <- setRoster(draftForecast,showForecast=input$chartShowForecastedRoster)
    values$rosterData <- rosters
    values$availChartY <- input$chartY
    values$availChartX <- input$chartX
  })
  
  ## Refresh draft from sleeper ####
  observeEvent(input$RefreshDraft,{
    #update draft results
    draftResults <- draftPopulateResults(dfDraft,draftResults)
    values$dResult <- draftResults 
    picksToUpdate <- updateDraftFromSleeper(draftId,draftResults)
    dfDraft <- values$data
    dfDraft <- draftPopulate(picksToUpdate,dfDraft)
    values$data <- dfDraft
    draftResults <- draftPopulateResults(dfDraft,draftResults)
    
    #Get playerTaken and players available
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
    
    values$dResult <- draftResults 
    draftForecast <- forecastDraft(draftResults,ff)
    values$dForecast <- draftForecast
    
    output$draftForecasted = renderText(
      draftTablePopulate(dfDraft, draftForecast)
    )
    
    
    ## DataAvail ########################################### 
    output$dataAvail = DT::renderDataTable({
      dtF <- datatable(dfAvail[,c('name','pos','team','age')], 
                       options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 25)) %>%
        formatStyle("pos",target = 'row',
                    backgroundColor = styleEqual(levels=c("QB","RB","WR","TE","DST","K"),
                                                 values=c("pink","lightgreen","lightblue","orange","violet","lightgrey")))
      
      if("Team" %in% names(draftForecast)){
        myPlayers <- subset(draftForecast,Team==MyTeam & Selected=="forecast","name",drop=T)
        dtF <- dtF %>% 
          formatStyle(columns = "name", border = styleEqual(levels=myPlayers, values=rep('3px dashed red',length(myPlayers))))
      }
      dtF
    })
    
    output$dataAvailALL = DT::renderDataTable({
      datatable(dfAvail, options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 25)) %>%
        formatStyle("pos",target = 'row',
                    backgroundColor = styleEqual(levels=c("QB","RB","WR","TE","DST","K"),
                                                 values=c("pink","lightgreen","lightblue","orange","violet","lightgrey"))) %>%
        formatRound(columns = colnames(values$dataAvail)[8:35])
    })
    
    output$dataAvailPrjWk = renderText(leagueProjectionplayersAvailKable(
      leagueProjectionplayersAvail(dfAvail,allPlayers)))
    
    ## Update Roster ####
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
    
    ## DataForcastALL ########################################### 
    output$dataForcastALL = DT::renderDataTable({
      dFall <- draftForecast #values$dForecast
      dFall <- dFall[dFall$Selected == "forecast",c("Overall","Round","Team","Pick","pos","ForcastComment","points","lower","upper","vor")]
      datatable(dFall, options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 25)) %>%
        formatStyle("pos",target = 'row',
                    backgroundColor = styleEqual(levels=c("QB","RB","WR","TE","DST","K"),
                                                 values=c("pink","lightgreen","lightblue","orange","violet","lightgrey"))) %>%
        formatRound(columns = colnames(dFall)[7:ncol(dFall)])
    })
    
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
    
    save(dfDraft,teams,playersAvail,rosters,availPlayers,playersTaken,draftResults,draftForecast,playersTakenCount,StartPickTime, file = draftFile)
    
  })
  
  
  ##update Projections ####
  # observeEvent(input$RefreshLeaguePrj,{
  #   lPrj <- leagueProjection(allPlayers,values$dForecast,starterPositions)
  #   values$lPrj <- lPrj
  #   
  #   output$leagueProjChart = renderPlotly({leagueProjection_Plot(lPrj)})
  #   
  #   output$leagueProjTable = renderText(leagueProjection_Kable(leagueProjection_Table(lPrj)))
  #   
  #   output$leagueProjRank = renderText(leagueProjection_RankKable(leagueProjection_Rank(leagueProjection_Table(lPrj))))
  #   
  #   output$leagueProjWeek = renderText(leagueProjection_WeekKable(leagueProjection_Week(lPrj,input$leaguePrjWk)))
  #   
  # })
  
  ## Output updates ########################################### 
  # output$draftData = DT::renderDataTable({
  #   datatable(values$dForecast, options = list(lengthMenu = c(100, 50, 25, 10),
  #                                            columnDefs = list(list(visible = FALSE, targets = 5:ncol(values$dForecast))), 
  #                                            pageLength = 50)) %>%
  #     formatStyle(valueColumns="Selected",target = 'cell',columns = "Pick",color = styleEqual(levels="forecast",values="lightblue"))
  # })
  
  
  
  
})
