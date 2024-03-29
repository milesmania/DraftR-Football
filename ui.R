library(rhandsontable)
library(plotly)

shinyUI(
  navbarPage("Dynasty Maker!!  (BETA)", id = "tabs",
             tabPanel("Draft",
                      fluidRow(
                        column(2,actionButton("RefreshDraft", "Refresh From Sleeper")),
                        column(6,htmlOutput("nextPick")),
                        column(4,p(textOutput("pickTimeElapsed"))),
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          DT::dataTableOutput("dataAvail"),
                          width = 3),
                        mainPanel(htmlOutput("draftForecasted"),
                                  br(),hr(),br(),
                                  fluidRow(column(4,h3("Projected Weekly Scores")),
                                           column(2,actionButton("RefreshNextAvail", "Project Picks"))
                                           ),
                                  fluidRow(column(2,sliderInput("nextAvailQB","Forecast QB",min=1,max=10,value=5)),
                                           column(2,sliderInput("nextAvailRB","Forecast RB",min=1,max=20,value=5)),
                                           column(2,sliderInput("nextAvailWR","Forecast WR",min=1,max=20,value=5)),
                                           column(2,sliderInput("nextAvailTE","Forecast TE",min=1,max=10,value=5)),
                                           column(2,sliderInput("nextAvailDST","Forecast DST",min=1,max=10,value=2)),
                                           column(2,sliderInput("nextAvailK","Forecast K",min=1,max=10,value=2))
                                  ),
                                  htmlOutput("dataNextAvailQB"),
                                  htmlOutput("dataNextAvailRB"),
                                  htmlOutput("dataNextAvailWR"),
                                  htmlOutput("dataNextAvailTE"),
                                  htmlOutput("dataNextAvailDST"),
                                  htmlOutput("dataNextAvailK"),
                                  htmlOutput("dataAvailPrjWk"))
                      )),
             tabPanel("Roster",
                      checkboxInput("chartShowForecastedRoster", "Show Forecasted Players", TRUE),
                      tableOutput("rosterData")),
             # tabPanel("Results",
             #          column(3,DT::dataTableOutput("draftData")),
             #          column(9,htmlOutput("draftForecasted")
             #                 )
             # ),
             tabPanel("Projections",
                      actionButton("RefreshLeaguePrj", "Refresh Projections"),
                      fluidRow(
                        column(4,plotOutput("draftTotalChart")
                        ),
                        column(8,h3("Projected Weekly Scores"),br(),
                               htmlOutput("leagueProjTable")
                        )
                      ),hr(),
                      fluidRow(
                        column(4,plotlyOutput("leagueProjChart", height = "500px")
                        ),
                        column(8,h3("Projected Weekly Ranking"),br(),
                               htmlOutput("leagueProjRank")
                        )
                      ),hr(),
                      fluidRow(
                        h3("Projected Weekly"), 
                        sliderInput("leaguePrjWk","Select Week",min=1,max=17,value=1),br(),
                               htmlOutput("leagueProjWeek")
                      )
             ),
             tabPanel("Available Charts",
                      column(4,plotOutput("rbChart")),
                      column(4,plotOutput("wrChart")),
                      column(4,
                             plotOutput("qbChart"),plotOutput("teChart"),plotOutput("dstChart"),plotOutput("kChart"),
                             selectInput("chartY","Chart Y-Axis",c("Position Rank"="pos_rank","Overall Rank"="overallRank",
                                                                   "Position ECR"="pos_ecr","Overall ECR"="ecr","Average ADP" = "adp","Auction Value"="aav")),
                             selectInput("chartX","Chart X-Axis",c("Points"="points","VOR"="vor","Auction Value"="aav")),
                             checkboxInput("chartShowTaken", "Show Taken Players", TRUE)
                      )
             ),
             tabPanel("Tables",
                      h3("Forecasted Draft"),
                      DT::dataTableOutput("dataForcastALL"),
                      h3("Available Players"),
                      DT::dataTableOutput("dataAvailALL")),
             tabPanel("Settings",
                      h2("Draft Settings"),
                      textInput("leagueId","Sleeer League Id", value = leagueId),
                      textInput("draftId","Sleeper Draft Id", value = draftId),
                      conditionalPanel(condition = "input.myTeam != ''",
                                       selectizeInput("myTeam","Select My Team",choices=teams,selected=MyTeam)
                                       ),
                      actionButton("saveSettings","Save Settings")
             )
  )
)