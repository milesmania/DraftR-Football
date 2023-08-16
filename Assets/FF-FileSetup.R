download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
rawDataFile <- "Data/FFRawData.RData"
draftFile <- "Data/FFDraftData.RData"
fffile <- paste0("Data/ffa_customrankings",Year(Sys.Date()),"-0.csv")

player_table <- player_table %>% updateTeamNames()

rawData <- getFFAnalytics_RawData(ffDataFile=rawDataFile)
#rawDataWR <- rawData[["WR"]]
ffProjections <- getFFAnalytics_Projections_CSV(fffile,data_result=rawData)

config <- setConfigTxt()
if(is.null(config)){
  draftId <- leagueId <- MyTeam <- ""
  teams <- character()
} 
if(nchar(leagueId)>0){
  teams <- getUsersFromSleeper(leagueId,draftId)
}

rosterPositions <- c('QB-1','RB-1','RB-2','WR-1','WR-2','RB|WR-1','TE-1','DST-1','K-1','BE-1','BE-2','BE-3','BE-4','BE-5','BE-6','BE-7','BE-8')

starterPositions <- rosterPositions[!grepl("BE",rosterPositions)]
nRounds <- 14
availChartY <- "rank"
availChartX <- "points"
ff <- read.csv(fffile,stringsAsFactors = F)

#colnames(ff)[c(1,2,4)] <- c("id", "name","pos")
colnames(ff)[colnames(ff)=="position"] <- "pos"
#Set up ordered table
ffCols <- c("pos","name","team","age","bye","points","upper","lower","vor","vorHigh","vorLow","dropoff","risk","adp","sleeper")
ffColsNotIn <- ffCols[!(ffCols %in% colnames(ff))]
if(length(ffColsNotIn) > 0){
  print(paste('Removing ffCols:',paste(ffColsNotIn, collapse = ", ")))
  ffCols <- ffCols[!(ffCols %in% ffColsNotIn)]
} 
fft<-ff[,ffCols]
numCols <- c("points","upper","lower","vor","vorHigh","vorLow","dropoff","risk","adp","sleeper")
#fft[,numCols] <- apply(fft[,numCols],c(1,2),FUN=fdyn,digits=1)

#Set-Up rankings table 
ffd<-ff[,c("id","pos","name","points","vor","risk")]

fft <- fft[!grepl("LB|DL|DB",fft$pos),]; rownames(fft) <- 1:nrow(fft)
ff <- ff[!grepl("LB|DL|DB",ff$pos),]; rownames(ff) <- 1:nrow(ff)

pFileName <- gsub("Draft","Player",draftFile)
allPlayers <- updatePlayersFromSleeper(pFileName=pFileName,leagueId)

#ff <- ff %>% left_join(allPlayers[,c("player_id","pId")], by = c("player_id" = "player_id"))


ff$pId <- unlist(sapply(1:nrow(ff),function(x) paste(ff[x,'name'],ff[x,'team'],ff[x,'pos'],sep="|")))
#Remove Duplicates
#ff_dups <- ff %>% group_by(pId) %>% filter(n()>1)
ff <- ff %>% filter(!duplicated(pId))

ff <- updateMissingIds(ff,allPlayers)
ffd <- add_adp_fd(ff,allPlayers)

ff <- getManualProjections_Strength(ff=ff)

leagueInfo <- getLeagueInfo(leagueId)
if(nrow(leagueInfo)==1){
  keeperFile <- paste0("Data/Keepers-",gsub(" ","",leagueInfo$name),leagueInfo$season,'-',Sys.Date(),".xlsx")
  keeperFile <- gsub("[^\x01-\x7F]", "", keeperFile)
  prevLeague <- leagueInfo$previous_league_id #prevLeague <- leagueInfo$league_id
  if(!file.exists(keeperFile) & file.exists(fffile)){
    if(!is.null(prevLeague)) sKeepers <- getKeeperDraftRound(leagueId = leagueId,ff,writeFile=keeperFile, allPlayers=allPlayers)
  }
}
