download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
draftFile <- "Data/FFDraftData.RData"
#fffile <- paste0(download_location,"/ffa_customrankings2017-3.csv")
#fffile <- paste0(download_location,"/ffa_customrankings",Year(Sys.Date()),"-0.csv")
fffile <- paste0("Data/ffa_customrankings",Year(Sys.Date()),"-0.csv")
ffProjections <- getFFAnalytics_Projections_CSV(fffile)

#469196363834322944 #2019 MXC 469304291434164225 #340916602432159744 #2018 MXC 338482281133907968 #340196925133344768 #340208591887724544
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

#Set up ordered table
ffCols <- c("pos","name","team","age","bye","points","upper","lower","vor","vorHigh","vorLow","dropoff","risk","adp","sleeper")
#ffCols[!(ffCols %in% colnames(ff))]
fft<-ff[,ffCols]
numCols <- c("points","upper","lower","vor","vorHigh","vorLow","dropoff","risk","adp","sleeper")
#fft[,numCols] <- apply(fft[,numCols],c(1,2),FUN=fdyn,digits=1)

#Set-Up rankings table 
ffd<-ff[,c("id","pos","name","points","vor","risk")]

fft <- fft[!grepl("LB|DL|DB",fft$pos),]; rownames(fft) <- 1:nrow(fft)
ff <- ff[!grepl("LB|DL|DB",ff$pos),]; rownames(ff) <- 1:nrow(ff)

ff$pId <- unlist(sapply(1:nrow(ff),function(x) paste(ff[x,'name'],ff[x,'team'],ff[x,'pos'],sep="|")))

ff <- getManualProjections_Weekly(ff=ff)

pFileName <- gsub("Draft","Player",draftFile)
allPlayers <- updatePlayersFromSleeper(pFileName=pFileName,leagueId)
