library(rhandsontable)
library(DT)
library(rjson)
library(DescTools)

library(XML)
library(dplyr)
library(purrr)

library(ggplot2)
library(ggrepel)
library(ggthemes)

library(RSelenium)

library(kableExtra)
#library(stringdist)

if(!("ffanalytics" %in% installed.packages())){
  devtools::install_github("FantasyFootballAnalytics/ffanalytics")
}

library(ffanalytics)

#library(nflfastR)
#nflTeams <- teams_colors_logos

DraftYear <- Year(Sys.Date())

if(!exists("player_table")){
  apiUrl <- paste0("https://api.myfantasyleague.com/",DraftYear,"/export?TYPE=players&L=&APIKEY=&DETAILS=1&SINCE=&PLAYERS=&JSON=1")
  player_table <<- httr::GET(apiUrl) %>%
    httr::content() %>% `[[`("players") %>% `[[`("player") %>%
    purrr::map(tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE", "PK", "Def", "DE", "DT", "LB", "CB", "S")) %>%
    dplyr::select(id, name, position, team, weight, draft_year, draft_team, draft_round, draft_pick, birthdate) %>%
    tidyr::extract(name, c("last_name", "first_name"), "(.+),\\s(.+)") %>%
    dplyr::mutate(birthdate = as.Date(as.POSIXct(as.numeric(birthdate), origin = "1970-01-01")),
                  position = dplyr::recode(position, Def = "DST", PK = "K"),
                  age = as.integer(lubridate::year(Sys.time()) - lubridate::year(birthdate)),
                  exp = DraftYear - as.integer(draft_year))
}
