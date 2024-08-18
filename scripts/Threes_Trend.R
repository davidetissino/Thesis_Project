

## TRENDS 


# 2023-24 RS shot range 
'https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=5ft%20Range&Division=&GameScope=&GameSegment=&ISTRound=&LastNGames=0&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision='


# 23-24 RS pace data (advanced stats)
'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='


# 23-24 RS 3PA per game (traditional stats)
'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='


## PACE TREND OVER THE YEARS ##
## Pace since 1996-97 NBA Regular Season to 2023-24 RS 

library(tidyverse)
library(jsonlite)
library(httr)
library(ggplot2)
library(ggthemes)
library(tictoc)

# Load custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='bold', size=17, margin=margin(t=15)),
      axis.title.y = element_text(face='bold', size=17, margin=margin(r=15), angle = 90),
      axis.text.x = element_text(face='bold', size = 13),
      axis.text.y = element_text(face='bold', size = 13), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=17, hjust = 0),
      plot.subtitle=element_text(size=12, hjust = 0, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}



## THREES MADE ====

# headers Ryan Davis (?)
headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)


### Threes PER GAME function ----
THREES_PG <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=',
                season,
                '&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=')
  
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  threes_pg <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(threes_pg) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  # adds specific column indicating season
  threes_pg$Season <- season
  
  threes_pg <- threes_pg[, -c(6:10, 14:54)]
  
  return(threes_pg)
  
}

# vector of years since 1996, with correct format for url 
year <- paste0(1996:2023, '-', substr(1997:2024, 3, 4))


# map all single dfs into one 
tic()
threes_PG <- map_df(year, THREES_PG)
toc()






### Threes TOTALS function ----
THREES_TOT <- function(season) {
  
  url <- paste0(
    'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=',
    season, 
    '&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='
  )
  
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  threes_tot <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(threes_tot) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  # adds specific column indicating season
  threes_tot$Season <- season
  
  threes_tot <- threes_tot[, -c(6:10, 14:54)]
  
  return(threes_tot)
  
}

# vector of years since 1996, with correct format for url 
year <- paste0(1996:2023, '-', substr(1997:2024, 3, 4))

# map all single dfs into one 
tic()
threes_TOT <- map_df(year, THREES_TOT)
toc()


