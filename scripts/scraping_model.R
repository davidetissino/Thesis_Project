

library(rvest)

url <- 'https://www.basketball-reference.com/leagues/NBA_2024_advanced.html'

#Here, we indicate that this is the table we want to extract
all_stats <- read_html(url) %>%  
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T)




library(tidyverse)
library(jsonlite)
library(httr)

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

url <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
tot_24_rs <- data.frame(json_resp$resultSets$rowSet)

colnames(tot_24_rs) <- json_resp[["resultSets"]][["headers"]][[1]]  


tot_24_rs$year <- 2024



tot_24_rs_2 <- tot_24_rs[, c(2, 4:7, 10:13, 15,16,18,19, 23, 24, 26, 27, 31, 67)]

tot_24_rs_2 <- tot_24_rs_2[, -c(2)]

colnames(tot_24_rs_2)[1] <- 'Player'
colnames(tot_24_rs_2)[2] <- 'Tm'
colnames(tot_24_rs_2)[3] <- 'Age'
colnames(tot_24_rs_2)[4] <- 'G'
colnames(tot_24_rs_2)[5] <- 'win_pct'
colnames(tot_24_rs_2)[6] <- 'MP'
colnames(tot_24_rs_2)[7] <- 'FG'
colnames(tot_24_rs_2)[9] <- 'X3P'
colnames(tot_24_rs_2)[10] <- 'X3PA'
colnames(tot_24_rs_2)[11] <- 'FT'
colnames(tot_24_rs_2)[13] <- 'TRB'
colnames(tot_24_rs_2)[18] <- 'Year'



tot_24_rs_2 <- tot_24_rs_2 %>% 
  mutate_at(
    vars(c(3:17)), 
    as.numeric
  )

tot_24_rs_2$X2P <- tot_24_rs_2$FG - tot_24_rs_2$X3P
tot_24_rs_2$X2PA <- tot_24_rs_2$FGA - tot_24_rs_2$X3PA



tot_24_rs_2$MP_pg <- tot_24_rs_2$MP / tot_24_rs_2$G
tot_24_rs_2$FG_pg <- tot_24_rs_2$FG / tot_24_rs_2$G
tot_24_rs_2$FGA_pg <- tot_24_rs_2 $FGA/ tot_24_rs_2$G
tot_24_rs_2$X3P_pg <- tot_24_rs_2$X3P / tot_24_rs_2$G
tot_24_rs_2$X3PA_pg <- tot_24_rs_2$X3PA / tot_24_rs_2$G
tot_24_rs_2$X2P_pg <- tot_24_rs_2$X2P / tot_24_rs_2$G
tot_24_rs_2$X2PA_pg <- tot_24_rs_2$X2PA / tot_24_rs_2$G
tot_24_rs_2$FT_pg <- tot_24_rs_2$FT / tot_24_rs_2$G
tot_24_rs_2$FTA_pg <- tot_24_rs_2$FTA / tot_24_rs_2$G
tot_24_rs_2$TRB_pg <- tot_24_rs_2$TRB / tot_24_rs_2$G
tot_24_rs_2$AST_pg <- tot_24_rs_2$AST / tot_24_rs_2$G
tot_24_rs_2$STL_pg <- tot_24_rs_2$STL / tot_24_rs_2$G
tot_24_rs_2$BLK_pg <- tot_24_rs_2$BLK / tot_24_rs_2$G
tot_24_rs_2$PTS_pg <- tot_24_rs_2$PTS / tot_24_rs_2$G





ballref <- all_stats[, c(2, 3, 8, 28, 29)]


colnames(ballref)[4] <- 'RPM'

# keep only one instance (overall)
ballref <- ballref %>%
  group_by(Player) %>%
  summarise(
    Pos = paste(unique(Pos), collapse = "-"),   # Concatenate unique positions
    PER = first(PER),                           # Take the first occurrence of PER
    RPM = first(RPM),                           # Take the first occurrence of RPM
    VORP = first(VORP)                          # Take the first occurrence of VORP
  )


ball_nba <- merge(tot_24_rs_2, ballref, by = 'Player')



ball_nba$Pos <- sub("-.*", "", ball_nba$Pos)






