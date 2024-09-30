## TRENDS 


# 2023-24 RS shot range 
#'https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=5ft%20Range&Division=&GameScope=&GameSegment=&ISTRound=&LastNGames=0&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision='


# 23-24 RS pace data (advanced stats)
# 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='


# 23-24 RS 3PA per game (traditional stats)
# 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='


## PACE TREND OVER THE YEARS ##
## Pace since 1996-97 NBA Regular Season to 2023-24 RS 
## DATASET SAVED 


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
      text = element_text(family='PT Mono'),
      axis.title.x = element_text(color = 'black', margin = margin(t = 30, b =25), family = 'K2D', face = 'bold', size = 19), 
      axis.title.y = element_text(angle = 90, color = 'black', margin = margin(r = 25, l = 15), family = 'K2D', face = 'bold', size = 19), 
      axis.text = element_text(color = 'grey40', face = 'bold', size = 13),
      panel.background = element_rect('grey98'), 
      plot.background = element_rect('grey98'),
      plot.title = element_text(margin = margin(b=30, t = 10), size = 35, hjust = 0.6, family = 'Proxima Nova', face = 'bold'),
      plot.subtitle=element_text(size=12, hjust = 0, vjust = -1), 
      panel.grid.major = element_line(color='grey80', linetype = 'dashed'),
      plot.margin = unit(c(0.5, 1, 0, 0.2), "inches")
      ) 
}




# AVERAGE NBA PACE TREND ####

# NBA RS advanced stats url --> pace data
'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='


# headers
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



### Scraping function depending on season (season format: 2023-24)
### PACE Function ----

advanced <- function(season) {
  
  # Custom url dependent on NBA RS 
  url <- paste0('https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=',
                '&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&',
                'Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=', 
                season, 
                '&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=')
  
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  pace_data <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(pace_data) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  # Adds specific column indicating season
  pace_data$Season <- season
  
  # Removes not necessary columns
  # pace_data <- pace_data[, - c(8, 10:23, 25, 27:46)]
  
  return(pace_data)
 
}

# EG: dataset for pace during 2023-24 Regular Season
# pace_2024 <- pace('2023-24')

# Vector of years since 1996, with correct format for url (2023-24)
year <- paste0(1996:2023, '-', substr(1997:2024, 3, 4))

# Maps all single dataframes into one 
adv_stats <- map_df(year, advanced)



## Load Dataset ====
adv_stats <- read.csv("/Users/davidetissino/Desktop/Last/Tesi/Thesis/Datasets/Teams_Advanced_All.csv") %>% 
  .[, - c(8, 10:23, 25, 27:46)]


# Convert pace data to numeric 
pace_stats$PACE <- as.numeric(pace_stats$PACE)

# New dataframe with Season and League Average Pace 
avg_pace <- pace_stats %>% 
  group_by(Season) %>% 
  summarise(avg_pace = mean(PACE)) %>% 
  mutate(Season = factor(Season, levels = unique(Season)))

# Add single year column, better for graph
avg_pace$year <- gsub('-.*', '', avg_pace$Season) %>% 
  as.numeric(avg_pace$year)

# Adds 1 to previous column
# EG: Season 1996-97 --> Year 1996 --> Year 1997
avg_pace$year <- avg_pace$year + 1



### Plot ----

ggplot(
    avg_pace, 
    aes(x = year, y = avg_pace, group = 1)
  ) + 
  geom_line(
    color = 'grey70', 
    size = 1.7
  ) + 
  geom_point(
    shape = 21, 
    color = 'black', 
    fill = 'dodgerblue', 
    size = 4
  ) + 
  theme_davide() + 
  theme(
    plot.title = element_text(margin = margin(b=30, t = 10), size = 35, hjust = -0.5, family = 'Proxima Nova', face = 'bold'),
    panel.background = element_rect('grey98'),
    plot.background = element_rect('grey98'),
    panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
    text = element_text(family='PT Mono'),
    plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"), 
    axis.title.x = element_text(color = 'black', margin = margin(t = 30, b =25), family = 'K2D', face = 'bold', size = 19), 
    axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 15), family = 'K2D', face = 'bold', size = 19), 
    axis.text = element_text(color = 'grey40'),
    plot.caption = element_text(margin = margin(b = 20))
     
  ) + 
  labs(
    x = 'NBA Regular Season', 
    y = 'Average Pace', 
    title = 'Evolution of Pace in the NBA',
    caption = 'Source: stats.nba.com'
  ) +
  # Customize X-axis ticks 
  scale_x_continuous(
    breaks = c(seq(1997, 2024, 4), 2025)
  ) + 
  # Customize Y-axis ticks
  scale_y_continuous(
    breaks = c(90, 92.5, 95, 97.5, 100, 102.5)
  )


# ggsave('/Users/davidetissino/Desktop/Last/Tesi/Thesis/Graphs/pace_evolution.png', dpi = 'retina')


ggsave('/Users/davidetissino/Desktop/Pace.png', dpi = 'retina', width = 9, height = 9)





#### --------------- ###


# AVERAGE SECONDS / POSS. TREND ####

## Per Possession Function ----

per_poss <- function(season) {
  
  url <- paste0('http://stats.inpredictable.com/nba/ssnTeamPoss.php?season=',
                season, 
                '&po=0&frdt=1996-11-01&todt=1997-06-13&view=off&sort=aotop&order=ASC')
  
  per_possession_stats <- read_html(url) %>%  
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  colnames <- per_possession_stats[1, ]
  
  colnames(per_possession_stats) <- colnames
  
  per_possession_stats <- per_possession_stats[-c(1, nrow(per_possession_stats), nrow(per_possession_stats) - 1), ]
  
  per_possession_stats$Season <- season + 1
  
  per_possession_stats$Year <- season + 1
  
  return(per_possession_stats)

}

# Vector of seasons to scrape (EG: 1996 for 1996-97 RS)
year <- 1996:2023

# Maps all single dataframes into one 
per_poss_stats <- map_df(year, per_poss)


## Load Dataset ----
per_poss_stats <- read.csv('/Users/davidetissino/Desktop/Last/Tesi/Thesis/Datasets/Teams_Per_Possession_All.csv') 


# remove not necessary columns
per_poss_stats <- per_poss_stats[, -c(1, 7:23)]

# change column names
colnames(per_poss_stats)[4] <- 'seconds_per'
colnames(per_poss_stats)[5] <- 'league_rank'

# Create single dataframe with Season & Average Seconds 
avg_seconds <- per_poss_stats %>% 
  group_by(Season) %>% 
  summarise(avg_seconds = mean(seconds_per)) %>% 
  mutate(Season = factor(Season, levels = unique(Season)),
         year = unique(per_poss_stats$Year))


## Plot ----

ggplot(
  avg_seconds, 
  aes(x = year, y = avg_seconds, group = 1)
) + 
  geom_line(
    color = 'grey70', 
    size = 1.7
  ) + 
  geom_point(
    shape = 21, 
    color = 'black', 
    fill = 'dodgerblue', 
    size = 4
  ) + 
  theme_davide() + 
  theme(
    plot.title = element_text(margin = margin(b=30, t = 10), size = 35, hjust = 0.6, family = 'Proxima Nova', face = 'bold'),
    panel.background = element_rect('grey98'),
    plot.background = element_rect('grey98'),
    panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
    text = element_text(family='PT Mono'),
    plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"), 
    axis.title.x = element_text(color = 'black', margin = margin(t = 30, b =25), family = 'K2D', face = 'bold', size = 19), 
    axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 15), family = 'K2D', face = 'bold', size = 19), 
    axis.text = element_text(color = 'grey40'),
    plot.caption = element_text(margin = margin(b = 20))
  ) + 
  labs(
    x = 'NBA Regular Season', 
    y = 'Seconds Per Possession', 
    title = 'NBA Average Seconds Per Possession',
    caption = 'Source: http://stats.inpredictable.com'
  ) +
  # Customize X-axis ticks
  scale_x_continuous(
    breaks = c(seq(1997, 2024, 4), 2025)
  ) +
  # Customize Y-axis ticks
  scale_y_continuous(
    breaks = c(14, 14.25, 14.5, 14.75, 15, 15.25, 15.5, 15.75, 16)
  )


ggsave('/Users/davidetissino/Desktop/Per.png', dpi = 'retina', width = 9, height = 9)





#### ----------------- ###
# SHOTS ATT. & SECONDS / POSS. TREND ####


# NBA RS advanced stats url --> pace data
'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

# headers
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



### Scraping function depending on season (season format: 2023-24)
# Traditional Stats Function ====

traditional <- function(season) {
  
  # Custom url dependent on NBA RS 
  url <- paste0('https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=',
                season,
                '&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=')
    
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  trad_data <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(trad_data) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  # Adds specific column indicating season
  trad_data$Season <- season
  
  return(trad_data)
  
}


# Vector of years since 1996, with correct format for url (2023-24)
year <- paste0(1996:2023, '-', substr(1997:2024, 3, 4))

# Maps all single dataframes into one 
trad_stats <- map_df(year, traditional)



# Load Datasets ####
library(tidyverse)
library(jsonlite)
library(httr)
library(ggplot2)
library(ggthemes)

# custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='PT Mono'),
      axis.title.x = element_text(color = 'black', margin = margin(t = 30, b =25), family = 'K2D', face = 'bold', size = 19), 
      axis.title.y = element_text(angle = 90, color = 'black', margin = margin(r = 25, l = 15), family = 'K2D', face = 'bold', size = 19), 
      axis.text = element_text(color = 'grey40', face = 'bold', size = 13),
      panel.background = element_rect('grey98'), 
      plot.background = element_rect('grey98'),
      plot.title = element_text(margin = margin(b=30, t = 10), size = 35, hjust = 0.6, family = 'Proxima Nova', face = 'bold'),
      plot.subtitle=element_text(size=12, hjust = 0, vjust = -1), 
      panel.grid.major = element_line(color='grey80', linetype = 'dashed'),
      plot.margin = unit(c(0.5, 1, 0, 0.2), "inches")
    ) 
}


## Traditional Dataset ====
trad_stats <- read.csv('/Users/davidetissino/Desktop/Last/Tesi/Thesis/Datasets/Teams_Traditional_All.csv') 


## Per Possession Dataset ====
per_poss_stats <- read.csv('/Users/davidetissino/Desktop/Last/Tesi/Thesis/Datasets/Teams_Per_Possession_All.csv') 
per_poss_stats <- per_poss_stats[, -c(1, 6:24)]
colnames(per_poss_stats)[4] <- 'seconds_per'


#### Teams Data, Slugs, Colors ----
tms <- read.csv('/Users/davidetissino/Desktop/R/data/teams.csv') %>% 
  .[, -c(4:6)]



#### Historical Franchises ----
# headers
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


url <- 'https://stats.nba.com/stats/franchisehistory?LeagueID=00&Season=2023-24'

res <- GET(url = url, add_headers(.headers = headers))

resp <- fromJSON(content(res, 'text'))

franchises <- data.frame(resp$resultSets$rowSet[[1]])

colnames(franchises) <- resp[['resultSets']][['headers']][[1]]


# keep inactive teams relative to our time frame 
franchises <- franchises %>% 
  filter(
    franchises$END_YEAR >= 1996,
    franchises$END_YEAR < 2023
  )

# keep only relevant columns 
franchises <- franchises[, c(2:6)]

# merge city and name into one 
franchises$team <- paste(franchises$TEAM_CITY, franchises$TEAM_NAME)

# add missing slugs 
franchises$slugTeam <- c('NJN', 'CHH', 'LAC', 'VAN', 'NOH', 'NOK', 'SEA', 'WAS')

# remove columns to merge 
franchises <- franchises[, -c(2:5)]

colnames(franchises)[1] <- 'idTeam'

# merge into one 
teams <- rbind(tms, franchises)

teams <- teams[-33, ]



# Prepare Datasets ====

## Traditional Dataset ----
# create one total Shot Attempted column
trad_stats$SA <- trad_stats$FGA + trad_stats$FG3A

# remove some columns
trad_stats <- trad_stats[, -c(7, 8, 11, 14:54)]

# appropriate year column (EG: 1996 for 1996-97 NBA RS)
trad_stats$year <- gsub('-.*', '', trad_stats$Season) %>% 
  as.numeric(trad_stats$year)

trad_stats$year <- trad_stats$year + 1

# to merge
colnames(trad_stats)[2] <- 'team'

trad_stats$team[trad_stats$team == 'LA Clippers'] <- 'Los Angeles Clippers'


###### TRADITIONAL + TEAMS ====
# merge traditional stats (trad_stats) w/ teams info/slugs
traditional_all <- merge(trad_stats, teams, by = 'team') %>%
  .[, -2] %>% 
  .[c(1, 14, 13, 12, 2:9, 11, 10)]

# create unique ID for each historical team to merge 
# ID = teamID + '_' + slug + '_' + season
# EG: 1996-97 Atlanta Hawks ATL --> 1610612737_ATL_97
traditional_all$unique_ID <- paste0(traditional_all$idTeam, '_', traditional_all$year)


## Per Possession Dataset ----
colnames(per_poss_stats)[1] <- 'slugTeam'

###### PER POSSESSION + TEAMS =====
per_poss_all <- merge(per_poss_stats, teams, by = 'slugTeam')

# remove rows (all Washington Bullets after 1997)
per_poss_all <- per_poss_all %>% 
  filter(
    team != 'Washington Bullets', 
    Year >= 1997
  ) %>% 
  .[c(7, 1, 6, 5, 2:4)]

# unique ID
per_poss_all$unique_ID <- paste0(per_poss_all$idTeam, '_', per_poss_all$Year)


### TRADITIONAL + PER POSS. ####
final <- merge(per_poss_all, traditional_all, by = 'unique_ID')

# keep only relevant columns
final <- final[, -c(3:6, 9:11)]




ggplot(
  final, 
  aes(
    x = seconds_per, 
    y = FGA
    )
  ) + 
  geom_point(
    shape = 21, 
    color = 'black', 
    fill = 'dodgerblue', 
    size = 3.5
  ) + 
  geom_smooth(
    method = 'lm', 
    color = 'firebrick'
  ) +
  geom_text(
    label = 'FGA = 143.03 - 3.96 * (Seconds Per Possession)',
    x = 14, 
    y = 71, 
    size = 4,
    fontface = 'bold'
  ) +
  theme_davide() + 
  theme(
    plot.caption = element_text(margin = margin(b = 15))
  ) +
  labs(
    x = 'Seconds per Possession', 
    y = 'Field Goals Attempted', 
    title = 'FGA vs. Seconds Per Possession', 
    caption = 'Source: stats.nba.com, inpredictable.com'
  )
  

ggsave('/Users/davidetissino/Desktop/FGA_Seconds1.png', dpi = 'retina', width = 11, height = 9)

    




