## THREE POINT TRENDS 

# 2023-24 RS shot range 
'https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=5ft%20Range&Division=&GameScope=&GameSegment=&ISTRound=&LastNGames=0&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision='

# 23-24 RS pace data (advanced stats)
'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

# 23-24 RS 3PA per game (traditional stats)
'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

# 23-24 RS 3PT totals (tot traditional stats)
'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='


## THREES TREND OVER THE YEARS ##

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
      text = element_text(family='Pt Mono'), 
      axis.title.x = element_text(color = 'black', margin = margin(t = 30, b =25), family = 'K2D', face = 'bold', size = 19), 
      axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 15), family = 'K2D', face = 'bold', size = 19, angle = 90), 
      axis.text = element_text(color = 'grey30'),
      axis.text.x = element_text(face='bold', size = 13),
      axis.text.y = element_text(face='bold', size = 13), 
      panel.background = element_rect('grey98'), 
      plot.background = element_rect('grey98'),
      plot.title = element_text(margin = margin(b=15, t = 10), face='bold', size=30, hjust = 0, family = 'Proxima Nova'),
      plot.subtitle=element_text(size=15, hjust = 0, margin = margin(b = 10), family = 'Proxima Nova'), 
      panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
      plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"), 
    ) 
}



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



#Threes PER GAME ####
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

  return(threes_pg)
  
}

# vector of years since 1996, with correct format for url 
year <- paste0(1996:2023, '-', substr(1997:2024, 3, 4))

## map all single dfs into one 
# tic()
# threes_PG <- map_df(year, THREES_PG)
# toc()


### Load Threes PG CSV ====
threes_PG <- read.csv('/Users/davidetissino/Desktop/Tesi/Thesis/Datasets/Threes_PG_YoY.csv')


# Summarise by YoY average 3PA PG 
avg_threes <- threes_PG %>% 
  group_by(Season) %>% 
  summarise(
    three_avg = mean(FG3A), 
    pct_avg = mean(FG3_PCT)
    ) %>% 
  mutate(Season = factor(Season, levels = unique(Season)))


# year column to plot 
avg_threes$year <- gsub('-.*', '', avg_threes$Season) %>% 
  as.numeric(avg_threes$year)

# Adds 1 to previous column
# EG: Season 1996-97 --> Year 1996 --> Year 1997
avg_threes$year <- avg_threes$year + 1


### Plot ----
ggplot(
  avg_threes, 
  aes(x = year, y = three_avg, group = 1)
) + 
  geom_line(
    color = 'grey70', 
    size = 1.7
  ) + 
  geom_point(
    shape = 21, 
    color = 'black', 
    fill = 'firebrick2', 
    size = 4
  ) + 
  theme_davide() + 
  theme(
    plot.title = element_text(margin = margin(b=30, t = 10), size = 30, hjust = -0.3, family = 'Proxima Nova', face = 'bold'),
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
    y = 'Average Three-Pointers Attempted', 
    title = 'Three-Pointers Attempted in the NBA',
    caption = 'Source: stats.nba.com'
  ) 



ggsave('/Users/davidetissino/Desktop/3PA_YoY_Avg.png', dpi = 'retina', width = 9, height = 9)






# Threes TOTALS function ####
THREES_TOT <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=',
                season, 
                '&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=')
  
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  threes_tot <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(threes_tot) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  # adds specific column indicating season
  threes_tot$Season <- season
  
  threes_tot <- threes_tot[, -c(17:26, 29:54)]
  
  return(threes_tot)
  
}

# vector of years since 1996, with correct format for url 
year <- paste0(1996:2023, '-', substr(1997:2024, 3, 4))

# # map all single dfs into one 
# tic()
# threes_TOT <- map_df(year, THREES_TOT)
# toc()


### Load Threes TOT csv  -----
threes_TOT <- read.csv('/Users/davidetissino/Desktop/Tesi/Thesis/Datasets/Threes_TOT_YoY.csv')

# summarise by YoY ToT threes 
avg_threes_TOT <- threes_TOT %>% 
  group_by(Season) %>% 
  summarise(
    three_avg = mean(FG3A)
    ) %>% 
  mutate(Season = factor(Season, levels = unique(Season)))

# year column to plot
avg_threes_TOT$year <- gsub('-.*', '', avg_threes_TOT$Season) %>% 
  as.numeric(avg_threes_TOT$year)

# Adds 1 to previous column
# EG: Season 1996-97 --> Year 1996 --> Year 1997
avg_threes$year <- avg_threes$year + 1


### Plot ----
ggplot(
  avg_threes_TOT, 
  aes(x = year, y = three_avg, group = 1)
) + 
  geom_line(
    color = 'grey70', 
    size = 1.7
  ) + 
  geom_point(
    shape = 21, 
    color = 'black', 
    fill = 'firebrick2', 
    size = 4
  ) + 
  theme_davide() + 
  theme(
    plot.title = element_text(margin = margin(b=30, t = 10), size = 30, hjust = -0.1, family = 'Proxima Nova', face = 'bold'),
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
    y = 'Total Three-Pointers Attempted', 
    title = 'Total 3-Pointers Attempted Yearly',
    caption = 'Source: stats.nba.com'
  ) 



ggsave('/Users/davidetissino/Desktop/3PA_YoY_ToT.png', dpi = 'retina', width = 9, height = 9)






# THREES as % OF TOTAL SHOTS ####
Shot_Dashboard <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguedashteamptshot?CloseDefDistRange=&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=Overall&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=',
                season, 
                '&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight=')
  
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  threes_tot <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(threes_tot) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  # adds specific column indicating season
  threes_tot$Season <- season
  
  return(threes_tot)
  
}

# vector of years since 1996, with correct format for url 
year <- paste0(2013:2023, '-', substr(2014:2024, 3, 4))

## map all single dfs into one 
# tic()
# threes_PG <- map_df(year, Shot_Dashboard)
# toc()


# Load dataset
shot_dash <- read.csv('/Users/davidetissino/Desktop/Tesi/Thesis/Datasets/Shot_Dash_AllTms_RS.csv')


### BoxPlot Graph ====
# remove some columns
shot_dash <- shot_dash[, -c(4:14)]

# year column to plot
shot_dash$year <- gsub('-.*', '', shot_dash$Season) %>% 
  as.numeric(shot_dash$year)

# Adds 1 to previous column
# EG: Season 1996-97 --> Year 1996 --> Year 1997
shot_dash$year <- shot_dash$year + 1


##### Plot ----
ggplot(
  data = shot_dash, 
  aes(
    x=as.character(year),
    y=FG3M)
) +  
  geom_boxplot(
    fill = 'firebrick4',
    color = 'black', 
    alpha = 0.4,
    outlier.shape = 21, 
    outlier.size = 4.5,
    lwd = 1.2, 
    fatten = 1.5
    
  ) + 
  labs(
    x="NBA Regular Season", 
    y="Total 3-Pointers Made", 
    title = 'Distribution of Total 3-Pointers Made', 
    caption = 'Source: stats.nba.com'
  ) + 
  theme_davide() +
  theme(
    plot.title = element_text(margin = margin(b=30, t = 10), size = 30, hjust = 0.5, family = 'Proxima Nova', face = 'bold'),
    panel.background = element_rect('grey98'),
    plot.background = element_rect('grey98'),
    panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
    text = element_text(family='PT Mono'),
    plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"), 
    axis.title.x = element_text(color = 'black', margin = margin(t = 30, b =25), family = 'K2D', face = 'bold', size = 19), 
    axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 15), family = 'K2D', face = 'bold', size = 19), 
    axis.text = element_text(color = 'grey40'),
    plot.caption = element_text(margin = margin(b = 20))
    
  )


ggsave('/Users/davidetissino/Desktop/3PM_BoxPlots.png', dpi = 'retina', width = 9, height = 9)



# Load CSV
threes_TOT <- read.csv('/Users/davidetissino/Desktop/Tesi/Thesis/Datasets/Threes_TOT_YoY.csv')

boxes <- threes_TOT[, -c(3:10, 14:18)]

# year column to plot
boxes$year <- gsub('-.*', '', boxes$Season) %>% 
  as.numeric(boxes$year)

# Adds 1 to previous column
# EG: Season 1996-97 --> Year 1996 --> Year 1997
boxes$year <- boxes$year + 1

# keep only some values 
boxes <- boxes %>% 
  filter(
    year == 2000 |
    year == 2005 | 
    year == 2010 | 
    year == 2015 | 
    year == 2020 |
    year == 2024
  )

#### Plot 2 ----
ggplot(
  data = boxes, 
  aes(
    x=as.character(year),
    y=FG3A)
) +  
  geom_boxplot(
    fill = 'firebrick4',
    color = 'black', 
    alpha = 0.4,
    outlier.shape = 21, 
    outlier.size = 4.5,
    lwd = 1.2, 
    fatten = 1.5
    
  ) + 
  labs(
    x="NBA Regular Season", 
    y="Total 3-Pointers Attempted", 
    title = 'Distribution of Total 3-Pointers Attempted', 
    caption = 'Source: stats.nba.com'
  ) + 
  theme_davide() +
  theme(
    plot.title = element_text(margin = margin(b=30, t = 10), size = 30, hjust = 0.5, family = 'Proxima Nova', face = 'bold'),
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
  scale_y_continuous(
    breaks = c(1000, 1500, 2000, 2500, 3000, 3500), 
    labels = c('1000' ,'1500', '2000', '2500', '3000', '3500')
  )


ggsave('/Users/davidetissino/Desktop/3PM_BoxPlots_2.png', dpi = 'retina', width = 9, height = 9)







### 3-Pointers as % of Totals YoY ====
avg <- threes_TOT

# column for 2-pointers
avg$FG2M <- avg$FGM - avg$FG3M
avg$FG2A <- avg$FGA - avg$FG3A

# take yearly means
avg <- avg %>% 
  group_by(Season) %>% 
  summarise(
    avg_FGM = mean(FGM), 
    avg_FGA = mean(FGA), 
    avg_FG2M = mean(FG2M), 
    avg_FG2A = mean(FG2A), 
    avg_FG3M = mean(FG3M), 
    avg_FG3A = mean(FG3A)
  ) %>% 
mutate(
  Season = factor(Season, levels = unique(Season))
)

# percentage column
avg$pct <- avg$avg_FG3A / avg$avg_FGA

# pivot longer
avg_long <- pivot_longer(
  avg, 
  cols = c('avg_FG2A', 'avg_FG3A'),
  names_to = 'type', 
  values_to = 'val'
)

# remove some columns 
avg_long <- avg_long[, c(1, 6:8)]

# year column to plot
avg_long$year <- gsub('-.*', '', avg_long$Season) %>% 
  as.numeric(avg_long$year)

# Adds 1 to previous column
# EG: Season 1996-97 --> Year 1996 --> Year 1997
avg_long$year <- avg_long$year + 1


##### Plot ----
ggplot(
  avg_long, 
  aes(fill=type, y=val, x=year)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_davide() + 
  labs(
    x = 'NBA Regular Season', 
    y = 'Shots Attempted', 
    title = '3-Pointers as Percentage of Total Attempts', 
    caption = 'Source: stats.nba.com'
  ) +
  theme(
    plot.title = element_text(margin = margin(b=30, t = 10), size = 27, hjust = 0.5, family = 'Proxima Nova', face = 'bold'),
    panel.background = element_rect('grey98'),
    plot.background = element_rect('grey98'),
    panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
    text = element_text(family='PT Mono'),
    plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"), 
    axis.title.x = element_text(color = 'black', margin = margin(t = 30, b = 10), family = 'K2D', face = 'bold', size = 19), 
    axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 15), family = 'K2D', face = 'bold', size = 19), 
    axis.text = element_text(color = 'grey40'),
    plot.caption = element_text(margin = margin(t = 10, b = 20)), 
    legend.position = 'bottom'
  ) + 
  scale_fill_manual(
    values = c('grey60', 'firebrick3')
  ) +
  scale_x_continuous(
    breaks = c(seq(1997, 2024, 4), 2025)
  )




ggsave('/Users/davidetissino/Desktop/3PM_as_Percentage.png', dpi = 'retina', width = 9, height = 9)



# THREES VS WIN% ####
threes_PG <- threes_PG[, -c(14:54)]

# year column to plot 
threes_PG$year <- gsub('-.*', '', threes_PG$Season) %>% 
  as.numeric(threes_PG$year)

# Adds 1 to previous column
# EG: Season 1996-97 --> Year 1996 --> Year 1997
threes_PG$year <- threes_PG$year + 1


### Plot ----
ggplot(
  threes_PG, 
  aes(
    y = W_PCT,
    x = FG3M)
  ) + 
  geom_point(
    shape = 21, 
    size = 2.7, 
    color = 'black', 
    fill = 'firebrick2', 
    alpha = 0.7
  ) + 
  geom_smooth(
    method = 'lm', 
    color = 'dodgerblue'
  ) +
  theme_davide() + 
  labs(
    x = '3-Pointers Made Per Game', 
    y = 'Win Percentage', 
    title = 'Win Percentage vs. 3-Pointers Made', 
    caption = 'Source: stats.nba.com'
  ) +
  theme(
    plot.title = element_text(margin = margin(b=30, t = 10), size = 27, hjust = 0.5, family = 'Proxima Nova', face = 'bold'),
    panel.background = element_rect('grey98'),
    plot.background = element_rect('grey98'),
    panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
    text = element_text(family='PT Mono'),
    plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"), 
    axis.title.x = element_text(color = 'black', margin = margin(t = 30, b = 10), family = 'K2D', face = 'bold', size = 19), 
    axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 15), family = 'K2D', face = 'bold', size = 19), 
    axis.text = element_text(color = 'grey40'),
    plot.caption = element_text(margin = margin(t = 10, b = 20)), 
    legend.position = 'bottom'
  ) + 
  scale_x_continuous(
    breaks = c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5),
    labels = c('0', '2.5', '5', '7.5', '10', '12.5', '15', '17.5')
  ) + 
  scale_y_continuous(
    breaks = c(0.15, 0.3, 0.45, 0.6, 0.75, 0.9), 
    labels = c('0.15', '0.3', '0.45', '0.6', '0.75', '0.9')
  )
  

model <- lm(data = threes_PG, W_PCT ~ FG3M)

summary(model)

ggsave('/Users/davidetissino/Desktop/WinPercentage_vs_3PTMade.png', dpi = 'retina', width = 11, height = 9)
















threes_PG_97 <- THREES_PG(year[1])
threes_PG_98 <- THREES_PG(year[2])
threes_PG_99 <- THREES_PG(year[3])
threes_PG_00 <- THREES_PG(year[4])
threes_PG_01 <- THREES_PG(year[5])
threes_PG_02 <- THREES_PG(year[6])
threes_PG_03 <- THREES_PG(year[7])
threes_PG_04 <- THREES_PG(year[8])
threes_PG_05 <- THREES_PG(year[9])
threes_PG_06 <- THREES_PG(year[10])
threes_PG_07 <- THREES_PG(year[11])
threes_PG_08 <- THREES_PG(year[12])
threes_PG_09 <- THREES_PG(year[13])
threes_PG_10 <- THREES_PG(year[14])
threes_PG_11 <- THREES_PG(year[15])
threes_PG_12 <- THREES_PG(year[16])
threes_PG_13 <- THREES_PG(year[17])
threes_PG_14 <- THREES_PG(year[18])
threes_PG_15 <- THREES_PG(year[19])
threes_PG_16 <- THREES_PG(year[20])
threes_PG_17 <- THREES_PG(year[21])
threes_PG_18 <- THREES_PG(year[22])
threes_PG_19 <- THREES_PG(year[23])
threes_PG_20 <- THREES_PG(year[24])
threes_PG_21 <- THREES_PG(year[25])
threes_PG_22 <- THREES_PG(year[26])
threes_PG_23 <- THREES_PG(year[27])
threes_PG_24 <- THREES_PG(year[28])





threes_PG_YoY <- rbind(
  threes_PG_97, 
  threes_PG_98,
  threes_PG_99,
  threes_PG_00,
  threes_PG_01,
  threes_PG_02,
  threes_PG_03,
  threes_PG_04,
  threes_PG_05,
  threes_PG_06,
  threes_PG_07, 
  threes_PG_08, 
  threes_PG_09, 
  threes_PG_10,
  threes_PG_11,
  threes_PG_12,
  threes_PG_13,
  threes_PG_14,
  threes_PG_15,
  threes_PG_16,
  threes_PG_17,
  threes_PG_18,
  threes_PG_19,
  threes_PG_20,
  threes_PG_21,
  threes_PG_22,
  threes_PG_23,
  threes_PG_24
)





