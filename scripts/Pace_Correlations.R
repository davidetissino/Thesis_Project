# CORRELATION MATRICES

library(corrplot)
library(tidyverse)


# Load Datasets
adv_stats <- read.csv("/Users/davidetissino/Desktop/Last/Tesi/Thesis/Datasets/Teams_Advanced_All.csv")
trad_stats <- read.csv('/Users/davidetissino/Desktop/Last/Tesi/Thesis/Datasets/Teams_Traditional_All.csv') 

# add unique ID columns 
adv_stats$unique_ID <- paste0(adv_stats$TEAM_ID, '_', adv_stats$Season)
trad_stats$unique_ID <- paste0(trad_stats$TEAM_ID, '_', trad_stats$Season)



# traditional NO ranking
trad_NoRank <- trad_stats[, -c(1:5, 7, 8, 11, 14, 19, 24:26, 29:56)]

# assign column names 
colnames_NO <- c('W%', 'FGA', 'FG%', '3PA', '3P%', 'FTA', 'FT%', 'OREB', 'DREB', 'AST', 'TOV', 'STL', 'BLK', 'PTS', '+/-')

colnames(trad_NoRank) <- colnames_NO

# traditional NO rank correlations
corrplot(cor(trad_NoRank), method = 'color', addCoef.col = 'black', number.cex = 1.6,
         col = colorRampPalette(c("dodgerblue", "white", "firebrick3"))(100),
         tl.col = 'black', tl.cex = 1, tl.srt = 0, cl.cex = 1.6)



# traditional WITH ranking
trad_rank <- trad_stats[, -c(1:31, 33:35, 37:38, 40:43, 45, 48:52, 55)]

# keep only pace rank and unique ID
pace_rank <- adv_stats[, -c(1:44, 46, 47)]

# merge pace rank to traditional stats ranks
ranks <- merge(trad_rank, pace_rank, by = 'unique_ID') %>% 
  .[, -c(1, 2)]

# rearrange columns
ranks <- ranks[, c(8, 6, 1:5, 7)]

colnames <- c('PACE_Rank', 'PTS_Rank', 'FG%_Rank', '3P%_Rank', 'DREB_Rank', 'AST_Rank', 'TOV_Rank', '+/-_Rank')

colnames(ranks) <- colnames

ranks <- ranks[, -6]

# correlation of RANKS 
corrplot(cor(ranks), method = 'color', addCoef.col = 'black', number.cex = 2.7,
         col = colorRampPalette(c("dodgerblue", "white", "firebrick3"))(100),
         tl.col = 'black', tl.srt = 0, tl.cex = 1.4, cl.cex = 1.7)










