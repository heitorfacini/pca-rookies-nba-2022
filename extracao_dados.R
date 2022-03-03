if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("PatrickChodowski/NBAr")
library(NBAr)


players <- get_players('2021', only_current = 1)
manter <- c('display_first_last', 'from_year')
players <- players[manter]
rookies <- subset(players, players['from_year']==2021)
names(rookies) <- c('player_name','year')


general <- get_general('2021', type = 'Player', 'Base')
manter_general <- c('player_name','gp','w_pct','min','reb','ast','tov', 'stl', 'blk', 'pts', 'plus_minus',
                    'fg3m')
general <- general[manter_general]

advanced <- get_general('2021', type = 'Player', 'Advanced')
manter_advanced = c('player_name', 'team_abbreviation','off_rating','def_rating','ts_pct')
advanced <- advanced[manter_advanced]

rookies_stats <- merge(x=rookies,y=general,by="player_name",all.x=TRUE)
rookies_stats <- merge(x=rookies_stats, y=advanced, by="player_name", all.x=TRUE)
rookies_stats <- subset (rookies_stats, rookies_stats['gp']>=15)
rookies_stats['total_mins'] <- rookies_stats['gp']*rookies_stats['min']
rookies_stats <- subset(rookies_stats, rookies_stats['total_mins']>= 225)

write.csv(rookies_stats, 'rookies_stats.csv', row.names = FALSE)
