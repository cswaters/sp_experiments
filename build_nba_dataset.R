library(tidyverse)
library(lubridate)
library(zoo)
library(caret)

replace_with_zeros <- function(df, cols){
  df %>% 
    mutate_at(.vars = cols, funs(replace(., is.na(.), 0)))
}

stat_per_minute <- function(stat, min){
  # points per minute is a more useful metric
  # for our purposes it's useful because
  # it takes overtime into account 
  stat / min
} 



df <- read_csv('data/nba_features.csv')
df$X1 <- NULL

df <- replace_with_zeros(df, c('ot','awayot','homeot'))


df[['min_played']] <- 48 + (df[['ot']] * 5)

df <- df %>% 
  arrange(Year,Month,Day)
df[['gid']] <- 1:nrow(df)
df$seas <- ifelse(df$Month < 10, df$Year - 1, df$Year)
 
non_gather_cols <- setdiff(names(df),c('home','away'))
nba_long <- df %>% 
  gather(location, team, -one_of(non_gather_cols))

nba_long <- nba_long %>%
  left_join(nba_long %>%
              select(gid, opp = team)) %>%
  filter(team != opp) %>%
  mutate(
    tm_score = ifelse(location == 'away', awayscore, homescore),
    opp_score = ifelse(location == 'home', awayscore, homescore)
  ) 

betting_cols <- non_gather_cols[(17:45)]
score_cols <- setdiff(non_gather_cols, betting_cols)[3:14]
date_cols <- setdiff(non_gather_cols, c(betting_cols, score_cols))[c(1:2,45:57,60)]
stat_cols <- setdiff(non_gather_cols, c(betting_cols, score_cols, date_cols))[7:42]

away_team_stats <- stat_cols[startsWith(stat_cols, 'a')]
home_team_stats <- stat_cols[!startsWith(stat_cols, 'a')]
team_stat_cols <- str_replace_all(home_team_stats, 'h','tm_')
opp_stat_cols <- str_replace_all(home_team_stats, 'h','opp_')  

away_scores <- score_cols[startsWith(score_cols, 'a')]
home_scores <- score_cols[startsWith(score_cols, 'h')]

tm_scores_cols <- str_replace_all(away_scores, 'away', 'tm_')
opp_score_cols <- str_replace_all(away_scores, 'away', 'opp_')

at_home <- nba_long %>% 
  filter(location == 'home') %>% 
  select(team,opp,gid,location,
         one_of(home_scores),
         one_of(away_scores),
         one_of(home_team_stats), 
         one_of(away_team_stats)) %>% 
  set_names(c('team','opp','gid','location',
              tm_scores_cols, opp_score_cols,
              team_stat_cols, opp_stat_cols))

on_road <- nba_long %>% 
  filter(location == 'away') %>% 
  select(team,opp,gid, location,
         one_of(away_scores),
         one_of(home_scores),
         one_of(away_team_stats), 
         one_of(home_team_stats)) %>% 
  set_names(c('team','opp','gid','location',
              tm_scores_cols, opp_score_cols,
              team_stat_cols, opp_stat_cols))

tmp <- bind_rows(at_home, on_road)

nba_full <- nba_long %>% 
  select(-one_of(home_team_stats),
         -one_of(away_team_stats),
         -one_of(home_scores),
         -one_of(away_scores),
         -tm_score, -opp_score
         ) %>% 
  left_join(tmp)


nba_nested <- nba_full %>% 
  mutate(hour = hour(time)) %>% 
  select(-time) %>% 
  group_by(team, seas) %>% 
  nest()

add_features_nba <- . %>% 
  arrange(date) %>%
  mutate(
    is_home = (location == 'home') * 1,
    result = (tm_score > opp_score) * 1,
    # win or lose
    lag_result = lag(result),
    # win or lose last game
    games = row_number(date),
    # games played (includes row)
    lag_games_played = games - 1,
    #games played up until game
    w_pct = cummean(result),
    # win percentage
    lag_w_pct = lag(w_pct),
    # win % up until game
    last_5_wpct = rollmeanr(lag_w_pct, k = 5, fill = NA),
    #win % of last 5 games
    mov = tm_score - opp_score,
    # game MOV
    lag_mov_avg = lag(cummean(mov)),
    # Cum MOV up until game
    last_5_mov = rollmeanr(lag_mov_avg, k = 5, fill = NA),
    # Cum MOV last 5 games
    lag_avg_tm_score = lag(cummean(tm_score)),
    # Season cum avg team pts
    lag_avg_opp_score = lag(cummean(opp_score)),
    # Season cum avg opp pts
    day_between = as.numeric(date - lag(date)),
    # days between games,
    lag_ptspm_off = stat_per_minute(lag_avg_tm_score, min_played),
    #pts per min
    lag_ptspm_def = stat_per_minute(lag_avg_opp_score, min_played),
    #opp pts per min
    ppm_ratio = lag_ptspm_off / lag_ptspm_def, # points per min ratio
    # 
    lag_avg_team_oreb = lag(cummean(tm_oreb)),
    lag_avg_opp_oreb = lag(cummean(opp_oreb)),
    lag_avg_team_dreb = lag(cummean(tm_dreb)),
    lag_avg_opp_dreb = lag(cummean(opp_dreb)),
    
    lag_team_oreb_pm = stat_per_minute(lag_avg_team_oreb,min_played),
    lag_team_dreb_pm = stat_per_minute(lag_avg_team_dreb,min_played),
    lag_opp_oreb_pm = stat_per_minute(lag_avg_opp_oreb,min_played),
    lag_opp_dreb_pm = stat_per_minute(lag_avg_opp_dreb,min_played),
    
    lag_team_oreb_ratio = lag_team_oreb_pm / lag_opp_dreb_pm,
    lag_team_dreb_ratio = lag_team_dreb_pm / lag_opp_oreb_pm
  )
  

nba <- nba_nested %>% 
  mutate(features = map(data, add_features_nba)) %>% 
  unnest(features)


cols_for_dataset <-
  c(
    'lag_result',
    'lag_w_pct',
    'last_5_wpct',
    'last_5_mov',
    'lag_mov_avg',
    'lag_ptspm_off',
    'lag_ptspm_def',
    'ppm_ratio',
    'lag_games_played',
    'day_between',
    'lag_team_oreb_pm',
    'lag_team_dreb_pm',
    'lag_opp_oreb_pm',
    'lag_opp_dreb_pm',
    'lag_team_oreb_ratio',
    'lag_team_dreb_ratio'
  )

home_col_names <-c('gid', paste0('h_', cols_for_dataset))
away_col_names <-c('gid', paste0('v_', cols_for_dataset))


nba_ext <- df %>% # use original dataset in home vs. away format
  left_join(
    nba %>%
      filter(is_home == 1) %>% # add only data where team is at home
      select(gid, one_of(cols_for_dataset)) %>% # only features listed above
      set_names(home_col_names) # change feature names to vector prefixed with h_
    
  ) %>%
  left_join(
    nba %>%
      filter(is_home == 0) %>% # add only data where team is away
      select(gid, one_of(cols_for_dataset)) %>% # only features listed above
      set_names(away_col_names) # change feature names to vector prefixed with v_
  )

nba_ext <- nba_ext %>%
  mutate(
    mov = homescore - awayscore,
    home_win = ((mov > 0) * 1) %>% factor(),
  )


names(nba_ext)
at_home <- nba %>% 
  filter(is_home == 1) %>% 
  left_join(
    select(nba_ext, 
           gid,
           opp_lag_ptspm_off = v_lag_ptspm_off,
           opp_lag_ptspm_def = v_lag_ptspm_def,
           opp_lag_w_pct = v_lag_w_pct,
           opp_ppm_ratio = v_ppm_ratio,
           opp_oreb_ratio = v_lag_team_oreb_ratio,
           opp_dreb_ratio = v_lag_team_dreb_ratio)
  )

on_road <- nba %>% 
  filter(is_home == 0) %>% 
  left_join(
    select(nba_ext, 
           gid,
           opp_lag_ptspm_off = h_lag_ptspm_off,
           opp_lag_ptspm_def = h_lag_ptspm_def,
           opp_lag_w_pct = h_lag_w_pct,
           opp_ppm_ratio = h_ppm_ratio,
           opp_oreb_ratio = h_lag_team_oreb_ratio,
           opp_dreb_ratio = h_lag_team_dreb_ratio)
  )


add_opp_features <- function(df){
  df %>% 
    mutate(opp_def_ppm_avg = rollmeanr(opp_lag_ptspm_def,5,fill=NA),
           opp_off_ppm_avg = rollmeanr(opp_lag_ptspm_off,5,fill=NA),
           opp_wpct_avg = rollmeanr(opp_lag_w_pct, 5, fill=NA),
           opp_avg_ptspm_ratio = rollmeanr(opp_ppm_ratio, 5, fill=NA),
           opp_avg_oreb_pm = rollmeanr(opp_oreb_ratio, 5, fill = NA),
           opp_avg_dreb_pm = rollmeanr(opp_dreb_ratio, 5, fill = NA),
    )
}

nba_nested <- bind_rows(at_home, on_road) %>% 
  group_by(team, seas) %>% 
  nest()

nba <- nba_nested %>% 
  mutate(new_features = map(data, add_opp_features)) %>% 
  unnest(new_features)


cols_for_dataset <-
  c('lag_result',
    'lag_w_pct',
    'last_5_wpct',
    'last_5_mov',
    'lag_mov_avg',
    'lag_ptspm_off',
    'lag_ptspm_def',
    'ppm_ratio',
    'lag_games_played',
    'day_between',
    'lag_opp_oreb_pm',
    'lag_opp_dreb_pm',
    'lag_team_oreb_ratio',
    'lag_team_dreb_ratio',
    tail(names(nba),12) # new opp features
  )

home_col_names <-c('gid', paste0('h_', cols_for_dataset))
away_col_names <-c('gid', paste0('v_', cols_for_dataset))

nba_ext <- df %>%
  left_join(
    nba %>%
      filter(is_home == 1) %>%
      select(gid, one_of(cols_for_dataset)) %>%
      set_names(home_col_names)
    
  ) %>%
  left_join(
    nba %>%
      filter(is_home == 0) %>%
      select(gid, one_of(cols_for_dataset)) %>%
      set_names(away_col_names)
  )

nba_ext <- nba_ext %>%
  mutate(
    mov = homescore - awayscore,
    home_win = ((mov > 0) * 1) %>% factor()
  )


export_cols <- names(nba_ext)[c(3,4,15,16,18,23,24,25,48,49,51,88:157)]

nba_ext %>% 
  select(one_of(export_cols)) %>% 
  write_csv('data/nba_feature_engineer.csv')