##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    Compiles the matches from data
##
##      This file defines functions used in the calculation of elo,
##        as well as the plots that show up in the app.
##
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(RSQLite)
library(tidyverse)

source('elo_functions.R')

run_fresh <- FALSE

if (run_fresh) {
  # Connect to data base ----------------------------------------------------
  con <- dbConnect(SQLite(), dbname = "../input/database.sqlite")
  # list all tables
  # dbListTables(con)

  Match        <- tbl_df(dbGetQuery(con, "SELECT * FROM Match"))
  League       <- tbl_df(dbGetQuery(con, "SELECT * FROM League"))
  Team         <- tbl_df(dbGetQuery(con, "SELECT * FROM Team"))
  Country      <- tbl_df(dbGetQuery(con, "SELECT * FROM Country"))
  # select columns

  ## How many games and teams by league?
  Match %>% group_by(league_id, country_id) %>% summarise(games = n(), teams = length(unique(home_team_api_id))) %>%
    left_join(League %>% rename(league_name = name)) %>% left_join(Country %>% rename(country_name = name))

  all_matches <- Match %>%
    select(
      game_num = id,
      date,
      t1_id = home_team_api_id,
      t2_id = away_team_api_id,
      t1_score = home_team_goal,
      t2_score = away_team_goal,
      league_id,
      country_id
    ) %>%
    left_join(League %>% select(league_id = id, league_name = name)) %>%
    left_join(Country %>% rename(country_id = id, country_name = name)) %>%
    left_join(Team %>% select(t1_id = team_api_id, t1 = team_long_name)) %>%
    left_join(Team %>% select(t2_id = team_api_id, t2 = team_long_name))


  ## ignore ties
  singles_games <- all_matches %>%
    filter(t1_score != t2_score) #%>%
  # filter(year(as.Date(date)) == 2016)
  singles_games %>% write_rds('data/no_tie_matches.rds')

  league_games <- singles_games %>%
    split(.$country_name) %>%
    map( ~ RunCalculationLoop(.x, use_full_name = 'none'))

  league_games %>% write_rds('data/league_games.rds')

} else {
  league_games <- read_rds('data/league_games.rds')
  no_tie_matches <- read_rds('data/no_tie_matches.rds')
  all_matches <- read_rds('data/all_matches.rds')
}