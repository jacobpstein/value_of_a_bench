###############################################
# Load in Data for understanding bench Performance
# Session Info:
# R version 4.2.1 (2022-06-23)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.3.1
###############################################

# Load packages
library(tidyverse) # the usual
library(nbastatR) # api wrapper, super buggy
library(janitor) # for easy cross-tabs and a few other aesthetic fixes
library(lubridate) # dates
library(ggrepel) # for labels
library(rstanarm) # Bayesian models
library(lme4) # mixed effect models
library(hoopR) # another api wrapper, but for ESPN, also includes NBA stats, also kind of buggy
library(viridis) # neat colors
library(httr) # for directly querying the NBA API rather than using wrapper packages
library(jsonlite) # for dealing with JSON output from NBA.com

# set seed
set.seed(20222712)

# going to need to expand the conneciton size for our api calls
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*3)

# get team info
team_df <- teams_annual_stats(all_active_teams = T,
                   modes = c("Totals"),
                   return_message = TRUE,
                   nest_data =F)

# add in team abbreviations
team_abbrv_df <- nba_teams() %>% select(nameTeam, slugTeam)
  
# combine with our team_df
team_df2 <- team_df %>% left_join(team_abbrv_df %>% select(nameTeam, slugTeam) %>% unique())

# let's just look at season level stats
# we'll pull data across different stat types, which is what the tables option means
# this loads the dataframes into your environment automatically
bref_players_stats(seasons = c(2012:2023)
                                     , tables = c(
                                       "totals"
                                     ))


# create a dataframe that just notes who is a starter by each season
starter_df <- dataBREFPlayerTotals %>% rename(team = slugTeamBREF) %>% 
  # if players started more than half of the total games, we consider them a starter
  mutate(starter = ifelse(countGamesStarted> countGames*.5, 1, 0)) %>% 
  # might want to comment this out, but let's just keep this simple for now and only
  # select a few columns
  select(slugSeason, yearSeason, namePlayer, team, idPlayerNBA, minutesTotals, starter, ptsTotals)

# merge in team win percentage with out player level info
player_season_win_df <- dataBREFPlayerTotals %>% 
  left_join(starter_df) %>% 
  left_join(team_df2 %>% select(pctWins, wins, idTeam, "team" = slugTeam, slugSeason), by = c("team", "slugSeason"))

# pull game to game stats from NBA's API directly---------
# this should take anywhere from one to five minutes or so depending on 
# your internet connection and probably some other stuff like how many
# people are checking stats

# Function to retrieve player data for 2011 through 2023
# this is pretty much a basic api function that could be wrapped up into 
# something more sophisticated

# Set the headers
headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# Specify the seasons of interest
start_season <- 2011
end_season <- 2022

# create null object
df <- NULL

for (season in start_season:end_season) {
  season_string <- paste0(season, "-", (season + 1) %% 100)  # Convert season to "yyyy-yy" format
  url <- paste0("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Per100Possessions&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season="
  , season_string
  , "&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  res <- GET(url = url, add_headers(.headers=headers))
  
  json_res <- fromJSON(content(res, "text"))
  tmp_dat <- data.frame(json_res$resultSets$rowSet[[1]]) 
  
  names(tmp_dat) <- data.frame(json_res[["resultSets"]][["headers"]])$c..PLAYER_ID....PLAYER_NAME....NICKNAME....TEAM_ID....TEAM_ABBREVIATION...

  
  df[[season_string]] <- tmp_dat
  
}

# we have the season name as the name of each list
# let's create a function to bring the season into each data frame
named_df_list <- Map(function(df, name) {transform(df, season = name)}, df, names(df))

# combine all of our list objects into a data frame
advanced_player_df <- do.call(rbind, named_df_list) %>% 
  mutate(across(-c(2, 3, 5, 79), as.numeric)) %>% 
  left_join(starter_df)
  
