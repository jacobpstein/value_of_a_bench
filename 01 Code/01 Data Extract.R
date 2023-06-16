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
library(rstanarm) # Bayesian models
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
team_df2 <- team_df %>% 
  left_join(team_abbrv_df %>% select(nameTeam, slugTeam) %>% unique()) 

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

# pull game to game stats from NBA's API directly---------
# this should take anywhere from one to five minutes or so depending on 
# your internet connection and probably some other stuff like how many
# people are checking stats

# Function to retrieve player data for 2011 through 2023
# this is pretty much a basic api function that could be wrapped up into 
# something more sophisticated

# Set the headers
headers <- c(
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
  
  # create a url object, this can be updated depending on the NBA end point we want
  url <- paste0("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Per100Possessions&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season="
  , season_string
  , "&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  # query the site
  res <- GET(url = url, add_headers(.headers=headers))
  
  # convert to json
  json_res <- fromJSON(content(res, "text"))
  
  # convert to a dataframe
  tmp_dat <- data.frame(json_res$resultSets$rowSet[[1]]) 
  
  # the json file contains multiple objects with headers and values split, add the headers back in
  names(tmp_dat) <- data.frame(json_res[["resultSets"]][["headers"]])$c..PLAYER_ID....PLAYER_NAME....NICKNAME....TEAM_ID....TEAM_ABBREVIATION...

  # toss all of our data frames for each year into a list
  df[[season_string]] <- tmp_dat
  
}

# we have the season name as the name of each list
# let's create a function to bring the season into each data frame
named_df_list <- Map(function(df, name) {transform(df, season = name)}, df, names(df))

# combine all of our list objects into a data frame
# this is more or less what I imagine we'll work with for analysis
player_team_df <- do.call(rbind, named_df_list) %>% 
  mutate(across(-c(2, 3, 5, 79), as.numeric)) %>% 
  left_join(starter_df, by = c("season" = "slugSeason"
                               , "PLAYER_ID" = "idPlayerNBA"
                               )) %>% 
  # add in our team totals
  left_join(team_df2, by = c("TEAM_ID" = "idTeam",  "season" = "slugSeason"))

write.csv(player_team_df, "03 Data/advanced player stats and team stats.csv")


# to pull team stats from NBA run the code below:

# create null object
df_team <- NULL

for (season in start_season:end_season) {
  season_string <- paste0(season, "-", (season + 1) %% 100)  # Convert season to "yyyy-yy" format
  
  # create a url object, this can be updated depending on the NBA end point we want
  url <- paste0("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season="
                , season_string
                , "&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")
  # query the site
  res <- GET(url = url, add_headers(.headers=headers))
  
  # convert to json
  json_res <- fromJSON(content(res, "text"))
  
  # convert to a dataframe
  tmp_dat <- data.frame(json_res$resultSets$rowSet[[1]]) 
  
  # the json file contains multiple objects with headers and values split, add the headers back in
  names(tmp_dat) <- data.frame(json_res[["resultSets"]][["headers"]])$c..TEAM_ID....TEAM_NAME....GP....W....L....W_PCT....MIN....FGM...
  
  # toss all of our data frames for each year into a list
  df_team[[season_string]] <- tmp_dat
  
}


# we have the season name as the name of each list
# let's create a function to bring the season into each data frame
named_df_team_list <- Map(function(df_team, name) {transform(df_team, season = name)}, df_team, names(df_team))

# combine all of our list objects into a data frame
# this is more or less what I imagine we'll work with for analysis
team_df <- do.call(rbind, named_df_team_list) %>% 
  mutate(across(-c(2, 55), as.numeric)) 
  