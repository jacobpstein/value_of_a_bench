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

# combine all of our list objects into a data frame
advanced_player_df <- bind_rows(df)


# visual inspection-----
# probably should move this to its own file
p1 <- player_season_win_df %>% 
  # just look at players that were in at least 30 games
  filter(countGames>=30) %>% 
  mutate(start_bench = ifelse(starter == 1, "Starters", "Bench")
  ) %>% 
  ggplot(aes(x = pctEFG, y = pctWins)) +
  geom_hline(aes(yintercept = mean(pctWins, na.rm=T)), linetype = 2, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(pctEFG, na.rm=T)), linetype = 2, alpha = 0.5) +
  geom_point(aes(col = start_bench), shape = 21, size = 2, stroke = 2, alpha = 0.8, fill = "white") +
  geom_smooth(aes(col = start_bench), method = "lm", se = F) +
  scale_fill_manual(values = c("#E41134", "#00265B")) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.position = "top") +
  ggpubr::stat_cor(method = "pearson") +
  facet_wrap(~yearSeason) +
  labs(x = "Effective Shooting %", y = "Win Percentage"
       , title = "Relationship between True Shooting and Win Percentage for Bench and Starting Players"
       , caption = "data: basketball-reference.com\nwizardspoints.substack.com"
  )

p1

# for each season let's define bench and non-bench points
p2 <- player_season_win_df %>% 
  filter(is.na(wins)!=T) %>% 
  group_by(yearSeason, team, starter, wins) %>% 
  summarize(points = sum(ptsTotals, na.rm=T)) %>% 
  pivot_wider(names_from = "starter", values_from = "points") %>% 
  rename("bench" = `0`
         , "starters" = `1`) %>% 
  mutate(wiz = ifelse(team == "WAS", "Wizards", "Other")
  ) %>% 
  ggplot(aes(x = starters, y = bench)) +
  geom_hline(aes(yintercept = mean(bench, na.rm=T)), linetype = 2, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(starters, na.rm=T)), linetype = 2, alpha = 0.5) +
  geom_point(aes(col = wiz), shape = 21, size = 2, stroke = 2, alpha = 0.8, fill = "white") +
  geom_smooth(method = "lm", se = F, color = "grey") +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.position = "NA") +
  ggpubr::stat_cor(method = "pearson") +
  facet_wrap(~yearSeason) +
  labs(x = "Total Starting Player Points", y = "Total Bench Player Points"
       , title = "Relationship between Bench and Starting Players Total Points"
       , caption = "data: basketball-reference.com\nwizardspoints.substack.com"
  )

ggsave("02 Output/Starting vs bench shooting.png", p2, w = 14, h = 12, dpi = 300)

# let's look at bench points by winning and losing team
p3 <- starter_df %>% 
  select(yearSeason, starter, ptsTotals) %>% 
  group_by(yearSeason, starter) %>% 
  summarize(points = sum(ptsTotals, na.rm=T)) %>% 
  mutate(starter = ifelse(starter == 0, "Bench Player", "Starting Player")) %>% 
  ggplot(aes(x = yearSeason, y = points)) +
  geom_area(aes(fill = starter), position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#E41134", "#00265B")) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_classic() + 
  theme(legend.position = "top"
        , legend.title = element_blank()) +
  labs(x = "Season", y = "Total Points"
       , title = "Starting and Bench Player Points\nAcross All Teams Combined, 2012-22"
       , caption = "data: basketball-reference.com\nwizardspoints.substack.com"
  )

ggsave("02 Output/Starting vs bench shooting over time.png", p3, w = 14, h = 12, dpi = 300)
