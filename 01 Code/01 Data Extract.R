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
team_abbrv_df <- nba_teams() %>% 
  
# combine with our team_df
team_df2 <- team_df %>% left_join(team_abbrv_df %>% select(nameTeam, slugTeam) %>% unique())

# let's just look at season level stats
# we'll pull data across different stat types, which is what the tables option means
# this loads the dataframes into your environment automatically
bref_players_stats(seasons = c(2012:2023)
                                     , tables = c("totals"
                                       , "advanced"
                                       # , "per_poss"
                                       , "per_game"
                                       # , "per_minute"
                                     ))


# create a dataframe that just notes who is a starter by each season
starter_df <- dataBREFPlayerTotals %>% rename(team = slugTeamBREF) %>% 
  # if players started more than half of the total games, we consider them a starter
  mutate(starter = ifelse(countGamesStarted> countGames*.5, 1, 0)) %>% 
  # might want to comment this out, but let's just keep this simple for now and only
  # select a few columns
  select(slugSeason, yearSeason, namePlayer, team, idPlayerNBA, minutesTotals, starter, ptsTotals)

# merge in team win percentage with out player level info
player_season_win_df <- dataBREFPlayerAdvanced %>% 
  left_join(starter_df) %>% 
  left_join(team_df2 %>% select(pctWins, wins, idTeam, "team" = slugTeam, slugSeason), by = c("team", "slugSeason"))

# visual inspection-----
player_season_win_df %>% 
  # just look at players that were in at least 30 games
  filter(countGames>=30) %>% 
  mutate(start_bench = ifelse(starter == 1, "Starters", "Bench")
  ) %>% 
  ggplot(aes(x = pctTrueShooting, y = pctWins)) +
  geom_hline(aes(yintercept = mean(pctWins, na.rm=T)), linetype = 2, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(pctTrueShooting, na.rm=T)), linetype = 2, alpha = 0.5) +
  geom_point(aes(col = start_bench), shape = 21, size = 2, stroke = 2, alpha = 0.8, fill = "white") +
  geom_smooth(aes(col = start_bench), method = "lm", se = F) +
  scale_fill_manual(values = c("#E41134", "#00265B")) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.position = "NA") +
  ggpubr::stat_cor(method = "pearson") +
  facet_wrap(~yearSeason)
  

# for each season let's define bench and non-bench points
player_season_win_df %>% 
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
  facet_wrap(~yearSeason)



