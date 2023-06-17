###############################################
# EDA understanding bench Performance
# Session Info:
# R version 4.2.1 (2022-06-23)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.3.1
###############################################

# Load packages
library(tidyverse) # the usual
library(readr) # fancy data load 
library(geomtextpath) # for labeled lines


# set seed
set.seed(5292023)

# read in data
player_team_df <- read_csv("03 Data/advanced player stats and team stats.csv", col_types = cols(...1 = col_skip(), X = col_skip()))

# a few things to look into:
# bench player wins vs. starting player wins
# starters vs. bench relationship
# trend of bench minutes/games overtime by team (maybe just focus on Wiz)


# take a look at bench vs starter net rating and win percentage
p1 <- player_team_df %>% 
  filter(is.na(starter)!=T) %>% 
  ggplot(aes(x = log(NET_RATING), y = pctWins)) +
  geom_point(aes(col = factor(starter)), alpha = 0.3, shape = 21) +
  geom_smooth(aes(col = factor(starter)), method = "lm", se = F) +
  facet_wrap(~season) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_classic() + 
  theme(legend.position = "NA"
        , legend.title = element_blank()) +
  labs(x = "Player Net Rating (log)", y = "Team Win %"
       , title = "Starting and Bench Player Net Rating and\nOverall Team Win Percentage, 2011-23"
       , subtitle = "Bench players are in red, starters are in blue"
       , caption = "data: basketball-reference.com, nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("02 Output/Starting vs bench shooting over time.png", p1, w = 14, h = 12, dpi = 300)

# let's look at average player win percentage for bench players by year
p2 <- player_team_df %>% 
  filter(starter == 0) %>%
  mutate(wiz = ifelse(TEAM_ABBREVIATION == "WAS", "Wizards", "All Other Teams")) %>% 
  summarize(mean_win_pct = mean(W_PCT, na.rm=T)
            , .by = c(yearSeason, wiz)) %>% 
  ggplot(aes(x = yearSeason, y = mean_win_pct)) +
  geom_textline(aes(col = wiz, label = wiz), hjust = 0.08, lwd = 2, size = 6) +
  scale_color_manual(values = c("#E41134", "#6C6463")) +
  scale_x_continuous(breaks = c(seq(2012, 2022, 2))) +
  theme_classic() + 
  theme(legend.position = "NA") +
  labs(x = "", y = "Win %"
       , title = "Average Per Season Bench Player Win Percentage Over the Past Decade"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) 

ggsave("02 Output/average per season bench player win pct.png", p2, w = 14, h = 12, dpi = 300)



# let's start a little more basic with the distribution of offensive and defensive rating
p3 <- player_team_df %>% 
  filter(is.na(starter)!=T
         & GP>=10 # trim the tails a bit by limiting this to players with 10 or more games in a season
  ) %>% 
  mutate(starter = ifelse(starter == 1, "Starter", "Bench Player")) %>% 
  select(season, team, PLAYER_ID, starter, "Offensive Rating" = OFF_RATING, "Defensive Rating" = DEF_RATING) %>% 
  pivot_longer(cols = c(5:6)) %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(fill = starter), alpha = 0.6) +
  scale_fill_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.title = element_blank()
        , legend.position = "top") +
  facet_wrap(~name) +
  labs(title = "Starting and Bench Player Offense and\nDefensive Rating Distribution, 2011-23"
       , caption = "data: basketball-reference.com, nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("02 Output/distribution of off and def ratings.png", p3, w = 14, h = 12, dpi = 300)

# defense is pretty similar, but offensive seems different
player_team_df %>% 
  filter(is.na(starter)!=T
         & GP>=10 # trim the tails a bit by limiting this to players with 10 or more games in a season
  ) %>% 
  mutate(starter = ifelse(starter == 1, "Starter", "Bench Player")) %>% 
  select(season, team, PLAYER_ID, starter, "Offensive Rating" = OFF_RATING, "Defensive Rating" = DEF_RATING) %>% 
  pivot_longer(cols = c(5:6)) %>% 
  group_by(name, starter) %>% 
  summarize(mean = mean(value, na.rm=T))

# just check with a t-test
# offense
t.test(player_team_df$OFF_RATING[player_team_df$GP>=10 & is.na(player_team_df$starter)!=T] ~ player_team_df$starter[player_team_df$GP>=10 & is.na(player_team_df$starter)!=T] )

# defense
t.test(player_team_df$DEF_RATING[player_team_df$GP>=10 & is.na(player_team_df$starter)!=T] ~ player_team_df$starter[player_team_df$GP>=10 & is.na(player_team_df$starter)!=T] )

# overall net rating
t.test(player_team_df$NET_RATING[player_team_df$GP>=10 & is.na(player_team_df$starter)!=T] ~ player_team_df$starter[player_team_df$GP>=10 & is.na(player_team_df$starter)!=T] )

# let's look at the distribution by season
p4 <- player_team_df %>% 
  filter(is.na(starter)!=T
         & GP>=10 # trim the tails a bit by limiting this to players with 10 or more games in a season
  ) %>% 
  mutate(starter = ifelse(starter == 1, "Starter", "Bench Player")) %>% 
  select(season, team, PLAYER_ID, starter, "Offensive Rating" = OFF_RATING, "Defensive Rating" = DEF_RATING) %>% 
  pivot_longer(cols = c(5:6)) %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(fill = starter), alpha = 0.6) +
  scale_fill_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.title = element_blank()
        , legend.position = "top") +
  facet_wrap(~name+season) +
  labs(title = "Starting and Bench Player Offense and\nDefensive Rating Distribution by Season"
       , caption = "data: basketball-reference.com, nba.com/stats\nwizardspoints.substack.com"
  )

ggsave("02 Output/distribution of off and def ratings by season.png", p4, w = 14, h = 12, dpi = 300)

# let's take a look at the gap between starters and bench players this season
p5 <-
  player_team_df %>%
  filter(season == "2022-23" & is.na(starter)!=T & GP>=10) %>% 
  select(teamName, NET_RATING, starter, MIN) %>% 
  mutate(starter = ifelse(starter == 1, "Starters", "Bench Players")) %>% 
  summarize("Net Rating" = mean(NET_RATING)
            , "Minutes" = mean(MIN), .by = c(teamName, starter)) %>% 
  ggplot(aes(x = `Net Rating`, y = teamName)) +
  ggforce::geom_link(data =
                 player_team_df %>%
                 filter(season == "2022-23" & is.na(starter)!=T & GP>=10) %>%
                 select(teamName, NET_RATING, starter, MIN) %>%
                 mutate(starter = ifelse(starter == 1, "Starters", "Bench Players")) %>%
                 summarize("Net Rating" = mean(NET_RATING)
                           ,  .by = c(teamName, starter)) %>% pivot_wider(names_from = starter, values_from = `Net Rating`)
                 , aes(x = `Bench Players`, xend = Starters, y = reorder(teamName, Starters), yend = reorder(teamName, Starters)
                       , alpha = stat(index)
                       )
               , color = "gray70"
  ) +
  geom_point(aes(color = starter, size = Minutes)) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.position = "NA") +
  labs(y = "", title = "Starting and Bench Player Net Rating by Team for the 2022-23 Season"
       , subtitle = "Only players with 10 or more games played considered\ndots are sized by average minutes"
       , caption = "data: basketball-reference.com, nba.com/stats\nwizardspoints.substack.com"
  )

p5
ggsave("02 Output/net rating gap for 2022-23.png", p5, w = 16, h = 12, dpi = 300)

# let's look at the same figure but just for the Wizards
p6 <- 
  player_team_df %>%
  filter(teamName == "Wizards" & is.na(starter)!=T & GP>=10) %>% 
  select(season, NET_RATING, starter, MIN) %>% 
  mutate(starter = ifelse(starter == 1, "Starters", "Bench Players")) %>% 
  summarize("Net Rating" = mean(NET_RATING)
            , "Minutes" = mean(MIN), .by = c(season, starter)) %>% 
  ggplot(aes(x = `Net Rating`, y = fct_rev(season))) +
  ggforce::geom_link(data =
                       player_team_df %>%
                       filter(teamName == "Wizards" & is.na(starter)!=T & GP>=10) %>% 
                       select(season, NET_RATING, starter, MIN) %>%
                       mutate(starter = ifelse(starter == 1, "Starters", "Bench Players")) %>%
                       summarize("Net Rating" = mean(NET_RATING)
                                 ,  .by = c(season, starter)) %>% pivot_wider(names_from = starter, values_from = `Net Rating`)
                     , aes(x = `Bench Players`, xend = Starters, y = fct_rev(season), yend = fct_rev(season)
                           , alpha = stat(index)
                     ),
                      color = "gray70"
  ) +
  geom_point(aes(color = starter, size = Minutes)) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.position = "NA") +
  labs(y = "", title = "Starting and Bench Player Net Rating by Season for the Wizards"
       , subtitle = "Only players with 10 or more games played considered\ndots are sized by average minutes"
       , caption = "data: basketball-reference.com, nba.com/stats\nwizardspoints.substack.com"
  )

p6

ggsave("02 Output/net rating gap for the wizards.png", p6, w = 16, h = 12, dpi = 300)
