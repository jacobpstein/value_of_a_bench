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
library(hrbrthemes) # neat ggplot themes
library(sjPlot) # 538 theme
library(geomtextpath) # for labeled lines


# set seed
set.seed(5292023)

# read in data
player_team_df <- read_csv("03 Data/advanced player stats and team stats.csv", col_types = cols(...1 = col_skip()))

# import 538 data
df_538 <- read_csv("03 Data/player and team stats with 538 data.csv", col_types = cols(...1 = col_skip())) %>% 
  mutate(bench = ifelse(starter == 0, 1, 0))

# a few things to look into:
# bench player wins vs. starting player wins
# starters vs. bench relationship
# trend of bench minutes/games overtime by team (maybe just focus on Wiz)


# take a look at bench vs starter net rating and win percentage
p1 <- player_team_df %>% 
  filter(is.na(starter)!=T) %>% 
  ggplot(aes(x = log(net_rating), y = team_w_pct)) +
  geom_point(aes(col = factor(starter)), alpha = 0.3, shape = 21) +
  geom_smooth(aes(col = factor(starter)), method = "lm", se = F) +
  facet_wrap(~season) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.position = "NA"
        , legend.title = element_blank()
        , text = element_text(size = 22)
        ) +
  labs(x = "Player Net Rating (log)", y = "Team Win %"
       , title = "Starting and Bench Player Net Rating and\nOverall Team Win Percentage, 2011-23"
       , subtitle = "Bench players are in red, starters are in blue"
       , caption = "All data are per 100 possessions\ndata: nba.com/stats\nwizardspoints.substack.com"
  )

p1

ggsave("02 Output/Starting vs bench shooting over time.png", p1, w = 14, h = 12, dpi = 300)

# let's look at average player win percentage for bench players by year
p2 <- player_team_df %>% 
  filter(starter == 0) %>%
  mutate(wiz = ifelse(team_abbreviation == "WAS", "Wizards", "All Other Teams")
         , season = as.numeric(substr(season, 1, 4))) %>% 
  summarize(mean_win_pct = mean(w_pct, na.rm=T)
            , .by = c(season, wiz)) %>% 
  ggplot(aes(x = season, y = mean_win_pct)) +
  geom_textline(aes(col = wiz, label = wiz), hjust = 0.08, lwd = 2, size = 6) +
  scale_color_manual(values = c("#E41134", "#6C6463")) +
  scale_x_continuous(breaks = c(seq(2012, 2022, 2))) +
  theme_classic() + 
  theme(legend.position = "NA"
        , text = element_text(size = 22)
        ) +
  labs(x = "", y = "Win %"
       , title = "Average Per Season Bench Player Win Percentage Over the Past Decade"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) 

p2

ggsave("02 Output/average per season bench player win pct.png", p2, w = 14, h = 12, dpi = 300)



# let's start a little more basic with the distribution of offensive and defensive rating
p3 <- player_team_df %>% 
  filter(is.na(starter)!=T
         & gp>=10 # trim the tails a bit by limiting this to players with 10 or more games in a season
  ) %>% 
  mutate(starter = ifelse(starter == 1, "Starter", "Bench Player")) %>% 
  select(season, team_name, player_id, starter, "Offensive Rating" = off_rating, "Defensive Rating" = def_rating) %>% 
  pivot_longer(cols = c(5:6)) %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(fill = starter), alpha = 0.6) +
  scale_fill_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.title = element_blank()
        , legend.position = "top"
        , text = element_text(size = 22)
        ) +
  facet_wrap(~name) +
  labs(title = "Starting and Bench Player Offense and\nDefensive Rating Distribution, 2011-23"
       , caption = "All data are per 100 posessions\ndata: basketball-reference.com, nba.com/stats\nwizardspoints.substack.com"
  )

p3

ggsave("02 Output/distribution of off and def ratings.png", p3, w = 14, h = 12, dpi = 300)

# defense is pretty similar, but offensive seems different
player_team_df %>% 
  filter(is.na(starter)!=T
         & gp>=10 # trim the tails a bit by limiting this to players with 10 or more games in a season
  ) %>% 
  mutate(starter = ifelse(starter == 1, "Starter", "Bench Player")) %>% 
  select(season, team_name, player_id, starter, "Offensive Rating" = off_rating, "Defensive Rating" = def_rating) %>% 
  pivot_longer(cols = c(5:6)) %>% 
  group_by(name, starter) %>% 
  summarize(mean = mean(value, na.rm=T))

# just check with a t-test
# offense
t.test(player_team_df$off_rating[player_team_df$gp>=10 & is.na(player_team_df$starter)!=T] ~ player_team_df$starter[player_team_df$gp>=10 & is.na(player_team_df$starter)!=T] )

# defense
t.test(player_team_df$def_rating[player_team_df$gp>=10 & is.na(player_team_df$starter)!=T] ~ player_team_df$starter[player_team_df$gp>=10 & is.na(player_team_df$starter)!=T] )

# overall net rating
t.test(player_team_df$net_rating[player_team_df$gp>=10 & is.na(player_team_df$starter)!=T] ~ player_team_df$starter[player_team_df$gp>=10 & is.na(player_team_df$starter)!=T] )

# let's look at the distribution by season
p4 <- player_team_df %>% 
  filter(is.na(starter)!=T
         & gp>=10 # trim the tails a bit by limiting this to players with 10 or more games in a season
  ) %>% 
  mutate(starter = ifelse(starter == 1, "Starter", "Bench Player")) %>% 
  select(season, team_name, player_id, starter, "Offensive Rating" = off_rating, "Defensive Rating" = def_rating) %>% 
  pivot_longer(cols = c(5:6)) %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(fill = starter), alpha = 0.6) +
  scale_fill_manual(values = c("#E41134", "#00265B")) +
  theme_classic() + 
  theme(legend.title = element_blank()
        , legend.position = "top"
        , text = element_text(size = 22)
        ) +
  facet_wrap(~name+season) +
  labs(title = "Starting and Bench Player Offense and\nDefensive Rating Distribution by Season"
       , caption = "Data are per 100 posessions\ndata: basketball-reference.com, nba.com/stats\nwizardspoints.substack.com"
  )

p4

ggsave("02 Output/distribution of off and def ratings by season.png", p4, w = 20, h = 14, dpi = 300)

# let's take a look at the gap between starters and bench players this season
p5 <-
  player_team_df %>%
  filter(season == "2022-23" & is.na(starter)!=T & gp>=10) %>% 
  select(team_name, net_rating, starter, min) %>% 
  mutate(starter = ifelse(starter == 1, "Starters", "Bench Players")) %>% 
  summarize("Net Rating" = mean(net_rating)
            , "minutes" = mean(min), .by = c(team_name, starter)) %>% 
  ggplot(aes(x = `Net Rating`, y = team_name)) +
  ggforce::geom_link(data =
                 player_team_df %>%
                 filter(season == "2022-23" & is.na(starter)!=T & gp>=10) %>%
                 select(team_name, net_rating, starter, min) %>%
                 mutate(starter = ifelse(starter == 1, "Starters", "Bench Players")) %>%
                 summarize("Net Rating" = mean(net_rating)
                           ,  .by = c(team_name, starter)) %>% pivot_wider(names_from = starter, values_from = `Net Rating`)
                 , aes(x = `Bench Players`, xend = Starters, y = reorder(team_name, Starters), yend = reorder(team_name, Starters)
                       , alpha = stat(index)
                       )
               , color = "gray70", show.legend = FALSE
  ) +
  geom_point(aes(color = starter, size = minutes)) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  scale_size_continuous(range = c(1, 10), guide = FALSE) +
  theme_classic() + 
  theme(legend.position = "top"
        , legend.title = element_blank()
        , text = element_text(size = 24)
        , panel.grid.major.x = element_line()
        
        ) +
  labs(y = "", title = "Average Starting and Bench Player Net Rating by Team for the 2022-23 Season"
       , subtitle = "Only players with 10 or more games played considered\nDots are sized by average minutes"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) + guides(colour = guide_legend(override.aes = list(size=5)))

p5
ggsave("02 Output/net rating gap for 2022-23.png", p5, w = 18, h = 12, dpi = 300)

# let's look at the same figure but just for the Wizards
p6 <- 
  player_team_df %>%
  filter(team_name == "Washington Wizards" & is.na(starter)!=T & gp>=10) %>% 
  select(season, net_rating, starter, min) %>% 
  mutate(starter = ifelse(starter == 1, "Starters", "Bench Players")) %>% 
  summarize("Net Rating" = mean(net_rating)
            , "minutes" = mean(min), .by = c(season, starter)) %>% 
  ggplot(aes(x = `Net Rating`, y = fct_rev(season))) +
  ggforce::geom_link(data =
                       player_team_df %>%
                       filter(team_name == "Washington Wizards" & is.na(starter)!=T & gp>=10) %>% 
                       select(season, net_rating, starter, min) %>%
                       mutate(starter = ifelse(starter == 1, "Starters", "Bench Players")) %>%
                       summarize("Net Rating" = mean(net_rating)
                                 ,  .by = c(season, starter)) %>% pivot_wider(names_from = starter, values_from = `Net Rating`)
                     , aes(x = `Bench Players`, xend = Starters, y = fct_rev(season), yend = fct_rev(season)
                           , alpha = stat(index)
                     ),
                      color = "gray70", show.legend = FALSE
  ) +
  geom_point(aes(color = starter, size = minutes)) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  scale_size_continuous(range = c(1, 12), guide = FALSE) +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
  theme_classic() + 
  theme(legend.position = "top"
        , text = element_text(size = 24)
        , legend.title = element_blank()
        , panel.grid.major.x = element_line()
  ) +
  labs(y = "", title = "Average Starting and Bench Player Net Rating by Season for the Wizards"
       , subtitle = "Only players with 10 or more games played considered\nDots are sized by average minutes"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) + guides(colour = guide_legend(override.aes = list(size=5)))


p6

ggsave("02 Output/net rating gap for the wizards.png", p6, w = 18, h = 12, dpi = 300)


# let's look at the actual Wizards data
player_team_df %>%
  filter(team_name == "Washington Wizards" & season == "2022-23" & is.na(starter)!=T & gp>=10) %>% 
  select(player_name, net_rating, starter, min) %>% 
  arrange(desc(net_rating),desc(min))

# look at RAPTOR over time----
p7 <- df_538 %>% 
  filter(is.na(starter)!=T) %>%
  group_by(season, team_name, starter_char) %>% 
  summarize(raptor = mean(raptor_total)) %>% 
  ggplot(aes(x = season, y = raptor)) +
  geom_line(aes(col = starter_char), size = 2) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  theme_538() + 
  theme(legend.position = "top"
        , legend.title = element_blank()
        , text = element_text(size = 22)
  ) +
  labs(x = "", y = "Total Raptor", title = "Average Starting and Bench Player RAPTOR"
       , subtitle = "Breaking down the FiveThirtyEight data by bench and starters"
       , caption = "data: fivethirtyeight.com and nba.com/stats\nwizardspoints.substack.com"
  ) +
  facet_wrap(~team_name, drop = TRUE) 

p7

ggsave("02 Output/raptor by team.png", p7, w = 28, h = 12, dpi = 300)

p8 <- df_538 %>% 
  filter(is.na(starter)!=T) %>%
  group_by(season, team_name, starter_char) %>% 
  summarize(raptor = mean(net_rating_start_status)) %>% 
  ggplot(aes(x = season, y = raptor)) +
  geom_line(aes(col = starter_char), size = 2) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  theme_538() + 
  theme(legend.position = "top"
        , legend.title = element_blank()
        , text = element_text(size = 22)
  ) +
  labs(x = "", y = "Total Net Rating", title = "Average Starting and Bench Player Net Rating"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  facet_wrap(~team_name, drop = TRUE) 

p8

ggsave("02 Output/net rating by team.png", p8, w = 28, h = 12, dpi = 300)


# look at net rating over time----
p9 <- 
  player_team_df %>% 
  mutate(season = as.numeric(substr(season, 1, 4))) %>% 
  summarize(mean_win_pct = mean(net_rating, na.rm=T)
            , .by = c(season, team_name, starter_char)) %>% 
  ggplot(aes(x = season, y = mean_win_pct)) +
  geom_line(aes(col = starter_char), linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = 6) +
  scale_color_manual(values = c("#E41134", "#00265B")) +
  scale_x_continuous(breaks = c(seq(2012, 2022, 2)), labels = c(seq(12, 22, 2)) )+
  theme_classic() + 
  theme(legend.position = "top"
        , legend.title = element_blank()
        , text = element_text(size = 26)
  ) +
  labs(x = "", y = "Net Rating"
       , title = "Average Starter and Bench Net Rating Over the Past Decade"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) + facet_wrap(~team_name)


p9

ggsave("02 Output/net rating by team over time.png", p9, w = 24, h = 12, dpi = 300)

