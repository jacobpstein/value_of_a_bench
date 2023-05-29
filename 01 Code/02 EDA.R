###############################################
# EDA understanding bench Performance
# Session Info:
# R version 4.2.1 (2022-06-23)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.3.1
###############################################

# Load packages
library(tidyverse) # the usual

# set seed
set.seed(5292023)

# read in data
player_team_df <- read.csv("03 Output\advanced player stats and team stats.csv")

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
