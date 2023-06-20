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
library(gt) # nice tables
library(lme4) # multi-level models
library(rstanarm)
library(performance)
library(tidymodels) # just to play around
tidymodels_prefer() # resolve conflicts

# set seed
set.seed(5292023)

# load data
df <- read_csv("03 Data/advanced player stats and team stats.csv", col_types = cols(...1 = col_skip(), X = col_skip()))

# to do:
# build out a very basic model and then go from there, but here is a starting point
# right now we have a multi-level structure to the data--multiple rows per team and season
# but it might be worth collapsing the data: the tradeoff is loss of variation but it will
# simplify things a big

df_wide <- df %>% 
  # drop our character variables
  select(TEAM_NAME, season, starter_char, NET_RATING, team_W_PCT) %>% 
  # collapse by team, season, and starter
  group_by(TEAM_NAME, season, starter_char) %>% 
  summarize(NET_RATING = mean(NET_RATING, na.rm=T)
            , team_W_PCT = mean(team_W_PCT, na.rm=T)) %>%
  pivot_wider(names_from = starter_char, values_from = NET_RATING) %>% 
  left_join(
    df %>% 
      # drop our character variables
      select(TEAM_NAME, season, starter, NET_RATING, MIN, team_W_PCT) %>% 
      group_by(TEAM_NAME, season, starter) %>% 
      summarize(total_bench_minutes = sum(MIN, na.rm=T)
                ) %>% 
      filter(starter == 0) %>% select(-starter)
  )

df_wide %>% 
  ggplot(aes(mean_bench_minutes, team_W_PCT)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor(fun = "pearson")

m1 <- lm(team_W_PCT ~ 
           Bench 
         + Starter
         +  total_bench_minutes
         , data = df_wide)


summary(m1)
performance(m1)
check_model(m1)