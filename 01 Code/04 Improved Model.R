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
library(rstanarm) # bayesian models
library(performance) # model assessment
library(sjPlot) # model plots
library(caret) # for data splitting and training
library(rpart) # CART
library(rpart.plot) # plotting cart models
library(arm) # scaling the data

# set seed
set.seed(5292023)

# load data---

# first let's load in just our NBA data
df <- read_csv("03 Data/advanced player stats and team stats.csv", col_types = cols(...1 = col_skip(), X = col_skip())) %>% 
  mutate(bench = ifelse(starter == 0, 1, 0))

# we'll see how model results compare when using 538's raptor metric
df_538 <- read_csv("03 Data/player and team stats with 538 data.csv", col_types = cols(...1 = col_skip())) %>% 
  mutate(bench = ifelse(starter == 0, 1, 0))

# to do:
# build out a very basic model and then go from there, but here is a starting point
# right now we have a multi-level structure to the data--multiple rows per team and season
# but it might be worth collapsing the data: the tradeoff is loss of variation but it will
# simplify things a big

df_wide <- df %>% 
  # drop our character variables
  dplyr::select(TEAM_NAME, season, starter_char, NET_RATING, team_W_PCT, team_W) %>% 
  # collapse by team, season, and starter
  group_by(TEAM_NAME, season, starter_char) %>% 
  summarize(NET_RATING = mean(NET_RATING, na.rm=T)
            , team_W_PCT = mean(team_W_PCT, na.rm=T)
            , team_W = mean(team_W, na.rm=T)) %>%
  pivot_wider(names_from = starter_char, values_from = NET_RATING) %>% 
  left_join(
    df %>% 
      # drop our character variables
      dplyr::select(TEAM_NAME, season, starter, NET_RATING, MIN, team_W_PCT) %>% 
      group_by(TEAM_NAME, season, starter) %>% 
      summarize(total_bench_minutes = sum(MIN, na.rm=T)
                ) %>% 
      filter(starter == 0) %>% 
      dplyr::select(-starter)
  )

df_wide %>% 
  ggplot(aes(total_bench_minutes, team_W_PCT)) +
  geom_point(aes(col = TEAM_NAME), shape =21) +
  geom_smooth(method = "lm", se = F) +
  ggpubr::stat_cor(fun = "pearson") +
  theme_classic() + 
  theme(legend.position = "NA"
        , legend.title = element_blank()
        , text = element_text(size = 22)
  ) +
  labs(x = "Total Bench Minutes", y = "Team Win %"
       , title = "Bench Minutes and\nOverall Team Win Percentage, 2011-23"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) + facet_wrap(~TEAM_NAME)

m1 <- lm(team_W_PCT ~ 
           Bench 
         + Starter
         +  total_bench_minutes
         , data = df_wide)

standardize(m1) # scaled results
summary(standardize(m1))
performance(standardize(m1))
check_model(standardize(m1))

# 538 model----

df_wide_538 <- df_538 %>% 
  filter(is.na(starter_char)!=T) %>% 
  # drop our character variables
  dplyr::select(team_name, season, starter_char, predator_total, team_w_pct) %>% 
  # collapse by team, season, and starter
  group_by(team_name, season, starter_char) %>% 
  summarize(predator = mean(predator_total, na.rm=T)
            , team_w_pct = mean(team_w_pct, na.rm=T)
            ) %>%
  pivot_wider(names_from = starter_char, values_from = predator) %>% 
  left_join(
    df_538 %>% 
      # drop our character variables
      dplyr::select(team_name, season, starter, predator_total, mp, team_w_pct) %>% 
      group_by(team_name, season, starter) %>% 
      summarize(total_bench_minutes = sum(mp, na.rm=T)
      ) %>% 
      filter(starter == 0) %>% dplyr::select(-starter)
  )

df_wide_538 %>% 
  ggplot(aes(total_bench_minutes, team_w_pct)) +
  geom_point(aes(col = team_name), shape =21) +
  geom_smooth(method = "lm", se = F) +
  ggpubr::stat_cor(fun = "pearson") +
  theme_classic() + 
  theme(legend.position = "NA"
        , legend.title = element_blank()
        , text = element_text(size = 22)
  ) +
  labs(x = "Total Bench Minutes", y = "Team Win %"
       , title = "Bench Minutes and\nOverall Team Win Percentage, 2011-23"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) + facet_wrap(~team_name)

m2 <- lm(team_w_pct ~ 
           Bench 
         + Starter
         +  total_bench_minutes
         , data = df_wide_538)


summary(m2)
performance(m2)
check_model(m2)


# interaction model using 538 data----
m3 <- lm(team_w_pct ~ 
           predator_total
         + mp*starter_char
         , data = df_538)


summary(m3)
performance(m3)
check_model(m3)

# visualize the predicted interaction effect
p1 <- plot_model(m3, type = "int") + theme_538() +
  labs(x = "Minutes Played"
       , y = "Team Win %"
       , title = "Predicted Team Win Percentage by Player Status"
       , caption = "data: fivethirtyeight.com and nba.com/stats\nwizardspoints.substack.com"
  ) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "top"
        , legend.title = element_blank()
        , text = element_text(size = 22))

p1

ggsave("02 Output/interaction model results.png", p1, w = 16, h = 12, dpi = 300)


# interaction model using 538 data for the wizards----
m3_wiz <- lm(team_w_pct ~ 
           predator_total
         + mp*starter_char
         , data = df_538[df_538$team_name=="Washington Wizards",])


summary(m3_wiz)
performance(m3)
check_model(m3)

# visualize the predicted interaction effect
p1_wiz <- plot_model(m3_wiz, type = "int") + theme_538() +
  labs(x = "Minutes Played"
       , y = "Team Win %"
       , title = "Predicted Team Win Percentage by Player Status for the Washington Wizards"
       , caption = "data: fivethirtyeight.com and nba.com/stats\nwizardspoints.substack.com"
  ) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "top"
        , legend.title = element_blank()
        , text = element_text(size = 22))

p1_wiz

ggsave("02 Output/interaction model results for the Wizards.png", p1_wiz, w = 16, h = 12, dpi = 300)

# regression tree----
m2 <- rpart(
  formula = team_W_PCT ~ 
    Bench 
  + Starter
  +  total_bench_minutes,
  data    = df_wide,
)


rpart.plot(m2)