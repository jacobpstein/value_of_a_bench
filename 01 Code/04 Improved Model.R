###############################################
# Improve linear models to estimate bench Performance
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
library(broom) # clean up
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
  mutate(bench = ifelse(starter == 0, 1, 0)) %>% 
  janitor::clean_names()

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
  dplyr::select(team_name, season, starter_char, net_rating, team_w_pct) %>% 
  # collapse by team, season, and starter
  group_by(team_name, season, starter_char) %>% 
  summarize(net_rating = mean(net_rating, na.rm=T)
            , team_w_pct = mean(team_w_pct, na.rm=T)
  ) %>%
  ungroup() %>% 
  mutate_at(.vars = c("net_rating",  "team_w_pct"), .funs = arm::rescale) %>% 
  pivot_wider(names_from = starter_char, values_from = net_rating) %>% 
  left_join(
    df %>% 
      # drop our character variables
      dplyr::select(team_name, season, starter, net_rating, min, team_w_pct) %>% 
      group_by(team_name, season, starter) %>% 
      summarize(total_bench_minutes = sum(min, na.rm=T)
      ) %>% 
      filter(starter == 0) %>% 
      dplyr::select(-starter) %>% 
      ungroup() %>% 
      mutate(total_bench_minutes = arm::rescale(total_bench_minutes))
  )

df_wide %>% 
  ggplot(aes(total_bench_minutes, team_w_pct)) +
  geom_point(aes(col = team_name), shape =21) +
  geom_smooth(method = "lm", se = F) +
  ggpubr::stat_cor(fun = "pearson") +
  theme_classic() + 
  theme(legend.position = "NA"
        , legend.title = element_blank()
        , text = element_text(size = 22)
  ) +
  labs(x = "Total Bench minutes", y = "Team Win %"
       , title = "Bench minutes and\nOverall Team Win Percentage, 2011-23"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) + facet_wrap(~team_name)

m1 <- lm(team_w_pct ~ 
           Bench 
         + Starter
         +  total_bench_minutes
         , data = df_wide)

summary((m1))
performance((m1))
check_model((m1))


# model just for wiz----
m1_wiz <- lm(team_w_pct ~ 
           Bench 
         + Starter
         +  total_bench_minutes
         , data = df_wide[df_wide$team_name=="Washington Wizards",])

summary(m1_wiz)
performance(m1_wiz)
check_model(m1_wiz)
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
  labs(x = "Total Bench minutes", y = "Team Win %"
       , title = "Bench minutes and\nOverall Team Win Percentage, 2011-23"
       , caption = "data: nba.com/stats\nwizardspoints.substack.com"
  ) + facet_wrap(~team_name)

m2 <- lm(team_w_pct ~ 
           Bench 
         + Starter
         +  total_bench_minutes
         , data = df_wide_538)


summary(standardize(m2))
performance(standardize(m2))
check_model(standardize(m2))


# interaction model using 538 data----
m3 <- lm(team_w_pct ~ 
           predator_total
         + mp*starter_char
         , data = df_538)


summary(standardize(m3))
performance(standardize(m3))
check_model(standardize(m3))

# visualize the predicted interaction effect
p1 <- plot_model(m3, type = "int") + theme_538() +
  labs(x = "minutes Played"
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
         + mp*starter
         , data = df_538[df_538$team_name=="Washington Wizards",])


summary(m3_wiz)
performance(m3)
check_model(m3)

# visualize the predicted interaction effect
p1_wiz <- plot_model(m3_wiz, type = "int") + theme_538() +
  labs(x = "minutes Played"
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

# model by team----
m_group <- df %>% 
  janitor::clean_names() %>% 
  ungroup() %>% 
  nest_by(team_name) %>%
  mutate(fit_win_pct = list(lm(team_w_pct ~ 
                                 net_rating
                               + min*starter, data = data))) %>%
  summarise(tidy(fit_win_pct))

m_group %>% 
  filter(term == "min:starter") %>% 
  arrange(desc(estimate))
  

# regression tree----
m2 <- rpart(
  formula = team_w_pct ~ 
    Bench 
  + Starter
  +  total_bench_minutes,
  data    = df_wide,
)


rpart.plot(m2)