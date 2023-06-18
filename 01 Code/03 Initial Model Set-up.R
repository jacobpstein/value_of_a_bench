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
library(lme4) # multi-level models
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

df_collapse <- df %>% 
  # drop our character variables
  select(-PLAYER_NAME, -team, -PLAYER_ID,-cityTeam, -modeSearch, -TEAM_ABBREVIATION,-NICKNAME, -namePlayer, -descriptionNBAFinalsAppearance, -isNBAChampion) %>% 
  # collapse by team, season, and starter
  group_by(teamName, nameTeam, season, yearSeason, starter) %>% 
  summarize_all(.funs = mean, na.rm=T) 

# let's just get a sense of the relationships in our data for starters
df_collapse %>% ungroup() %>% filter(starter == 1) %>% 
  select(-teamName, -nameTeam, -season, -contains("RANK")) %>% select(1:47) %>% select(-contains("E_"), -contains("sp_work"), -starter) %>% 
  cor() %>%
  as_tibble(rownames = "var") %>%
  mutate(across(-var, round, 4)) %>%
  gt(rowname_col = "var")

# and for the bench
df_collapse %>% ungroup() %>% filter(starter == 1) %>% 
  select(-teamName, -nameTeam, -season, -contains("RANK")) %>% select(1:47) %>% select(-contains("E_"), -contains("sp_work"), -starter) %>% 
  cor() %>%
  as_tibble(rownames = "var") %>%
  mutate(across(-var, round, 4)) %>%
  gt(rowname_col = "var")

# net rating, PIE, offensive rating, true shooting, fg pct, player win pct all stand out
# offsenive rating and net rating are redundant with each other, same with ts and ft pct

# train-test split
df_split <- initial_split(df_collapse, prop = 0.80, strata = yearSeason)
df_train <- training(df_split)
df_test  <-  testing(df_split)

# create some resampling folds
df_folds <- vfold_cv(df_train, strata = yearSeason)

# let's do two basic model engines to starts: linear and lasso, we can build this out
# to do some ensemble models
lm_model <- linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = pctWins, predictors = c(AGE
                                                  , GP
                                                  , NET_RATING
                                                  , TS_PCT
                                                  , W_PCT
                                                  , starter))

lm_fit <- fit(lm_wflow, df_train)

# resample
ctrl_preds <- control_resamples(save_pred = TRUE)

resample_basic <- fit_resamples(lm_wflow, df_folds, control = ctrl_preds)

collect_metrics(resample_basic) # ok


# Define the multi-level model
m1 <- lmer(pctWins ~ OFF_RATING + AGE + GP + MIN + USG_PCT + (1 | team) + (1 | starter), data = df)

# Check the summary of the model
summary(m1)

# Check the diagnostic plots
plot(m1)
