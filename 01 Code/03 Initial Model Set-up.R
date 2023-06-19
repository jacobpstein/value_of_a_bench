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
  select(where(is.numeric), TEAM_NAME, season, starter, everything()) %>% 
  # collapse by team, season, and starter
  group_by(TEAM_NAME, season, starter) %>% 
  summarize_all(.funs = mean, na.rm=T) %>% 
  ungroup()

# let's just get a sense of the relationships in our data for starters and win percentage
df_collapse %>% ungroup() %>% filter(starter == 1) %>% 
  select(where(is.numeric)) %>% 
  cor() %>%
  as_tibble(rownames = "var") %>%
  mutate(across(-var, round, 4)) %>%
  select(var, team_W_PCT) %>% 
  arrange(desc(team_W_PCT)) %>% 
  gt(rowname_col = "var")

# and for the bench
df_collapse %>% ungroup() %>% filter(starter == 1) %>% 
  select(where(is.numeric)) %>% 
  cor() %>%
  as_tibble(rownames = "var") %>%
  mutate(across(-var, round, 4)) %>%
  select(var, team_W_PCT) %>% 
  arrange(desc(team_W_PCT)) %>% 
  gt(rowname_col = "var")

# net rating, PIE, age, offensive rating, and player win pct all stand out
# offsenive rating and net rating are redundant with each other

# we see a similar trend if we keep the data as is, but the magnitudes are lower

# let's just get a sense of the relationships in our data for starters
df %>% ungroup() %>% filter(starter == 1) %>% 
  select(where(is.numeric)) %>% 
  cor() %>% as_tibble(rownames = "var") %>%
  mutate(across(-var, round, 4)) %>%
  select(var, team_W_PCT) %>% 
  arrange(desc(team_W_PCT)) %>% 
  gt(rowname_col = "var")

df %>% ungroup() %>% filter(starter == 0 & GP>=10) %>% 
  select(where(is.numeric)) %>% 
  cor() %>% as_tibble(rownames = "var") %>%
  mutate(across(-var, round, 4)) %>%
  select(var, team_W_PCT) %>% 
  arrange(desc(team_W_PCT)) %>% 
  gt(rowname_col = "var")

# train-test split
df_split <- initial_split(df_collapse, prop = 0.80, strata = season)
df_train <- training(df_split)
df_test  <-  testing(df_split)

# create some resampling folds
df_folds <- vfold_cv(df_train)

# let's do two basic model engines to starts: linear and lasso, we can build this out
# to do some ensemble models
lm_model <- linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = team_W_PCT, predictors = c(AGE
                                                  , GP
                                                  , NET_RATING
                                                  , PIE
                                                  , W_PCT
                                                  , starter
                                                  , MIN
                                                  # , teamName # team fixed effects
                                                  ))

lm_fit <- fit(lm_wflow, df_train)

# resample
ctrl_preds <- control_resamples(save_pred = TRUE)

resample_basic <- fit_resamples(lm_wflow, df_folds, control = ctrl_preds)

collect_metrics(resample_basic) # ok, this seems to fit the data pretty well

# let's also take a look at individual covariates
lm_fit %>% 
  extract_fit_engine() %>% 
  summary()

# now we should add some interaction terms to specifically account for 
# we need to add a new recipe
simple_df <- 
  recipe(team_W_PCT ~ AGE + NET_RATING + GP + TS_PCT + starter + W_PCT + MIN
         , data = df_train) %>%
  step_interact( ~starter:all_numeric_predictors()) 
simple_df

lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_df)

lm_wflow


lm_fit <- fit(lm_wflow, df_train)

# resample
ctrl_preds <- control_resamples(save_pred = TRUE)

resample_basic <- fit_resamples(lm_wflow, df_folds, control = ctrl_preds)

collect_metrics(resample_basic) # ok, this seems to fit the data pretty well

# let's also take a look at individual covariates
lm_fit %>% 
  extract_fit_engine() %>% 
  summary()

df_test_res <- predict(lm_fit, new_data = df_test) %>% bind_cols(df_test %>% select(team_W_PCT))
df_test_res 

# good fit
ggplot(df_test_res, aes(x = team_W_PCT, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5, col = "blue") + 
  labs(y = "Predicted Win %", x = "Win %") +
  theme_classic()

# this is nice as far as prediction, but we're more interested in the coefficient on starter



# sandbox------
# Define the multi-level model
m1 <- lmer(team_W_PCT ~ NET_RATING + AGE + GP + MIN + USG_PCT + PIE + TS_PCT  + (1 | teamName) + (1 | starter), data = df_collapse)

# Check the summary of the model
summary(m1)

# Check the diagnostic plots
plot(m1)

sjPlot::tab_model(m1)