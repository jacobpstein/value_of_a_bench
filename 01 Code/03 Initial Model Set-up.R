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

# Define the multi-level model
m1 <- lmer(pctWins ~ OFF_RATING + AGE + GP + MIN + USG_PCT + (1 | team) + (1 | starter), data = df)

# Check the summary of the model
summary(m1)

# Check the diagnostic plots
plot(m1)
