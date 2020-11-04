# Load packages ----------------------------------------------------------------
library(tidyverse)
library(here)

# Source functions -------------------------------------------------------------
source('https://gist.githubusercontent.com/elliottmorris/c70fd4d32049c9986a45e2dfc07fb4f0/raw/65020f8aa760348e5adb08945db84d45bf847704/election_night_live_model.R')

# Read current results ---------------------------------------------------------
results <- read_csv(here("results.csv"),
                    col_types = cols(biden_win = col_integer(),
                                     .default = col_character())) %>%
  mutate(state_abbr = str_replace_all(state_abbr, "-", ""))

add_biden <- NULL
add_trump <- NULL

# Simulate new results ---------------------------------------------------------
biden <- results %>%
  filter(biden_win == 1L) %>%
  pull(state_abbr) %>%
  c(add_biden) %>%
  unique()

trump <- results %>%
  filter(biden_win == 0L) %>%
  pull(state_abbr) %>%
  c(add_trump) %>%
  unique()

update_prob(biden_states = biden,
            trump_states = trump,
            biden_scores_list = NULL)
