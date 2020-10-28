library(tidyverse)
library(lubridate)
library(rpredictit)

# State and abbreviations ------------------------------------------------------
states <- tibble(state_name = state.name,
                 state_abbr = state.abb) %>%
  bind_rows(tribble(           ~state_name, ~state_abbr,
                    "District of Columbia",        "DC",
                                    "ME-1",     "ME-1",
                                    "ME-2",     "ME-2",
                                    "NE-1",     "NE-1",
                                    "NE-2",     "NE-2",
                                    "NE-3",     "NE-3"))

# FiveThirtyEight --------------------------------------------------------------
d538 <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_state_toplines_2020.csv")
d538 <- d538 %>%
  select(state, trump = winstate_inc, biden = winstate_chal, timestamp) %>%
  mutate(timestamp = as_datetime(timestamp, format = "%H:%M:%S %d %b %Y")) %>%
  group_by(state) %>%
  slice_max(order_by = timestamp, n = 1) %>%
  ungroup() %>%
  full_join(states, by = c("state" = "state_name")) %>%
  select(state_name = state, state_abbr, trump, biden) %>%
  arrange(state_name) %>%
  mutate(model = "FiveThirtyEight", .before = 1)

# Economist --------------------------------------------------------------------
temp <- tempfile()
download.file("https://cdn.economistdatateam.com/us-2020-forecast/data/president/economist_model_output.zip", temp)
dEcon <- read_csv(unz(temp, "output/site_data//state_averages_and_predictions_topline.csv"))
unlink(temp)

dEcon <- dEcon %>%
  mutate(trump = 1 - projected_win_prob) %>%
  select(state, trump, biden = projected_win_prob) %>%
  bind_rows(tibble(state = c("ME-1", "ME-2", "NE-1", "NE-2", "NE-3"),
                   trump = c(rep(pull(filter(., state == "ME"), trump), 2),
                             rep(pull(filter(., state == "NE"), trump), 3)),
                   biden = c(rep(pull(filter(., state == "ME"), biden), 2),
                             rep(pull(filter(., state == "NE"), biden), 3)))) %>%
  full_join(states, by = c("state" = "state_abbr")) %>%
  select(state_name, state_abbr = state, trump, biden) %>%
  arrange(state_name) %>%
  mutate(model = "Economist", .before = 1)

# PredictIt --------------------------------------------------------------------
markets <- all_markets()

dPredit <- markets %>%
  filter(str_detect(name, "in the 2020 presidential") |
           str_detect(name, "in the  2020 presidential"),
         !str_detect(name, "Electoral College"),
         !str_detect(name, "popular vote")) %>%
  select(name, contract_name, lastTradePrice) %>%
  group_by(name) %>%
  mutate(state = str_replace_all(name, "Which party will win ", ""),
         state = str_replace_all(state, " in the .*$", ""),
         state = str_replace_all(state, "-0", "-"),
         state = str_replace_all(state, "DC", "District of Columbia"),
         candidate = case_when(contract_name == "Democratic" ~ "biden",
                               contract_name == "Republican" ~ "trump"),
         prob = lastTradePrice / sum(lastTradePrice)) %>%
  ungroup() %>%
  select(state, candidate, prob) %>%
  pivot_wider(names_from = candidate, values_from = prob) %>%
  full_join(states, by = c("state" = "state_name")) %>%
  select(state_name = state, state_abbr, trump, biden) %>%
  arrange(state_name) %>%
  mutate(model = "PredictIt", .before = 1)

# Decision Desk HQ -------------------------------------------------------------


# 
