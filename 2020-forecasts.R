library(tidyverse)
library(lubridate)
library(rpredictit)
library(readxl)
library(rvest)

# Functions --------------------------------------------------------------------
paste_after <- function(xs, ys) paste0(ys, xs)

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
# https://projects.fivethirtyeight.com/2020-election-forecast/
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
# https://projects.economist.com/us-2020-forecast/president
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
# https://www.predictit.org/markets/13/Prez-Election
dPredit <- all_markets()

dPredit <- dPredit %>%
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

# Jack Kersting ----------------------------------------------------------------
# https://projects.jhkforecasts.com/presidential-forecast/
dJHK <- read_csv("https://data.jhkforecasts.com/2020-presidential.csv")

dJHK <- dJHK %>%
  filter(state != "US") %>%
  select(forecastDate, state, candidate, win) %>%
  rowid_to_column() %>%
  group_by(state, candidate) %>%
  slice_max(order_by = rowid, n = 1) %>%
  ungroup() %>%
  mutate(state = str_replace_all(state, "Maine CD", "ME"),
         state = str_replace_all(state, "Nebraska CD", "NE"),
         short_name = case_when(str_detect(candidate, "Trump") ~ "trump",
                                str_detect(candidate, "Biden") ~ "biden"),
         win = win / 100) %>%
  filter(!is.na(short_name)) %>%
  select(state, short_name, win) %>%
  pivot_wider(names_from = short_name, values_from = win) %>%
  full_join(states, by = c("state" = "state_name")) %>%
  select(state_name = state, state_abbr, trump, biden) %>%
  arrange(state_name) %>%
  mutate(model = "JHK", .before = 1)

# Lean Tossup ------------------------------------------------------------------
# https://leantossup.ca/us-presidency/
temp <- tempfile()
download.file("http://www.leantossup.ca/US_Pres_2020/US_State_Results.xlsx", temp)
dLT <- read_excel(temp, range = cell_cols("K:BN"))
unlink(temp)

dLT <- dLT %>%
  pivot_longer(cols = everything(), names_to = "state_name",
               values_to = "winner") %>%
  count(state_name, winner) %>%
  complete(state_name, winner, fill = list(n = 0)) %>%
  group_by(state_name) %>%
  mutate(pct = n / sum(n),
         winner = tolower(winner),
         state_name = str_replace_all(state_name, "-0", "-")) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = winner, values_from = pct) %>%
  full_join(states, by = "state_name") %>%
  select(state_name, state_abbr, trump, biden) %>%
  arrange(state_name) %>%
  mutate(model = "Lean Tossup", .before = 1)

# Princeton Election Consortium ------------------------------------------------
# pec: https://election.princeton.edu/for-fellow-geeks/
dPEC <- read_csv("https://election.princeton.edu/election2020/data/EV_stateprobs.csv",
                 col_names = FALSE)

dPEC <- dPEC %>%
  select(state_abbr = X5, biden = X1) %>%
  mutate(state_abbr = case_when(state_abbr == "M1" ~ "ME-1",
                                state_abbr == "M2" ~ "ME-2",
                                state_abbr == "N1" ~ "NE-1",
                                state_abbr == "N2" ~ "NE-2",
                                state_abbr == "N3" ~ "NE-3",
                                TRUE ~ state_abbr),
         biden = biden / 100,
         trump = 1 - biden) %>%
  full_join(states, by = "state_abbr") %>%
  select(state_name, state_abbr, trump, biden) %>%
  arrange(state_name) %>%
  mutate(model = "Princeton", .before = 1)

# Electoral Polls --------------------------------------------------------------
# https://electoralpolls.com/
dEP <- read_html("https://electoralpolls.com/projection") %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE, header = FALSE) %>%
  as_tibble()

dEP <- dEP %>%
  slice(3:58) %>%
  select(state_name = X2, pct = X8, cum_ec = X9) %>%
  mutate(pct = parse_number(pct) / 100,
         cum_ec = parse_number(cum_ec),
         increase = cum_ec > lag(cum_ec, default = TRUE),
         trump = case_when(increase ~ 1 - pct,
                           !increase ~ pct),
         biden = case_when(increase ~ pct,
                           !increase ~ 1 - pct),
         state_name = str_replace_all(state_name, " \\(.*\\)", ""),
         state_name = str_replace_all(state_name, "Maine CD", "ME"),
         state_name = str_replace_all(state_name, "Nebraska CD", "NE"),
         state_name = str_replace_all(state_name, "Tipping Point", ""),
         state_name = str_trim(state_name)) %>%
  full_join(states, by = "state_name") %>%
  select(state_name, state_abbr, trump, biden) %>%
  arrange(state_name) %>%
  mutate(model = "Electoral Polls", .before = 1)

# Reed Forecasts ---------------------------------------------------------------
# https://reedforecasts.com/
dReed <- read_html("https://reedforecasts.com/") %>%
  html_nodes("script") %>%
  as.character() %>%
  str_subset("main\\..*\\.js") %>%
  str_extract("/static/js/.*\\.js") %>%
  paste_after("https://reedforecasts.com") %>%
  read_html() %>%
  html_text() %>%
  str_sub(start = 1L, end = 5000L) %>%
  str_extract("\\{\".*\\)\\}") %>%
  str_split(",") %>%
  flatten_chr() %>%
  map_df(function(.x) {
    state <- str_extract(.x, "\".*\"") %>%
      str_replace_all("\"", "")
    
    pct <- .x %>%
      str_split(":") %>%
      flatten_chr() %>%
      .[2] %>%
      str_replace_all("[^0-9\\.]", "") %>%
      as.numeric()
    
    tibble(state_name = state, trump = 1 - (pct / 100), biden = pct / 100)
  }) %>%
  filter(state_name != "National") %>%
  mutate(state_name = str_replace_all(state_name, "Maine CD", "ME"),
         state_name = str_replace_all(state_name, "Nebraska CD", "NE")) %>%
  full_join(states, by = "state_name") %>%
  select(state_name, state_abbr, trump, biden) %>%
  arrange(state_name) %>%
  mutate(model = "Reed", .before = 1)
  
# Decision Desk HQ -------------------------------------------------------------
# https://forecast.decisiondeskhq.com/president


# Plural Vote ------------------------------------------------------------------
# http://www.pluralvote.com/article/2020-forecast/


# Progress Campaign ------------------------------------------------------------
# https://www.ourprogress.org/forecast


# Race to the White House ------------------------------------------------------
# https://www.racetothewh.com/president

