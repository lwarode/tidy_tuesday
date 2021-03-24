library(tidyverse)
library(ggsci)
library(countrycode)
library(tidytext)

# tidy tuesday data -------------------------------------------------------
unvotes_tt <- tidytuesdayR::tt_load("2021-03-23")

# unvotes
unvotes_tt %>% 
  pluck("unvotes") -> unvotes

# roll_calls
unvotes_tt %>% 
  pluck("roll_calls") -> roll_calls

# issues
unvotes_tt %>% 
  pluck("issues") -> issues



# data inspection ---------------------------------------------------------
unvotes_tt %>% 
  map(glimpse)

# unvotes
unvotes %>% 
  map(table)

# roll_calls
roll_calls %>% 
  map(table)

# issues
issues %>% 
  map(table)



# data manipulation -------------------------------------------------------
unvotes_rc <- unvotes %>% 
  left_join(roll_calls, by = "rcid") #%>% 
  # left_join(issues, by = "rcid")

unvotes_all %>% 
  filter(! is.na(vote))

unvotes_all %>% 
  distinct(country, rcid)

unvotes_all %>% 
  filter(country_code == "US")

unvotes %>% 
  filter(! is.na(vote))

unvotes %>% 
  filter(country_code == "US")

issues %>% 
  map(table)

# tidytext
unvotes_rc %>% 
  unnest_tokens(word, short) 



# data visualization ------------------------------------------------------

# ggplot2 theme updates
theme_set(theme_linedraw())

theme_update(strip.background = element_blank(),
             strip.text = element_text(color = "black"),
             panel.grid.minor = element_blank(),
             legend.position = "top")


