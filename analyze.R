library(tidyverse)
library(lubridate)

df <- list.files('data/halls') %>%
  paste('data/halls/', ., sep='') %>%
  lapply(read_csv) %>%
  do.call(rbind, .)

df$crit_count <- df$summary %>% 
  str_extract('([0-9]+) critical', group=1) %>% 
  parse_number()

df$noncrit_count <- df$summary %>% 
  str_extract('([0-9]+) non-critical', group=1) %>% 
  parse_number()

df$date <- df$date %>% 
  parse_date_time("%d-%b-%Y")

df$year <- df$date %>%
  year()

df$month <- df$date %>%
  strftime("%B")

df %>%
  write.csv('data/all-halls.csv', row.names=FALSE)

df <- read_csv('data/all-halls.csv')

test <- df %>%
  filter(hall == 'Stevenson Dining Hall')

test <- test %>%
  select(type, date, crit_count, noncrit_count)

test <- test %>%
  filter(grepl('Standard', type))

df %>%
  filter(date == '2025-04-09') %>%
  select(hall, crit_count, noncrit_count)

df %>%
  filter(date == '2025-04-09') %>%
  filter(hall == 'Wilder Hall') %>%
  pull(critical)
