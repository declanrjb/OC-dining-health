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

crits_by_date <- df %>%
  filter(grepl('Standard', type)) %>%
  filter(hall == 'Stevenson Dining Hall') %>%
  group_by(date, hall) %>%
  summarize(crits = sum(crit_count))

crits_by_date %>%
  ggplot(aes(x = date, y = crits)) +
  geom_col()

test <- df %>% filter(hall == 'Stevenson Dining Hall') %>% arrange(date)
test <- test %>% filter(grepl('Standard',type,ignore.case=TRUE) | grepl('Control Point',type,ignore.case=TRUE))
test <- test %>% select(type, date, crit_count, noncrit_count)

stevie <- df %>% 
  filter(hall == 'Stevenson Dining Hall') %>% 
  arrange(date)

df %>%
  filter(year(date) >= 2019) %>%
  filter(grepl('Standard',type,ignore.case=TRUE) | grepl('Control Point',type,ignore.case=TRUE)) %>%
  group_by(hall) %>%
  summarize(crits = sum(crit_count)) %>%
  arrange(-crits) %>%
  write.csv('viz/crits-with-town.csv', row.names=FALSE)

df %>%
  filter(year(date) >= 2019) %>%
  filter(grepl('Standard',type,ignore.case=TRUE) | grepl('Control Point',type,ignore.case=TRUE)) %>%
  group_by(hall) %>%
  summarize(Violation_Inspections = length(which(crit_count > 0)), Inspections = length(crit_count)) %>%
  mutate(Clean_Inspections = Inspections - Violation_Inspections, Violation_Rate = Violation_Inspections / Inspections) %>%
  mutate(hall = paste(hall, " (", Inspections, " standard inspections)", sep="")) %>%
  arrange(desc(Violation_Rate)) %>%
  write.csv('viz/crit-rate.csv', row.names=FALSE)

df %>% 
  arrange(desc(date)) %>%
  select(hall, type, date, crit_count, noncrit_count, link) %>%
  mutate(link = paste("<a href='", link, "'>Click Here</a>", sep='')) %>%
  rename(Location = hall) %>%
  rename(`Inspection Type` = type) %>%
  rename(`Critical Violations` = crit_count) %>%
  rename(`Non-Critical Violations` = noncrit_count) %>%
  rename(`Inspectors' Notes` = link) %>%
  mutate(date = strftime(date, "%b. %Y")) %>%
  rename(Date = date) %>%
  write.csv('viz/searchable-database.csv', row.names=FALSE)
