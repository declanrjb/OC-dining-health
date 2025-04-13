library(tidyverse)
library(rvest)
library(lubridate)

filter_vec <- function(vec, condition) {
  vec %>%
    lapply(condition) %>%
    unlist() %>%
    which() %>%
    vec[.] %>%
    return()
} 

get_critical <- function(page) {
  text <- page %>%
    html_nodes('#general') %>%
    html_text()
  if (length(text) > 0) {
    return(text)
  } else {
    return('')
  }
}

get_noncritical <- function(page) {
  text <- page %>%
    html_nodes('#manager') %>%
    html_text()
  if (length(text) > 0) {
    return(text)
  } else {
    return('')
  }
}

url <- 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=5A9F8BE97B8078C388257BF6007CF58C'

page <- read_html(url)

df <- page %>% 
  html_nodes('table') %>% 
  .[5] %>% 
  html_table() %>%
  .[[1]]

colnames(df) <- c('type', 'date', 'summary')

df$link <- page %>% 
  html_nodes('a') %>% 
  html_attr('href') %>%
  filter_vec(function(link) {grepl('Food-Inspection', link)}) %>%
  paste('https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/', ., sep='')

pages <- df$link %>%
  lapply(read_html)

df$critical <- pages %>%
  lapply(get_critical) %>%
  unlist() %>%
  gsub('\t', '', .)

df$non_critical <- pages %>%
  lapply(get_noncritical) %>%
  unlist() %>%
  gsub('\t', '', .)

df %>%
  write.csv('data/stevie.csv', row.names=FALSE)
