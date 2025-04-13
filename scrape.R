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
