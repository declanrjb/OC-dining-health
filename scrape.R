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

scrape_reports <- function(url) {
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

  return(df)
}

scrape_location <- function(name, url) {
  df <- scrape_reports(url)
  df <- df %>%
    mutate(hall = name)
  df %>%
    write.csv(paste('data/halls/',name,'.csv',sep=''), row.names=FALSE)
}

scrape_location('Stevenson Dining Hall', 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=5A9F8BE97B8078C388257BF6007CF58C')

scrape_location('Lord Saunders', 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=11F70733510C9EE988257BF6007CF58A')

scrape_location("Azariah's Cafe", 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=39CE77300A1A455885257C16004E9472')

scrape_location('Wilder Hall', "https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=9AFBD4E8FC2F72E888257BF6007CF58B")

scrape_location('Heritage Dining Hall', 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=467771A0DB5AD6D3852580BC00634686')

scrape_location('Clarity Dining Hall', 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=292FF0FEB1376D91852586B700672DAE')

scrape_location("Lorenzo's Pizzera", 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=19ACAA8226B1E00C88257BF6007CF55B')

scrape_location("Aladdin's Eatery", 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=F3FF25989547897988257BF6007CF77F')

scrape_location('Thi Ni Thai', 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=813C5D536CC964C78525844D004AEFDC')

scrape_location('The Feve', 'https://healthspace.com/Clients/Ohio/LorainCounty/Web.nsf/Food-FacilityHistory?OpenView&RestrictToCategory=60465B48E880F3E288257BF6007CF5C3')
