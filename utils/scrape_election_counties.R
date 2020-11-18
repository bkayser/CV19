#library(RCurl)
library(rvest)
library(stringr)
library(tidyr)

# This project will attempt to compile a list of shutdown and reopen events state by state.
# This is the URL where I'm scraping data on state orders for stay at home and reopening.
# I can't load it directly because the charts are all done with D3/js so I have to save it from Chrome directly before loading it here.

abbrevs <- read.csv('data/state_abbrev.csv', stringsAsFactors = F) %>% 
  select(State, State.Code=Code)

#saveRDS(FIPS.lookup, 'data/fips_table.RDS')
FIPS.lookup <- readRDS('data/fips_table.RDS')
ct.towns <- readRDS('data/CT.township.FIPS.lookup.RDS') %>% mutate(FIPS = as.character(FIPS))

cities <- read.csv('data/uscities.csv') %>%
  filter(state_id %in% c('ME', 'MA', 'NH', 'RI', 'VT', 'CT')) %>%
  select(city, state_name, FIPS=county_fips) %>%
  arrange(state_name, city) %>%
  mutate(FIPS = as.character(FIPS),
         city = str_to_lower(city) %>% 
           str_replace('^st. ', 'st-') %>%
           str_replace('old-orchrd-bch', 'old-orchard-beach') %>%
           str_replace_all(' ', '-')) %>%
  as_tibble()

  #saveRDS(ct.towns, 'data/CT.township.FIPS.lookup.RDS')  
  # read.csv('http://data.ctdata.org/dataset/291b54dd-f0be-4407-beee-4e66cdca6841/resource/d5f8442a-2f2a-4a21-a21b-643ddb56577f/download/total-households-town-2018.csv') %>%
  # mutate(Town = str_to_lower(Town),
  #        FIPS = FIPS %/% 100000) %>%
  # select(Town, FIPS) %>%
  # unique()


scrape_state = function(state) {
  
  state.code <- filter(abbrevs, State == state)$State.Code
  state.name <- filter(abbrevs, State == state)$State
  url <- stringr::str_c('https://www.nbcnews.com/politics/2020-elections/', str_to_lower(str_replace_all(state, ' ', '-')), '-results')
  
  # So I need to load that page with chrome and then save it as a complete web page, `data/wapo.hmtl`.
  
  state_page <- 
    read_html(url, encoding="text/html", 
              config=list(httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")))
    
  data <- state_page %>% 
    html_node("script#__NEXT_DATA__") %>%
    html_text() %>%
    jsonlite::fromJSON()
  
  
  # Presidential results
  #data$props$pageProps$globalMarqueeInfo$latestPresidentResults
  
  map_data <- data$props$pageProps$liveUpdateData$stateMapData
  
  counties <- lapply(names(map_data), function(fips) {
    candidates <- map_data[[fips]]$candidates %>% 
      filter(name %in% c('Joe Biden', 'Donald Trump'))
    candidates$FIPS <- fips
    select(candidates, FIPS, name, vote, percentOfVote)
  }) %>% bind_rows() %>%
    rename(votes = vote,
           candidate = name, 
           percent = percentOfVote) %>%
    mutate(candidate = factor(str_replace(candidate, ".* ", ""))) %>%
    pivot_wider(names_from='candidate', 
                values_from = c('votes', 'percent'),
                names_glue = "{candidate}_{.value}") %>%
    filter(Biden_votes > 0 & Trump_votes > 0) %>%
    mutate(State = state.name,
           FIPS = str_replace(FIPS, ' ', '-'), 
           FIPS = str_replace(FIPS, '-plt$', ''),
           FIPS = str_replace(FIPS, '-cty-to?wn.*$', '')) %>%
    select(State, FIPS, everything())
  
  if (str_detect(counties$FIPS[1], "^\\d*$", negate = T)) {
    # Convert names to FIPS codes
    counties <- rename(counties, city = FIPS) %>%
      left_join(cities, by = c(city='city', State='state_name')) 
    
    if (any(is.na(counties$FIPS))) {
      warning("Missing FIPS for ", str_c(counties$city[is.na(counties$FIPS)], collapse=", "))
    }
    counties <- counties %>%
      filter(!is.na(FIPS)) %>%
      select(FIPS, everything()) %>%
      group_by(State, FIPS) %>%
      summarize(Biden_votes = sum(Biden_votes),
                Trump_votes = sum(Trump_votes),
                Biden_percent = round(100 * (Biden_votes / (Biden_votes + Trump_votes)), 1),
                Trump_percent = 100 - Biden_percent)
  }
  if (state.code == 'CTx') {
    # Convert names to FIPS codes
    counties <- rename(counties, Town = FIPS) %>%
      left_join(ct.towns, by = c(Town='Town')) %>%
      select(FIPS, everything(), -Town ) %>%
      group_by(State, FIPS) %>%
      summarize(Biden_votes = sum(Biden_votes),
                Trump_votes = sum(Trump_votes),
                Biden_percent = round(100 * (Biden_votes / (Biden_votes + Trump_votes)), 1),
                Trump_percent = 100 - Biden_percent)
  }
  
  unemployment <- data$props$pageProps$covidUnemploymentData$unemploymentData[[str_to_lower(state.code)]]

  summary <- data$props$pageProps$liveUpdateData$stateResultsSummary$races$candidates[[1]] %>%
    mutate(percent = as.numeric(str_extract(formattedPercentVote, "[.\\d]*")),
           candidate = factor(str_replace(name, ".* ", ""))) %>%
    select(candidate, 
           votes,
           percent) %>%
    pivot_wider(names_from='candidate',
                values_from = c('votes', 'percent'),
                names_glue = "{candidate}_{.value}") %>%
    mutate(State = state.name, 
           Unemployed = last(unemployment$sixtyDayTrend),
           Unemployed.Change = unemployment$monthlyChange,
           Winner = ifelse(Biden_votes > Trump_votes, 'Biden', 'Trump')) %>%
    select(State, Unemployed, Unemployed.Change, Winner, Biden_votes, Biden_percent, Trump_votes, Trump_percent)
  
  return(list(counties = counties, 
              summary = summary))
}

function() {
  election.counties <- tibble()
  election.states <- tibble()
  
  for (State in abbrevs$State) {
    print(State)
    data <- scrape_state(State)
    election.counties <- bind_rows(election.counties, data$counties)
    election.states <- bind_rows(election.states, data$summary)
  }
}

