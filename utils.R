library(tidyverse)
library(lubridate)
library(scales)
library(wpp2019)


read_and_clean <- function(infile) {
  # Read the data
  cv.wide <- read.csv(infile, stringsAsFactors = F)
  is.countries <- 'Country.Region' %in% names(cv.wide)
  if (is.countries) {
    # This is the countries data so column names need to be adjusted
    cv.wide <- rename(cv.wide,
                      Province_State=Province.State,
                      Country_Region=Country.Region,
                      Long_=Long) %>%
      mutate(Admin2=NA,
             Combined_Key=ifelse(str_length(Province_State) > 0, 
                                 paste(Country_Region, sep=", ", Province_State), 
                                 Country_Region),
             Country_Region = ifelse(Country_Region == 'China', Combined_Key, Country_Region)) # Treat each province in china as a separate country
  } else {
    # There are two counties without FIPS codes for Kansas City, and "Dukes and Nantucket".  I think these double-count so I'm removing them.
    cv.wide <- filter(cv.wide, !is.na(FIPS))
    
  }
  # Identify the date columns so we can gather them
  datecols <- grep('^X', names(cv.wide))
  
  # Gather the date columns into a single pair of columns, "date" and "count"
  # This converts the data from a wide format to a narrow format more amenable to
  # graphing and reshaping
  cv.wide <- gather(cv.wide, key='date', value='count', all_of(datecols)) %>%
    select(State=Province_State, starts_with('FIPS'), County=Admin2, Country=Country_Region, Lat, Long=Long_, Key=Combined_Key, Date=date, Count=count) %>%
    mutate(Date=mdy(str_sub(Date, 2)),
           State=as.factor(State),
           Country = as.factor(Country),
           Key = as.factor(Key)) %>%
    as_tibble() 
  if (is.countries) {
    cv.wide <- group_by(cv.wide,
                        Country, Date) %>%
      summarize(Lat = mean(Lat),
                Long = mean(Long),
                Count = sum(Count)) %>%
      ungroup() %>%
      mutate(Country = as.factor(Country),
             Key = Country)
  }
  return(cv.wide)
}

calculate_differentials <- function(infection_data, target_variables) {
  infection_data <- arrange(infection_data, Key, Date)
  boundaries <- which(infection_data$Key[2:nrow(infection_data)] != infection_data$Key[1:nrow(infection_data)-1])
  target_variables.diff <- paste0(target_variables, '.Diff')
  target_variables.diff3 <- paste0(target_variables, '.Diff3')
  # Calculate 1 day differential  
  X1 <- infection_data[, target_variables]
  X0 <- infection_data[c(1, (1:nrow(X1)-1)), target_variables]
  infection_data[, target_variables.diff] <- X1 - X0
  
  # Calculate the 3 day differential
  X1 <- infection_data[ , target_variables]
  X0 <- infection_data[c(1, 1, 1, 1:(nrow(X1)-3)), target_variables]
  infection_data[, target_variables.diff3] <- round((X1 - X0) / 3.0)
  
  infection_data[boundaries + 1, target_variables.diff] <- 0
  infection_data[c(boundaries + 1,
                   boundaries + 2,
                   boundaries + 3), target_variables.diff3] <- 0

  for (var in 1:length(target_variables)) {
    target_var <- target_variables[var]
    target_var.diff3 <- target_variables.diff3[var]
    growth_var <- paste0(target_var, '.Growth3')
    rate_var <- paste0(target_var, '.Diff3.Per100K')
    per_capita_var <- paste0(target_var, '.Per100K')
    growth_rows <- (infection_data[,target_var.diff3] != 0 & infection_data[,target_var] != 0)[,1]
    infection_data[growth_rows, growth_var]     <- round(infection_data[growth_rows, target_var.diff3] / infection_data[growth_rows, target_var], 4)
    infection_data[growth_rows, rate_var]       <- infection_data[growth_rows, target_var.diff3] * 100000 / infection_data$Population[growth_rows]
    infection_data[, per_capita_var] <- infection_data[, target_var] * 100000 / infection_data$Population
    infection_data[!growth_rows, c(growth_var, rate_var) ] <- list(0,0)
  }
  return(infection_data)
}

prep_geographic_data <- function(data) {
  state_names <- as.character(read.csv('data/state_abbrev.csv')$State)
  
  for (i in 1:nrow(data)) {
    if (data$Name[i] %in% state_names) {
      state <- data$Name[i]
    }
    data[i, 'State'] <- state
  }
  filter(data, Name != State) %>%
    select(State, County = Name, Area='Sq.Mi.') %>%
    mutate(State = as.character(State), 
           County = as.character(County))
  
}
