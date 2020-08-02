library(tidyverse)
library(lubridate)

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

calculate_differentials <- function(data, target_variables, growth=T) {
  infection_data <- arrange(data, Key, Date)
  boundaries <- which(infection_data$Key[2:nrow(infection_data)] != infection_data$Key[1:nrow(infection_data)-1])
  target_variables.diff <- paste0(target_variables, '.Diff')
  target_variables.diff5 <- paste0(target_variables, '.Diff5')
  # Calculate 1 day differential  
  X1 <- infection_data[, target_variables]
  X0 <- infection_data[c(1, (1:nrow(X1)-1)), target_variables]
  infection_data[, target_variables.diff] <- X1 - X0
  
  # Calculate the 5 day differential
  X1 <- infection_data[ , target_variables]
  X0 <- infection_data[c(1, 1, 1, 1, 1, 1:(nrow(X1)-5)), target_variables]
  infection_data[, target_variables.diff5] <- round((X1 - X0) / 5.0, 1)
  
  infection_data[boundaries + 1, target_variables.diff] <- 0
  infection_data[c(boundaries + 1,
                   boundaries + 2,
                   boundaries + 3,
                   boundaries + 4, 
                   boundaries + 5), target_variables.diff5] <- 0

  if (growth) {
    for (var in 1:length(target_variables)) {
      target_var <- target_variables[var]
      target_var.diff5 <- target_variables.diff5[var]
      growth_var <- paste0(target_var, '.Growth5')
      rate_var <- paste0(target_var, '.Diff5.Per100K')
      per_capita_var <- paste0(target_var, '.Per100K')
      growth_rows <- (infection_data[,target_var.diff5] != 0 & infection_data[,target_var] != 0 & !is.na(infection_data[,target_var.diff5]))[,1]
      infection_data[growth_rows, growth_var]     <- round(infection_data[growth_rows, target_var.diff5] / infection_data[growth_rows, target_var], 4)
      infection_data[growth_rows, rate_var]       <- infection_data[growth_rows, target_var.diff5] * 100000 / infection_data$Population[growth_rows]
      infection_data[, per_capita_var] <- round(signif(infection_data[, target_var] * 100000 / infection_data$Population, 3))
      infection_data[!growth_rows, c(growth_var, rate_var) ] <- list(0,0)
    }
  }
  return(infection_data)
}


add_estimated_recoveries <- function(data) {

  mean <- 20
  start <- 6
  scale  <- 6
  shape <- (mean - start) / scale
  x <- 1:nrow(data)
  
  quantiles <- pgamma(x-start, scale = scale, shape = shape)
  probabilities <- quantiles - c(0, quantiles[1:(length(quantiles)-1)]) 
  # qplot(x, probabilities, geom='line') + geom_vline(xintercept=mean)
  
  data.list <- split(data, data$Key)
  
  estimate_recoveries <- function(df) {
    df <- arrange(df, Date)
    df$recovered.est <- 0
    for (i in 1:nrow(df)) {
      cases <- df$Cases[i]
      new_recoveries <- cases * probabilities
      projected_range <- i:nrow(df)
      df[projected_range, 'recovered.est'] <- df[projected_range, 'recovered.est'] + new_recoveries[1:length(projected_range)]
    }
    return(mutate(df, recovered.est = round(recovered.est)))
  }
  lapply(data.list, estimate_recoveries) %>% 
    bind_rows() %>%
    rename(Recovered=recovered, Est.Recovered=recovered.est)
}


get_geographic_data <- function() {
  data <- read.csv('../data/county_area.csv', skip=7)
  state_names <- as.character(read.csv('../data/state_abbrev.csv')$State)
  
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

filter_cvdata <- function(data, states, show.all=F) {
  if (!show.all | length(states) == 0) {
    data %>%
      filter(State %in% states) %>%
      mutate(State=fct_drop(State)) %>%
      arrange(State, Date) %>%
      filter( Cases > 0)
  } else {
    data %>%
      group_by(Date) %>%
      summarize(Population = sum(Population),
                Cases = sum(Cases),
                Cases.Per100K = Cases * 100000 / Population,
                Cases.Diff5 = sum(Cases.Diff5),
                Cases.Diff = sum(Cases.Diff),
                Cases.Growth5 = Cases.Diff5 / Cases,
                
                Deaths = sum(Deaths, na.rm=T),
                Deaths.Per100K = Deaths * 100000 / Population,
                Deaths.Diff = sum(Deaths.Diff),
                Deaths.Diff5 = sum(Deaths.Diff5, na.rm=T),
                Deaths.Growth5 = Deaths.Diff5 / Deaths,
                
                Testing.Total = sum(Testing.Total),
                Testing.Total.Diff5 = sum(Testing.Total.Diff5),
                Testing.Rate.Total = Testing.Total / Population,
                Testing.Positive.Rate.Total = sum(positive) / Testing.Total,
                Testing.Rate.Weekly = Testing.Total.Diff5 / Population,
                Testing.Positive.Rate = sum(positive.Diff5) / Testing.Total.Diff5,
                Tests.Per.Capita = 100000 * Testing.Total / Population,
                inIcuCurrently = sum(inIcuCurrently, na.rm=T)
                )
  }
}

party_fill <- function() {
  scale_fill_manual(values = c('D' = '#6666FF', 'R'='#FF6666'), na.value='#BBBBBB')
}
