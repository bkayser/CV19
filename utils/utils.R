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

state_summary_plot = function(data, 
                              state, 
                              state.orders,
                              overlay='Deaths.Diff5', 
                              show.all=F, 
                              show.lockdown=T,
                              show.trend=T){
  start_date <- mdy('02/26/2020')
  end_date <- max(data$Date)
  
  state.data <- filter_cvdata(data, state, show.all) %>% 
    filter(Cases > 0 & Date > start_date) %>%
    mutate(Daily.Increase=round(Cases.Diff5))
  if (show.all) {
    name <- 'All States'
    orders <- state.orders
  } else {
    name <- state
    orders <- filter(state.orders, State==name) 
  } 
  start_date.adj <- max(start_date, min(state.data$Date))
  
  date.breaks <- seq(end_date,
                     start_date.adj,
                     by="-1 week") %>% rev()
  last.recorded.value <- tail(state.data, 1) %>% pull(overlay)
  # scale the overlay so it fits in the graph
  if (overlay == 'Cases.Diff') {
    scale.factor <- 1
  } else {
    scale.factor <- 1.1 * max(state.data$Cases.Diff5, na.rm=T) / max(state.data[overlay], na.rm=T)
  }
  state.data$Overlay <- unlist(state.data[overlay]) * scale.factor
  overlay.label <- names(cvdata.cols)[cvdata.cols == overlay]
  overlay.format <- ifelse(str_ends(overlay, 'Growth5') | str_detect(overlay, 'Rate'), percent, comma)
  
  g <- ggplot(state.data) +
    aes(x=Date, y=Cases.Diff5) +
    geom_line(color='#999999') + 
    geom_text(aes(label=Daily.Increase), 
              nudge_y=4,
              check_overlap=T,
              color='black') +
    ggtitle(paste(name, 'Change in Confirmed Cases, Five Day Average'),
            subtitle = str_c("Reported data through ", format(end_date, "%B %d, %Y"))) +
    ylab("Daily Increase") +
    geom_text(data=~ tail(.x, 1), 
              aes(label=paste0(overlay.label, ': ',overlay.format(last.recorded.value))),
              y=last.recorded.value * scale.factor,
              color='red',
              size=6,
              nudge_y=0.1 * last.recorded.value * scale.factor,
              nudge_x=-12,
              show.legend = F) +
    ggthemes::theme_few() + 
    theme(text = element_text(size=14),
          title = element_text(size=18),
          legend.position='bottom',
          legend.key.size=unit(22, 'point'),
          legend.key.width = unit(8, 'point'),
          legend.text=element_text(size=14,lineheight = 12),
          legend.spacing=unit(12,'points')) +
    xlab(NULL)  +
    scale_x_date(breaks=date.breaks, date_labels = '%m/%d', date_minor_breaks='1 day')
  
  if (scale.factor != 1) {
    g <- g + scale_y_continuous(name='Daily Increase', 
                                sec.axis = sec_axis(trans = ~./scale.factor, 
                                                    name = overlay.label,
                                                    labels = overlay.format)) 
  } else {
    g <- g + scale_y_continuous(name='Daily Increase') 
  }
  
  if (str_ends(overlay, "5" )) {
    g <- g + geom_line(aes(y=Overlay), color='#FF3333', alpha=0.6) 
  } else {
    g <- g + geom_bar(aes(y=Overlay), 
                      fill='#FF3333', alpha=0.1, stat='identity')      
  }
  if (show.lockdown) {
    if (any(orders$type == 'close')) {
      lockdown.range <- filter(state.data, Date >= min(orders$date))
      if (any(orders$type == 'open')) {
        lockdown.range <- filter(lockdown.range, Date <= max(orders$date))
      }
    } else {
      lockdown.range <- head(state.data, 0)
    }
    g <- g + 
      geom_vline(data=orders,
                 aes(xintercept=date, color=type, alpha='0'),
                 size=1, 
                 show.legend = T) +
      scale_alpha_manual(values=0, guide=F) +
      geom_area(data=lockdown.range,
                #aes(ymax=Cases.Diff5),
                fill='#aec0c6',
                alpha=0.2) 
    if (show.all) {
      g <- g +
        geom_vline(data=orders,
                   aes(xintercept=date, color=type),
                   alpha=0.5,
                   size=0.5, 
                   show.legend = F) +
        scale_color_manual(name=NULL, 
                           values=c(close='#3333CC', open='#DD6666'),
                           labels=c('Restrictions put in place',
                                    'Restrictions lifted')) 
    } else {
      g <- g +
        geom_vline(data=orders,
                   aes(xintercept=date, color=type),
                   alpha=0.5,
                   linetype=2,
                   size=0.5, 
                   show.legend = F) +
        scale_color_manual(name=NULL, 
                           values=c(close='#3333CC', open='#DD6666'),
                           labels=paste(orders$desc, format(orders$date, '%B %d'))) 
    }
    
  }
  if (show.trend) {
    g <- g + geom_smooth(alpha=0.05, method='loess', color='black', fill='green', fullrange=T, span=0.5, size=0.5, linetype=3) 
  }
  g
}

testing.trend <- function(cvdata, timerange=90, states, show.all=F) {
  end_date <- max(cvdata$Date)
  df <- filter_cvdata(cvdata, states, show.all) %>%
    filter(!is.na(Testing.Total.Diff5) &                  # Filter missing values
             Testing.Total.Diff5 > 0 &                    # Filter missing values
             Date > end_date - days(timerange) &  # Recent time window
             Testing.Positive.Rate < 0.55)
  fill.df <- tibble(x=c(min(df$Tests.Per.Capita)-1000, max(df$Tests.Per.Capita)+500),
                    top = rep(max(0.15, max(df$Testing.Positive.Rate))+0.01, 2),
                    blue = req(0.15, 2),
                    green = req(0.05, 2))
  g <-  df %>%    # Remove outliers
    ggplot() +
    geom_ribbon(data=fill.df, inherit.aes = F, fill='red', alpha=0.1,
                aes(x=x, ymax=top, ymin=blue)) +
    geom_ribbon(data=fill.df, inherit.aes = F, fill='yellow', alpha=0.1,
                aes(x=x, ymax=blue, ymin=green)) +
    geom_ribbon(data=fill.df, inherit.aes = F, fill='green', alpha=0.2,
                aes(x=x, ymax=green, ymin=0)) +
    geom_line(show.legend = F, size=0.2) +
    geom_point(show.legend = F, size=0.6) +
    scale_y_continuous(labels=percent)
    
  if (show.all) {
    g <- g + aes(x=Tests.Per.Capita, y=Testing.Positive.Rate) +
      ggtitle(subtitle = paste0('Last ', timerange, ' days'),
              paste0('Per Capita Testing vs. Test Positive Rate')) 
  } else {
    g <- g + aes(x=Tests.Per.Capita, y=Testing.Positive.Rate, color=State) +
      ggtitle(subtitle = paste0('Last ', timerange, ' days'),
              paste0('Per Capita Testing vs. Test Positive Rate')) +
      geom_label(data=~group_by(.x, State.Code) %>% summarize(State = last(State),
                                                              Date = last(Date),
                                                              Testing.Positive.Rate=last(Testing.Positive.Rate),
                                                              Tests.Per.Capita=last(Tests.Per.Capita)),
                 aes(label=State.Code), 
                 show.legend=F,
                 fill='white')  
  }
  g + 
    xlab('Tests per 100K Population') +
    ylab('Rate of Positive Tests') 
    
}
