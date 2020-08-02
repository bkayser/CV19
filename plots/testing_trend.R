
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
