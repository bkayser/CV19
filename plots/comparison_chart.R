
# Depends on utils.R
# Requires cvdata.us.by_state

comparison_chart <- function(cvdata.by_state, 
                             states, 
                             column = 'Deaths', 
                             show.all = F,
                             scale = 'normal', 
                             theme.default = ggthemes::theme_few()) {

  df <- filter_cvdata(cvdata.by_state, states, show.all)
  df[, 'value'] <- df[, column]
  
  df$value.formatted <- format_value(column, df$value)

  value.label <- names(cvdata.cols)[cvdata.cols == column]
  end_date <- max(df$Date)
  
  df.labels <- 
    filter(df, Date == end_date) %>% 
    arrange(desc(value))
  
  if (show.all) {
    df.labels$label =paste(value.label, df.labels$value.formatted)
  } else {
    df.labels$label =paste(df.labels$State.Code, df.labels$value.formatted)
  }
  
  vertical.separation <- (max(df$value, na.rm=T) - min(df$value, na.rm=T)) / 60
  if (nrow(df.labels) > 1) {
    for (row in 2:nrow(df.labels)) {
      if (any(is.na(df.labels$value[(row-1):row]))) {
        next;
      }
      if (df.labels$value[row-1] - df.labels$value[row] < vertical.separation) {
        df.labels$value[row] <- df.labels$value[row-1] - vertical.separation
      }
    }
  }
  
  start_date.adj <- mdy('03-14-2020')
  breaks <- seq(end_date,
                start_date.adj,
                by="-1 week") %>% rev()
  if (breaks[1] != start_date.adj) {
    breaks <- c(breaks[1]-days(7), breaks)
    start_date.adj <- breaks[1]
  }
  g <- ggplot(df) +
    geom_line() + 
    ggtitle(str_c(value.label, ifelse(show.all, ', All States', ', by State'))) +
    coord_cartesian(xlim=c(start_date.adj, end_date + round(0.08 * 7 * length(breaks)))) +
    scale_x_date(breaks=breaks, date_labels = '%m/%d', date_minor_breaks='1 day') +
    theme.default +
    theme(legend.position=c(0.1, 0.8)) 
  
  #labels <- filter(df, Date == end_date) %>% mutate(label = paste(value.label, value.formatted))
  if (show.all) {
    g <- g + aes(Date, value) 
  } else {
    g <- g + aes(Date, value, color=State) 
  }
  g <- g + geom_text(data = df.labels,
                     aes(label = label),
                     size=4,
                     hjust='left',
                     nudge_x = 2,
                     show.legend=F) 
  if (scale == 'log') {
    g <- g + scale_y_log10(labels=comma) + labs(subtitle = 'Log Scale') 
  } else {
    g <- g + scale_y_continuous(labels=comma)
  }
  g+
    ylab(NULL) +
    xlab(NULL)
}

function () {

  comparison_chart(cvdata.us.by_state, c('Oregon', 'California', 'Arizona', 'Texas'), 
                   column = 'Deaths.Diff5.Per100K',
                   show.all=T)
  
}
