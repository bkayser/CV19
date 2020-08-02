
# Depends on utils.R
# Requires cvdata.us.by_state

comparison_chart <- function(cvdata.by_state, 
                             states, 
                             column, 
                             show.all, 
                             scale, 
                             theme.default = ggthemes::theme_few()) {

  df <- filter_cvdata(cvdata.by_state, states, show.all)
  df[, 'value'] <- df[,column]
  df[, 'value.label'] <- overlay.format <- ifelse(str_ends(column, 'Growth5') | str_detect(column, 'Rate'), 
                                                  percent(df$value), 
                                                  comma(df$value))
  value.label <- names(cvdata.cols)[cvdata.cols == column]
  end_date <- max(df$Date)
  
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
    coord_cartesian(xlim=c(start_date.adj, end_date)) +
    scale_x_date(breaks=breaks, date_labels = '%m/%d', date_minor_breaks='1 day') +
    theme.default +
    theme(legend.position=c(0.1, 0.9)) 
  
  if (show.all) {
    g <- g + aes(Date, value) +
      geom_text(data = ~tail(.x, 1), 
                aes(label = paste(value.label, value.label)),
                size=8,
                color='red',
                hjust='right',
                nudge_x = -2) 
  } else {
    g <- g + aes(Date, value, color=State) 
  }
  
  if (scale == 'log') {
    g <- g + scale_y_log10(labels=comma) + labs(subtitle = 'Log Scale') 
  } else {
    g <- g + scale_y_continuous(labels=comma)
  }
  g+
    ylab(NULL) +
    xlab(NULL)
}
