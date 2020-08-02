
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
