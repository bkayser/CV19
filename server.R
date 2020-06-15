library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
source('utils/labels.R')

cvdata.us.by_state <- readRDS('data/cvdata.us.by_state.RDS') 
state.orders <- readRDS('data/orders.events.RDS')

theme.default <- 
  theme_tufte() + 
  theme(text = element_text(size=14),
        title = element_text(size=18))

# Define server logic required to draw the metric plots
shinyServer(function(input, output) {

  data <- function(states, show.all=F) {
    if (!show.all | length(states) == 0) {
      cvdata.us.by_state %>%
        filter(State %in% states) %>%
        mutate(State=fct_drop(State)) %>%
        arrange(State, Date) %>%
        filter( Cases > 0)
    } else {
      cvdata.us.by_state %>%
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
                  
                  Testing.Rate = sum(total) / Population,
                  Testing.Positive.Rate = sum(positive) / sum(total),
                  inIcuCurrently = sum(inIcuCurrently, na.rm=T))
    }
  }
  
  start_date <- reactive({
    mdy('02/26/2020')
  })
  end_date <- reactive({
    max(cvdata.us.by_state$Date)
  })
  
  output$comparison_charts <- renderPlot({
    show.all <- input$all_states
    df <- data(input$states, show.all)
    df[, 'value'] <- df[,input$content]
    df[, 'value.label'] <- ifelse(str_ends(input$content, 'Growth5'), percent(df$value), comma(df$value))
    value.label <- names(cvdata.cols)[cvdata.cols == input$content]
    
    start_date.adj <- mdy('03-14-2020')
    breaks <- seq(end_date(),
                  start_date.adj,
                  by="-1 week") %>% rev()
    if (breaks[1] != start_date.adj) {
      breaks <- c(breaks[1]-days(7), breaks)
      start_date.adj <- breaks[1]
    }
    g <- ggplot(df) +
      geom_line() + 
      ggtitle(str_c(value.label, ifelse(show.all, ', All States', ', by State'))) +
      coord_cartesian(xlim=c(start_date.adj, end_date())) +
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
    
    if (input$scale == 'log') {
      g <- g + scale_y_log10(labels=comma) + labs(subtitle = 'Log Scale') 
    } else {
      g <- g + scale_y_continuous(labels=comma)
    }
    g+
      ylab(NULL) +
      xlab(NULL)
  })
  
  output$detail_charts <- renderPlot({
    show.all <- input$combined
    state.data <- data(input$state, show.all) %>% 
      filter(Cases > 0 & Date > start_date()) %>%
      mutate(Daily.Increase=round(Cases.Diff5))
    if (show.all) {
      name <- 'All States'
      orders <- state.orders
    } else {
      name <- input$state
      orders <- filter(state.orders, State==name) 
    } 
    start_date.adj <- max(start_date(), min(state.data$Date))

    date.breaks <- seq(end_date(),
                       start_date.adj,
                       by="-1 week") %>% rev()
    last.recorded.value <- tail(state.data, 1) %>% pull(input$overlay)
    # scale the overlay so it fits in the graph
    if (input$overlay == 'Cases.Diff') {
      scale.factor <- 1
    } else {
      scale.factor <- 1.1 * max(state.data$Cases.Diff5, na.rm=T) / max(state.data[input$overlay], na.rm=T)
    }
    state.data$Overlay <- unlist(state.data[input$overlay]) * scale.factor
    overlay.label <- names(cvdata.cols)[cvdata.cols == input$overlay]
    overlay.format <- ifelse(str_ends(input$overlay, 'Growth5') | str_ends(input$overlay, 'Rate'), percent, comma)
    
    g <- ggplot(state.data) +
      aes(x=Date, y=Cases.Diff5) +
      geom_line(color='#999999') + 
      geom_text(aes(label=Daily.Increase), 
                nudge_y=4,
                check_overlap=T,
                color='black') +
      ggtitle(paste(name, 'Change in Confirmed Cases, Five Day Average'),
              subtitle = str_c("Reported data through ", format(max(cvdata.us.by_state$Date), "%B %d, %Y"))) +
      ylab("Daily Increase") +
      geom_text(data=~ tail(.x, 1), 
                aes(label=paste0(overlay.label, ': ',overlay.format(last.recorded.value))),
                y=last.recorded.value * scale.factor,
                color='red',
                size=6,
                nudge_y=0.1 * last.recorded.value * scale.factor,
                nudge_x=-12,
                show.legend = F) +
      theme.default +
      theme(legend.position='bottom',
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
      
    if (str_ends(input$overlay, "5" )) {
      g <- g + geom_line(aes(y=Overlay), color='#FF3333', alpha=0.6) 
    } else {
      g <- g + geom_bar(aes(y=Overlay), 
                        fill='#FF3333', alpha=0.1, stat='identity')      
    }
    if (input$show_lockdown) {
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
    if (input$show_trend) {
      g <- g + geom_smooth(alpha=0.05, method='loess', color='black', fill='green', fullrange=T, span=0.5, size=0.5, linetype=3) 
    }
    g
  })
})

