library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
source('utils/labels.R')

cvdata.us.by_state <- readRDS('data/cvdata.us.by_state.RDS') 
state.orders <- readRDS('data/orders.events.RDS')
state.details <- readRDS('data/states.status.RDS')

# Define server logic required to draw the metric plots
shinyServer(function(input, output) {

  data <- function(states, show.all=F) {
    filter_cvdata(cvdata.us.by_state, states, show.all)
  }
  
  output$comparison_charts <- renderPlot({
    show.all <- input$all_states
    end_date <- max(cvdata.us.by_state$Date)
    
    df <- data(input$states, show.all)
    df[, 'value'] <- df[,input$content]
    df[, 'value.label'] <- overlay.format <- ifelse(str_ends(input$content, 'Growth5') | str_detect(input$content, 'Rate'), 
                                                    percent(df$value), 
                                                    comma(df$value))
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
  
  output$state_status <- renderUI({
    if (input$combined) {
      tag("p", "All 50 states with some shutdown orders.")
    } else {
      details <- filter(state.details, State == input$state)
      
      tag("dl",
          list(class="state_details",
               tag("dt", "Governor"),
               tag("dd", details$gov.name),
               tag("dt", "Party"),
               tag("dd", list(details$gov.party, class=details$gov.party)),
               tag("dt", "Current Status"),
               tag("dd", details$status),
               tag("dt", "Still Open"),
               tag("dd", details$still_open),
               tag("dt", "Still Closed"),
               tag("dd", details$still_closed)))
          
    }
  })
  
  
  output$detail_charts <- renderPlot({
    state_summary_plot(cvdata.us.by_state,
                       input$combined,
                       input$state,
                       input$overlay,
                       input$show_lockdown,
                       input$show_trend)
  })
})

