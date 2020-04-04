library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_light())

cvdata <- readRDS('data/cvdata.us.by_state.RDS')

# Define server logic required to draw the metric plots
shinyServer(function(input, output) {

  output$charts <- renderPlot({
      
      data <- cvdata %>%
        filter(State %in% input$states) %>%
        mutate(State=fct_drop(State)) %>%
        arrange(State, Date) %>%
        filter( Confirmed > 0)
      
      data[, 'value'] <- data[,input$content]
        
      start_date <- min(data$Date)
      end_date <- max(data$Date)
      breaks <- seq(end_date,
                    start_date,
                    by="-1 week") %>% rev()
      if (breaks[1] != start_date) {
        breaks <- c(breaks[1]-days(7), breaks)
        start_date <- breaks[1]
      }
      g <- ggplot(data) +
        aes(Date, value, color=State) + 
        geom_line() + 
        ggtitle(str_c('COVID-19 ', input$content, ', by State')) +
        xlab(NULL) +
        coord_cartesian(xlim=c(start_date, end_date)) +
        scale_x_date(breaks=breaks, date_labels = '%m/%d', date_minor_breaks='1 day')
      
      if (input$scale == 'log') {
        g + scale_y_log10(labels=comma) + labs(subtitle = 'Log Scale') 
      } else {
        g + scale_y_continuous()
      }
      
        
    })
})

