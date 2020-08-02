library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
source('utils/labels.R')
source('utils/utils.R')
source('plots/comparison_chart.R')
source('plots/testing_trend.R')
source('plots/state_summary_plot.R')

# Define server logic required to draw the metric plots
shinyServer(function(input, output) {


  cvdata.us.by_state <- readRDS('data/cvdata.us.by_state.RDS') 
  state.details <- readRDS('data/states.status.RDS')
  state.orders <- readRDS('data/orders.events.RDS')
  theme.default <- ggthemes::theme_few()
  end_date <- max(cvdata.us.by_state$Date)
  test.timerange <- 90


  output$about <- renderUI({
      tagList(
        HTML(read_file('about.html')),
        tags$h3(paste0("Data reported through ", strftime(end_date, "%A, %B %e, %Y"), "."))
        )
  })
  
  output$testing_trends <- renderPlot({
    testing.trend(cvdata.us.by_state, 
                  timerange = 90,
                  states = input$states_t,
                  show.all = input$all_states_t)
  })
  output$comparison_charts <- renderPlot({
    comparison_chart(cvdata.us.by_state, input$states, input$content, input$all_states, input$scale)
  })
  
  output$state_status <- renderUI({
    if (input$combined) {
      tag("p", "All 50 states with some shutdown orders.")
    } else {
      details <- filter(state.details, State == input$state)
      
      tags$dl(
          class="state_details",
               tags$dt("Governor"),
               tags$dd(details$gov.name),
               tags$dt("Party"),
               tags$dd(list(details$gov.party, class=details$gov.party)),
               tags$dt("Current Status"),
               tags$dd(details$status),
               tags$dt("Still Open"),
               tags$dd(details$still_open),
               tags$dt("Still Closed"),
               tags$dd(details$still_closed))
          
    }
  })
  
  
  output$detail_charts <- renderPlot({
    state_summary_plot(cvdata.us.by_state,
                       state=input$state,
                       state.orders=state.orders,
                       overlay=input$overlay,
                       show.all=input$combined,
                       show.lockdown=input$show_lockdown,
                       show.trend=input$show_trend)
  })
})

