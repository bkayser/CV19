library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_light())

cvdata <- readRDS('data/cvdata.us.by_state.RDS')
cvdata.us.by_state <- readRDS('data/cvdata.us.by_state.RDS')
state.orders <- readRDS('data/orders.events.RDS')

# Define server logic required to draw the metric plots
shinyServer(function(input, output) {

  data <- reactive({
    cvdata %>%
      filter(State %in% input$states) %>%
      mutate(State=fct_drop(State)) %>%
      arrange(State, Date) %>%
      filter( Cases > 0)
  })
  output$comparison_charts <- renderPlot({
      df <- data()
      df[, 'value'] <- df[,input$content]
        
      start_date <- min(df$Date)
      end_date <- max(df$Date)
      breaks <- seq(end_date,
                    start_date,
                    by="-1 week") %>% rev()
      if (breaks[1] != start_date) {
        breaks <- c(breaks[1]-days(7), breaks)
        start_date <- breaks[1]
      }
      g <- ggplot(df) +
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
  
  output$detail_charts <- renderPlot({
    name <- input$state
    state.data <- filter(cvdata.us.by_state, State == name) %>%
      mutate(Daily.Increase=round(Cases.Diff5))
    orders <- filter(state.orders, State==name)
    start_date <- mdy('02/26/2020')
    end_date <- max(state.data$Date)
    date.breaks <- seq(end_date,
                       start_date,
                       by="-1 week") %>% rev()
    last.recorded.value <- tail(state.data, 1) %>% pull(input$overlay)
    # scale the overlay so it fits in the graph
    scale.factor <- 1.1 * max(state.data$Cases.Diff5) / last.recorded.value
    state.data$Overlay <- unlist(state.data[input$overlay]) * scale.factor
    
    ggplot(state.data) +
      aes(x=Date, y=Cases.Diff5) +
      geom_line(color='blue') + 
      geom_text(aes(label=Daily.Increase), 
                nudge_y=4,
                check_overlap=T,
                color='black') +
      geom_smooth(alpha=0.05, method='loess', color='blue', fill='blue', fullrange=T, span=0.5, size=0.5, linetype=3) +
      ggtitle(paste(name, 'Change in Confirmed Cases, Five Day Average'),
              subtitle = str_c("Reported data through ", format(max(cvdata.us.by_state$Date), "%B %d, %Y"))) +
      ylab("Daily Increase") +
      geom_text(data=~ tail(.x, 1), 
                aes(label=paste0(input$overlay, ': ',comma(last.recorded.value))),
                y=last.recorded.value * scale.factor,
                color='red',
                size=4,
                nudge_y=0.1 * last.recorded.value * scale.factor,
                nudge_x=-12,
                show.legend = F) +
      geom_vline(data=orders,
                 aes(xintercept=date, color=type),
                 alpha=0.5,
                 size=1, 
                 show.legend = F) +
      geom_text(data=orders,
                aes(x=date,
                    colour=type,
                    y=0.03*last.recorded.value*scale.factor * ifelse(type=='open', 1, -1),
                    label=paste(desc, format(date, '%B %d'))),
                hjust='left',
                nudge_x=1,
                show.legend = F) +
      scale_color_manual(values=c(close='#11AA11', open='#CC6666')) +
      xlab(NULL)  +
      scale_y_continuous(name='Daily Increase', sec.axis = sec_axis(trans = ~./scale.factor, name = input$overlay)) +
      geom_line(aes(y=Overlay), color='red') +
      coord_cartesian(xlim=c(ymd('2020-03-15'), end_date)) +
      scale_x_date(breaks=date.breaks, date_labels = '%m/%d', date_minor_breaks='1 day')
  })
})

