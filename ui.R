library(shiny)
source('labels.R')

states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
            "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
            "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming")

shinyUI(fluidPage(title="COVID-19 Data Console",
    
    # Application title
    titlePanel("Exploring COVID-19 Data"),
    tabsetPanel(
      tabPanel("State Growth Detail", 
               sidebarLayout(
                 sidebarPanel(width=3,
                   helpText("Select the state to display:"),
                   selectInput('state', 'State', states, 
                               selected = 'Oregon',
                               multiple = FALSE,
                               selectize = T, width = NULL),
                   helpText("Select the data to overlay:"),
                   radioButtons('overlay', 
                                label = NULL, 
                                choices = cvdata.cols,
                                selected = 'Cases',
                                inline = F),
                   checkboxInput("show_lockdown", "Show Lockdown", TRUE),
                   checkboxInput("show_trend", "Show Trendline", FALSE)
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(width=9,
                   plotOutput("detail_charts", width="100%", height="800px"))
               )),
      tabPanel("State Comparisons",
               sidebarLayout(
                 sidebarPanel(width=3,
                   helpText("Select the content to display, either Confirmed Cases, or Deaths."),
                   radioButtons('content', NULL, cvdata.cols, 'Cases', inline=F),
                   helpText("To change the selected states, edit the field and delete states, or add new ones from the drop down list."),
                   selectInput('states', 'States', states, 
                               selected = c('New York', 'Oregon', 'California', 'Arizona'),
                               multiple = TRUE,
                               selectize = TRUE, width = NULL),
                   checkboxInput("all_states", "Show National Summary", value=FALSE),
                   helpText("Select the scale for the Y axis.  Log Scale can make it easier to compare states, but can be misleading about the magnitude.  Choose normal to see an accurate perspective."),
                   radioButtons('scale', 'Scale', c('log','normal'), 'normal', inline = T)
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(width=9,
                   fluid=T, plotOutput("comparison_charts", height="800px"))
               ))
    )
))