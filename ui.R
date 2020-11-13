library(shiny)
source('utils/labels.R')

variable_choices <- cvdata.cols

shinyUI(fluidPage(title="COVID-19 Data Console",
                  theme="bootstrap.css",
    # Application title
    titlePanel("Exploring COVID-19 Data"),
    tabsetPanel(
      tabPanel("Testing",
               sidebarLayout(
                 sidebarPanel(width=3,
                              helpText("To change the selected states, edit the field and delete states, or add new ones from the drop down list."),
                              selectInput('states_t', 'States', states, 
                                          selected = c('Florida', 'Texas', 'New York', 'Oregon', 'California', 'Arizona'),
                                          multiple = TRUE,
                                          selectize = TRUE, width = NULL),
                              checkboxInput("all_states_t", "Show National Summary", value=FALSE),
                              helpText("This plot shows the trend of positive test rate as more testing is done.  As a region tests more and more people you would hope the rate of positive tests starts to decline as infection rates decrease.  If the positive test rate continues to increase that would suggest increasing spread.  An ideal chart would show a high per capita test rate and a low positive rate: bottom-right is good.")
                 ),
                 mainPanel(width=9, fluid=T,
                           plotOutput("testing_trends", height="800px"))
               )),
      tabPanel("State Growth Detail", 
               sidebarLayout(
                 sidebarPanel(width=3,
                   helpText("Select the state to display:"),
                   selectInput('state', 'State', states, 
                               selected = 'Oregon',
                               multiple = FALSE,
                               selectize = T, width = NULL),
                   checkboxInput("combined", "Show National Summary", value=FALSE),
                   helpText("Select the data to overlay:"),
                   radioButtons('overlay', 
                                label = NULL, 
                                choices = variable_choices,
                                selected = 'Deaths.Diff5',
                                inline = F),
                   checkboxInput("show_lockdown", "Show Lockdown", TRUE),
                   checkboxInput("show_trend", "Show Trendline", FALSE)
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(width=9,
                           plotOutput("detail_charts", width="100%", height="800px"),
                           htmlOutput('state_status'))
               )),
      tabPanel("State Comparisons",
               sidebarLayout(
                 sidebarPanel(width=3,
                   helpText("Select the content to display, either Confirmed Cases, or Deaths."),
                   radioButtons('content', NULL, cvdata.cols, 'Cases', inline=F),
                   helpText("To change the selected states, edit the field and delete states, or add new ones from the drop down list."),
                   selectInput('states', 'States', states, 
                               selected = c('Florida', 'Texas', 'Oregon', 'California', 'Arizona'),
                               multiple = TRUE,
                               selectize = TRUE, width = NULL),
                   checkboxInput("all_states", "Show National Summary", value=FALSE),
                   helpText("Select the scale for the Y axis.  Log Scale can make it easier to compare states, but can be misleading about the magnitude.  Choose normal to see an accurate perspective."),
                   radioButtons('scale', 'Scale', c('log','normal'), 'normal', inline = T)
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(width=9,
                   fluid=T, plotOutput("comparison_charts", height="800px"))
               )),
      
      tabPanel("About",
               wellPanel(htmlOutput("about")))
    )
))