library(shiny)

states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
            "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
            "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming")

shinyUI(fluidPage(
    
  
    # Application title
    titlePanel("Exploring COVID-19 Data"),
    
    sidebarLayout(
        sidebarPanel(
          helpText("Select the content to display, either Confirmed Cases, or Deaths."),
          radioButtons('content', NULL, c('Cases', 'Deaths'), 'Cases', inline=T),
          helpText("To change the selected states, edit the field and delete states, or add new ones from the drop down list."),
          selectInput('states', 'States', states, 
                      selected = c('New York', 'Oregon', 'Indiana', 'California', 'Washington', 'Arizona'),
                      multiple = TRUE,
                      selectize = TRUE, width = NULL),
          helpText("Select the scale for the Y axis.  Log Scale can make it easier to compare states, but can be misleading about the magnitude.  Choose normal to see an accurate perspective."),
          radioButtons('scale', 'Scale', c('log','normal'), 'log', inline = T),
          # sliderInput("beta",
            #             "Infection Rate:",
            #             min = 0.02,
            #             max = 0.62,
            #             value = 0.2),
            # sliderInput("gamma",
            #             "Recovery Rate:",
            #             min = 0.1,
            #             max = 1.0,
            #             value = 0.5),

            #h2('Instructions'),
          
          
          ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("charts", width="100%", height="500px")
        )
    )
))