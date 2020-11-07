cvdata.cols <- c('Cases' = 'Cases', 
                 'Cases Per 100K' = 'Cases.Per100K',
                 'New Cases' = 'Cases.Diff5' ,
                 'New Cases Per 100K' = 'Cases.Diff5.Per100K',
                 'Deaths' = 'Deaths',
                 'Deaths Per 100K' = 'Deaths.Per100K',
                 'New Deaths' = 'Deaths.Diff5',
                 'New Deaths Per 100K' = 'Deaths.Diff5.Per100K',
                 'Weekly Population Test Rate' = 'Testing.Rate.Weekly',
                 'Total Population Test Rate' = 'Testing.Rate.Total',
                 'Weekly Positive Test Rate' = 'Testing.Positive.Rate',
                 'Total Positive Rate' = 'Testing.Positive.Rate.Total',
                 'Population In ICU' = 'inIcuCurrently')

states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
            "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
            "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming")

value_formatter <- function(column) {
  if (str_ends(column, 'Growth5') | str_detect(column, 'Rate')) {
    function(value){ percent(value, accuracy=0.1) }
  } else if (column == 'Deaths.Diff5.Per100K') {
    function(value){ format(value, digits=2) }
  } else {
    function(value) { comma(value, accuracy=1) }
  }
}
format_value <- function(column, value) {
  value_formatter(column)(value)
}
