
library(tidyverse)
library(scales)
library(caTools)

cvdata.us <- readRDS('data/cvdata.us.RDS')

# Create a nestedn hierarchy of node structures:
#
# { 
#    name: "name",
#    children: [
#      node,
#      node,
#      ...
#    ]
#  }
# 
# {
#    name: "name", value: "value"
# }
#
#
#  Division => State => County => [ Cases, Deaths, Cases.Per100K, Deaths.Per100K ]
#
latest <- filter(cvdata.us, Date == max(cvdata.us$Date)) %>% arrange(Division, State, County) 

concat <- function(df) {
  paste0('{ "name": "flare", "children": [ \n',
         split(df, fct_drop(df$Division)) %>%
           lapply(function(division) { 
             paste0(' { "name": "', division$Division[1],'", "children": [\n',
                    split(division, fct_drop(division$State)) %>%
                      lapply(function(state){
                        paste0('{"name": "', state$State[1], '", "children": [\n',
                               paste0('{"name": "', state$County,
                                      '","value": ', state$Cases, 
                                      ',"growth": ', pmax(0,state$Cases.Growth5),
                                      '}',
                                      collapse=", "),
                               '\n]}')
                      }) %>% str_c(collapse=","),
                    ']}')}) %>% str_c(collapse=","),
         '\n]}\n')
}

write(concat(latest), "cviz/dist/sample.json")