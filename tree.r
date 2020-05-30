
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

build.hierarchy <- function() {
  cvdata.counties <- readRDS('data/cvdata.us.RDS')
  cvdata.states <- readRDS('data/cvdata.us.by_state.RDS')
  # First make states
  
  latest <- max(cvdata.states$Date)
  latest.growth <- sym(paste0('Growth_', ymd(latest)))
  latest.cases <- sym(paste0('Cases_', ymd(latest)))
  counties <- cvdata.counties %>%
    rename(Growth=Cases.Growth5) %>%
    pivot_wider(id_cols = c('FIPS', 'County', 'State.Code'),
                names_from = 'Date',
                values_from = c('Cases','Growth')) %>%
    mutate(Cases = !!latest.cases,
           Growth = !!latest.growth,
           id = paste0("FIPS", FIPS),
           State.Code = as.character(State.Code),
           County = as.character(County)) %>% 
    rename(parent=State.Code, name=County)
  
  states <- cvdata.states %>%
    mutate(State=as.character(State), 
           State.Code=as.character(State.Code), 
           parent = paste0('Region', as.integer(Division))) %>%
    select(name=State, id=State.Code, parent) %>%
    unique()

    # #    
#   states <- cvdata.states %>%
#     rename(Growth=Cases.Growth5) %>%
#     pivot_wider(id_cols = c('State','State.Code', 'Division'),
#                 names_from = 'Date',
#                 values_from = c('Cases','Growth')) %>%
#     mutate(parent = paste0('Region', as.integer(Division)),
#            Cases=!!latest.cases,
#            Growth=!!latest.growth,
#            State.Code = as.character(State.Code),
#            State = as.character(State)) %>%
#     rename(id=State.Code, name=State) %>%
#     select(-Division)
  
  regions <- cvdata.states %>%
    select(name = Division) %>%
    unique() %>%
    mutate(id = paste0('Region', as.integer(name)),
           parent = 'flare', 
           name = as.character(name)) 
  
  bind_rows(tibble(name='flare', id='flare', parent=''),
            regions,
            states,
            counties) %>%
    select(id, name, parent, Cases, Growth, everything())
}

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



# write(concat(latest), "cviz/dist/sample.json")
write.csv(build.hierarchy(), "cviz/dist/covid.csv", na="0", row.names = F)
