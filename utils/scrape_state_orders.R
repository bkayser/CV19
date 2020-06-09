library(RCurl)
library(rvest)
library(stringr)
library(tidyr)

# This project will attempt to compile a list of shutdown and reopen events state by state.
# This is the URL where I'm scraping data on state orders for stay at home and reopening.
# I can't load it directly because the charts are all done with D3/js so I have to save it from Chrome directly before loading it here.

scrape_state_orders = function() {
  url <- 'https://www.washingtonpost.com/graphics/2020/national/states-reopening-coronavirus-map/?itid=lk_inline_manual_3&itid=lk_inline_manual_3&tid=lk_inline_manual_3&tid=lk_inline_manual_3'
  
  # So I need to load that page with chrome and then save it as a complete web page, `wapo.hmtl`.
  
  states <- 
    read_html(file("wapo.html"), encoding="text/html", 
              config=list(httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36"))) %>%
    html_nodes("div.state-container")
  
  graph.events <- 
    lapply(states, function(state) {
      name <- html_node(state, "h3") %>% html_text()
      orders <- html_nodes(state, 'g.state-order:not(g.is-hidden)')
      v <- lapply(orders, function(order){
        type <- html_attr(order, "class")
        if (str_detect(type, "stay-at-home-end")) {
          type <- 'open'
        } else {
          type <- 'close'
        }
        date <- as.Date(html_node(order, "text.order-date") %>% html_text() %>% strptime("%B %e"))
        label <- html_nodes(order, "text.order-label")
        desc <- html_text(trim=T, html_nodes(order, "text.order-label"))
        #      desc <- lapply(html_nodes(order, "text.order-label tspan"),
        #                     function(line) { html_text(line) }) %>% paste(collapse=' ')
        tibble(State=name, type=type, date=date, desc=desc)
      }) %>% bind_rows()
    }) %>% bind_rows() %>%
    # Cleanup the text
    mutate(desc = str_replace(desc, "stayat", "stay at") %>%
             str_replace("Beganreopening", "Began reopening") %>%
             str_replace("physicaldistancing", "physical distancing") %>%
             str_replace("shelterin", "shelter in") %>%
             str_replace("healthyat", "healthy at"))
  
  detailed.events <- lapply(states, function(state){
    name <- html_node(state, "h3") %>% html_text()
    gov <- html_node(state, "h3 + div > span ") %>% html_text() %>% str_match("Governor: (.*) \\((.)\\)")
    gov.name <- gov[1,2]
    gov.party <- gov[1,3]
    status <- html_node(state, "span.highlight") %>% html_text()
    open.detail <- NA
    closed.detail <- NA
    for (item in html_nodes(state, "li")) {
      heading <- html_node(item, 'b') %>% html_text()
      if (str_detect(heading, "closed")) {
        closed.detail <- html_text(item)
      } else if (str_detect(heading, "open")) {
        open.detail <- html_text(item)
      }
    }
    tibble(State=name, gov.name=gov.name, gov.party=gov.party, status=status, still_open = open.detail, still_closed = closed.detail)
    
  }) %>% bind_rows()
  
  
  # Save the data.
  
  abbrevs <- read.csv('data/state_abbrev.csv', stringsAsFactors = F) %>% select(State, State.Code=Code)
  saveRDS(left_join(graph.events, abbrevs), "data/orders.events.RDS")
  saveRDS(left_join(detailed.events, abbrevs), "data/states.status.RDS")
}



