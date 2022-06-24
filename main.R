source("./R/scrape.R")

endpoint <- "https://decisionaid.ohri.ca/"

links <- scrape_links(endpoint)

df <- scrape_data(endpoint, links) %>%
  parse_languages() %>% 
  readr::write_csv("./data/OHRI.csv")
