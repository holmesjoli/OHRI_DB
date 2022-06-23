library(rvest)

endpoint <- "https://decisionaid.ohri.ca/"

links <- xml2::read_html(glue::glue(endpoint, "AZlist.html")) %>%
  html_nodes("#content") %>%
  html_elements("ul > li > a") %>% 
  html_attr("href")

df <- lapply(links[1:5], function(x) {

  print(x)
  
  page <- xml2::read_html(glue::glue(endpoint, x)) %>%
    html_nodes("#content")
  
  selector <- "#tblHealthy > tbody > tr:nth-child({i}) > td.top.dbfield"

  designer <- page %>%
    html_nodes(glue::glue(selector, i = 6)) %>%
    html_text()

  if (length(designer) == 0) {
    designer <- NA
  }

  data.frame(title = page %>%
               html_nodes("#tblHealthy > thead > tr > th.top.dbfield") %>%
               html_text(),
             opts = page %>%
               html_nodes(glue::glue(selector, i = 2)) %>%
               html_text(),
             updated = page %>%
               html_nodes(glue::glue(selector, i = 3)) %>%
               html_text(),
             format = page %>%
               html_nodes(glue::glue(selector, i = 4)) %>%
               html_text(),
             artifact_link = page %>%
               html_nodes(glue::glue(selector, i = 5)) %>%
               html_elements("a") %>%
               html_attr("href"),
             designer = designer,
             developer = page %>%
               html_nodes(glue::glue(selector, i = 7)) %>%
               html_text(),
             condition = page %>%
               html_nodes(glue::glue(selector, i = 8)) %>%
               html_text(),
             deliberation_point = page %>%
               html_nodes(glue::glue(selector, i = 9)) %>%
               html_text(),
             language = page %>%
               html_nodes(glue::glue(selector, i = 10)) %>%
               html_text()
             )
}) %>% dplyr::bind_rows()
