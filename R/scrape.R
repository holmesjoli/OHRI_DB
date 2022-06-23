library(magrittr)

#' @title Scrape links
#' @description Returns a list of links from the OHRI database
#' @param endpoint the OHRI endpoint
#' @return list
scrape_links <- function(endpoint) {
  xml2::read_html(glue::glue(endpoint, "AZlist.html")) %>%
    rvest::html_nodes("#content") %>%
    rvest::html_elements("ul > li > a") %>% 
    rvest::html_attr("href")
}

#' @title Scrape element
#' @description Returns the text from a specific element in the OHRI database
#' @return string
scrape_element <- function(page, i = NULL, selector = "#tblHealthy > tbody > tr:nth-child({i}) > td.top.dbfield") {

  if (!is.null(i)) {
    selector <- glue::glue(selector, i) 
  }

  x <- page %>%
    rvest::html_node(selector) %>%
    rvest::html_text()

  if (length(x) == 0) {
    x <- page %>%
      rvest::html_nodes(selector) %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href")
  }

  return(x)
}

#' @title Scrape data
#' @description Returns the data from each entry in the OHRI database
#' @param endpoint the OHRI endpoint
#' @param links a list of the links to iterate through
#' @return data.frame
scrape_data <- function(endpoint, links) {
  lapply(links, function(x) {
    
    page <- xml2::read_html(glue::glue(endpoint, x)) %>%
      rvest::html_nodes("#content")
    
    data.frame(title = scrape_element(page, selector = "#tblHealthy > thead > tr > th.top.dbfield"),
               opts = scrape_element(page, 2),
               updated = scrape_element(page, 3),
               format = scrape_element(page, 4),
               artifact_link = scrape_element(page, 5),
               designer = scrape_element(page, 6),
               developer = scrape_element(page, 7),
               condition = scrape_element(page, 8),
               deliberation_point = scrape_element(page, 9),
               language = scrape_element(page, 10)
    )
  }) %>% 
    dplyr::bind_rows()
}
