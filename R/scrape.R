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
scrape_element <- function(page, i = NULL, selector = "#tblHealthy > tbody > tr:nth-child({i}) > td.dbfield") {
  
  if (!is.null(i)) {
    selector <- glue::glue(selector, i = i)
  }
  
  x <- page %>%
    rvest::html_nodes(selector) %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href")

  if (length(x) == 0) {
    x <- page %>%
      rvest::html_nodes(selector) %>%
      rvest::html_text()
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

    artifact_link <- scrape_element(page, i = 5)
    
    id <- regmatches(x, gregexpr("[[:digit:]]+", x))[[1]]

    df <- data.frame(id = id,
               title = scrape_element(page, selector = "#tblHealthy > thead > tr > th.top.dbfield"),
               audience = scrape_element(page, i = 1),
               opts = scrape_element(page, i = 2),
               updated = scrape_element(page, i = 3),
               format = scrape_element(page, i = 4),
               artifact_link = artifact_link,
               designer = scrape_element(page, i = 6),
               developer = scrape_element(page, i = 7),
               condition = scrape_element(page, i = 8),
               deliberation_point = scrape_element(page, i = 9),
               language = scrape_element(page, i = 10)
    )

    if(tools::file_ext(artifact_link) == "pdf") {
      download.file(artifact_link, glue::glue("./artifacts/{id}.pdf", id=id), mode="wb")
    }
    
    return(df)

  }) %>% 
    dplyr::bind_rows()
}
