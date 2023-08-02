#' Search Studies in Eudract
#'
#' @param query a text query
#' @param size number of results to return
#'
#' @return a list of results
#' @export
#'
#' @importFrom stringr str_extract_all
#' @importFrom httr GET content
#' @examples
#' search_studies("covid")
search_studies <- function(query, size = NULL) {
  httr::set_config(config(ssl_verifypeer = 0L))
  url <- "https://www.clinicaltrialsregister.eu/ctr-search/search"
  next_page <- "&page=1"
  ids <- c()
  while (length(next_page) > 0) {
    page_id <- str_extract_all(next_page, "\\d")
    r <- GET(
      url,
      query = list("query" = trimws(query), "page" = unlist(page_id)[1])
    )
    text <- content(r, as = "text")
    next_page <- str_extract_all(
      text,
      '(?<=href=\\").*?(?=\\"\\saccesskey=\\"n\\">\\s*Next)',
      simplify = TRUE
    )
    found <- unlist(str_extract_all(text, "20\\d{2}-\\d{6}-\\d{2}"))
    ids <- c(ids, unique(found))
    if (!is.null(size) && length(ids) >= size) {
      ids <- ids[1:size]
      break
    }
  }
  lapply(ids, info)
}


#' Get info for 1 study
#'
#' @param eudract an eudract id
#'
#' @return a list of results
#' @export
#'
#' @importFrom stringr str_extract_all
#' @importFrom httr GET content parse_url build_url
#' @examples
#' info("2015-001314-10")
info <- function(eudract) {
  httr::set_config(config(ssl_verifypeer = 0L))
  url <- parse_url("https://www.clinicaltrialsregister.eu/")

  if (!validate_id(eudract_id = eudract)) {
    return(NULL)
  }
  url$path <- "ctr-search/search"
  r <- GET(build_url(url), query = list("query" = eudract))
  text <- content(r, as = "text")

  full_url <- str_extract_all(
    text,
    sprintf("ctr-search/trial/%s/[A-Z][A-Z]", eudract),
    simplify = TRUE
  )

  if (length(full_url) == 0) {
    return(NULL)
  }
  url$path <- full_url[1]
  r_full <- GET(build_url(url))
  parse_data(r_full)
}
