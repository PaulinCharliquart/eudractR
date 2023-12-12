#' Search Studies in Eudract
#'
#' @param query a text query
#' @param size number of results to return
#'
#' @return a list of results
#' @export
#'
#' @importFrom stringr str_extract_all
#' @importFrom httr2 request req_url_query req_perform resp_body_string
#' @examples
#' search_studies("dupilumab")
search_studies <- function(query, size = NULL) {
  req <- request("https://www.clinicaltrialsregister.eu/ctr-search/search")
  next_page <- "&page=1"
  ids <- c()
  while (length(next_page) > 0) {
    page_id <- str_extract_all(next_page, "\\d")
    text <- req %>%
      req_url_query(query = trimws(query), page = unlist(page_id)[1]) %>%
      req_perform() %>%
      resp_body_string()
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
  lapply(ids, fetch_study)
}


#' Fetch info for a clinical study
#'
#' @param eudract an eudract id
#' @param cache_file RDS file used to cache data
#'
#' @return a list of results
#' @export
#'
#' @importFrom stringr str_extract_all
#' @importFrom httr2 request req_url_path req_perform resp_body_string
#' @examples
#' fetch_study("2015-001314-10")
fetch_study <- function(eudract, cache_file = NULL) {
  if (!validate_id(eudract_id = eudract)) {
    return(NULL)
  }
  if (!is.null(cache_file)) {
    cached_data <- read_cache(eudract, cache_file)
    if (!is.null(cached_data)) {
      return(cached_data)
    }
  }
  req <- request("https://www.clinicaltrialsregister.eu/")
  text <- req %>%
    req_url_path("ctr-search/search") %>%
    req_url_query("query" = eudract) %>%
    req_perform() %>%
    resp_body_string()

  full_url <- str_extract_all(
    text,
    sprintf("ctr-search/trial/%s/[A-Z][A-Z]", eudract),
    simplify = TRUE
  )

  if (length(full_url) == 0) {
    return(NULL)
  }
  data <- req %>%
    req_url_path(full_url[1]) %>%
    req_perform() %>%
    resp_body_string() %>%
    parse_data()

  if (!is.null(cache_file)) {
    write_cache(eudract, data, cache_file)
  }
  data
}
