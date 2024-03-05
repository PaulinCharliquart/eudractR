base_url <- "https://www.clinicaltrialsregister.eu/"


#' Search Studies in Eudract
#'
#' @param query a term to search
#' @param size number of results to return
#'
#' @return a list of results
#' @export
#'
#' @importFrom curl new_handle handle_setopt curl_fetch_memory
#' @examples
#' \dontrun{
#' search_studies("dupilumab")
#' }
search_studies <- function(query, size = NULL) {
  h <- new_handle()
  handle_setopt(h)
  handle_setopt(h, ssl_verifypeer = FALSE)
  url <- paste0(base_url, "ctr-search/search?query=", escape_html(query))
  next_page <- "&page=1"
  ids <- c()
  while (length(next_page) > 0 && !is.na(unlist(next_page)[1])) {
    page_id <- extract_all(next_page, "\\d")
    req <- curl_fetch_memory(paste0(url, "&page=", unlist(page_id)[1]), h)
    text <- rawToChar(req$content)
    next_page <- extract_all(
      text,
      '(?<=href=\\").*?(?=\\"\\saccesskey=\\"n\\">\\s*Next)'
    )
    found <- unlist(extract_all(text, "20\\d{2}-\\d{6}-\\d{2}"))
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
#' @importFrom curl new_handle handle_setopt curl_fetch_memory
#' @examples
#' \dontrun{
#' fetch_study("2015-001314-10")
#' }
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
  h <- new_handle()
  handle_setopt(h, ssl_verifypeer = FALSE)
  url <- paste0(base_url, "ctr-search/search?query=", eudract)
  req <- curl_fetch_memory(url, h)
  text <- rawToChar(req$content)
  full_url <- extract_all(
    text,
    sprintf("ctr-search/trial/%s/[A-Z][A-Z]", eudract)
  )

  if (length(full_url) == 0 || is.na(unlist(full_url)[1])) {
    return(NULL)
  }
  req <- curl_fetch_memory(paste0(base_url, unlist(full_url)[1]), h)
  text <- rawToChar(req$content)
  data <- parse_data(text)

  if (!is.null(cache_file)) {
    write_cache(eudract, data, cache_file)
  }
  data
}
