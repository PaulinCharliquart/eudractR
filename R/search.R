search <- function(query, size = NULL, cache_file = NULL) {
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


info <- function(eudract, cache_file = NULL) {
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
