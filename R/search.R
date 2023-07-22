search <- function(query, size = NULL, cache_file = NULL) {
  url <- "https://www.clinicaltrialsregister.eu/ctr-search/search"
  next_page <- "&page=1"
  ids <- c()
  while (length(next_page) > 0) {
    page_id <- str_extract_all(next_page, "\\d")
    r <- GET(
      url,
      query = list("query" = query, "page" = unlist(page_id)[1])
    )
    text <- content(r, as = "text")
    next_page <- str_extract_all(
      text,
      '(?<=href=\\").*?(?=\\"\\saccesskey=\\"n\\">\\s*Next)'
    )
    found <- unlist(str_extract_all(text, "20\\d{2}-\\d{6}-\\d{2}"))
    ids <- c(ids, unique(found))
    if (!is.null(size) && length(ids) >= size) {
      ids <- ids[size]
      break
    }
    data <- ids
    data
  }
}


info <- function(eudract_id, cache_file=NULL) {
      url <- "https://www.clinicaltrialsregister.eu/ctr-search/search"

    if (!validate_id(eudract_id=eudract)) {
        return(NULL)
    }
    r <- GET(url, query=list("query"= eudract))
    full_url <- str_extract_all(
                    r"ctr-search/trial/{}/[A-Z][A-Z]".format(eudract), r.text
                )
    r.raise_for_status
                if len(full_url) == 0:
                    return None
}
