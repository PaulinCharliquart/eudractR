#' Validate eudract id
#'
#' @param eudract_id an eudract id to test
#'
#' @return TRUE or FALSE
#' @export
validate_id <- function(eudract_id) {
  if (!grepl("^20\\d{2}-\\d{6}-\\d{2}$", eudract_id)) {
    return(FALSE)
  }
  today_year <- as.integer(format(Sys.Date(), "%Y"))
  eudract_year <- tryCatch(
    as.integer(substr(eudract_id, 1, 4)),
    error = function(e) 0
  )
  eudract_year > 2000 && eudract_year <= today_year
}


#' Parse data
#'
#' @param x a text to parse
#'
#' @return a named list
#'
#' @importFrom xml2 read_html xml_text xml_find_all
#' @noRd
parse_data <- function(x) {
  section <- c(
    "A. PROTOCOL INFORMATION",
    "B. SPONSOR INFORMATION",
    "C. APPLICANT IDENTIFICATION",
    "D. IMP IDENTIFICATION",
    "E. GENERAL INFORMATION ON THE TRIAL",
    "F. POPULATION OF TRIAL SUBJECTS",
    "G. INVESTIGATOR NETWORKS TO BE INVOLVED IN THE TRIAL",
    "N. REVIEW BY THE COMPETENT AUTHORITY OR ETHICS COMMITTEE IN THE COUNTRY CONCERNED",
    "P. END OF TRIAL"
  )
  data <- sapply(section, function(x) NULL)
  text <- read_html(x)
  field_id <- lapply(
    xml_find_all(text, ".//td[@class='first']"), function(x) trimws(xml_text(x))
  )
  field_name <- lapply(
    xml_find_all(text, ".//td[@class='second']"), function(x) trimws(xml_text(x))
  )
  field_value <- lapply(
    xml_find_all(text, ".//td[@class='third']"), function(x) trimws(xml_text(x))
  )
  res <- list()
  for (i in seq_len(length(field_id))) {
    key <- paste(field_id[i], field_name[i], sep = ":")
    res[[key]] <- field_value[i]
  }
  for (k in names(data)) {
    val <- list()
    for (k_child in names(res)) {
      to_test <- sprintf("^%s[.]", substr(k_child, 1, 1))
      if (grepl(to_test, k_child)) {
        val[[k_child]] <- res[[k_child]]
      }
    }
    data[[k]] <- val
  }
  data
}

#' Write cache
#'
#' @param key a key to cache data
#' @param data a object to cache
#' @param cache_file a file to store cached data
#'
#' @return TRUE or FALSE
#' @noRd
write_cache <- function(key, data, cache_file = ".eudract.rds") {
  if (!file.exists(cache_file)) {
    db <- list()
  } else {
    db <- readRDS(cache_file)
  }
  db[[key]] <- data
  saveRDS(db, cache_file)
}

#' Read cache
#'
#' @param key a key to cache data
#' @param cache_file a file to store cached data
#'
#' @return Cached data if exist. Otherwise NULL
#' @noRd
read_cache <- function(key, cache_file = ".eudract.rds") {
  if (!file.exists(cache_file)) {
    return(NULL)
  }
  db <- readRDS(cache_file)
  db[[key]]
}


extract_all <- function(text, pattern) {
  match_positions <- gregexpr(pattern, text, perl = TRUE)
  regmatches(text, match_positions)
}

# from htmltools
escape_html <- local({
  .html_specials <- list(
    `&` = "&amp;",
    `<` = "&lt;",
    `>` = "&gt;"
  )
  .html_specials_pat <- paste(names(.html_specials), collapse = "|")
  .html_specials_attr <- c(
    .html_specials,
    `'` = "&#39;",
    `"` = "&quot;",
    `\r` = "&#13;",
    `\n` = "&#10;"
  )
  .html_specials_pat_attr <- paste(names(.html_specials_attr), collapse = "|")

  function(text, attribute = FALSE) {
    pat <- if (attribute) {
      .html_specials_pat_attr
    } else {
      .html_specials_pat
    }

    text <- enc2utf8(as.character(text))
    # Short circuit in the common case that there's nothing to escape
    if (!any(grepl(pat, text, useBytes = TRUE))) {
      return(trimws(text))
    }

    specials <- if (attribute) {
      .html_specials_attr
    } else {
      .html_specials
    }

    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed = TRUE, useBytes = TRUE)
    }
    Encoding(text) <- "UTF-8"

    return(trimws(text))
  }
})
