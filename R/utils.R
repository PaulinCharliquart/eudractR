validate_id <- function(eudract_id) {
  test_id <- grepl("^20\\d{2}-\\d{6}-\\d{2}$", eudract_id)
  if (!test_id) {
    return(FALSE)
  }
  today_year <- as.integer(format(Sys.Date(), "%Y"))
  eudract_year <- tryCatch(
    as.integer(substr(eudract_id, 1, 4)),
    error = function(e) 0
  )
  eudract_year > 2000 && eudract_year <= today_year
}


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
    html_elements(text, "td.first"), function(x) trimws(html_text(x))
  )
  field_name <- lapply(
    html_elements(text, "td.second"), function(x) trimws(html_text(x))
  )
  field_value <- lapply(
    html_elements(text, "td.third"), function(x) trimws(html_text(x))
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
