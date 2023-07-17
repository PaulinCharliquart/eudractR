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
