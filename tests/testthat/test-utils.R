test_that("Test validate_id", {
  expect_false(validate_id("eekzpoe"))
  expect_true(validate_id("2015-001314-10"))
  expect_false(validate_id("2099-001314-10"))
})


test_that("Test read cache", {
  db <- tempfile()
  expect_null(read_cache("ehehe", cache_file = "hehehehe"))
  cached_data <- list(a = 1, b = 2)
  write_cache("hello", cached_data, cache_file = db)
  expect_equal(read_cache("hello"), cached_data, cache_file = db)
})
