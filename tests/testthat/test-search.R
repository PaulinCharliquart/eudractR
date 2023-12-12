test_that("Test fetch_study", {
  expect_snapshot(fetch_study("2015-001314-10"))
  expect_identical(fetch_study("fake_id"), NULL)
  expect_null(fetch_study("2021-123456-12"))
  db <- tempfile()
  a <- fetch_study("2015-001314-10", db)
  expect_true(!is.null(read_cache("2015-001314-10", db)))
  expect_equal(a, read_cache("2015-001314-10", db))
})

test_that("Test search_studies", {
  expect_snapshot(search_studies("EFC14280"))
  expect_equal(length(search_studies("eeeeeeeeee")), 0)
  expect_equal(length(search_studies("covid", 10)), 10)
})
