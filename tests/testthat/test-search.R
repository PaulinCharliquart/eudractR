test_that("Test info", {
  expect_snapshot(info("2015-001314-10"))
  expect_null(info("fake_id"))
  expect_null(info("2021-123456-12"))
})

test_that("Test search", {
  expect_snapshot(search("EFC14280"))
  expect_equal(length(search("eeeeeeeeee")), 0)
  expect_equal(length(search("covid", 10)), 10)
})
