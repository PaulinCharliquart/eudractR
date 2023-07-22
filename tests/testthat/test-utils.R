test_that("Test validate_id", {
  expect_false(validate_id("eekzpoe"))
  expect_true(validate_id("2015-001314-10"))
  expect_false(validate_id("2099-001314-10"))
})
