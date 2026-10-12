test_that("tests run without AWS credentials", {
  expect_error(paws.common::locate_credentials(), "credentials")
})
