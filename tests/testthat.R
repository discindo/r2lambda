# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(r2lambda)

# tests/testthat/setup.R uses withr to keep tests away from real AWS
# credentials, so the suite can't run without it.
if (requireNamespace("withr", quietly = TRUE)) {
  test_check("r2lambda")
}
