# Extracted from test-lambda-deploy.R:144

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "r2lambda", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
fake_lambda_service <- function(state = "Active", invoke = NULL) {
  list(
    get_function = function(FunctionName) {
      list(Configuration = list(State = state))
    },
    invoke = if (is.null(invoke)) {
      function(...) list(Payload = charToRaw('{"parity":"odd"}'))
    } else {
      invoke
    }
  )
}

# test -------------------------------------------------------------------------
expect_error(
  deploy_lambda("f", policy_arns = "arn:p", role_arn = "arn:r"),
  "not both"
)
