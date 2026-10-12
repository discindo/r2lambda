# Extracted from test-lambda-deploy.R:135

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
used_role <- NULL
local_mocked_bindings(
  push_lambda_image = function(tag) "uri",
  create_lambda_exec_role = function(...) stop("should not create a role"),
  create_lambda_function = function(
    tag,
    ecr_image_uri,
    lambda_role_arn,
    ...
  ) {
    used_role <<- lambda_role_arn
    list(FunctionArn = "arn:fn")
  }
)
result <- deploy_lambda("f", role_arn = "arn:aws:iam::1:role/existing")
