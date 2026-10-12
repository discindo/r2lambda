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

test_that("invoke_lambda aborts when the function is not ready", {
  local_mocked_bindings(
    aws_connect = function(service) fake_lambda_service(state = "Pending")
  )
  expect_error(
    invoke_lambda("f", "RequestResponse", list(number = 3)),
    "state `Pending`"
  )
})

test_that("invoke_lambda passes AWS errors through", {
  local_mocked_bindings(
    aws_connect = function(service) {
      fake_lambda_service(invoke = function(...) stop("AccessDenied"))
    }
  )
  expect_error(
    invoke_lambda("f", "RequestResponse", list(number = 3)),
    "AccessDenied"
  )
})

test_that("invoke_lambda sends unboxed JSON and returns the response", {
  sent <- NULL
  local_mocked_bindings(
    aws_connect = function(service) {
      fake_lambda_service(invoke = function(...) {
        sent <<- list(...)
        list(Payload = charToRaw('{"parity":"odd"}'), LogResult = NULL)
      })
    }
  )
  expect_output(
    invoke_lambda(
      "f",
      "RequestResponse",
      list(number = 3),
      include_logs = TRUE
    ),
    "odd"
  )
  expect_equal(as.character(sent$Payload), '{"number":3}')
  expect_equal(sent$LogType, "Tail")
})

test_that("deploy_lambda reports the AWS error when function creation fails", {
  local_mocked_bindings(
    push_lambda_image = function(tag) "uri",
    create_lambda_exec_role = function(tag) list(Role = list(Arn = "arn")),
    create_lambda_function = function(...) stop("ResourceConflictException")
  )
  expect_error(deploy_lambda("f"), "ResourceConflictException")
})

test_that("retry_until_role_ready retries only while the role is not ready", {
  calls <- 0
  f <- function() {
    calls <<- calls + 1
    if (calls < 3) {
      stop("The role defined for the function cannot be assumed")
    }
    "ok"
  }
  expect_equal(retry_until_role_ready(f, wait = 0), "ok")
  expect_equal(calls, 3)

  calls <- 0
  g <- function() {
    calls <<- calls + 1
    stop("AccessDenied")
  }
  expect_error(retry_until_role_ready(g, wait = 0), "AccessDenied")
  expect_equal(calls, 1)

  h <- function() stop("cannot be assumed")
  expect_error(retry_until_role_ready(h, tries = 2, wait = 0), "assumed")
})
