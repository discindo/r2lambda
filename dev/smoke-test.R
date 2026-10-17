# Smoke test against a real AWS account.
#
# Builds, tests, deploys, invokes and schedules the parity example, then
# deletes everything with delete_lambda(). It uses the AWS credentials of the
# current session, takes about 10 minutes, and costs a few cents at most.
# Docker must be running.
#
# Run from the package root:
#   Rscript dev/smoke-test.R
#   ARCH=arm64 Rscript dev/smoke-test.R
#
# delete_lambda() runs even when a step fails. If the script is killed before
# it gets there, run delete_lambda() with the tag printed at the start.

devtools::load_all()

architecture <- Sys.getenv("ARCH", "x86_64")
tag <- paste0("r2lambda-smoke-", format(Sys.time(), "%Y%m%d%H%M%S"))
message("Smoke test tag: ", tag)

check <- function(ok, what) {
  if (!isTRUE(ok)) {
    stop("FAILED: ", what, call. = FALSE)
  }
  message("OK: ", what)
}

wait_until <- function(condition, what, timeout = 300, every = 10) {
  deadline <- Sys.time() + timeout
  while (!isTRUE(condition())) {
    if (Sys.time() > deadline) {
      stop("Timed out waiting for ", what, call. = FALSE)
    }
    Sys.sleep(every)
  }
  message("OK: ", what)
}

local({
  on.exit({
    deleted <- delete_lambda(tag)
    str(deleted)
    lambda_gone <- tryCatch(
      {
        aws_connect("lambda")$get_function(FunctionName = tag)
        FALSE
      },
      error = function(e) {
        grepl("ResourceNotFoundException", conditionMessage(e))
      }
    )
    check(lambda_gone, "function deleted")
    check(length(delete_lambda_exec_roles(tag)) == 0, "no roles left")
  })

  build_lambda(
    tag = tag,
    runtime_function = "parity",
    runtime_path = system.file("parity.R", package = "r2lambda"),
    architecture = architecture
  )

  local_response <- test_lambda(tag, list(number = 2))
  check(
    grepl('"even"', rawToChar(local_response$stdout)),
    "local invocation returns even"
  )

  deploy_lambda(tag, Timeout = 30, MemorySize = 512)
  lambda_service <- aws_connect("lambda")
  wait_until(
    function() {
      lambda_service$get_function(FunctionName = tag)$Configuration$State ==
        "Active"
    },
    "function is Active"
  )

  response <- invoke_lambda(
    function_name = tag,
    invocation_type = "RequestResponse",
    payload = list(number = 2),
    include_logs = TRUE
  )
  check(
    identical(rawToChar(response$Payload), '{"parity":"even"}'),
    "deployed invocation returns even"
  )

  # Scheduled invocations send an EventBridge event, which parity() can't
  # take, so they fail. Each attempt still writes a START line to the log.
  schedule_lambda(tag, "rate(1 minute)")
  logs <- aws_connect("cloudwatchlogs")
  started_at <- as.numeric(Sys.time()) * 1000
  wait_until(
    function() {
      events <- tryCatch(
        logs$filter_log_events(
          logGroupName = paste0("/aws/lambda/", tag),
          startTime = started_at,
          filterPattern = "START"
        )$events,
        error = function(e) list()
      )
      length(events) > 0
    },
    "scheduled invocation ran",
    timeout = 240,
    every = 30
  )
})

message("Smoke test passed.")
