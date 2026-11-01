# Smoke test against a real AWS account.
#
# Builds, tests, deploys, invokes and schedules the parity example, then
# deletes everything with delete_lambda(). It uses the AWS credentials of the
# current session, takes about 10 minutes, and costs a few cents at most.
# Docker must be running. A preflight checks Docker, the AWS credentials and
# the region, and prints the account, before anything is created.
#
# Run from the package root:
#   Rscript dev/smoke-test.R
#   ARCH=arm64 Rscript dev/smoke-test.R
#
# Before cleanup, the function's CloudWatch logs are saved to
# dev/<tag>.log (git-ignored).
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

# All CloudWatch log events of the function, across result pages. `...` goes
# to filter_log_events(), e.g. startTime or filterPattern.
log_events <- function(tag, ...) {
  logs <- aws_connect("cloudwatchlogs")
  events <- list()
  token <- NULL
  repeat {
    page <- logs$filter_log_events(
      logGroupName = paste0("/aws/lambda/", tag),
      nextToken = token,
      ...
    )
    events <- c(events, page$events)
    token <- page$nextToken
    if (length(token) == 0 || !nzchar(token)) break
  }
  events
}

# Writes the function's CloudWatch log events to a file before
# delete_lambda() removes the log group. Never fails, so cleanup still runs.
save_function_logs <- function(tag, path) {
  result <- tryCatch(
    {
      events <- log_events(tag)
      lines <- vapply(
        events,
        function(e) {
          # Format milliseconds by hand: %OS3 truncates them (.123 -> .122).
          time <- sprintf(
            "%s.%03dZ",
            format(
              .POSIXct(e$timestamp %/% 1000, tz = "UTC"),
              "%Y-%m-%d %H:%M:%S"
            ),
            as.integer(e$timestamp %% 1000)
          )
          paste(time, e$logStreamName, trimws(e$message, "right"))
        },
        character(1)
      )
      writeLines(lines, path)
      paste(length(lines), "log events saved to", path)
    },
    error = function(e) {
      paste("Could not save function logs:", conditionMessage(e))
    }
  )
  message(result)
}

# Preflight: fail before anything is created in AWS.
preflight <- function(what, expr) {
  tryCatch(expr, error = function(e) {
    stop("FAILED: ", what, ": ", conditionMessage(e), call. = FALSE)
  })
}

docker_info <- preflight("Docker", {
  sys::exec_internal("docker", "info", error = FALSE)
})
check(docker_info$status == 0, "Docker daemon running")

identity <- preflight("AWS credentials", {
  aws_connect("sts")$get_caller_identity()
})
message(
  "OK: AWS identity ",
  identity$Arn,
  " (account ",
  identity$Account,
  ")"
)

ecr_token <- preflight("AWS region and ECR access", {
  aws_connect("ecr")$get_authorization_token()
})
region <- sub(
  "^.*\\.ecr\\.([^.]+)\\.amazonaws.*$",
  "\\1",
  ecr_token$authorizationData[[1]]$proxyEndpoint
)
message("OK: AWS region ", region)

local({
  on.exit({
    save_function_logs(tag, file.path("dev", paste0(tag, ".log")))
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
    roles_left <- aws_connect("iam")$list_roles(
      PathPrefix = lambda_exec_role_path(tag)
    )$Roles
    check(length(roles_left) == 0, "no roles left")
  })

  # A scheduled run calls the handler with no arguments, so give `number` a
  # default.
  runtime_path <- file.path(tempdir(), "parity.R")
  writeLines(
    sub(
      "function(number)",
      "function(number = 2)",
      readLines(system.file("parity.R", package = "r2lambda")),
      fixed = TRUE
    ),
    runtime_path
  )
  check(
    any(grepl("function(number = 2)", readLines(runtime_path), fixed = TRUE)),
    "runtime has a default for number"
  )
  build_lambda(
    tag = tag,
    runtime_function = "parity",
    runtime_path = runtime_path,
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

  schedule_lambda(tag, "rate(1 minute)")
  started_at <- as.numeric(Sys.time()) * 1000
  scheduled_events <- function(pattern) {
    log_events(tag, startTime = started_at, filterPattern = pattern)
  }
  # REPORT is the last line of a finished run, whether it failed or not. The
  # log group may not exist yet, so a failed query means "not yet".
  wait_until(
    function() {
      length(tryCatch(scheduled_events("REPORT"), error = function(e) NULL)) > 0
    },
    "scheduled invocation ran",
    timeout = 240,
    every = 30
  )
  # lambdr logs a failed run's error with logger::log_error().
  errors <- scheduled_events("ERROR")
  for (event in errors) {
    message(event$message)
  }
  check(length(errors) == 0, "scheduled invocation succeeded")
})

message("Smoke test passed.")
