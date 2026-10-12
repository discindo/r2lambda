test_that("fetch_ecr_repo returns the URI of an existing repository", {
  local_mocked_bindings(
    aws_connect = function(service) {
      list(
        describe_repositories = function(repositoryNames) {
          list(repositories = list(list(repositoryUri = "reg/parity")))
        },
        create_repository = function(...) stop("should not create")
      )
    }
  )
  expect_equal(fetch_ecr_repo("parity"), "reg/parity")
})

test_that("fetch_ecr_repo creates a missing repository", {
  local_mocked_bindings(
    aws_connect = function(service) {
      list(
        describe_repositories = function(repositoryNames) {
          stop("RepositoryNotFoundException (HTTP 400).")
        },
        create_repository = function(repositoryName, ...) {
          list(
            repository = list(repositoryUri = paste0("reg/", repositoryName))
          )
        }
      )
    }
  )
  expect_equal(fetch_ecr_repo("parity"), "reg/parity")
})

test_that("fetch_ecr_repo passes other AWS errors through", {
  local_mocked_bindings(
    aws_connect = function(service) {
      list(describe_repositories = function(...) stop("AccessDenied"))
    }
  )
  expect_error(fetch_ecr_repo("parity"), "AccessDenied")
})

test_that("schedule_lambda aborts when the function does not exist", {
  local_mocked_bindings(
    aws_connect = function(service) {
      list(get_function = function(FunctionName) {
        stop("ResourceNotFoundException")
      })
    }
  )
  expect_error(
    schedule_lambda("missing", "rate(5 minutes)"),
    "Cannot find deployed lambda function missing"
  )
})

test_that("schedule_lambda builds valid rule and statement names", {
  rule <- NULL
  statement <- NULL
  local_mocked_bindings(
    aws_connect = function(service) {
      list(get_function = function(FunctionName) {
        list(Configuration = list(FunctionArn = "arn:fn"))
      })
    },
    create_event_rule_for_schedule = function(rule_name, rate) {
      rule <<- rule_name
      "arn:rule"
    },
    lambda_add_permission_for_schedule = function(
      function_name,
      scheduled_event_name,
      scheduled_rule_arn
    ) {
      statement <<- scheduled_event_name
    },
    add_lambda_to_eventridge = function(rule_name, lambda_function_arn) NULL
  )
  schedule_lambda(
    "a_function_with_a_rather_long_name",
    "cron(0/5 8-17 ? * MON-FRI *)"
  )
  expect_lte(nchar(rule), 64)
  expect_match(rule, "^[.A-Za-z0-9_-]+$")
  expect_match(statement, "^[A-Za-z0-9_-]+$")
})

test_that("test_lambda finds the image by tag and removes the container", {
  removed <- NULL
  fake_docker <- list(
    image = list(
      list = function() {
        data.frame(repo_tags = I(list("reg.example/parity:latest", NA)))
      }
    ),
    container = list(
      run = function(...) NULL,
      remove = function(id, force) {
        removed <<- id
      }
    )
  )
  local_mocked_bindings(
    docker_client = function(...) fake_docker,
    .package = "stevedore"
  )
  local_mocked_bindings(
    exec_internal = function(cmd, args) {
      list(stdout = charToRaw('{"parity":"even"}'), stderr = raw())
    },
    .package = "sys"
  )
  expect_output(test_lambda("parity", list(number = 2)), "even")
  expect_type(removed, "character")

  expect_error(test_lambda("other", list()), "No image tagged")
})
