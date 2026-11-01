not_found <- function(code = "ResourceNotFoundException") {
  function(...) stop(code, " (HTTP 404). Not found.")
}

# Fake AWS services that record each call in `calls`, in order.
fake_services <- function(calls, ...) {
  record <- function(name, value = list()) {
    function(...) {
      calls$log <- c(calls$log, name)
      value
    }
  }
  defaults <- list(
    lambda = list(
      get_function = function(FunctionName) {
        list(Configuration = list(FunctionArn = "arn:fn"))
      },
      delete_function = record("delete_function"),
      list_functions = function(...) {
        list(Functions = list(list(Role = "arn:aws:iam::1:role/other")))
      }
    ),
    eventbridge = list(
      list_rule_names_by_target = function(...) {
        list(RuleNames = list("schedule_rule_rate_1_minute_parity"))
      },
      list_targets_by_rule = function(Rule) {
        list(Targets = list(list(Id = "1", Arn = "arn:fn")))
      },
      remove_targets = record("remove_targets"),
      delete_rule = record("delete_rule")
    ),
    iam = list(
      list_roles = function(...) {
        list(
          Roles = list(list(
            RoleName = "parity--0e4d6c1a-5b2f-4c3d-9e8f-1a2b3c4d5e6f",
            Arn = "arn:aws:iam::1:role/r2lambda/eu-west-1/parity/parity--0e4d"
          )),
          IsTruncated = FALSE
        )
      },
      list_attached_role_policies = function(RoleName) {
        list(AttachedPolicies = list(list(PolicyArn = "arn:policy")))
      },
      detach_role_policy = record("detach_role_policy"),
      list_role_policies = function(RoleName) list(PolicyNames = list()),
      delete_role_policy = record("delete_role_policy"),
      delete_role = record("delete_role")
    ),
    ecr = list(delete_repository = record("delete_repository")),
    cloudwatchlogs = list(delete_log_group = record("delete_log_group"))
  )
  services <- utils::modifyList(defaults, list(...))
  local_mocked_bindings(
    aws_region = function() "eu-west-1",
    .env = parent.frame()
  )
  function(service) services[[service]]
}

local_fake_docker <- function(
  calls,
  images = "reg/parity:latest",
  env = parent.frame()
) {
  local_mocked_bindings(
    has_docker = function() TRUE,
    docker = function(args, ...) {
      if (args[[1]] == "rmi") {
        calls$log <- c(calls$log, "rmi")
        return(character())
      }
      images
    },
    .env = env
  )
}

test_that("delete_lambda deletes every resource, schedules first", {
  calls <- new.env()
  local_mocked_bindings(aws_connect = fake_services(calls))
  local_fake_docker(calls)

  deleted <- delete_lambda("parity")

  expect_equal(
    calls$log,
    c(
      "remove_targets",
      "delete_rule",
      "delete_function",
      "detach_role_policy",
      "delete_role",
      "delete_repository",
      "delete_log_group",
      "rmi"
    )
  )
  expect_equal(deleted$lambda_function, "parity")
  expect_equal(deleted$local_image, "reg/parity:latest")
  expect_equal(deleted$log_group, "/aws/lambda/parity")
})

test_that("delete_lambda keeps the images when asked", {
  calls <- new.env()
  local_mocked_bindings(aws_connect = fake_services(calls))
  local_fake_docker(calls)

  delete_lambda("parity", keep_ecr_image = TRUE, keep_local_image = TRUE)

  expect_false("delete_repository" %in% calls$log)
  expect_false("rmi" %in% calls$log)
})

test_that("delete_lambda skips resources that don't exist", {
  calls <- new.env()
  local_mocked_bindings(
    aws_connect = fake_services(
      calls,
      lambda = list(get_function = not_found()),
      ecr = list(delete_repository = not_found("RepositoryNotFoundException")),
      cloudwatchlogs = list(delete_log_group = not_found())
    )
  )

  deleted <- delete_lambda("parity", keep_local_image = TRUE)

  expect_equal(calls$log, c("detach_role_policy", "delete_role"))
  expect_null(deleted$lambda_function)
  expect_null(deleted$ecr_repository)
  expect_null(deleted$log_group)
})

test_that("delete_lambda passes other AWS errors through", {
  calls <- new.env()
  local_mocked_bindings(
    aws_connect = fake_services(
      calls,
      lambda = list(get_function = function(...) stop("AccessDeniedException"))
    )
  )
  expect_error(delete_lambda("parity"), "AccessDeniedException")
})

test_that("delete_lambda_exec_roles deletes unused roles on the tag's path", {
  role <- function(name) {
    list(RoleName = name, Arn = paste0("arn:aws:iam::1:role/", name))
  }
  pages <- list(
    list(
      Roles = list(role("parity--a"), role("parity--used")),
      IsTruncated = TRUE,
      Marker = "next"
    ),
    list(Roles = list(role("parity--b")), IsTruncated = FALSE)
  )
  function_pages <- list(
    list(
      Functions = list(list(Role = "arn:aws:iam::1:role/other")),
      NextMarker = "next"
    ),
    list(Functions = list(list(Role = "arn:aws:iam::1:role/parity--used")))
  )
  deleted_roles <- character()
  local_mocked_bindings(
    aws_region = function() "eu-west-1",
    aws_connect = function(service) {
      list(
        list_functions = function(Marker = NULL) {
          if (is.null(Marker)) function_pages[[1]] else function_pages[[2]]
        },
        list_roles = function(PathPrefix, Marker = NULL) {
          expect_equal(PathPrefix, "/r2lambda/eu-west-1/parity/")
          if (is.null(Marker)) pages[[1]] else pages[[2]]
        },
        list_attached_role_policies = function(RoleName) list(),
        list_role_policies = function(RoleName) {
          list(PolicyNames = list("inline"))
        },
        delete_role_policy = function(RoleName, PolicyName) {
          expect_false(RoleName %in% deleted_roles)
        },
        delete_role = function(RoleName) {
          deleted_roles <<- c(deleted_roles, RoleName)
        }
      )
    }
  )

  expected <- c("parity--a", "parity--b")
  expect_equal(delete_lambda_exec_roles("parity"), expected)
  expect_equal(deleted_roles, expected)
})

test_that("delete_local_image removes every image for the tag", {
  removed <- NULL
  local_mocked_bindings(
    has_docker = function() TRUE,
    docker = function(args, ...) {
      if (args[[1]] == "rmi") {
        removed <<- args[-1]
        return(character())
      }
      c("a/parity:latest", "b/parity:latest", "a/other:latest")
    }
  )
  expect_equal(
    delete_local_image("parity"),
    c("a/parity:latest", "b/parity:latest")
  )
  expect_equal(removed, c("a/parity:latest", "b/parity:latest"))
})

test_that("delete_local_image warns instead of failing when docker fails", {
  local_mocked_bindings(
    has_docker = function() TRUE,
    docker = function(args, ...) stop("Cannot connect to the Docker daemon")
  )
  expect_warning(
    expect_null(delete_local_image("parity")),
    "Failed to delete the local Docker image"
  )
})

test_that("remove_lambda_schedules keeps rules with other targets", {
  removed <- list()
  deleted_rules <- character()
  local_mocked_bindings(
    aws_connect = function(service) {
      list(
        list_rule_names_by_target = function(TargetArn, NextToken = NULL) {
          if (is.null(NextToken)) {
            list(RuleNames = list("schedule_rule_shared"), NextToken = "next")
          } else {
            list(RuleNames = list("my_rule"), NextToken = character())
          }
        },
        list_targets_by_rule = function(Rule) {
          if (Rule == "schedule_rule_shared") {
            list(
              Targets = list(
                list(Id = "1", Arn = "arn:fn"),
                list(Id = "2", Arn = "arn:other")
              )
            )
          } else {
            list(Targets = list(list(Id = "a", Arn = "arn:fn")))
          }
        },
        remove_targets = function(Rule, Ids) {
          removed[[Rule]] <<- Ids
        },
        delete_rule = function(Name) {
          deleted_rules <<- c(deleted_rules, Name)
        }
      )
    }
  )

  expect_equal(remove_lambda_schedules("arn:fn"), character())
  expect_equal(
    removed,
    list(schedule_rule_shared = list("1"), my_rule = list("a"))
  )
  expect_equal(deleted_rules, character())
})
