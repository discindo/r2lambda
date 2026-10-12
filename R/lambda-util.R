is_docker_installed <- function() {
  checkmate::assert_true(nzchar(Sys.which("docker")))
}

#' check_system_deps
#' @noRd
check_system_dependencies <- function() {
  logger::log_debug(
    "[check_system_dependencies] Checking if `docker` is installed."
  )
  is_docker_installed()

  logger::log_debug("[check_system_dependencies] Done.")
}

#' connect to an aws service
#' @param service character, the name of a service, e.g., "lambda" or "iam".
#' Should be a function exported by `paws` (see `getNamespaceExports("paws")`)
#' @examples
#' \dontrun{
#' aws_connect("lambda")
#' }
#' @importFrom glue glue glue_collapse single_quote double_quote
#' @export
aws_connect <- function(service) {
  logger::log_debug("[aws_connect] Checking requested service.")

  if (!service %in% getNamespaceExports("paws")) {
    msg <- glue(
      "The service `{service}` does not appear to be available in `paws`."
    )
    logger::log_error(msg)
    rlang::abort(msg)
  }

  logger::log_debug("[aws_connect] Connecting to AWS.")

  .service <- get(service, envir = asNamespace("paws"))
  .service()
}

#' install_deps_line
#' @noRd
install_deps_line <- function(deps) {
  checkmate::assert_character(deps)

  repo <- single_quote(
    "https://packagemanager.posit.co/cran/__linux__/rhel9/latest"
  )
  glued <- glue_collapse(single_quote(deps), sep = ", ")
  glue('RUN Rscript -e "install.packages(c({glued}), repos = {repo})"')
}

#' runtime_line
#' @noRd
runtime_line <- function(runtime) {
  checkmate::assert_character(runtime)
  rt <- double_quote(runtime)
  glue("CMD [{rt}]")
}

#' support script line
#' @noRd
support_line <- function(support) {
  checkmate::assert_character(support)
  glue("COPY {support} {support}")
}

#' renv_line
#' @noRd
renv_line <- function(renvlock_path) {
  checkmate::assert_character(renvlock_path)
  cmd1 <- glue("COPY {renvlock_path} renv.lock")
  cmd2 <- "RUN Rscript -e 'renv::restore()'"
  glue_collapse(c(cmd1, cmd2), sep = "\n")
}

#' parse password from ecr token
#' @noRd
parse_password <- function(ecr_token) {
  ecr_token |>
    jsonlite::base64_dec() |>
    rawToChar() |>
    gsub(pattern = "^AWS:", replacement = "")
}

#' Run a docker CLI command
#'
#' @param args character vector of arguments to `docker`
#' @param std_in path to a file to send to standard input
#' @param capture logical, return standard output as lines instead of
#' printing it
#' @noRd
docker <- function(args, std_in = NULL, capture = FALSE) {
  if (capture) {
    result <- sys::exec_internal("docker", args, std_in = std_in, error = FALSE)
    status <- result$status
  } else {
    status <- sys::exec_wait("docker", args, std_in = std_in)
  }
  if (status != 0) {
    msg <- glue("`docker {args[[1]]}` failed with exit status {status}.")
    if (capture) {
      msg <- paste(msg, rawToChar(result$stderr))
    }
    logger::log_error(msg)
    rlang::abort(msg)
  }
  if (capture) {
    out <- rawToChar(result$stdout)
    return(strsplit(out, "\n")[[1]])
  }
  invisible(NULL)
}

#' create_lambda_dockerfile
#'
#' @param folder path to store the Dockerfile
#' @param runtime_function name of the runtime function
#' @param runtime_path path to the script containing the runtime function
#' @param support_path path to the supporting code (if any). Can be `NULL`
#' (default) or a character vector of paths to scripts.
#' @param renvlock_path path to the renv.lock file (if any)
#' @param dependencies list of dependencies (if any)
#'
#' @details Use either `renvlock_path` or `dependencies` to install required
#' packages, not both. By default, both are `NULL`, so the Docker image will
#' have no additional packages installed.
#'
#' @examples
#' \dontrun{
#' runtime_function <- "parity"
#' runtime_path <- system.file("parity.R", package = "r2lambda")
#' folder <- "~/Desktop/parity-lambda"
#' dependencies <- NULL
#'
#' create_lambda_dockerfile(
#'   folder = folder,
#'   runtime_function = runtime_function,
#'   runtime_path = runtime_path,
#'   dependencies = dependencies
#' )
#' create_lambda_image(folder, tag = "test-tag")
#' dir.exists(folder)
#' dir(folder)
#' unlink(folder, recursive = TRUE)
#' }
#' @noRd
create_lambda_dockerfile <-
  function(
    folder,
    runtime_function,
    runtime_path,
    support_path = NULL,
    renvlock_path = NULL,
    dependencies = NULL
  ) {
    logger::log_debug("[create_lambda_dockerfile] Validating inputs.")

    checkmate::assert_character(
      x = folder,
      min.chars = 1,
      len = 1
    )

    if (checkmate::test_directory_exists(folder)) {
      msg <- glue(
        "[create_lambda_dockerfile] Directory {folder} exists. Please choose another name." # nolint
      )
      logger::log_error(msg)
      rlang::abort(msg)
    }

    checkmate::assert_character(
      x = runtime_function,
      min.chars = 1,
      len = 1,
      any.missing = FALSE
    )

    checkmate::assert_character(
      x = runtime_path,
      min.chars = 1,
      any.missing = FALSE
    )

    if (!checkmate::test_file_exists(runtime_path)) {
      msg <- glue(
        "[create_lambda_dockerfile] Can't access runtime script {runtime_path}."
      )
      logger::log_error(msg)
      rlang::abort(msg)
    }

    checkmate::assert_character(
      x = support_path,
      min.chars = 1,
      null.ok = TRUE
    )

    if (!is.null(support_path)) {
      lapply(support_path, function(x) {
        if (!checkmate::test_file_exists(x)) {
          msg <- glue(
            "[create_lambda_dockerfile] Can't access support script {x}."
          )
          logger::log_error(msg)
          rlang::abort(msg)
        }
      })
    }

    checkmate::assert_character(
      x = dependencies,
      min.chars = 1,
      null.ok = TRUE
    )

    checkmate::assert_character(
      x = renvlock_path,
      min.chars = 1,
      null.ok = TRUE
    )

    if (!is.null(renvlock_path)) {
      if (!checkmate::test_file_exists(renvlock_path)) {
        msg <- glue(
          "[create_lambda_dockerfile] Can't access renv.lock file {renvlock_path}." # nolint
        )
        logger::log_error(msg)
        rlang::abort(msg)
      }
    }

    # Can't use both dependencies and renvlock
    if (!is.null(renvlock_path) && !is.null(dependencies)) {
      msg <- "[create_lambda_dockerfile] Can't use both dependencies and renv."
      logger::log_error(msg)
      rlang::abort(msg)
    }

    logger::log_debug(
      "[create_lambda_dockerfile] Creating directory with Dockerfile and runtime script." # nolint
    )

    docker_template <- system.file(
      "lambdr_dockerfile.txt",
      package = "r2lambda"
    )

    dir.create(folder)
    dir.exists(folder)

    file.copy(runtime_path, folder)
    file.rename(
      from = file.path(folder, basename(runtime_path)),
      to = file.path(folder, "runtime.R")
    )

    if (!is.null(support_path)) {
      lapply(support_path, function(x) {
        file.copy(x, folder)
      })
    }

    file.copy(docker_template, folder, recursive = FALSE)
    file.rename(
      from = file.path(folder, basename(docker_template)),
      to = file.path(folder, "Dockerfile")
    )

    logger::log_debug(
      "[create_lambda_dockerfile] Updating Dockerfile with dependencies and runtime info." # nolint
    )

    if (!is.null(dependencies)) {
      deps_string <- install_deps_line(deps = c(dependencies))
      write(deps_string, file = file.path(folder, "Dockerfile"), append = TRUE)
    }

    if (!is.null(renvlock_path)) {
      file.copy(renvlock_path, folder)
      renv_string <- renv_line(basename(renvlock_path))
      write(renv_string, file = file.path(folder, "Dockerfile"), append = TRUE)
    }

    runtime_string <- runtime_line(runtime_function)

    write(runtime_string, file = file.path(folder, "Dockerfile"), append = TRUE)

    if (!is.null(support_path)) {
      lapply(support_path, function(x) {
        support_string <- support_line(basename(x))
        write(
          support_string,
          file = file.path(folder, "Dockerfile"),
          append = TRUE
        )
      })
    }

    logger::log_debug("[create_lambda_dockerfile] Done.")
  }

#' Fetch or create ecr repo
#'
#' @param tag a tag for the image
#' @noRd
fetch_ecr_repo <- function(tag) {
  logger::log_debug("[fetch_ecr_repo] Checking if repository already exists.")
  ecr_service <- aws_connect("ecr")

  repos <- tryCatch(
    ecr_service$describe_repositories(repositoryNames = tag)$repositories,
    error = function(e) {
      if (!grepl("RepositoryNotFoundException", conditionMessage(e))) {
        stop(e)
      }
      list()
    }
  )

  if (length(repos) > 0) {
    logger::log_debug("[fetch_ecr_repo] Repository exists. Fetching URI.")
    repo_uri <- repos[[1]]$repositoryUri
  } else {
    logger::log_debug(
      "[fetch_ecr_repo] Creating new repository and fetching URI."
    )
    repo_meta <- ecr_service$create_repository(
      repositoryName = tag,
      imageScanningConfiguration = list(scanOnPush = TRUE)
    )
    repo_uri <- repo_meta$repository$repositoryUri
  }
  logger::log_debug("[fetch_ecr_repo] Done.")
  return(repo_uri)
}

#' create_lambda_container
#'
#' @param folder path to the folder containing the lambda runtime
#' script and Dockerfile
#' @param tag a tag for the image
#' @noRd
create_lambda_image <- function(folder, tag) {
  logger::log_debug("[create_lambda_image] Validating inputs.")
  checkmate::assert_character(tag)
  checkmate::assert_character(folder)
  checkmate::assert_directory_exists(folder)

  checkmate::assert_file_exists(file.path(folder, "Dockerfile"))
  checkmate::assert_file_exists(file.path(folder, "runtime.R"))

  logger::log_debug(
    "[create_lambda_image] Confirming that files are not empty."
  )
  checkmate::assert_numeric(
    x = file.size(file.path(folder, "Dockerfile")),
    lower = 1,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    .var.name = "Check if file is empty."
  )

  checkmate::assert_numeric(
    x = file.size(file.path(folder, "runtime.R")),
    lower = 1,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    .var.name = "Check if file is empty."
  )

  logger::log_debug("[create_lambda_image] Create or fetch a remote tag.")
  repo_uri <- fetch_ecr_repo(tag = tag)

  logger::log_debug("[create_lambda_image] Building docker image.")

  # Lambda runs x86_64 by default and rejects multi-platform image indexes,
  # which buildx creates when it adds provenance attestations.
  docker(c(
    "build",
    "--platform",
    "linux/amd64",
    "--provenance=false",
    "--tag",
    repo_uri,
    folder
  ))

  logger::log_debug("[create_lambda_image] Done.")
}

#' push_lambda_container to AWS ECR
#'
#' @param tag the tag of an existing local image
#' @noRd
push_lambda_image <- function(tag) {
  logger::log_debug("[push_lambda_image] Validating inputs.")
  checkmate::assert_character(tag)

  logger::log_debug("[push_lambda_image] Authenticating Docker with AWS ECR.")

  ecr_service <- aws_connect("ecr")
  ecr_token <- ecr_service$get_authorization_token()
  ecr_password <- parse_password(
    ecr_token$authorizationData[[1]]$authorizationToken
  )
  repo_uri <- fetch_ecr_repo(tag)
  server_address <- gsub(pattern = "/.*$", replacement = "", x = repo_uri)

  # Pass the password on standard input so it doesn't show in the process
  # list.
  password_file <- tempfile()
  on.exit(unlink(password_file), add = TRUE)
  file.create(password_file)
  Sys.chmod(password_file, "0600")
  writeLines(ecr_password, password_file)
  docker(
    c("login", "--username", "AWS", "--password-stdin", server_address),
    std_in = password_file
  )

  logger::log_debug("[push_lambda_image] Pushing Docker image to AWS ECR.")
  docker(c("push", paste0(repo_uri, ":latest")))

  logger::log_debug("[push_lambda_image] Done.")
  invisible(repo_uri)
}

#' create_lambda_exec_role
#' @param tag the tag of an existing local image
#' @param policy_arns ARNs of IAM policies to attach besides the basic
#' execution policy
#' @noRd
create_lambda_exec_role <- function(tag, policy_arns = NULL) {
  logger::log_debug("[create_lambda_exec_role] Validating inputs.")
  checkmate::assert_character(tag)
  checkmate::assert_character(policy_arns, min.chars = 1, null.ok = TRUE)

  logger::log_debug("[create_lambda_exec_role] Getting Lambda role json.")
  role <- system.file("lambda-role.json", package = "r2lambda")
  role_string <- lambdr::as_stringified_json(jsonlite::fromJSON(role))
  role_name <- paste(tag, uuid::UUIDgenerate(1), sep = "--")

  logger::log_debug("[create_lambda_exec_role] Creating Lambda execution role.")
  iam_service <- aws_connect("iam")
  role_meta <- iam_service$create_role(
    RoleName = role_name,
    AssumeRolePolicyDocument = role_string
  )

  basic_policy <- "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole" # nolint
  for (policy_arn in unique(c(basic_policy, policy_arns))) {
    logger::log_debug("[create_lambda_exec_role] Attaching {policy_arn}.")
    iam_service$attach_role_policy(RoleName = role_name, PolicyArn = policy_arn)
  }

  logger::log_debug("[create_lambda_exec_role] Done.")
  invisible(role_meta)
}

#' Retry a call while a new IAM role is not yet usable by Lambda
#'
#' IAM changes take a few seconds to propagate. Until then, Lambda rejects the
#' role with "cannot be assumed".
#' @param f function to call
#' @param tries number of attempts
#' @param wait seconds between attempts
#' @noRd
retry_until_role_ready <- function(f, tries = 10, wait = 3) {
  for (i in seq_len(tries)) {
    result <- tryCatch(f(), error = function(e) e)
    if (!inherits(result, "error")) {
      return(result)
    }
    if (i == tries || !grepl("cannot be assumed", conditionMessage(result))) {
      stop(result)
    }
    logger::log_debug("[retry_until_role_ready] Role not ready. Retrying.")
    Sys.sleep(wait)
  }
}

#' create_lambda_function
#'
#' @param tag the tag of an existing local image
#' @param ecr_image_uri the URI of the image to use
#' @param lambda_role_arn the arn of the execution role created for this lambda
#' @param ... arguments passed onto `paws::lambda()$create_function()`.
#' For example `Timeout` to increase the execution time, `MemorySize`
#' to request more memory, and `Environment` to set environment variables.
#'
#' @noRd
create_lambda_function <-
  function(tag, ecr_image_uri, lambda_role_arn, ...) {
    logger::log_debug("[create_lambda_function] Validating inputs.")
    checkmate::assert_character(tag)

    logger::log_debug("[create_lambda_function] Creating lambda function.")
    lambda_service <- aws_connect("lambda")
    create <- function() {
      lambda_service$create_function(
        FunctionName = tag,
        Code = list(ImageUri = glue("{ecr_image_uri}:latest")),
        PackageType = "Image",
        Role = lambda_role_arn,
        ...
      )
    }
    lambda <- retry_until_role_ready(create)

    logger::log_debug("[create_lambda_function] Done.")
    invisible(lambda)
  }

#' Create a rule for event schedule
#'
#' @param rule_name character, the name of the rule (used later when adding
#' targets to the event)
#' @param rate character, the rate at which we want the function to run,
#' e.g., `5 minutes`, `1 minute`
#'
#' @noRd
create_event_rule_for_schedule <- function(rule_name, rate) {
  logger::log_debug(
    "[create_event_rule_for_schedule] Creating rule for event schedule."
  )

  aws_event <- aws_connect("eventbridge")

  rule <- tryCatch(
    expr = {
      aws_event$put_rule(
        Name = rule_name,
        ScheduleExpression = rate
      )
    },
    error = function(e) {
      logger::log_error(e$message)
      rlang::abort(e$message)
    }
  )

  logger::log_debug("[create_event_rule_for_schedule] Done.")
  return(rule$RuleArn)
}

#' Add permission for event to invoke lambda
#'
#' @param function_name character, the name of the lambda function to schedule
#' @param scheduled_event_name character, a name for our event
#' @param scheduled_rule_arn character, the ARN of the scheduled event role (as
#' returned from `create_event_rule_for_schedule`)
#'
#' @noRd
lambda_add_permission_for_schedule <-
  function(function_name, scheduled_event_name, scheduled_rule_arn) {
    logger::log_debug(
      "[lambda_add_permission_for_schedule] Adding permission for event schedule."
    ) # nolint
    aws_lambda <- aws_connect("lambda")
    aws_lambda$add_permission(
      FunctionName = function_name,
      StatementId = scheduled_event_name,
      Action = "lambda:InvokeFunction",
      Principal = "events.amazonaws.com",
      SourceArn = scheduled_rule_arn
    )
    logger::log_debug("[lambda_add_permission_for_schedule] Done.")
  }

#' Add lambda to eventbridge rule
#'
#' @param rule_name character, the name of the rule
#' @param lambda_function_arn character, the ARN of the lambda function
#'
#' @noRd
add_lambda_to_eventridge <-
  function(rule_name, lambda_function_arn) {
    logger::log_debug(
      "[add_lambda_to_eventridge] Adding lambda function to events."
    )
    aws_event <- aws_connect("eventbridge")
    aws_event$put_targets(
      Rule = rule_name,
      Targets = list(list(Id = 1, Arn = lambda_function_arn))
    )
    logger::log_debug("[add_lambda_to_eventridge] Done.")
  }
