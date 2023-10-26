is_aws_cli_installed <- function() {
  which_aws <- Sys.which("aws")
  IS_AWS_INSTALLED <- ifelse(which_aws[[1]] == "", FALSE, TRUE)
  checkmate::assert_true(IS_AWS_INSTALLED)
}

is_docker_installed <- function() {
  which_docker <- Sys.which("docker")
  IS_DOCKER_INSTALLED <- ifelse(which_docker[[1]] == "", FALSE, TRUE)
  checkmate::assert_true(IS_DOCKER_INSTALLED)
}

#' check_system_deps
#' @noRd
check_system_dependencies <- function() {
  # logger::log_debug("[check_system_dependencies] Checking if `aws cli` is installed.")
  # is_aws_cli_installed()

  logger::log_debug("[check_system_dependencies] Checking if `docker` is installed.")
  is_docker_installed()

  logger::log_debug("[check_system_dependencies] Done.")
}

#' connect to an aws service
#' @param service character, the name of a service, e.g., "lambda" or "iam". Should
#' be a function exported by `{paws}` (see `getNamespaceExports("paws")`)
#' @examples
#' \dontrun{
#' aws_connect("lambda")
#' }
#' @export
aws_connect <- function(service) {
  logger::log_debug("[aws_connect] Checking requested service.")

  if (!service %in% getNamespaceExports("paws")) {
    msg <- glue::glue("The service `{service}` does not appear to be available in `paws`.")
    logger::log_error(msg)
    rlang::abort(msg)
  }

  logger::log_debug("[aws_connect] Connecting to AWS.")

  .service <- utils::getFromNamespace(service, "paws")
  .service()
}

#' install_deps_line
#' @noRd
install_deps_line <- function(deps) {
  checkmate::assert_character(deps)

  repo <- glue::single_quote("https://packagemanager.rstudio.com/all/__linux__/centos7/latest")
  glued <- glue::glue_collapse(glue::single_quote(deps), sep = ", ")
  glue::glue('RUN Rscript -e "install.packages(c({glued}), repos = {repo})"')
}

#' runtime_line
#' @noRd
runtime_line <- function(runtime) {
  checkmate::assert_character(runtime)
  rt <- glue::double_quote(runtime)
  glue::glue("CMD [{rt}]")
}

#' parse password from ecr token
#' @noRd
parse_password <- function(ecr_token) {
  ecr_token %>%
    jsonlite::base64_dec() %>%
    rawToChar() %>%
    gsub(pattern = "^AWS:", replacement = "", x = .)
}

#' create_lambda_dockerfile
#'
#' @param folder path to store the Dockerfile
#' @param runtime_function name of the runtime function
#' @param runtime_path path to the script containing the runtime function
#' @param dependencies list of dependencies
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
  function(folder,
           runtime_function,
           runtime_path,
           dependencies) {
    logger::log_debug("[create_lambda_dockerfile] Validating inputs.")

    checkmate::assert_character(
      x = folder,
      min.chars = 1,
      len = 1
    )

    if (checkmate::test_directory_exists(folder)) {
      msg <- glue::glue("[create_lambda_dockerfile] Directory {folder} exists. Please choose another name.")
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
      len = 1,
      any.missing = FALSE
    )

    if (!checkmate::test_file_exists(runtime_path)) {
      msg <- glue::glue("[create_lambda_dockerfile] Can't access runtime script file {runtime_path}.")
      logger::log_error(msg)
      rlang::abort(msg)
    }

    checkmate::assert_character(
      x = dependencies,
      min.chars = 1,
      null.ok = TRUE
    )

    logger::log_debug("[create_lambda_dockerfile] Creating directory with Dockerfile and runtime script.")

    docker_template <- system.file("lambdr_dockerfile.txt", package = "r2lambda")

    dir.create(folder)
    dir.exists(folder)

    file.copy(runtime_path, folder)
    file.rename(
      from = file.path(folder, basename(runtime_path)),
      to = file.path(folder, "runtime.R")
    )

    file.copy(docker_template, folder, recursive = FALSE)
    file.rename(
      from = file.path(folder, basename(docker_template)),
      to = file.path(folder, "Dockerfile")
    )

    logger::log_debug("[create_lambda_dockerfile] Updating Dockerfile with dependencies and runtime info.")

    if (!is.null(dependencies)) {
      deps_string <- install_deps_line(deps = c(dependencies))
      write(deps_string,
        file = file.path(folder, "Dockerfile"),
        append = TRUE
      )
    }

    runtime_string <- runtime_line(runtime_function)

    write(runtime_string,
      file = file.path(folder, "Dockerfile"),
      append = TRUE
    )

    logger::log_debug("[create_lambda_dockerfile] Done.")
  }

#' Fetch or create ecr repo
#'
#' @param tag a tag for the image
#' @noRd
fetch_ecr_repo <- function(tag) {
  logger::log_debug("[fetch_ecr_repo] Checking if repository already exists.")
  ecr_service <- aws_connect("ecr")

  repos <- ecr_service$describe_repositories()$repositories
  repos_names <- sapply(repos, "[[", "repositoryName")

  if (tag %in% repos_names) {
    logger::log_debug("[fetch_ecr_repo] Repository exists. Fetching URI.")
    needed_repo <- repos_names == tag
    repo_uri <- repos[needed_repo][[1]]$repositoryUri
  } else {
    logger::log_debug("[fetch_ecr_repo] Creating new repository and fetching URI.")
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
#' @param folder path to the folder containing the lambda runtime script and Dockerfile
#' @param tag a tag for the image
#' @noRd
create_lambda_image <- function(folder, tag) {
  logger::log_debug("[create_lambda_image] Validating inputs.")
  checkmate::assert_character(tag)
  checkmate::assert_character(folder)
  checkmate::assert_directory_exists(folder)

  checkmate::assert_file_exists(file.path(folder, "Dockerfile"))
  checkmate::assert_file_exists(file.path(folder, "runtime.R"))

  logger::log_debug("[create_lambda_image] Confirming that files are not empty.")
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

  docker_cli <- stevedore::docker_client()
  docker_cli$image$build(context = folder, tag = repo_uri)

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
  ecr_password <- parse_password(ecr_token$authorizationData[[1]]$authorizationToken)
  repo_uri <- fetch_ecr_repo(tag)
  server_address <- repo_uri %>% gsub(pattern = "/.*$", replacement = "", x = .)

  docker_cli <- stevedore::docker_client()
  docker_cli$login(
    username = "AWS",
    password = ecr_password,
    serveraddress = server_address
  )

  logger::log_debug("[push_lambda_image] Pushing Docker image to AWS ECR.")

  tryCatch(
    expr = {
      docker_cli$image$push(name = repo_uri)
    },
    error = function(e) {
      Sys.sleep(2)
      logger::log_debug("[push_lambda_image] Pushing Docker image to AWS ECR. Second try.")
      docker_cli$image$push(name = repo_uri)
    }
  )

  logger::log_debug("[push_lambda_image] Done.")
  invisible(repo_uri)
}

#' create_lambda_exec_role
#' @param tag the tag of an existing local image
#' @noRd
create_lambda_exec_role <- function(tag) {
  logger::log_debug("[create_lambda_exec_role] Validating inputs.")
  checkmate::assert_character(tag)

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

  logger::log_debug("[create_lambda_exec_role] Attaching basic execution role policy.")
  iam_service$attach_role_policy(
    RoleName = role_name,
    PolicyArn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
  )

  logger::log_debug("[create_lambda_exec_role] Done.")
  invisible(role_meta)
}

#' delete_lambda_exec_role
#' @noRd
delete_lambda_exec_role <- function(tag) {
  ## TODO: this probably won't work as there is a policy attached to the role
  iam_service <- aws_connect("iam")
  iam_service$delete_role(
    RoleName = tag
  )
}

#' create_lambda_function
#'
#' @param tag the tag of an existing local image
#' @param ecr_image_uri the URI of the image to use
#' @param lambda_role_arn the arn of the execution role created for this lambda
#' @param set_aws_envvars logical, whether to set the local AWS secrets to the
#' deployed Lambda environment. This is useful if the Lambda needs to access
#' other AWS service. When `TRUE`, the following envvars are set: `PROFILE`, `REGION`,
#' `SECRET_ACCESS_KEY`, and `ACCESS_KEY_ID`. They are fetched using `Sys.getenv()`.
#'
#' @param ... arguments passed onto `paws.compute:::lambda_create_function()`.
#' For example `Timeout` to increase the execution time, and `MemorySize` to request
#' more memory
#'
#' @noRd
create_lambda_function <-
  function(tag,
           ecr_image_uri,
           lambda_role_arn,
           set_aws_envvars = FALSE,
           ...) {
    logger::log_debug("[create_lambda_function] Validating inputs.")
    checkmate::assert_character(tag)

    envvar_list <- list(Variables = list())

    ## TODO: enable adding arbitrary envvars
    if (isTRUE(set_aws_envvars)) {
      envvar_list <- list(
        Variables = list(
          REGION = Sys.getenv("REGION"),
          PROFILE = Sys.getenv("PROFILE"),
          SECRET_ACCESS_KEY = Sys.getenv("SECRET_ACCESS_KEY"),
          ACCESS_KEY_ID = Sys.getenv("ACCESS_KEY_ID")
        )
      )
    }

    logger::log_debug("[create_lambda_function] Creating lambda function.")
    lambda_service <- aws_connect("lambda")
    lambda <- lambda_service$create_function(
      FunctionName = tag,
      Code = list(ImageUri = glue::glue("{ecr_image_uri}:latest")),
      PackageType = "Image",
      Role = lambda_role_arn,
      Environment = envvar_list,
      ...
    )

    logger::log_debug("[create_lambda_function] Done.")
    invisible(lambda)
  }

#' delete_lambda_function
#' @noRd
delete_lambda_function <- function(tag) {
  lambda_service <- aws_connect("lambda")
  lambda_service$delete_function(
    FunctionName = tag
  )
}

#' Create a rule for event schedule
#'
#' @param rule_name character, the name of the rule (used later when adding targets to the event)
#' @param rate character, the rate at which we want the function to run, e.g., `5 minutes`, `1 minute`
#'
#' @noRd
create_event_rule_for_schedule <- function(rule_name, rate) {
  logger::log_debug("[create_event_rule_for_schedule] Creating rule for event schedule.")

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
#' @param function_name character, the name of the lambda function we wish to schedule
#' @param scheduled_event_name character, a name for our event
#' @param scheduled_rule_arn character, the ARN of the scheduled event role (as
#' returned from `create_event_rule_for_schedule`)
#'
#' @noRd
lambda_add_permission_for_schedule <-
  function(function_name,
           scheduled_event_name,
           scheduled_rule_arn) {
    logger::log_debug("[lambda_add_permission_for_schedule] Adding permission for event schedule.")
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
    logger::log_debug("[add_lambda_to_eventridge] Adding lambda function to events.")
    aws_event <- aws_connect("eventbridge")
    aws_event$put_targets(Rule = rule_name, Targets = list(list(Id = 1, Arn = lambda_function_arn)))
    logger::log_debug("[add_lambda_to_eventridge] Done.")
  }
