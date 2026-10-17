has_docker <- function() {
  nzchar(Sys.which("docker"))
}

is_docker_installed <- function() {
  checkmate::assert_true(has_docker())
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
#' @return A `paws` client for the service: a list of functions, one per
#' service operation.
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

#' system_deps_line
#' @noRd
system_deps_line <- function(system_deps) {
  pkgs <- glue_collapse(system_deps, sep = " ")
  glue("RUN dnf -y install {pkgs} && dnf clean all")
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
#' (default) or a character vector of paths to files or folders.
#' @param renvlock_path path to the renv.lock file (if any)
#' @param dependencies list of dependencies (if any)
#' @param system_deps character vector of Amazon Linux 2023 packages to
#' install with `dnf` before the R packages, for R packages that need system
#' libraries, e.g. `"udunits2"` for `units`. Amazon Linux 2023 has no EPEL, so
#' some libraries, such as GDAL, are not available. Default is NULL.
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
    dependencies = NULL,
    system_deps = NULL
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
        if (!file.exists(x)) {
          msg <- glue(
            "[create_lambda_dockerfile] Can't access support file or folder {x}."
          )
          logger::log_error(msg)
          rlang::abort(msg)
        }
      })

      # Support files and folders land next to runtime.R under the names
      # file.copy() gives them, so the names must be distinct and not clash
      # with ours.
      support_names <- basename(support_path)
      reserved <- c("runtime.R", "Dockerfile", "renv.lock", "", ".", "..")
      if (anyDuplicated(support_names) || any(support_names %in% reserved)) {
        msg <- "[create_lambda_dockerfile] Support files and folders need distinct names other than runtime.R, Dockerfile and renv.lock." # nolint
        logger::log_error(msg)
        rlang::abort(msg)
      }
    }

    checkmate::assert_character(
      x = dependencies,
      min.chars = 1,
      null.ok = TRUE
    )

    # Package names go into a shell command, so allow only characters that
    # appear in package names.
    checkmate::assert_character(
      x = system_deps,
      pattern = "^[A-Za-z0-9][A-Za-z0-9._+-]*$",
      any.missing = FALSE,
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
        file.copy(x, folder, recursive = TRUE)
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

    if (!is.null(system_deps)) {
      write(
        system_deps_line(system_deps),
        file = file.path(folder, "Dockerfile"),
        append = TRUE
      )
    }

    if (!is.null(dependencies)) {
      deps_string <- install_deps_line(deps = c(dependencies))
      write(deps_string, file = file.path(folder, "Dockerfile"), append = TRUE)
    }

    if (!is.null(renvlock_path)) {
      file.copy(renvlock_path, file.path(folder, "renv.lock"))
      renv_string <- renv_line("renv.lock")
      write(renv_string, file = file.path(folder, "Dockerfile"), append = TRUE)
    }

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

    runtime_string <- runtime_line(runtime_function)

    write(runtime_string, file = file.path(folder, "Dockerfile"), append = TRUE)

    logger::log_debug("[create_lambda_dockerfile] Done.")
  }

#' Fetch or create ecr repo
#'
#' @param tag a tag for the image
#' @param create logical, whether to create the repository if it doesn't
#' exist. When `FALSE`, a missing repository is an error.
#' @noRd
fetch_ecr_repo <- function(tag, create = TRUE) {
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
  } else if (!create) {
    msg <- glue(
      "[fetch_ecr_repo] No ECR repository named {tag}. Build the image with build_lambda() first." # nolint
    )
    logger::log_error(msg)
    rlang::abort(msg)
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

#' Docker platforms for the Lambda architectures
#' @noRd
docker_platforms <- c(x86_64 = "linux/amd64", arm64 = "linux/arm64")

#' Lambda architecture of a local image
#'
#' @param image name of a local image
#' @noRd
image_architecture <- function(image) {
  arch <- docker(
    c("image", "inspect", "--format", "{{.Architecture}}", image),
    capture = TRUE
  )
  lambda_arch <- names(docker_platforms)[
    docker_platforms == paste0("linux/", arch)
  ]
  if (length(lambda_arch) != 1) {
    msg <- glue(
      "[image_architecture] Image {image} is built for `{glue_collapse(arch, sep = ', ')}`, which Lambda doesn't support. Build it for x86_64 or arm64." # nolint
    )
    logger::log_error(msg)
    rlang::abort(msg)
  }
  lambda_arch
}

#' Find the local image that build_lambda built for a tag
#'
#' build_lambda tags the image with its ECR URI, `<registry>/<tag>:latest`.
#' @param tag the tag given to build_lambda
#' @noRd
find_local_image <- function(tag) {
  tags <- docker(
    c("image", "ls", "--format", "{{.Repository}}:{{.Tag}}"),
    capture = TRUE
  )
  image <- unique(tags[endsWith(tags, paste0("/", tag, ":latest"))])

  if (length(image) == 0) {
    msg <- glue("[find_local_image] No image tagged */{tag}:latest found.")
    logger::log_error(msg)
    message("Available images:\n", glue_collapse(sep = "\n", tags))
    rlang::abort(msg)
  }
  if (length(image) > 1) {
    msg <- glue(
      "[find_local_image] Several images are tagged */{tag}:latest: {glue_collapse(image, sep = ', ')}. Remove the ones you don't need." # nolint
    )
    logger::log_error(msg)
    rlang::abort(msg)
  }
  image
}

#' create_lambda_container
#'
#' @param folder path to the folder containing the lambda runtime
#' script and Dockerfile
#' @param tag a tag for the image
#' @param architecture `"x86_64"` or `"arm64"`
#' @noRd
create_lambda_image <- function(folder, tag, architecture = "x86_64") {
  logger::log_debug("[create_lambda_image] Validating inputs.")
  checkmate::assert_character(tag)
  checkmate::assert_character(folder)
  checkmate::assert_directory_exists(folder)
  checkmate::assert_choice(architecture, names(docker_platforms))

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

  # Build for the Lambda architecture, not the local machine's. Lambda
  # rejects multi-platform image indexes, which buildx creates when it adds
  # provenance attestations.
  docker(c(
    "build",
    "--platform",
    docker_platforms[[architecture]],
    "--provenance=false",
    "--tag",
    repo_uri,
    folder
  ))

  logger::log_debug("[create_lambda_image] Done.")
}

#' push_lambda_container to AWS ECR
#'
#' @param repo_uri the ECR repository URI of an existing local image, as
#' returned by `fetch_ecr_repo()`
#' @noRd
push_lambda_image <- function(repo_uri) {
  logger::log_debug("[push_lambda_image] Validating inputs.")
  checkmate::assert_string(repo_uri, min.chars = 1)

  logger::log_debug("[push_lambda_image] Authenticating Docker with AWS ECR.")

  ecr_service <- aws_connect("ecr")
  ecr_token <- ecr_service$get_authorization_token()
  ecr_password <- parse_password(
    ecr_token$authorizationData[[1]]$authorizationToken
  )
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
#' @param architecture `"x86_64"` or `"arm64"`, matching the image
#' @param ... arguments passed onto `paws::lambda()$create_function()`.
#' For example `Timeout` to increase the execution time, `MemorySize`
#' to request more memory, and `Environment` to set environment variables.
#'
#' @noRd
create_lambda_function <-
  function(
    tag,
    ecr_image_uri,
    lambda_role_arn,
    architecture = "x86_64",
    ...
  ) {
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
        Architectures = list(architecture),
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
      Targets = list(list(Id = "1", Arn = lambda_function_arn))
    )
    logger::log_debug("[add_lambda_to_eventridge] Done.")
  }

#' Run an AWS call, treating a missing resource as already deleted
#'
#' @param expr the AWS call
#' @return the value of `expr`, or `NULL` if the resource does not exist
#' @noRd
ignore_not_found <- function(expr) {
  tryCatch(expr, error = function(e) {
    if (!grepl("NotFound|NoSuchEntity", conditionMessage(e))) {
      stop(e)
    }
    NULL
  })
}

#' Remove a Lambda function from its EventBridge schedules
#'
#' Removes the function from every rule that targets it, and deletes the rules
#' `schedule_lambda()` created once no other targets remain.
#' @param function_arn the ARN of the Lambda function
#' @return names of the deleted rules
#' @noRd
remove_lambda_schedules <- function(function_arn) {
  aws_event <- aws_connect("eventbridge")

  rules <- character()
  token <- NULL
  repeat {
    page <- aws_event$list_rule_names_by_target(
      TargetArn = function_arn,
      NextToken = token
    )
    rules <- c(rules, unlist(page$RuleNames))
    token <- page$NextToken
    if (length(token) == 0) break
  }

  deleted <- character()
  for (rule in rules) {
    targets <- aws_event$list_targets_by_rule(Rule = rule)$Targets
    is_function <- vapply(targets, function(x) x$Arn == function_arn, TRUE)
    logger::log_debug("[remove_lambda_schedules] Removing target from {rule}.")
    aws_event$remove_targets(
      Rule = rule,
      Ids = lapply(targets[is_function], function(x) x$Id)
    )
    if (all(is_function) && startsWith(rule, "schedule_rule_")) {
      logger::log_debug("[remove_lambda_schedules] Deleting rule {rule}.")
      aws_event$delete_rule(Name = rule)
      deleted <- c(deleted, rule)
    }
  }
  deleted
}

#' Delete the execution roles created for a Lambda function
#'
#' Deletes only roles named `<tag>--<uuid>`, as `create_lambda_exec_role()`
#' names them, so a role passed to `deploy_lambda(role_arn = )` is kept.
#' @param tag the tag of the Lambda function
#' @return names of the deleted roles
#' @noRd
delete_lambda_exec_roles <- function(tag) {
  iam_service <- aws_connect("iam")

  roles <- character()
  marker <- NULL
  repeat {
    page <- iam_service$list_roles(Marker = marker)
    roles <- c(roles, vapply(page$Roles, function(x) x$RoleName, ""))
    if (!isTRUE(page$IsTruncated)) {
      break
    }
    marker <- page$Marker
  }

  prefix <- paste0(tag, "--")
  suffix <- substring(roles, nchar(prefix) + 1)
  roles <- roles[startsWith(roles, prefix) & grepl("^[0-9a-f-]{36}$", suffix)]

  for (role in roles) {
    logger::log_debug("[delete_lambda_exec_roles] Deleting role {role}.")
    attached <- iam_service$list_attached_role_policies(RoleName = role)
    for (policy in attached$AttachedPolicies) {
      iam_service$detach_role_policy(
        RoleName = role,
        PolicyArn = policy$PolicyArn
      )
    }
    inline <- iam_service$list_role_policies(RoleName = role)
    for (policy_name in unlist(inline$PolicyNames)) {
      iam_service$delete_role_policy(RoleName = role, PolicyName = policy_name)
    }
    iam_service$delete_role(RoleName = role)
  }
  roles
}

#' Delete the local image built for a Lambda function
#'
#' @param tag the tag of the image
#' @return the name of the deleted image, or `NULL` if there is none
#' @noRd
delete_local_image <- function(tag) {
  if (!has_docker()) {
    logger::log_info("[delete_local_image] docker is not installed. Skipping.")
    return(NULL)
  }
  tags <- docker(
    c("image", "ls", "--format", "{{.Repository}}:{{.Tag}}"),
    capture = TRUE
  )
  if (!any(endsWith(tags, paste0("/", tag, ":latest")))) {
    return(NULL)
  }
  image <- find_local_image(tag)
  docker(c("rmi", image), capture = TRUE)
  image
}
