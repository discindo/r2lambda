#' build and tag lambda image locally
#'
#' @param tag A name for the Docker container and Lambda function
#' @param runtime_function name of the runtime function
#' @param runtime_path path to the script containing the runtime function
#' @param support_path path to the support files (if any). Either NULL
#' (the default) if all needed code is in the same `runtime_path` script, or a
#' character vector of paths to additional files or folders needed by the
#' runtime script. Each is copied next to the runtime script under its own
#' name, so `support_path = "R"` lets the runtime script
#' `source("R/helpers.R")`.
#' @param renvlock_path path to the renv.lock file (if any). Default is NULL.
#' @param dependencies list of dependencies (if any). Default is NULL.
#' @param architecture CPU architecture of the Lambda function, `"x86_64"`
#' (default) or `"arm64"`. `deploy_lambda` reads it from the image. Building
#' an arm64 image on an x86_64 machine, or the reverse, needs Docker's QEMU
#' emulation.
#' @param system_deps character vector of Amazon Linux 2023 packages to
#' install with `dnf` before the R packages, for R packages that need system
#' libraries, e.g. `"udunits2"` for `units`. Amazon Linux 2023 has no EPEL, so
#' some libraries, such as GDAL, are not available. Default is NULL.
#'
#' @details Use either `renvlock_path` or `dependencies` to install required
#' packages, not both. By default, both are `NULL`, so the Docker image will
#' have no additional packages installed.
#'
#' @importFrom glue glue glue_collapse single_quote double_quote
#' @export
build_lambda <- function(
  tag,
  runtime_function,
  runtime_path,
  support_path = NULL,
  renvlock_path = NULL,
  dependencies = NULL,
  system_deps = NULL,
  architecture = c("x86_64", "arm64")
) {
  architecture <- match.arg(architecture)
  logger::log_info("[build_lambda] Checking system dependencies.")
  check_system_dependencies()

  logger::log_info("[build_lambda] Creating temporary working directory.")
  folder <- tempfile(pattern = paste0(tag, "-"))

  logger::log_info("[build_lambda] Creating Dockerfile.")
  tryCatch(
    expr = {
      create_lambda_dockerfile(
        folder = folder,
        runtime_function = runtime_function,
        runtime_path = runtime_path,
        renvlock_path = renvlock_path,
        support_path = support_path,
        dependencies = dependencies,
        system_deps = system_deps
      )
    },
    error = function(e) {
      msg <- "Failed to create a folder with Lambda Dockerfile and runtime script." # nolint
      logger::log_error(msg)
      rlang::abort(e$message)
    }
  )
  logger::log_warn(
    "[build_lambda] Created Dockerfile and lambda runtime script in temporary folder." # nolint
  )

  logger::log_info("[build_lambda] Building Docker image.")
  tryCatch(
    expr = {
      create_lambda_image(
        folder = folder,
        tag = tag,
        architecture = architecture
      )
    },
    error = function(e) {
      msg <- "Failed to create Lambda Docker image."
      logger::log_error(msg)
      rlang::abort(e$message)
    }
  )
  logger::log_warn(
    "[build_lambda] Docker image built. This can take up substantial amount of disk space." # nolint
  )
  logger::log_warn(
    "[build_lambda] Use `docker image ls` in your shell to see the image size."
  )
  logger::log_warn(
    "[build_lambda] Use `docker rmi <image>` in your shell to remove an image."
  )

  logger::log_success("[build_lambda] Done.")
}

#' test a lambda locally
#'
#' @param tag The tag of an existing local image tagged with ECR repo
#' (see `build_lambda`)
#' @param payload Named list. Arguments to lambda function.
#'
#' @examples
#' \dontrun{
#'   payload <- list(number = 2)
#'   test_lambda(tag = "parity1", payload)
#' }
#' @importFrom glue glue glue_collapse single_quote double_quote
#' @export
test_lambda <- function(tag, payload) {
  logger::log_info("[test_lambda] Converting payload list to json.")
  payload <- jsonlite::toJSON(payload, auto_unbox = TRUE)

  logger::log_info("[test_lambda] Checking image tag exists.")
  tags <- docker(
    c("image", "ls", "--format", "{{.Repository}}:{{.Tag}}"),
    capture = TRUE
  )
  # build_lambda tags the image with its ECR URI, <registry>/<tag>.
  repo_tag <- tags[endsWith(tags, paste0("/", tag, ":latest"))]

  if (length(repo_tag) == 0) {
    msg <- glue("[test_lambda] No image tagged */{tag}:latest found.")
    logger::log_error(msg)
    message("Available images:\n", glue_collapse(sep = "\n", tags))
    rlang::abort(msg)
  }
  repo_tag <- repo_tag[[1]]

  uid <- uuid::UUIDgenerate(1)
  logger::log_info("[test_lambda] Starting lambda container with name {uid}.")
  docker(
    c("run", "--detach", "--rm", "-p", "9000:8080", "--name", uid, repo_tag),
    capture = TRUE
  )
  on.exit(
    {
      logger::log_info("[test_lambda] Removing lambda container.")
      docker(c("rm", "--force", uid), capture = TRUE)
    },
    add = TRUE
  )

  logger::log_info("[test_lambda] Invoking local lambda instance.")
  # The runtime interface emulator takes a moment to start listening.
  arg <- c(
    "--silent",
    "--retry",
    "10",
    "--retry-connrefused",
    "--retry-delay",
    "1",
    "-XPOST",
    "http://localhost:9000/2015-03-31/functions/function/invocations",
    "-d",
    payload
  )
  response <- sys::exec_internal(cmd = "curl", args = arg)
  message("Response standard output:\n")
  response$stdout |> rawToChar() |> cat()
  cat("\n")
  message("Response standard error:\n")
  response$stderr |> rawToChar() |> cat()
  cat("\n")

  logger::log_success("[test_lambda] Done.")
  invisible(response)
}


#' deploy a local lambda image to AWS Lambda
#'
#' @param tag The tag of an existing local image tagged with ECR repo
#' (see `build_lambda`)
#' @param policy_arns character vector of IAM policy ARNs to attach to the
#' Lambda execution role, besides `AWSLambdaBasicExecutionRole`. Use it to let
#' the function call other AWS services, for example
#' `"arn:aws:iam::aws:policy/AmazonS3FullAccess"`. Inside the function, `paws`
#' clients then pick up the role's temporary credentials automatically.
#' @param role_arn ARN of an existing IAM role for the function to run as.
#' When given, no role is created, and `policy_arns` can't be used. The role
#' must trust `lambda.amazonaws.com`.
#' @param ... Arguments passed onto `paws::lambda()$create_function()`, for
#' example `Timeout`, `MemorySize` or `Environment`.
#'
#' @examples
#' \dontrun{
#'
#'   runtime_function <- "parity"
#'   runtime_path <- system.file("parity.R", package = "r2lambda")
#'   dependencies <- NULL
#'
#'   build_lambda(
#'     tag = "myrepo52",
#'     runtime_function = runtime_function,
#'     runtime_path = runtime_path,
#'     dependencies = dependencies
#'     )
#'
#'   deploy_lambda(tag = "myrepo52")
#'
#'   invoke_lambda(
#'     function_name = "myrepo52",
#'     payload = list(number = 3),
#'     invocation_type = "RequestResponse"
#'     )
#'
#' }
#' @importFrom paws ecr iam lambda s3 eventbridge
#' @export
deploy_lambda <-
  function(tag, policy_arns = NULL, role_arn = NULL, ...) {
    checkmate::assert_string(role_arn, min.chars = 1, null.ok = TRUE)
    if (!is.null(role_arn) && !is.null(policy_arns)) {
      msg <- "[deploy_lambda] Use either `role_arn` or `policy_arns`, not both. Attach policies to the existing role instead." # nolint
      logger::log_error(msg)
      rlang::abort(msg)
    }
    ## Other inputs are validated by lower-level functions

    logger::log_info(
      "[deploy_lambda] Pushing Docker image to AWS ECR. This may take a while."
    )
    ecr_image_uri <- tryCatch(
      expr = {
        push_lambda_image(tag = tag)
      },
      error = function(e) {
        msg <- "Failed to push Lambda Docker image to AWS ECR."
        logger::log_error(msg)
        rlang::abort(e$message)
      }
    )

    logger::log_warn(
      "[deploy_lambda] Docker image pushed to ECR. This can take up substantial resources and incur cost." # nolint
    )
    logger::log_warn(
      "[deploy_lambda] Use `paws::ecr()`, the AWS CLI, or the AWS console to manage your images." # nolint
    )

    architecture <- image_architecture(paste0(ecr_image_uri, ":latest"))
    logger::log_info("[deploy_lambda] Image architecture: {architecture}.")

    if (is.null(role_arn)) {
      logger::log_info("[deploy_lambda] Creating Lambda role and basic policy.")
      iam_lambda_role <- tryCatch(
        expr = {
          create_lambda_exec_role(tag = tag, policy_arns = policy_arns)
        },
        error = function(e) {
          msg <- "Failed to create Lambda execution role in AWS IAM."
          logger::log_error(msg)
          rlang::abort(e$message)
        }
      )
      role_arn <- iam_lambda_role$Role$Arn

      logger::log_warn(
        "[deploy_lambda] Created AWS role with basic lambda execution permissions." # nolint
      )
      logger::log_warn(
        "[deploy_lambda] Use `paws::iam()`, the AWS CLI, or the AWS console to manage your roles, and permissions." # nolint
      )
    } else {
      logger::log_info("[deploy_lambda] Using existing role {role_arn}.")
    }

    logger::log_info("[deploy_lambda] Creating Lambda function from image.")
    lambda <- tryCatch(
      expr = {
        create_lambda_function(
          tag = tag,
          ecr_image_uri = ecr_image_uri,
          lambda_role_arn = role_arn,
          architecture = architecture,
          ...
        )
      },
      error = function(e) {
        msg <- "Failed to create Lambda function."
        logger::log_error(msg)
        rlang::abort(e$message)
      }
    )
    logger::log_warn(
      "[deploy_lambda] Lambda function created. This can take up substantial resources and incur cost." # nolint
    )
    logger::log_warn(
      "[deploy_lambda] Use `paws::lambda()`, the AWS CLI, or the AWS console to manage your functions." # nolint
    )

    logger::log_warn("[deploy_lambda] Lambda function created successfully.")
    logger::log_warn(
      "[deploy_lambda] Pushed docker image to ECR with URI `{ecr_image_uri}`"
    )
    logger::log_warn(
      "[deploy_lambda] Lambda execution role ARN `{role_arn}`"
    )
    logger::log_warn(
      "[deploy_lambda] Created Lambda function `{lambda$FunctionName}` with ARN `{lambda$FunctionArn}`" # nolint
    )

    logger::log_success("[deploy_lambda] Done.")

    invisible(
      list(
        ECR_image_uri = ecr_image_uri,
        IAM_lambda_role_arn = role_arn,
        Lambda_function_arn = lambda$FunctionArn
      )
    )
  }

#' invoke a lambda function
#' @param function_name The name or arn of the function
#' @param invocation_type One of ‘DryRun’, ‘RequestResponse’,
#' or ‘Event’ see `?paws.compute::lambda_invoke`
#' @param payload A named list internally converted to json
#' @param include_logs logical, whether to show the lambda logs (default: FALSE)
#' @examples
#' \dontrun{
#'   invoke_lambda(
#'    function_name = "parity",
#'    payload = list(number = 3),
#'    invocation_type = "RequestResponse"
#'   )
#' }
#' @export
invoke_lambda <-
  function(function_name, invocation_type, payload, include_logs = FALSE) {
    logger::log_info("[invoke_lambda] Validating inputs.")

    # assumes .Renviron is set up
    lambda_service <- aws_connect("lambda")

    logger::log_info("[invoke_lambda] Checking function state.")
    state <- lambda_service$get_function(
      FunctionName = function_name
    )$Configuration$State # nolint
    ## TODO: Should we also check `LastUpdateStatus`
    ## (https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html)

    # An inactive function is invoked anyway: the call reactivates it, and if
    # it fails, the AWS error message says what to do next.
    if (!state %in% c("Active", "Inactive")) {
      msg <- glue(
        "[invoke_lambda] Can't invoke the function in state `{state}`. Try again if the state is `Pending`." # nolint
      )
      logger::log_error(msg)
      rlang::abort(msg)
    }

    logger::log_info("[invoke_lambda] Function state: {state}.")
    logger::log_info("[invoke_lambda] Invoking function.")
    response <- lambda_service$invoke(
      FunctionName = function_name,
      InvocationType = invocation_type,
      Payload = jsonlite::toJSON(payload, auto_unbox = TRUE),
      LogType = if (include_logs) "Tail" else "None"
    )

    message("\nLambda response payload: ")
    response$Payload |> rawToChar() |> cat()
    cat("\n")

    if (include_logs && length(response$LogResult) > 0) {
      message("\nLambda logs: ")
      jsonlite::base64_dec(response$LogResult) |> rawToChar() |> cat()
    }
    logger::log_success("[invoke_lambda] Done.")
    invisible(response)
  }

#' Put a deployed lambda function on a schedule
#'
#' @param lambda_function character, the name (or tag) of the function. A check
#' is done internally make sure the Lambda exists and fetch its ARN.
#' @param execution_rate character, the rate to run the lambda function. Can use
#' `rate` or `cron` specification. For details see the official documentation:
#' https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-create-rule-schedule.html
#'
#' @examples
#' \dontrun{
#'   # Make Tidytuesday lambda fetch the Tidytuesday dataset every wednesday
#' at 8 am schedule_lambda("Tidytuesday3", "cron(0 8 * * Wed)")
#' }
#'
#' @export

schedule_lambda <- function(lambda_function, execution_rate) {
  logger::log_info("[schedule_lambda] Validating inputs.")
  aws_lambda <- aws_connect("lambda")
  lambda_function_arn <- tryCatch(
    aws_lambda$get_function(
      FunctionName = lambda_function
    )$Configuration$FunctionArn,
    error = function(e) {
      msg <- glue(
        "[schedule_lambda] Cannot find deployed lambda function {lambda_function}"
      )
      logger::log_error(msg)
      rlang::abort(msg, parent = e)
    }
  )
  logger::log_info(
    "[schedule_lambda] Found lambda function {lambda_function}."
  )

  # Rule names allow [.-_A-Za-z0-9] and 64 characters; permission statement
  # IDs allow [-_A-Za-z0-9].
  rate_clean <- gsub("[^A-Za-z0-9_-]", "_", execution_rate)
  function_clean <- gsub("[^A-Za-z0-9_-]", "_", lambda_function)
  rule_name <- substr(
    glue("schedule_rule_{rate_clean}_{function_clean}"),
    1,
    64
  )
  logger::log_info(
    "[schedule_lambda] Creating event schedule rule with name {rule_name}"
  )

  rule_arn <- tryCatch(
    expr = {
      create_event_rule_for_schedule(
        rule_name = rule_name,
        rate = execution_rate
      )
    },
    error = function(e) {
      logger::log_error(e$message)
      rlang::abort(e$message)
    }
  )

  event_name <- glue("schedule_event_{rate_clean}_{function_clean}")
  logger::log_info(
    "[schedule_lambda] Adding permission to execute lambda to event with name {event_name}" # nolint
  )
  tryCatch(
    expr = {
      lambda_add_permission_for_schedule(
        function_name = lambda_function,
        scheduled_event_name = event_name,
        scheduled_rule_arn = rule_arn
      )
    },
    error = function(e) {
      logger::log_error(e$message)
      rlang::abort(e$message)
    }
  )

  logger::log_info(
    "[schedule_lambda] Adding lambda function {lambda_function} to eventbridge schedule." # nolint
  )
  tryCatch(
    expr = {
      add_lambda_to_eventridge(
        rule_name = rule_name,
        lambda_function_arn = lambda_function_arn
      )
    },
    error = function(e) {
      logger::log_error(e$message)
      rlang::abort(e$message)
    }
  )

  logger::log_info("[schedule_lambda] Done.")
}
