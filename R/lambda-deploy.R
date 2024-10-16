#' build and tag lambda image locally
#'
#' @param tag A name for the Docker container and Lambda function
#' @param runtime_function name of the runtime function
#' @param runtime_path path to the script containing the runtime function
#' @param dependencies list of dependencies
#' @importFrom glue glue glue_collapse single_quote double_quote
#' @export
build_lambda <- function(tag, runtime_function, runtime_path, dependencies) {

  logger::log_info("[build_lambda] Checking system dependencies.")
  check_system_dependencies()

  logger::log_info("[build_lambda] Creating temporary working directory.")
  tdir <- tempdir()
  folder <- file.path(tdir, tag)

  logger::log_info("[build_lambda] Creating Dockerfile.")
  tryCatch(
    expr = {
      create_lambda_dockerfile(
        folder = folder,
        runtime_function = runtime_function,
        runtime_path = runtime_path,
        dependencies = dependencies
      )
    },
    error = function(e) {
      msg <- "Failed to create a folder with Lambda Dockerfile and runtime script." # nolint
      logger::log_error(msg)
      rlang::abort(e$message)
    }
  )
  logger::log_warn("[build_lambda] Created Dockerfile and lambda runtime script in temporary folder.") # nolint

  logger::log_info("[build_lambda] Building Docker image.")
  tryCatch(
    expr = {
      create_lambda_image(folder = folder, tag = tag)
    },
    error = function(e) {
      msg <- "Failed to create Lambda Docker image."
      logger::log_error(msg)
      rlang::abort(e$message)
    }
  )
  logger::log_warn("[build_lambda] Docker image built. This can take up substantial amount of disk space.") # nolint
  logger::log_warn("[build_lambda] Use `docker image ls` in your shell to see the image size.") # nolint
  logger::log_warn("[build_lambda] Use `docker rmi <image>` in your shell to remove an image.") # nolint

  logger::log_success("[build_lambda] Done.")
}

#' test a lambda locally
#'
#' @param tag The tag of an existing local image tagged with ECR repo (see `build_lambda`) # nolint
#' @param payload Named list. Arguments to lambda function.
#'
#' @examples
#' \dontrun{
#'   payload <- list(number = 2)
#'   tag <- "449283523352.dkr.ecr.us-east-1.amazonaws.com/myrepo51:latest"
#'   test_lambda(tag, payload)
#' }
#' @importFrom glue glue glue_collapse single_quote double_quote
#' @export
test_lambda <- function(tag, payload) {

  logger::log_info("[test_lambda] Converting payload list to json.")
  payload <- jsonlite::toJSON(payload, auto_unbox = TRUE)

  logger::log_info("[test_lambda] Starting docker client.")
  docker_cli <- stevedore::docker_client()

  logger::log_info("[test_lambda] Fetching remote tag.")
  repo_uri <- fetch_ecr_repo(tag)
  repo_tag <- paste0(repo_uri, ':latest')

  logger::log_info("[test_lambda] Checking image tag exists.")
  images <- docker_cli$image$list()
  tags <- images[["repo_tags"]] |> unlist()
  tag_exists <- repo_tag %in% tags

  if (!tag_exists) {
    msg <- glue("[test_lambda] Image tagged {repo_tag} not found.")
    logger::log_error(msg)
    message("Available images:\n", glue_collapse(sep = "\n", tags))
    rlang::abort(msg)
  }

  uid <- uuid::UUIDgenerate(1)
  logger::log_info(glue("[test_lambda] Starting lambda container with name {uid}.")) # nolint
  docker_cli$container$run(
    image = repo_tag,
    port = "9000:8080",
    detach = TRUE,
    name = uid
  )

  logger::log_info("[test_lambda] Invoking local lambda instance.")
  arg <- c("-XPOST", "http://localhost:9000/2015-03-31/functions/function/invocations", "-d", payload) # nolint
  response <- sys::exec_internal(cmd = "curl", args = arg)
  message("Response standard output:\n")
  response$stdout |> rawToChar() |> cat()
  cat("\n")
  message("Response standard error:\n")
  response$stderr |> rawToChar() |> cat()
  cat("\n")

  logger::log_info("[test_lambda] Stopping running lambda container.")
  running_containers <- docker_cli$container$list()
  to_stop <- running_containers$id[running_containers$name == uid]
  docker_cli$container$remove(id = to_stop, force = TRUE)

  logger::log_success("[test_lambda] Done.")
  invisible(response)
}


#' deploy a local lambda image to AWS Lambda
#'
#' @param tag The tag of an existing local image tagged with ECR repo (see `build_lambda`) # nolint
#' @param set_aws_envvars logical, whether to set the local AWS secrets to the 
#' deployed Lambda environment (default = `FALSE`). This is useful if the Lambda needs to access # nolint
#' other AWS service. When `TRUE`, the following envvars are set: `PROFILE`, `REGION`, # nolint
#' `SECRET_ACCESS_KEY`, and `ACCESS_KEY_ID`. They are fetched using `Sys.getenv()`. # nolint
#' @param ... Arguments passed onto `create_lambda_function`
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
  function(tag, set_aws_envvars = FALSE, ...) {
    ## Inputs are validated by lower-level functions

    logger::log_info("[deploy_lambda] Pushing Docker image to AWS ECR. This may take a while.") # nolint
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

    logger::log_info("[deploy_lambda] Creating Lambda role and basic policy.")
    iam_lambda_role <- tryCatch(
      expr = {
        create_lambda_exec_role(tag = tag)
      },
      error = function(e) {
        msg <- "Failed to create Lambda execution role in AWS IAM."
        logger::log_error(msg)
        rlang::abort(e$message)
      }
    )

    logger::log_warn("[deploy_lambda] Created AWS role with basic lambda execution permissions.") # nolint
    logger::log_warn(
      "[deploy_lambda] Use `paws::iam()`, the AWS CLI, or the AWS console to manage your roles, and permissions." # nolint
    )

    ## TODO check if the role is OK before starting to create the lambda function
    ## As is, we are waiting ten seconds before creating the lambda
    ## but this could be too long in some cases or not long enough in other
    Sys.sleep(10)

    logger::log_info("[deploy_lambda] Creating Lambda function from image.")
    lambda <- tryCatch(
      expr = {
        create_lambda_function(
          tag = tag,
          ecr_image_uri = ecr_image_uri,
          lambda_role_arn = iam_lambda_role$Role$Arn,
          set_aws_envvars = set_aws_envvars,
          ...
        )
      },
      error = function(e) {
        msg <- "Failed to push Lambda Docker image to AWS ECR."
        logger::log_error(msg)
        rlang::abort(msg)
      }
    )
    logger::log_warn(
      "[deploy_lambda] Lambda function created. This can take up substantial resources and incur cost." # nolint
    )
    logger::log_warn(
      "[deploy_lambda] Use `paws::lambda()`, the AWS CLI, or the AWS console to manage your functions." # nolint
    )

    logger::log_warn("[deploy_lambda] Lambda function created successfully.")
    logger::log_warn(glue(
      "[deploy_lambda] Pushed docker image to ECR with URI `{ecr_image_uri}`"
    ))
    logger::log_warn(
      glue(
        "[deploy_lambda] Created Lambda execution role with ARN `{iam_lambda_role$Role$Arn}`" # nolint
      )
    )
    logger::log_warn(
      glue(
        "[deploy_lambda] Created Lambda function `{lambda$FunctionName}` with ARN `{lambda$FunctionArn}`" # nolint
      )
    )

    logger::log_success("[deploy_lambda] Done.")

    invisible(
      list(
        ECR_image_uri = ecr_image_uri,
        IAM_lambda_role_arn = iam_lambda_role$Role$Arn,
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
  function(function_name,
           invocation_type,
           payload,
           include_logs = FALSE) {

    logger::log_info("[invoke_lambda] Validating inputs.")

    # assumes .Renviron is set up
    lambda_service <- aws_connect("lambda")

    logger::log_info("[invoke_lambda] Checking function state.")
    state <- lambda_service$get_function(FunctionName = function_name)$Configuration$State
    ## TODO: Should we also check `LastUpdateStatus` 
    ## (https://docs.aws.amazon.com/lambda/latest/dg/functions-states.html)

    if (state == "Active" || state == "Inactive") {
      # if its inactive, ping it still, to activate it
      # it might fail to run, but the error message should clarify next steps
      logger::log_info("[invoke_lambda] Function state: {state}.")
      logger::log_info("[invoke_lambda] Invoking function.")
      response <- tryCatch(
        lambda_service$invoke(
        FunctionName = function_name,
        InvocationType = invocation_type,
        Payload = jsonlite::toJSON(payload),
        LogType = ifelse(include_logs, "Tail", "None")
      ), error = function(e) e$message)

    } else {
      logger::log_info(glue("[invoke_lambda] Failed to invoke the function due to {state} state.")) # nolint
      logger::log_info("[invoke_lambda] Please try again shortly if the reported state was `Pending`.") # nolint
    }

    message("\nLambda response payload: ")
    response$Payload |> rawToChar() |> cat()
    cat("\n")

    if (include_logs) {
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
#' `rate` or `cron` specification. For details consult the official documentation: # nolint
#' https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-create-rule-schedule.html # nolint
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
  fun_list <- aws_lambda$list_functions()
  lambda_names <- sapply(fun_list$Functions, "[[", "FunctionName")

  if (!lambda_function %in% lambda_names) {
    msg <- glue(
      "[schedule_lambda] Cannot find deployed lambda function {lambda_function}"
    ) 
    logger::log_error(msg)
    rlang::abort(msg)
  }

  logger::log_info(
    "[schedule_lambda] Found lambda function {lambda_function}. Fetching ARN."
  )
  lambda_index <- which(lambda_function == lambda_names)
  lambda_function_arn <- fun_list$Functions[[lambda_index]]$FunctionArn

  rate_clean <- gsub("\\(|\\)| |\\*|\\?", "_", execution_rate)
  rule_name <- glue("schedule_rule_{rate_clean}_{lambda_function}")
  logger::log_info(
    "[schedule_lambda] Creating event schedule rule with name {rule_name}"
  ) 

  rule_arn <- tryCatch(
    expr = {
      create_event_rule_for_schedule(
        rule_name = rule_name, rate = execution_rate
      ) 
    },
    error = function(e) {
      logger::log_error(e$message)
      rlang::abort(e$message)
    }
  )

  event_name <- glue("schedule_event_{rate_clean}_{lambda_function}")
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
