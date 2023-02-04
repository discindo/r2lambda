#' deploy an R script to AWS Lambda
#'
#' @param tag A name for the Docker container and Lambda function
#' @param runtime_function name of the runtime function
#' @param runtime_path path to the script containing the runtime function
#' @param dependencies list of dependencies
#'
#' @examples
#' \dontrun{
#'
#'   runtime_function <- "parity"
#'   runtime_path <- system.file("parity.R", package = "r2lambda")
#'   dependencies <- NULL
#'
#'   deploy_lambda(
#'     tag = "myrepo45",
#'     runtime_function = runtime_function,
#'     runtime_path = runtime_path,
#'     dependencies = dependencies
#'     )
#'
#'  invoke_lambda(
#'    function_name = "myrepo45",
#'    payload = list(number = 3),
#'    invocation_type = "RequestResponse"
#'   )
#'
#' }
#' @export
deploy_lambda <-
  function(tag, runtime_function, runtime_path, dependencies) {
    ## Inputs are validated by lower-level functions

    logger::log_info("[deploy_lambda] Checking system dependencies (`aws cli`, `docker`).")
    check_system_dependencies()

    logger::log_info("[deploy_lambda] Creating temporary working directory.")
    tdir <- tempdir()
    folder <- file.path(tdir, tag)

    logger::log_info("[deploy_lambda] Creating Dockerfile.")
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
        msg <- "Failed to create a folder with Lambda Dockerfile and runtime script."
        logger::log_error(msg)
        rlang::abort(e$message)
      }
    )
    logger::log_warn("[deploy_lambda] Created Dockerfile and lambda runtime script in temporary folder.")

    logger::log_info("[deploy_lambda] Building Docker image.")
    tryCatch(
      expr = {
        create_lambda_image(folder = folder, tag = tag)
      },
      error = function(e) {
        msg <- "Failed to create Lambda Docker image."
        logger::log_error(msg)
        stop(e$message)
      }
    )
    logger::log_warn("[deploy_lambda] Docker image built. This can take up substantial amount of disk space.")
    logger::log_warn("[deploy_lambda] Use `docker image ls` in your shell to see the image size.")
    logger::log_warn("[deploy_lambda] Use `docker rmi <image>` in your shell to remove an image.")

    logger::log_info("[deploy_lambda] Pushing Docker image to AWS ECR. This may take a while.")
    ecr_image_uri <- tryCatch(
      expr = {
        push_lambda_image(tag = tag)
      },
      error = function(e) {
        msg <- "Failed to push Lambda Docker image to AWS ECR."
        logger::log_error(msg)
        stop(e$message)
      }
    )

    logger::log_warn("[deploy_lambda] Docker image pushed to ECR. This can take up substantial resources and incur cost.")
    logger::log_warn("[deploy_lambda] Use `paws::ecr()`, the AWS CLI, or the AWS console to manage your images.")

    logger::log_info("[deploy_lambda] Creating Lambda role and basic policy.")
    iam_lambda_role <- tryCatch(expr = {
      create_lambda_exec_role(tag = tag)
    }, error = function(e) {
      msg <- "Failed to create Lambda execution role in AWS IAM."
      logger::log_error(msg)
      stop(e$message)
    })

    logger::log_warn("[deploy_lambda] Created AWS role with basic lambda execution permissions.")
    logger::log_warn("[deploy_lambda] Use `paws::iam()`, the AWS CLI, or the AWS console to manage your roles, and permissions.")

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
          lambda_role_arn = iam_lambda_role$Role$Arn
        )
      },
      error = function(e) {
        msg <- "Failed to push Lambda Docker image to AWS ECR."
        logger::log_error(msg)
        stop(msg)
      }
    )
    logger::log_warn("[deploy_lambda] Lambda function created. This can take up substantial resources and incur cost.")
    logger::log_warn("[deploy_lambda] Use `paws::lambda()`, the AWS CLI, or the AWS console to manage your functions.")

    logger::log_warn("[deploy_lambda] Lambda function created successfully.")
    logger::log_warn(glue::glue("[deploy_lambda] Pushed docker image to ECR with URI `{ecr_image_uri}`"))
    logger::log_warn(glue::glue("[deploy_lambda] Created Lambda execution role with ARN `{iam_lambda_role$Role$Arn}`"))
    logger::log_warn(glue::glue("[deploy_lambda] Created Lambda function `{lambda$FunctionName}` with ARN `{lambda$FunctionArn}`"))

    logger::log_success("[deploy_lambda] Done.")

    invisible(list(
      ECR_image_uri = ecr_image_uri,
      IAM_lambda_role_arn = iam_lambda_role$Role$Arn,
      Lambda_function_arn = lambda$FunctionArn
    ))
  }

#' invoke a lambda function
#' @param function_name The name or arn of the function
#' @param invocation_type One of ‘DryRun’, ‘RequestResponse’, or ‘Event’ see `?paws.compute::lambda_invoke`
#' @param payload A named list internally converted to json
#' @param include_logs logical, whether to show the lambda logs (default: FALSE)
#' @examples
#' \dontrun{
#'   invoke_lambda(
#'    function_name = "lambda-test88",
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
    lambda_service <- aws_connect(paws::lambda)

    logger::log_info("[invoke_lambda] Invoking function.")
    response <- lambda_service$invoke(
      FunctionName = function_name,
      InvocationType = invocation_type,
      Payload = jsonlite::toJSON(payload),
      LogType = ifelse(include_logs, "Tail", "None")
    )

    message("\nLambda response payload: ")
    response$Payload |> rawToChar() |> cat()
    cat()

    if (include_logs) {
      message("\nLambda logs: ")
      jsonlite::base64_dec(response$LogResult) |> rawToChar() |> cat()
    }
    logger::log_success("[invoke_lambda] Done.")
    invisible(response)
  }
