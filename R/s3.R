#' Update policy template with target S3 bucket
#' @param bucket_arn the ARN of an S3 bucket
#' @noRd
prepare_s3_put_policy <- function(bucket_arn) {
  role <- system.file("lambda-S3-put-role.json", package = "r2lambda")
  role_list <- jsonlite::fromJSON(role)
  role_list$Statement$Resource <- bucket_arn
  role_string <- lambdr::as_stringified_json(role_list)
  return(role_string)
}

grant_lambda_s3_put_access <- function(lambda, bucket) {
  # find lambda arn
  # find bucket arn
  # make policy
  # put it in AWS
  # attach it to a lambda
}

