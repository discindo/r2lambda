#' update policy template with target bucket
prepare_s3_put_policy <- function(bucket_arn) {
  role <- system.file("lambda-S3-put-role.json", package = "r2lambda")
  role_list <- jsonlite::fromJSON(role)
  role_list$Statement$Resource <- bucket_arn
  role_string <- lambdr::as_stringified_json(role_list)
  return(role_string)
}


# deploy_lambda(tag = "tidytuesday3")
