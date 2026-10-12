# Keep tests from reaching AWS with the developer's credentials: an AWS call
# that a test forgot to mock fails with "no credentials" before any request.
# paws falls back to ~/.aws when these files don't exist, so create them empty.
empty_aws_file <- withr::local_tempfile(
  lines = character(),
  .local_envir = testthat::teardown_env()
)
withr::local_envvar(
  AWS_ACCESS_KEY_ID = NA,
  AWS_SECRET_ACCESS_KEY = NA,
  AWS_SESSION_TOKEN = NA,
  AWS_PROFILE = NA,
  AWS_ROLE_ARN = NA,
  AWS_WEB_IDENTITY_TOKEN_FILE = NA,
  AWS_CONTAINER_CREDENTIALS_RELATIVE_URI = NA,
  AWS_CONTAINER_CREDENTIALS_FULL_URI = NA,
  AWS_SHARED_CREDENTIALS_FILE = empty_aws_file,
  AWS_CONFIG_FILE = empty_aws_file,
  AWS_EC2_METADATA_DISABLED = "true",
  AWS_REGION = "us-east-1",
  .local_envir = testthat::teardown_env()
)
