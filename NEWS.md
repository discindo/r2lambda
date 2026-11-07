# r2lambda 0.3.0

* First CRAN release.

* `build_lambda()` builds a Docker image for an R runtime script, with R
  packages from a list or an `renv.lock` file, Amazon Linux 2023 system
  packages, support files and folders, and `x86_64` or `arm64`
  architecture.

* `test_lambda()` runs the image in a local container and invokes it.

* `deploy_lambda()` pushes the image to the AWS Elastic Container Registry
  and creates the Lambda function, with a new execution role or an
  existing one.

* `invoke_lambda()` invokes a deployed function, `schedule_lambda()` runs
  it on an EventBridge schedule, and `delete_lambda()` removes its AWS
  resources and local image.

* `aws_connect()` creates a `paws` client for an AWS service.

Changes made before the first CRAN release are listed in
[CHANGELOG.md](https://github.com/discindo/r2lambda/blob/main/CHANGELOG.md).
