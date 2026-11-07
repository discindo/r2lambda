## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Examples

The `aws_connect()` example runs: it only creates a 'paws' client object,
which needs no credentials and sends no request to AWS. The other examples
are wrapped in `\dontrun{}`: they need AWS credentials, and they create,
invoke or delete billable AWS resources (ECR repositories, IAM roles, Lambda
functions and EventBridge schedules). `build_lambda()`, `test_lambda()`,
`deploy_lambda()` and `delete_lambda()` also need a running Docker daemon.
