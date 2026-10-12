# Builds a real image and runs it with the Lambda runtime emulator. Needs
# Docker and takes a few minutes, so it only runs when asked:
# R2LAMBDA_TEST_DOCKER=true. It makes no AWS calls.
skip_if_not(
  identical(Sys.getenv("R2LAMBDA_TEST_DOCKER"), "true"),
  "Set R2LAMBDA_TEST_DOCKER=true to build images"
)
skip_if_not(nzchar(Sys.which("docker")), "docker is not installed")

test_that("an image built from renv.lock has the pinned package versions", {
  tag <- paste0("r2lambda-test-", uuid::UUIDgenerate())
  image <- paste0("r2lambda.test/", tag)
  folder <- withr::local_tempfile()
  withr::defer(docker(c("rmi", "--force", image), capture = TRUE))

  create_lambda_dockerfile(
    folder = folder,
    runtime_function = "versions",
    runtime_path = test_path("fixtures", "pinned", "runtime.R"),
    renvlock_path = test_path("fixtures", "pinned", "renv.lock")
  )
  docker(c(
    "build",
    "--quiet",
    "--platform",
    docker_platforms[["x86_64"]],
    "--provenance=false",
    "--tag",
    image,
    folder
  ))

  withr::local_output_sink(nullfile())
  result <- suppressMessages(test_lambda(tag, list()))
  versions <- jsonlite::fromJSON(rawToChar(result$stdout))
  expect_equal(versions$glue, "1.6.2")
  expect_equal(versions$jsonlite, "1.8.8")
})
