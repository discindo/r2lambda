
if (FALSE) { # only in in interactive session

  test_that("aws_connect fails ok when bad service is requested", {
    expect_error(aws_connect("lambd"))
  })

  test_that("aws_connect works", {
    checkmate::expect_list(aws_connect("lambda"))
  })
}

test_that("install_deps_line fails ok with incorrect input", {

  deps <- list("a")
  expect_error(install_deps_line(deps = deps))

  deps <- list(1)
  expect_error(install_deps_line(deps = deps))

  deps <- c(1)
  expect_error(install_deps_line(deps = deps))

})

test_that("runtime_line fails ok with incorrect input", {

  runtime <- list("a")
  expect_error(runtime_line(runtime = runtime))

  runtime <- list(1)
  expect_error(runtime_line(runtime = runtime))

  runtime <- c(1)
  expect_error(runtime_line(runtime = runtime))

})

test_that("parse_password works", {
  expect_error(parse_password("AWS:my_password"))

  pass <- jsonlite::base64_enc("AWS:my_password")
  test <- parse_password(pass)
  expect_equal(test, "my_password")

  pass <- jsonlite::base64_enc("AWS:my_password:AWS:")
  test <- parse_password(pass)
  expect_equal(test, "my_password:AWS:")

  pass <- jsonlite::base64_enc("my_password")
  test <- parse_password(pass)
  expect_equal(test, "my_password")
})

test_that("runtime_line works with correct input", {
  runtime <- "my_fun"
  test <- runtime_line(runtime = runtime)
  expect_equal(test, 'CMD ["my_fun"]')
})


#####

folder <- tempdir()
unlink(folder, recursive = TRUE)

test_that("create_lambda_dockerfile works with correct input", {

  runtime_function <- "parity"
  runtime_path <- system.file("parity.R", package = "r2lambda")
  dependencies <- NULL

  create_lambda_dockerfile(
    folder = folder,
    runtime_function = runtime_function,
    runtime_path = runtime_path,
    dependencies = dependencies
    )

  expect_true(dir.exists(folder))
  expect_equal(dir(folder), c("Dockerfile", "runtime.R"))
  unlink(folder, recursive = TRUE)
})

test_that("create_lambda_dockerfile fails as expected", {

  runtime_function <- "party"
  runtime_path <- system.file("party.R", package = "r2lambda")
  dependencies <- NULL

  expect_error(
    create_lambda_dockerfile(
    folder = folder,
    runtime_function = runtime_function,
    runtime_path = runtime_path,
    dependencies = dependencies
  ))
  unlink(folder, recursive = TRUE)

  runtime_function <- list("party")
  runtime_path <- system.file("party.R", package = "r2lambda")
  dependencies <- NULL

  expect_error(
    create_lambda_dockerfile(
      folder = folder,
      runtime_function = runtime_function,
      runtime_path = runtime_path,
      dependencies = dependencies
    ))
  unlink(folder, recursive = TRUE)

  runtime_function <- NA
  runtime_path <- system.file("parity.R", package = "r2lambda")
  dependencies <- NULL

  expect_error(
    create_lambda_dockerfile(
      folder = folder,
      runtime_function = runtime_function,
      runtime_path = runtime_path,
      dependencies = dependencies
    )
  )
  unlink(folder, recursive = TRUE)

  runtime_function <- NA
  runtime_path <- list(system.file("parity.R", package = "r2lambda"))
  dependencies <- NULL

  expect_error(
    create_lambda_dockerfile(
      folder = folder,
      runtime_function = runtime_function,
      runtime_path = runtime_path,
      dependencies = dependencies
    )
  )
  unlink(folder, recursive = TRUE)

  runtime_function <- "parity"
  runtime_path <- system.file("parity.R", package = "r2lambda")
  dependencies <- list()

  expect_error(
    create_lambda_dockerfile(
      folder = folder,
      runtime_function = runtime_function,
      runtime_path = runtime_path,
      dependencies = dependencies
    )
  )
  unlink(folder, recursive = TRUE)

})


#####

folder <- tempdir()
unlink(folder, recursive = TRUE)

test_that("create_lambda_image fails ok when inputs are incorrect", {

    tag <- "testtag"
    folder <- tempdir()
    expect_error(create_lambda_image(folder = folder, tag = tag))

    tag <- "testtag"
    folder <- tempdir()
    dir.create(folder)
    expect_error(create_lambda_image(folder = folder, tag = tag))
    unlink(folder, recursive = TRUE)

    tag <- "testtag"
    folder <- tempdir()
    dir.create(folder)
    file.create(file.path(folder, "Dockerfile"))
    expect_error(create_lambda_image(folder = folder, tag = tag))
    unlink(folder, recursive = TRUE)

    tag <- "testtag"
    folder <- tempdir()
    dir.create(folder)
    file.create(file.path(folder, "runtime.R"))
    expect_error(create_lambda_image(folder = folder, tag = tag))
    unlink(folder, recursive = TRUE)

    tag <- "testtag"
    folder <- tempdir()
    dir.create(folder)
    file.create(file.path(folder, "Dockerfile"))
    file.create(file.path(folder, "runtime.R"))
    expect_error(create_lambda_image(folder = folder, tag = tag))
    unlink(folder, recursive = TRUE)

    tag <- "testtag"
    folder <- tempdir()
    dir.create(folder)
    file.create(file.path(folder, "Dockerfile"))
    write(x = "test that file is not empty", file.path(folder, "Dockerfile"), append = TRUE)
    file.create(file.path(folder, "runtime.R"))
    expect_error(create_lambda_image(folder = folder, tag = tag))
    unlink(folder, recursive = TRUE)

    tag <- "testtag"
    folder <- tempdir()
    dir.create(folder)
    file.create(file.path(folder, "Dockerfile"))
    file.create(file.path(folder, "runtime.R"))
    write(x = "test that file is not empty", file.path(folder, "runtime.R"), append = TRUE)
    expect_error(create_lambda_image(folder = folder, tag = tag))
    unlink(folder, recursive = TRUE)

})
