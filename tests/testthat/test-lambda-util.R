test_that("aws_connect fails ok when bad service is requested", {
  expect_error(aws_connect("lambd"))
})

test_that("aws_connect works", {
  checkmate::expect_list(aws_connect("lambda"))
})

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

test_that("renv_line works", {
  expect_error(renv_line(renvlock = 1))

  renvlock <- renv_line("renv.lock")
  test <- glue::glue_collapse(
    c(
      "COPY renv.lock renv.lock",
      "RUN Rscript -e 'renv::restore()'"
    ),
    sep = "\n"
  )
  expect_equal(test, renvlock)
})

#####

test_that("create_lambda_dockerfile works with correct input", {
  folder <- file.path(tempdir(), "test1")
  unlink(folder, recursive = TRUE)

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
  folder <- file.path(tempdir(), "test2")
  unlink(folder, recursive = TRUE)

  runtime_function <- "party"
  runtime_path <- system.file("party.R", package = "r2lambda")
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

  runtime_function <- list("party")
  runtime_path <- system.file("party.R", package = "r2lambda")
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

test_that("create_lambda_image fails ok when inputs are incorrect", {
  folder <- file.path(tempdir(), paste0("test", runif(1)))
  unlink(folder, recursive = TRUE, force = TRUE)

  tag <- "testtag"
  folder <- file.path(tempdir(), paste0("test", runif(1)))
  expect_error(create_lambda_image(folder = folder, tag = tag))

  tag <- "testtag"
  folder <- file.path(tempdir(), paste0("test", runif(1)))
  dir.create(folder)
  expect_error(create_lambda_image(folder = folder, tag = tag))
  unlink(folder, recursive = TRUE)

  tag <- "testtag"
  folder <- file.path(tempdir(), paste0("test", runif(1)))
  dir.create(folder)
  file.create(file.path(folder, "Dockerfile"))
  expect_error(create_lambda_image(folder = folder, tag = tag))
  unlink(folder, recursive = TRUE)

  tag <- "testtag"
  folder <- file.path(tempdir(), paste0("test", runif(1)))
  dir.create(folder)
  file.create(file.path(folder, "runtime.R"))
  expect_error(create_lambda_image(folder = folder, tag = tag))
  unlink(folder, recursive = TRUE)

  tag <- "testtag"
  folder <- file.path(tempdir(), paste0("test", runif(1)))
  dir.create(folder)
  file.create(file.path(folder, "Dockerfile"))
  file.create(file.path(folder, "runtime.R"))
  expect_error(create_lambda_image(folder = folder, tag = tag))
  unlink(folder, recursive = TRUE)

  tag <- "testtag"
  folder <- file.path(tempdir(), paste0("test", runif(1)))
  dir.create(folder)
  file.create(file.path(folder, "Dockerfile"))
  write(
    x = "test that file is not empty",
    file.path(folder, "Dockerfile"),
    append = TRUE
  )
  file.create(file.path(folder, "runtime.R"))
  expect_error(create_lambda_image(folder = folder, tag = tag))
  unlink(folder, recursive = TRUE)

  tag <- "testtag"
  folder <- file.path(tempdir(), paste0("test", runif(1)))
  dir.create(folder)
  file.create(file.path(folder, "Dockerfile"))
  file.create(file.path(folder, "runtime.R"))
  write(
    x = "test that file is not empty",
    file.path(folder, "runtime.R"),
    append = TRUE
  )
  expect_error(create_lambda_image(folder = folder, tag = tag))
  unlink(folder, recursive = TRUE)
})

test_that("create_lambda_dockerfile installs system packages before R packages", {
  folder <- withr::local_tempfile()
  create_lambda_dockerfile(
    folder = folder,
    runtime_function = "parity",
    runtime_path = system.file("parity.R", package = "r2lambda"),
    dependencies = "units",
    system_deps = c("udunits2", "jq")
  )
  dockerfile <- readLines(file.path(folder, "Dockerfile"))
  system_line <- which(
    dockerfile == "RUN dnf -y install udunits2 jq && dnf clean all"
  )
  r_line <- grep("install.packages(c('units')", dockerfile, fixed = TRUE)
  expect_length(system_line, 1)
  expect_lt(system_line, r_line)
})

test_that("create_lambda_dockerfile rejects unsafe system package names", {
  expect_error(
    create_lambda_dockerfile(
      folder = withr::local_tempfile(),
      runtime_function = "parity",
      runtime_path = system.file("parity.R", package = "r2lambda"),
      system_deps = "jq; rm -rf /"
    )
  )
})

test_that("create_lambda_dockerfile copies support folders with their contents", {
  src <- withr::local_tempdir()
  dir.create(file.path(src, "R", "sub"), recursive = TRUE)
  writeLines("helper <- 1", file.path(src, "R", "sub", "helper.R"))
  writeLines("x <- 1", file.path(src, "extra.R"))

  folder <- withr::local_tempfile()
  create_lambda_dockerfile(
    folder = folder,
    runtime_function = "parity",
    runtime_path = system.file("parity.R", package = "r2lambda"),
    support_path = c(file.path(src, "R"), file.path(src, "extra.R"))
  )

  expect_true(file.exists(file.path(folder, "R", "sub", "helper.R")))
  dockerfile <- readLines(file.path(folder, "Dockerfile"))
  expect_true(all(c("COPY R R", "COPY extra.R extra.R") %in% dockerfile))
  expect_equal(dockerfile[[length(dockerfile)]], 'CMD ["parity"]')
})

test_that("create_lambda_dockerfile rejects clashing support names", {
  src <- withr::local_tempdir()
  writeLines("x <- 1", file.path(src, "runtime.R"))
  expect_error(
    create_lambda_dockerfile(
      folder = withr::local_tempfile(),
      runtime_function = "parity",
      runtime_path = system.file("parity.R", package = "r2lambda"),
      support_path = file.path(src, "runtime.R")
    ),
    "distinct names"
  )
})

test_that("create_lambda_image builds for the requested architecture", {
  folder <- withr::local_tempdir()
  writeLines("FROM scratch", file.path(folder, "Dockerfile"))
  writeLines("x <- 1", file.path(folder, "runtime.R"))
  build_args <- NULL
  local_mocked_bindings(
    fetch_ecr_repo = function(tag) "reg/f",
    docker = function(args, ...) {
      build_args <<- args
    }
  )

  create_lambda_image(folder, tag = "f", architecture = "arm64")
  expect_equal(
    build_args[[which(build_args == "--platform") + 1]],
    "linux/arm64"
  )

  create_lambda_image(folder, tag = "f")
  expect_equal(
    build_args[[which(build_args == "--platform") + 1]],
    "linux/amd64"
  )

  expect_error(create_lambda_image(folder, tag = "f", architecture = "386"))
})

test_that("build_lambda rejects unknown architectures", {
  expect_error(
    build_lambda("f", "parity", "parity.R", architecture = "386"),
    "should be one of"
  )
})
