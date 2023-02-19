
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2lambda

<!-- badges: start -->

[![R-CMD-check](https://github.com/discindo/r2lambda/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/discindo/r2lambda/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/discindo/r2lambda/branch/main/graph/badge.svg)](https://app.codecov.io/gh/discindo/r2lambda?branch=main)
<!-- badges: end -->

The goal of `{r2lambda}` is to make it easier to go from an `R` script
to a deployed `AWS Lambda` function.

## Requirements

- [docker](https://docs.docker.com/get-docker/) is required to build,
  tag, and push the image.

## Installation

You can install the development version of `{r2lambda}` like so:

``` r
# install_packages("remotes")
remotes::install_github("discindo/r2lambda")
```

## Setup

`r2lambda` assumes environmental variables for connecting to AWS
services are available in the `R` session. This is typically done via an
`.Renviron` file that should include:

    ACCESS_KEY_ID = "YOUR AWS ACCESS KEY ID"
    SECRET_ACCESS_KEY = "YOUR AWS SECRET ACCESS KEY"
    PROFILE = "YOUR AWS PROFILE"
    REGION = "YOUR AWS REGION"

## Workflow

### Build a docker image for the lambda function

``` r
runtime_function <- "parity"
runtime_path <- system.file("parity.R", package = "r2lambda")
dependencies <- NULL

# Might take a while, its building a docker image
build_lambda(
 tag = "parity1",
 runtime_function = runtime_function,
 runtime_path = runtime_path,
 dependencies = dependencies
 )
```

### Test the lambda docker image locally

``` r
payload <- list(number = 2)
tag <- "parity1"
test_lambda(tag = "parity1", payload)
```

### Deploy to AWS Lambda

``` r
# Might take a while, its pushing it to a remote repository
deploy_lambda(tag = "parity1")
```

### Invoke deployed lambda

``` r
invoke_lambda(
  function_name = "parity1",
  invocation_type = "RequestResponse",
  payload = list(number = 2),
  include_logs = FALSE
)

#> Lambda response payload: 
#> {"parity":"even"}
```

## Code of Conduct

Please note that the r2lambda project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
