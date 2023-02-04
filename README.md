
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

## Run

``` r
runtime_function <- "parity"
runtime_path <- system.file("parity.R", package = "r2lambda")
dependencies <- NULL

# Might take a while, its building Docker image and pushing it to a remote repository
deploy_lambda(
  tag = "lambda-test",
  runtime_function = runtime_function,
  runtime_path = runtime_path,
  dependencies = dependencies
  )

invoke_lambda(
  function_name = "lambda-test",
  invocation_type = "RequestResponse",
  payload = list(number = 2),
  include_logs = FALSE
)

#> Lambda response payload: 
#> {"parity":"even"}
```
