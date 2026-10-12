versions <- function() {
  list(
    glue = as.character(packageVersion("glue")),
    jsonlite = as.character(packageVersion("jsonlite"))
  )
}
lambdr::start_lambda()
