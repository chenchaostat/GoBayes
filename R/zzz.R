# Package-level environment for GoBayes configuration

.gobayes_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .gobayes_env$server_url <- Sys.getenv(
    "GOBAYES_SERVER_URL",
    unset = ""
  )
  
  .gobayes_env$api_token <- NULL
  
  timeout_env <- Sys.getenv("GOBAYES_TIMEOUT", unset = "")
  
  .gobayes_env$timeout <- if (nchar(timeout_env) > 0) {
    as.numeric(timeout_env)
  } else {
    120
  }
  
  if (is.na(.gobayes_env$timeout) || .gobayes_env$timeout <= 0) {
    .gobayes_env$timeout <- 120
  }
}
