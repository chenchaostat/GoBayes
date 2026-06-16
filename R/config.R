# Server configuration functions

#' Set GoBayes Server URL
#'
#' @param url Character. Base URL of the GoBayes API server.
#' @param timeout Numeric. Request timeout in seconds. Default is 120.
#' @return Invisibly returns the previous server URL.
#' @export
gobayes_set_server <- function(url, timeout = 120) {
  if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
    stop("url must be a non-empty character string.")
  }
  
  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("timeout must be a positive number.")
  }
  
  url <- sub("/$", "", url)
  
  old_url <- .gobayes_env$server_url
  .gobayes_env$server_url <- url
  .gobayes_env$timeout <- timeout
  
  cli::cli_inform("GoBayes server has been configured.")
  
  invisible(old_url)
}

#' Get Current GoBayes Server URL
#'
#' @param masked Logical. Whether to mask the URL in output. Default TRUE.
#' @return Character. Current server URL.
#' @export
gobayes_get_server <- function(masked = TRUE) {
  url <- .gobayes_env$server_url
  
  if (masked) {
    return(.gobayes_mask(url))
  }
  
  url
}

#' Set GoBayes API Token
#'
#' @param token Character. API token issued by the GoBayes service.
#' @return Invisibly returns TRUE.
#' @export
gobayes_set_api_token <- function(token) {
  if (!is.character(token) || length(token) != 1 || nchar(token) == 0) {
    stop("token must be a non-empty character string.")
  }
  
  .gobayes_env$api_token <- token
  
  cli::cli_inform("GoBayes API token has been configured for this R session.")
  
  invisible(TRUE)
}

#' Clear GoBayes API Token
#'
#' @return Invisibly returns TRUE.
#' @export
gobayes_clear_api_token <- function() {
  .gobayes_env$api_token <- NULL
  
  cli::cli_inform("GoBayes API token has been cleared from this R session.")
  
  invisible(TRUE)
}

#' Check Whether API Token Is Available
#'
#' @return Logical. TRUE if an API token is available.
#' @export
gobayes_has_api_token <- function() {
  !is.null(.gobayes_get_api_token())
}

#' Health Check for GoBayes Server
#'
#' @return Invisibly returns TRUE if the server is reachable and healthy.
#' @export
gobayes_health_check <- function() {
  base_url <- .gobayes_env$server_url
  timeout <- .gobayes_env$timeout
  
  if (
    is.null(base_url) ||
    !is.character(base_url) ||
    length(base_url) != 1 ||
    nchar(base_url) == 0
  ) {
    cli::cli_abort(
      c(
        "x" = "GoBayes server is not configured.",
        "i" = "Use {.fn gobayes_set_server} or set environment variable {.envvar GOBAYES_SERVER_URL}."
      )
    )
  }
  
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append("health") |>
    httr2::req_timeout(timeout)
  
  token <- .gobayes_get_api_token()
  
  if (!is.null(token) && nchar(token) > 0) {
    req <- req |>
      httr2::req_auth_bearer_token(token)
  }
  
  tryCatch(
    {
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      
      if (status == 200) {
        cli::cli_inform(c("v" = "GoBayes server is reachable and healthy."))
        return(invisible(TRUE))
      }
      
      cli::cli_warn("GoBayes server returned a non-OK status.")
      invisible(FALSE)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Cannot reach GoBayes server.",
          "i" = "Please check server configuration, network connection, and authentication.",
          "i" = "Error: {conditionMessage(e)}"
        )
      )
    }
  )
}
