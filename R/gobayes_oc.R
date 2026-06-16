# Client-side function for Bayesian Go/No-Go OC table

#' Bayesian Go/No-Go Operating Characteristic Table
#'
#' Compute Bayesian Go/No-Go operating characteristics by calling a remote
#' GoBayes API server.
#'
#' @param n_total Integer. Total sample size.
#' @param alloc_ratio Numeric vector of length 2. Treatment/control allocation ratio.
#' @param delta Numeric. Default treatment-control difference threshold.
#' @param go_delta Numeric or NULL. Go threshold delta. If NULL, uses \code{delta}.
#' @param nogo_delta Numeric or NULL. No-Go threshold delta. If NULL, uses \code{delta}.
#' @param go_prob Numeric. Posterior probability cutoff for Go.
#' @param nogo_prob Numeric. Posterior probability cutoff for No-Go.
#' @param prior_t Numeric vector of length 2. Beta prior for treatment arm.
#' @param prior_c Numeric vector of length 2. Beta prior for control arm.
#' @param control_rates Numeric vector. True control response rates.
#' @param treatment_rates Numeric vector. True treatment response rates.
#' @param n_sim Integer. Number of Monte Carlo simulations.
#' @param mc_seed Integer. Monte Carlo random seed.
#' @param run_exact Logical. Whether to run exact enumeration.
#' @param run_mc Logical. Whether to run Monte Carlo simulation.
#' @param method5_pc_hat_boundary Numeric or NULL. Control rate used to derive method 5 boundary.
#' @param integration_tol Numeric. Integration tolerance.
#' @param digits Integer. Digits used in printing.
#'
#' @return An object of class \code{GoBayesOC}.
#' @export
#'
#' @examples
#' \dontrun{
#' gobayes_set_server("http://116.62.190.134:10002")
#'
#' res <- gobayes_oc(
#'   n_total = 75,
#'   alloc_ratio = c(2, 1),
#'   go_delta = 0.15,
#'   nogo_delta = 0.25,
#'   go_prob = 0.80,
#'   nogo_prob = 0.10,
#'   control_rates = 0.15,
#'   treatment_rates = c(0.15, 0.25, 0.35, 0.40, 0.45),
#'   n_sim = 10000,
#'   method5_pc_hat_boundary = 0.15
#' )
#'
#' print(res)
#' }
gobayes_oc <- function(
    n_total = 100,
    alloc_ratio = c(1, 1),
    delta = 0.05,
    go_delta = NULL,
    nogo_delta = NULL,
    go_prob = 0.70,
    nogo_prob = 0.30,
    prior_t = c(1, 1),
    prior_c = c(1, 1),
    control_rates = 0.30,
    treatment_rates = c(0.30, 0.35, 0.40),
    n_sim = 100000,
    mc_seed = 42,
    run_exact = TRUE,
    run_mc = TRUE,
    method5_pc_hat_boundary = NULL,
    integration_tol = 1e-10,
    digits = 1
) {
  if (!is.numeric(n_total) || length(n_total) != 1 || n_total <= 0) {
    stop("n_total must be a positive number.")
  }
  
  if (!is.numeric(alloc_ratio) || length(alloc_ratio) != 2 || any(alloc_ratio <= 0)) {
    stop("alloc_ratio must be a positive numeric vector of length 2.")
  }
  
  if (!is.numeric(delta) || length(delta) != 1 || delta < -1 || delta > 1) {
    stop("delta must be between -1 and 1.")
  }
  
  if (!is.null(go_delta) && (!is.numeric(go_delta) || length(go_delta) != 1)) {
    stop("go_delta must be numeric or NULL.")
  }
  
  if (!is.null(nogo_delta) && (!is.numeric(nogo_delta) || length(nogo_delta) != 1)) {
    stop("nogo_delta must be numeric or NULL.")
  }
  
  if (go_prob < 0 || go_prob > 1) {
    stop("go_prob must be between 0 and 1.")
  }
  
  if (nogo_prob < 0 || nogo_prob > 1) {
    stop("nogo_prob must be between 0 and 1.")
  }
  
  if (any(control_rates < 0 | control_rates > 1)) {
    stop("control_rates must be between 0 and 1.")
  }
  
  if (any(treatment_rates < 0 | treatment_rates > 1)) {
    stop("treatment_rates must be between 0 and 1.")
  }
  
  params <- list(
    n_total = n_total,
    alloc_ratio = alloc_ratio,
    delta = delta,
    go_delta = go_delta,
    nogo_delta = nogo_delta,
    go_prob = go_prob,
    nogo_prob = nogo_prob,
    prior_t = prior_t,
    prior_c = prior_c,
    control_rates = control_rates,
    treatment_rates = treatment_rates,
    n_sim = n_sim,
    mc_seed = mc_seed,
    run_exact = run_exact,
    run_mc = run_mc,
    method5_pc_hat_boundary = method5_pc_hat_boundary,
    integration_tol = integration_tol
  )
  
  result <- gobayes_api_call("bayesian_go_nogo_oc", params)
  
  result$result <- .gobayes_to_df(result$result)
  result$input_raw <- params
  result$digits <- digits
  
  class(result) <- "GoBayesOC"
  
  result
}

#' @export
print.GoBayesOC <- function(x, digits = x$digits %||% 1, ...) {
  old_width <- getOption("width")
  on.exit(options(width = old_width), add = TRUE)
  
  options(width = max(160, old_width))
  
  res <- x$result
  
  cat("\n")
  .gobayes_line("=", 92)
  cat(" Bayesian Go/No-Go Operating Characteristic Table\n")
  cat(" Five-Method Comparison\n")
  .gobayes_line("=", 92)
  
  if (is.null(res) || nrow(res) == 0) {
    cat("No result available.\n")
    return(invisible(x))
  }
  
  methods <- list(
    list(
      id = "Method 1",
      label = "Exact",
      go = "Go_Method1_Exact",
      pend = "Pend_Method1_Exact",
      nogo = "NoGo_Method1_Exact"
    ),
    list(
      id = "Method 2",
      label = "Boundary",
      go = "Go_Method2_Boundary",
      pend = "Pend_Method2_Boundary",
      nogo = "NoGo_Method2_Boundary"
    ),
    list(
      id = "Method 3",
      label = "Monte Carlo",
      go = "Go_Method3_MC",
      pend = "Pend_Method3_MC",
      nogo = "NoGo_Method3_MC"
    ),
    list(
      id = "Method 4",
      label = "Normal Approx",
      go = "Go_Method4_Normal",
      pend = "Pend_Method4_Normal",
      nogo = "NoGo_Method4_Normal"
    ),
    list(
      id = "Method 5",
      label = "Continuous Boundary + Normal",
      go = "Go_Method5_ContBdryNorm",
      pend = "Pend_Method5_ContBdryNorm",
      nogo = "NoGo_Method5_ContBdryNorm"
    )
  )
  
  fmt <- function(z) {
    ifelse(is.na(z), "NA", sprintf(paste0("%.", digits, "f"), z))
  }
  
  for (m in methods) {
    .gobayes_section(paste0(m$id, ": ", m$label, " probability (%)"))
    
    out <- data.frame(
      Control = paste0(round(100 * res$control_orr), "%"),
      Treatment = paste0(round(100 * res$treatment_orr), "%"),
      Difference = paste0(round(100 * res$orr_diff), "%"),
      Go = fmt(res[[m$go]]),
      Pending = fmt(res[[m$pend]]),
      NoGo = fmt(res[[m$nogo]]),
      stringsAsFactors = FALSE
    )
    
    print(out, row.names = FALSE, right = FALSE)
  }
  
  .gobayes_section("Method 5 Boundaries")
  
  boundary_cols <- c(
    "control_orr",
    "Method5_pc_hat_boundary",
    "Method5_NoGoBoundary_percent",
    "Method5_GoBoundary_percent"
  )
  
  boundary_show <- unique(res[, boundary_cols, drop = FALSE])
  
  names(boundary_show) <- c(
    "control_orr",
    "pc_hat_for_boundary",
    "NoGo_boundary_percent",
    "Go_boundary_percent"
  )
  
  .gobayes_print_df(boundary_show, digits = digits)
  
  .gobayes_section("Average Runtime Per Scenario")
  
  time_df <- data.frame(
    Method = c(
      "Method 1 Exact",
      "Method 2 Boundary",
      "Method 3 Monte Carlo",
      "Method 4 Normal Approx",
      "Method 5 Continuous Boundary + Normal"
    ),
    Seconds = c(
      mean(res$time_Method1_Exact, na.rm = TRUE),
      mean(res$time_Method2_Boundary, na.rm = TRUE),
      mean(res$time_Method3_MC, na.rm = TRUE),
      mean(res$time_Method4_Normal, na.rm = TRUE),
      mean(res$time_Method5_ContBdryNorm, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  
  .gobayes_print_df(time_df, digits = 2)
  
  .gobayes_section("Notes")
  cat("1. Method 1 uses exact enumeration over treatment and control responders.\n")
  cat("2. Method 2 derives integer decision boundaries conditional on control responders.\n")
  cat("3. Method 3 uses Monte Carlo simulation and may vary with n_sim and mc_seed.\n")
  cat("4. Method 4 uses a normal approximation to the Beta posterior difference.\n")
  cat("5. Method 5 derives continuous Bayesian boundaries and applies a normal approximation to the observed rate difference.\n")
  cat("\n")
  
  invisible(x)
}
