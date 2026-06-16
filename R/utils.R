# Internal formatting and conversion utilities

#' @keywords internal
.gobayes_to_df <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  if (is.data.frame(x)) {
    return(x)
  }
  
  if (
    is.list(x) &&
    length(x) > 0 &&
    all(vapply(x, is.list, logical(1)))
  ) {
    all_names <- unique(unlist(lapply(x, names), use.names = FALSE))
    
    rows <- lapply(x, function(row) {
      out <- stats::setNames(vector("list", length(all_names)), all_names)
      
      for (nm in all_names) {
        val <- row[[nm]]
        
        if (is.null(val)) {
          out[[nm]] <- NA
        } else if (length(val) == 1) {
          out[[nm]] <- val
        } else {
          out[[nm]] <- paste(val, collapse = ", ")
        }
      }
      
      as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    })
    
    return(do.call(rbind, rows))
  }
  
  as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

#' @keywords internal
.gobayes_line <- function(char = "-", width = 92) {
  cat(paste(rep(char, width), collapse = ""), "\n", sep = "")
}

#' @keywords internal
.gobayes_section <- function(title, width = 92) {
  cat("\n")
  cat(title, "\n", sep = "")
  .gobayes_line("-", width)
}

#' @keywords internal
.gobayes_print_df <- function(df, digits = 4) {
  if (is.null(df) || nrow(df) == 0) {
    cat("No data available.\n")
    return(invisible(NULL))
  }
  
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  
  numeric_cols <- vapply(df, is.numeric, logical(1))
  
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
    round(x, digits)
  })
  
  print(df, row.names = FALSE, right = FALSE)
  
  invisible(NULL)
}
