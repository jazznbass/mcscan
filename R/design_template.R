#' Design template
#'
#' @param ...
#'
#' @export
design_template <- function(...) {
  out <- as.list(substitute(...()))
  valid <- names(formals(scan::design))
  check <- sapply(names(out), function(x) !x %in% valid)
  if (any(check)) {
    stop("\nWrong argument(s): ",
         paste(names(out)[check], collapse = ", "))
  }
  out
}
