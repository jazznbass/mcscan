.onAttach <- function(lib, pkg, ...) {
  packageStartupMessage("\033[34m", "MC scan", "\033[31m")

}

.onLoad <- function(lib, pkg, ...) {
  # global options ----------------------------------------------------------
  op <- options()
  op_scan <- list(
    mcscan.progress.monitor = TRUE
  )

  toset <- !(names(op_scan) %in% names(op))
  if (any(toset)) options(op_scan[toset])

  invisible()
}

.onAttach()
