#' Convert various effect size measures
#'
#' @param r Pearson correlation
#' @param nA Measurement times phase A
#' @param nB Measurement times phase B
#' @export
convert_r_d <- function(r, nA, nB) {
  # Adapted from: https://stats.stackexchange.com/a/526809/183

  m <- nA + nB - 2
  h <- m / nA + m / nB
  d <- r * sqrt(h) /  sqrt(1 - r^2)

  d
}

#' @rdname convert_r_d
#' @export
#' @param tau Kendall's tau
convert_tau_r <- function(tau) sin(tau * pi/2)




