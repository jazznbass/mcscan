
#' Get a predefined function for the Monte Carlo procedure
#'
#' @param x Character with the name of a predefined function. Leave empty to
#'   get the names of all possible functions.
#'
#' @return A function or a list of function names.
#' @export
get_mcfn <- function(x) {
  if (missing(x)) {
    names(mcfn)
  } else {
    mcfn[[x]]
  }
}

#' List Object with predefined functions.
#'
#' @export
mcfn <- list(
  tau_u_trendA = function(x) {
    res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
    list(
      p = res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "p")],
      es = res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "Tau")]
    )
  },

  tau_u_trendA_trendB = function(x) {
    res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
     list(
      p = res[which(row.names(res) == "A vs. B + Trend B - Trend A"), which(names(res) == "p")],
      es = res[which(row.names(res) == "A vs. B + Trend B - Trend A"), which(names(res) == "Tau")]
    )
  },

  tau_u_AB = function(x) {
    res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
    list(
      p = res[which(row.names(res) == "A vs. B"), which(names(res) == "p")],
      es = res[which(row.names(res) == "A vs. B"), which(names(res) == "Tau")]
    )
  },

  tau_u_base = function(x) {
    res <- corrected_tau(x, continuity = FALSE, repeated = FALSE)
    list(p = res$p, es = res$tau)
  },

  tau_u_trendA_es = function(x) {
    res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
    res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "Tau")]
  },

  tau_u_trendA_trendB_es = function(x) {
    res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
    res[which(row.names(res) == "A vs. B + Trend B - Trend A"), which(names(res) == "Tau")]
  },

  tau_u_AB_es = function(x) {
    res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
    res[which(row.names(res) == "A vs. B"), which(names(res) == "Tau")]
  },

  tau_u_base_es = function(x) {
    corrected_tau(x, continuity = FALSE, repeated = FALSE)$tau
  },


  standard = function(x) {

    p <- unlist(lapply(x, function(y) y$p))
    es <- unlist(lapply(x, function(y) y$es))
    c(p = mean(p <= 0.05, na.rm = TRUE) * 100, es = mean(es, na.rm = TRUE))
  },

  perc_sig = function(x) {

    x <- unlist(lapply(x, function(y) y$p))
    mean(x <= 0.05, na.rm = TRUE) * 100
  },

  pos_perc_sig = function(x) {
    x <- unlist(lapply(x, function(y) {
      if (y$es <= 0) y$p <- 1
      y$p
    }
    ))
    mean(x <= 0.05, na.rm = TRUE) * 100
  },

  neg_perc_sig = function(x) {
    p <- unlist(lapply(x, function(y) {
      if (y$es >= 0) y$p <- 1
      y$p
    }
    ))
    mean(p <= 0.05, na.rm = TRUE) * 100
  },

  pos_neg_perc_sig = function(x) {
    x_pos <- unlist(lapply(x, function(y) {
      if (y$es <= 0) y$p <- 1
      y$p
    }
    ))
    prop_sig_pos <- mean(x_pos <= 0.05, na.rm = TRUE) * 100

    x_neg <- unlist(lapply(x, function(y) {
      if (y$es >= 0) y$p <- 1
      y$p
    }
    ))
    prop_sig_neg <- mean(x_neg <= 0.05, na.rm = TRUE) * 100

    c(pos = prop_sig_pos, neg = prop_sig_neg)

  },

  mean_es = function(x) {
    es <- unlist(lapply(x, function(y) y$es))
    mean(es, na.rm = TRUE)
  },

  mc_quantile = function(x) {
    out <- list()
    x <- quantile(x, c(0.16, 0.5, 0.84))
    out$P16 <- x[1]
    out$P50 <- x[2]
    out$P84 <- x[3]
    out
  },

  mc_quantile2 = function(x) {
    out <- list()
    x <- quantile(x)
    out$P25 <- x[2]
    out$P50 <- x[3]
    out$P75 <- x[4]
    out
  }
)


