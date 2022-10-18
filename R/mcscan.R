#' @export
mcscan <- function(design,
                   method = c("plm_level", "rand", "tauU"),
                   remove_slope = FALSE,
                   remove_level = FALSE,
                   n_sim = 100,
                   design_is_one_study = TRUE,
                   eval_function = "perc_sig",
                   labels = "values") {


  starttime <- proc.time()

  if (identical(eval_function, "perc_sig")) eval_function <- mc_perc_sig
  if (identical(eval_function, "mean")) eval_function <- mc_mean

  mc_fun <- unlist(
    lapply(
      method,
      function(x) if (inherits(x, "character")) scan:::mc_function(x) else x
    ),
    recursive = FALSE
  )

  # return object
  #out <- data.frame(Method = names(mc_fun))
  out <- tibble::tibble(Method = names(mc_fun))

  # remove effects

  if (remove_level) {
      design$cases <- lapply(
        design$cases,
        function(x) {x$level <- rep(0, length = length(x$length)); x}
      )
  }

  if (remove_slope) {
    design$cases <- lapply(
      design$cases,
      function(x) {x$slope <- rep(0, length = length(x$length)); x}
    )
  }

  # mc calculation ----------

  mc_tab <- mcscan:::.mc_scdf(
    design = design,
    n_sim = n_sim,
    methods = mc_fun,
    design_is_one_study = design_is_one_study,
    eval_function = eval_function
  )

  out <- cbind(out, mc_tab)
  #out <- dplyr::bind_cols(out, t(mc_tab))

  if(!identical(labels, NA)) names(out)[2:(length(labels) + 1)] <- labels

  attr(out, "methods") <- names(method)
  attr(out, "computation_duration") <- proc.time() - starttime
  #class(out) <- c("mcscan")
  out
}

.mc_scdf <- function(design,
                     n_sim,
                     methods,
                     design_is_one_study,
                     eval_function) {

  # Genrate random sample ----------------------------------------------------
  rand_sample <- list()

  if (design_is_one_study) {
    for(i in 1:n_sim) rand_sample[[i]] <- random_scdf(design = design)
  }

  if (!design_is_one_study) {
    tmp <- random_scdf(design = design)
    for (i in seq_along(tmp)) rand_sample[[i]] <- tmp[i]
  }

  # analyse random sample ---------------------------------------------------

  test_function <- function(method) {
    x <- lapply(rand_sample, method)
    do.call(eval_function, list(x))
  }
  out <-  lapply(methods, test_function)

  # return
  out
}

