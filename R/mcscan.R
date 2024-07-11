#' @export
mcscan <- function(design,
                   method = c("plm_level", "rand", "tauU"),
                   remove_slope = FALSE,
                   remove_level = FALSE,
                   n_sims = 100,
                   design_is_one_study = TRUE,
                   eval_function = "standard",
                   rf_arguments = NULL,
                   labels = NA) {


  starttime <- proc.time()

  if (inherits(eval_function, "character")) eval_function <- get_mcfn(eval_function)

  mc_fun <- unlist(
    lapply(
      method,
      function(x) if (inherits(x, "character")) get_mcfn(x) else x
    ),
    recursive = FALSE
  )

  # return object
  out <- data.frame(Method = names(mc_fun))
  #out <- tibble::tibble(Method = names(mc_fun))

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

  mc_tab <- .mc_scdf(
    design = design,
    n_sims = n_sims,
    methods = mc_fun,
    design_is_one_study = design_is_one_study,
    eval_function = eval_function,
    rf_arguments = rf_arguments
  )

  mc_tab <- t(as.data.frame(mc_tab))
  out <- cbind(out, mc_tab)

  if(!identical(labels, NA)) names(out)[2:(length(labels) + 1)] <- labels

  attr(out, "methods") <- names(method)
  attr(out, "computation_duration") <- proc.time() - starttime

  out
}

.mc_scdf <- function(design,
                     n_sims,
                     methods,
                     design_is_one_study,
                     eval_function,
                     rf_arguments = NULL) {

  # Generate random sample ----------------------------------------------------
  rand_sample <- list()

  if (design_is_one_study) {
    for(i in 1:n_sims) {
      rand_sample[[i]] <- do.call(
        random_scdf,
        c(list(design = design), rf_arguments)
      )
    }
  }

  if (!design_is_one_study) {
    tmp <- do.call(random_scdf, c(list(design = design), rf_arguments))
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

