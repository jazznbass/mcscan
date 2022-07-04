#' MC scan
#'
#' @param design
#' @param iterations
#' @param method
#' @param parameter
#' @param alpha_test
#' @param power_test
#' @param store_design
#' @param ...
#'
#' @export

mcstudy <- function(design,
                    iterations,
                    methods,
                    store_design = FALSE,
                    ...) {

  starttime <- proc.time()

  n_iterations <- prod(sapply(iterations, length))
  counter <- 0

  out <- list()

  iter <- expand.grid(iterations)

  mc_env <- new.env()

  for(i in 1:nrow(iter)) {

    x <- paste0(names(iter[i,]), " = ", iter[i,])
    x <- lapply(x, str2lang)
    for(j in x) eval(j, envir = mc_env)

    new_design <- do.call(
      "design",
      lapply(design, function(x) eval(x, envir = mc_env))
    )

    mctab <- mcscan(
      design = new_design,
      method = methods,
      design_is_one_study = FALSE,
      ...
    )

    attr(mctab, "iter") <- iter[i,]

    if (store_design) attr(mctab, "design") <- new_design

    out <- c(out, list(mctab))

    # output remaining time
    counter <- counter + 1
    .progress_feedback(counter, n_iterations, (proc.time() - starttime)[3])
  }

  cat("\n")
  attr(out, "methods") <- names(methods)
  attr(out, "iterations") <- iterations
  attr(out, "design") <- design
  attr(out, "computation_duration") <- proc.time() - starttime

  attr(out, "iterations_table") <- sapply(out, function(x) unlist(attr(x, "iter"))) %>%
    t() %>%
    as.data.frame()

  out
}
