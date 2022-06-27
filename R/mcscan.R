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

mcscan <- function(design,
                    iterations,
                    method,
                    parameter = "p",
                    alpha_test = TRUE,
                    power_test = TRUE,
                    store_design = FALSE,
                    ...) {

  starttime <- proc.time()

  n_iterations <- prod(sapply(iterations, length))
  counter <- 0

  return_pa <- list()

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

    pa <- mcpower(
      design = new_design,
      method = method,
      design_is_one_study = FALSE,
      alpha_test = alpha_test,
      power_test = power_test,
      binom_test = FALSE,
      ...
    )

    attr(pa, "iter") <- iter[i,]

    if (store_design) attr(pa, "design") <- new_design

    return_pa <- c(return_pa, list(pa))

    # output remaining time
    counter <- counter + 1
    .progress_feedback(counter, n_iterations, (proc.time() - starttime)[3])
  }

  cat("\n")

  attr(return_pa, "iterations") <- iterations
  attr(return_pa, "design") <- design
  attr(return_pa, "computation_duration") <- proc.time() - starttime

  attr(return_pa, "iterations_table") <- sapply(return_pa, function(x) unlist(attr(x, "iter"))) %>%
    t() %>%
    as.data.frame()

  return_pa
}
