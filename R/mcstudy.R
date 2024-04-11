#' Design frame for a Monte-Carlo study
#'
#' @param design A design template created with the design_template() function
#' @param iterations A list with variables and their iterations.
#' @param methods A list of functions that analyze the random datasets.
#' @param store_design If true, the random datasets are stroed in the returned object.
#' @param ... Further paramters passed to the mcscan() function.
#'
#' @export

mcstudy <- function(design,
                    iterations,
                    methods,
                    store_design = FALSE,
                    rf_arguments = NULL,
                    n_sims = 100,
                    ...) {

  starttime <- proc.time()

  counter <- 0

  out <- list()

  tmp <- new.env()
  iter <- list()
  for(i in seq_along(iterations)) {
    assign(
      x = names(iterations)[i],
      value = eval(iterations[[i]], envir = tmp),
      pos = tmp
    )
    iter[[names(iterations)[i]]] <- eval(iterations[[i]], envir = tmp)
  }

  iter <- expand.grid(iter)

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
      n_sims = n_sims,
      rf_arguments = rf_arguments,
      ...
    )

    attr(mctab, "iter") <- iter[i,]

    if (store_design) attr(mctab, "design") <- new_design

    out <- c(out, list(mctab))

    # output remaining time
    counter <- counter + 1
    .progress_feedback(counter, nrow(iter), (proc.time() - starttime)[3])
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
