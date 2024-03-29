
#' @export
mc_extract <- function(data_mc, var_y) {

  df <- sapply(data_mc, function(x) unlist(attr(x, "iter"))) %>%
    t() %>%
    as.data.frame()

  methods <- attr(data_mc, "methods")
  n_methods <- length(methods)
  for (i in 1:length(methods)) {
    df[[methods[i]]] <- sapply(data_mc, function(x) x[[var_y]][i])
  }

  df <- pivot_longer(
    df,
    cols = (ncol(df) - length(methods) + 1):ncol(df),
    names_to = "Methods",
    values_to = "y"
  )
  df$Methods <- factor(df$Methods, levels = unique(df$Methods))

  df

}


