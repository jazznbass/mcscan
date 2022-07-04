
#' MC Plot
#'
#' @param data_mc
#' @param line_curved
#' @param add_points
#' @param caption
#' @param var_x
#' @param var_shape
#' @param var_facet
#' @param ncol
#' @param reverse
#' @param marks
#' @param ylim
#' @param ylab
#' @param labels_col
#' @export

mcplot <- function(data_mc,
                    line_curved = FALSE,
                    add_points = TRUE,
                    caption = FALSE,
                    var_x = 1,
                    var_shape = 2,
                    var_facet = "Method",
                    var_col = 3,
                    ncol = 2,
                    reverse = FALSE,
                    marks = c(5, 80),
                    ylim = c(0, 100),
                    ylab = "Percentage",
                    labels_col = NULL) {

  # extract data
  df <- sapply(data_mc, function(x) unlist(attr(x, "iter"))) %>%
    t() %>%
    as.data.frame()

  rn <- function(df, ...) {
    vars <- c(...)
    for(i in seq_along(vars)) {
      id <- which(names(df) %in% vars[i])
      if (length(id) > 0) names(df)[id] <- names(vars[i])
    }
    df
  }

  # reorganize data
  #df <- df %>%
  #  rn(
  #    "Total number of measurement times" = "length",
  #    "Number of measurement times phase A" = "A_length",
  #    "Number of measurement\ntimes phase B" = "B_length",
  #    "Trend effect" =  "trend_effect",
  #    "Intervention effect" =  "level_effect",
  #    "Initial behavior frequency" = "problemintensity"
  #  )

  methods <- attr(data_mc, "methods")
  n_methods <- length(methods)
  for(i in 1:length(methods)) {
    df[[methods[i]]] <- sapply(data_mc, function(x) x$values[i])
  }


  df <- pivot_longer(df,
    cols = (ncol(df) - length(methods) + 1):ncol(df),
    names_to = "Method", values_to = "y")

  if (is.numeric(var_x)) var_x <- names(df)[var_x]
  if (is.numeric(var_shape)) var_shape <- names(df)[var_shape]
  if (is.numeric(var_col)) var_col <- names(df)[var_col]
  if (is.numeric(var_facet)) var_facet <- names(df)[var_facet]


  df[[var_shape]] <- factor(df[[var_shape]])
  df[[var_facet]] <- factor(df[[var_facet]])
  df[[var_col]] <- factor(df[[var_col]])

  if (!is.null(var_col)) {
    aes <- aes(x = !!sym(var_x), y = y, color = !!sym(var_col), shape = !!sym(var_shape))
  } else {
    aes <- aes(x = !!sym(var_x), y = y, shape = !!sym(var_shape))
  }

  p <- ggplot(df, aes)

  if (line_curved) p <- p + geom_smooth(method = "loess", se = FALSE, size = 0.5)
  if (!line_curved) p <- p + geom_line()
  if (add_points) p <- p + geom_point()

  if (!isTRUE(is.na(marks)))
    p <- p + geom_hline(yintercept = marks, size = 0.3, colour = "grey50")

  if (!isTRUE(is.na(ylim)))
    p <- p + ylim(ylim[1], ylim[2])

  if (!is.null(var_col)) {
    p <- p + theme_bw() +
      scale_color_brewer(palette = "Dark2", labels = levels(df[[var_col]]))
  }
  p <- p + scale_x_continuous(
    breaks = unique(df[[var_x]]),
    limits = c(min(df[[var_x]]), max(df[[var_x]]))
  )

  if (var_facet %in% names(df)) {
    id <- which(names(df) == var_facet)
    var_facet <- paste0("`", var_facet, "`")
    names(df)[id] <- var_facet
    p <- p + facet_wrap(var_facet, ncol = ncol ,labeller = .label_both)
  }

  p <- p + ylab(ylab)

  #p <- p + scale_color_manual(labels = labels_col, values = c("red", "blue"))

  if (isTRUE(caption)) {
    design <- attr(data_mc, "design")
    design <- paste0(names(design), " = ", design, collapse = "\n")
    iterations <- attr(data_mc, "iterations")
    iterations <- paste0(names(iterations), " = ", iterations, collapse = "\n")
    caption <- paste0(design, "\n", iterations, collape = "")
    p <- p + labs(caption = caption)
  }

  p
}

.label_both <- function (labels,
                         multi_line = FALSE,
                         sep = ": ",
                         pre = letters){
  value <- label_value(labels, multi_line = multi_line)
  variable <- ggplot2:::label_variable(labels, multi_line = multi_line)
  if (multi_line) {
    out <- vector("list", length(value))
    for (i in seq_along(out)) {
      out[[i]] <- paste(variable[[i]], value[[i]], sep = sep)
    }
  }
  else {
    value <- do.call("paste", c(value, sep = ", "))
    variable <- do.call("paste", c(variable, sep = ", "))
    out <- Map(paste, variable, value, sep = sep)
    out <- list(unname(unlist(out)))
  }


  for (i in seq_along(out)) {
    out[[i]] <- paste0(pre[1:length(out[[i]])], ") ", out[[i]])
  }

  out
}
