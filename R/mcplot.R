
#' Monte Carlo plot
#'
#' @param data_mc An object returned from the mcstudy() function.
#' @param add_points
#' @param caption
#' @param var_x Name of the variable to be placed on the x axis.
#' @param var_shape Name of the variable to be depicted in point shapes.
#' @param var_facet Name of the variable to create facets.
#' @param ncol Number of columns for the facets.
#' @param marks Vector with values for adding horizontal lines.
#' @param ylim Vector with two values defining the limits for the y axis.
#' @param ylab Character string with the y axis label.
#' @param labels_col Labels for the color legend.
#' @export

mcplot <- function(data_mc,
                   add_points = TRUE,
                   caption = FALSE,
                   var_y = "p",
                   var_x = 1,
                   var_shape = 2,
                   var_facet = "Methods",
                   var_col = 3,
                   ncol = 2,
                   marks = NULL,
                   ylim = NULL,
                   ylab = "Value",
                   labels_col = NULL,
                   template = NULL) {

  if (identical(template, "power")) {
    ylim <- c(0, 100)
    marks <- c(5, 80)
    ylab <- "Percentage"
    labels_col <- c("Alpha error", "Power")
    var_y <- "p"
  }

  df <- mc_extract(data_mc,var_y = var_y)

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

  # draw plot --------

  p <- ggplot(df, aes)
  p <- p + geom_line()
  if (add_points) p <- p + geom_point()

  if (!is.null(marks))
    p <- p + geom_hline(yintercept = marks, size = 0.3, colour = "grey50")

  if (!is.null(ylim))
    p <- p + ylim(ylim[1], ylim[2])

  if (!is.null(var_col)) {
    if (is.null(labels_col)) labels_col <- levels(df[[var_col]])
    p <- p + theme_bw() +
      scale_color_brewer(palette = "Dark2", labels = labels_col)
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
