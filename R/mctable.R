#' MC Table
#'
#' @param data_mc
#' @export

mctable <- function(data_mc,
                    wider = NULL,
                    format = "df",
                    digits = 2,
                    label_header = NULL) {

  df <- mc_extract(data_mc)

  methods <- unique(df$Methods)

  filter <- names(df)[apply(df, 2, function(x) length(unique(x)) != 1)]
  if (!"Methods" %in% names(df)) filter <- c(filter, "Methods")
  df <- df[, filter]

  id_var <- names(df)[!names(df) %in% c("Methods", "y")]

  out <- df %>% pivot_wider(names_from = "Methods", values_from = "y")


  if(!is.null(wider)) {
    out <- out %>% pivot_wider(
      names_from = all_of(wider),
      values_from = all_of(methods),
      names_vary = "slowest"
    )
    id_var <- id_var[!id_var %in% wider]
  }

  if (!is.null(digits)) out <- round(out, digits)

  if (format == "df") return(out)

  if (format == "html") {

    if (!is.null(wider)) {
      header <- c(" " = length(id_var), "2" = length(methods),
                  "3" = length(methods)
      )
      if (is.null(label_header)) {
        names(header)[2:3] <- paste(wider, levels(as.factor(df[[wider]])))
      } else {
        names(header)[2:3] <- label_header
      }

      names(out)[(length(id_var) + 1):ncol(out)] <- rep(methods, 2)
      out <- out %>%
        knitr::kable(escape = FALSE, row.names = FALSE, align = c("l", rep("c", ncol(out) - 1))) %>%
        kableExtra::kable_classic() %>%
        kableExtra::add_header_above(header)
    }
    return(out)

  }
  warning("unknown format")
}
