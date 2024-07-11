#' MC Table
#'
#' @param data_mc
#' @export

mctable <- function(data_mc,
                    wider = NULL,
                    format = "df",
                    digits = 2,
                    var_y = "p",
                    label_header = NULL) {

  df <- mc_extract(data_mc, var_y = var_y)

  methods <- unique(df$Methods)

  multiple_methods <- if (length(methods) == 1) FALSE else TRUE

  filter <- names(df)[apply(df, 2, function(x) length(unique(x)) != 1)]
  if (!"Methods" %in% names(df)) filter <- c(filter, "Methods")
  df <- df[, filter]

  id_var <- names(df)[!names(df) %in% c("Methods", "y")]

  out <- if (multiple_methods) {
    df %>% pivot_wider(names_from = "Methods", values_from = "y")
  } else df

  if(!is.null(wider)) {
    if (multiple_methods) {
      out <- out %>% pivot_wider(
        names_from = all_of(wider),
        values_from = all_of(methods),
        names_vary = "slowest"
      )
    }

    if (!multiple_methods) {
      out <- out %>% pivot_wider(
        names_from = all_of(wider),
        values_from = "y",
        names_vary = "slowest"
      )
    }

    id_var <- id_var[!id_var %in% wider]
  }

  if (!is.null(digits)) {
    out <- out %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
    #out <- round(out, digits)
  }

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

      names(out)[(length(id_var) + 1):ncol(out)] <- rep(as.character(methods), 2)
      out <- out %>%
        knitr::kable(
          escape = FALSE,
          row.names = FALSE,
          align = c("l", rep("c", ncol(out) - 1))
        ) %>%
        kableExtra::kable_classic(full_width = FALSE) %>%
        kableExtra::add_header_above(header)
    }
    return(out)

  }
  warning("unknown format")
}
