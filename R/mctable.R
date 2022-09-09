#' MC Table
#'
#' @param data_mc
#' @param caption
#' @param reference_category
#' @param first
#' @param second
#' @param digits
#' @export

mctable <- function(data_mc,
                    wider = NULL,
                    caption = TRUE,
                    reference_category = 0,
                    first = 1,
                    second = 2,
                    digits = 0) {


  df <- mc_extract(data_mc)

  methods <- unique(df$Methods)

  filter <- names(df)[apply(df, 2, function(x) length(unique(x)) != 1)]
  if (!"Methods" %in% names(df)) filter <- c(filter, "Methods")
  df <- df[, filter]

  id_var <- names(df)[!names(df) %in% c("Methods", "y")]

  df <- df %>%
    pivot_wider(names_from = "Methods", values_from = "y")


  if(!is.null(wider)) {
    df <- df %>% pivot_wider(
      names_from = all_of(wider),
      values_from = all_of(methods),
      names_vary = "slowest"
    )
    id_var <- id_var[!id_var %in% wider]
  }

  df


}
