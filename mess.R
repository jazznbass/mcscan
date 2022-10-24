library(mcscan)

design <- design_template(
  n = n_sims,
  start_value = 50,
  s = 10,
  level = level_effect,
  trend = rnorm(n_sims, 0, abs(0.1 / (A_length + B_length))),
  slope = rnorm(n_sims, 0, abs(0.1 / B_length)),
  phase_design = list(A = A_length, B = B_length),
  distribution = "gaussian"
)

iterations <- iterations_template(
  A_length = seq(3, 20, by = 2),
  B_length = c(10, 15, 20, 30, 40, 50),
  level_effect = c(0, 1),
  n_sims = 10
)



eval_function <- get_mcfn("standard")



methods <- list(
  'Tau AB' = get_mcfn("tau_u_AB"),
  'Tau trendA' = get_mcfn("tau_u_trendA"),
  'Tau trendA+B' = get_mcfn("tau_u_trendA_trendB"),
  'Tau adj' = get_mcfn("tau_u_base")
)

out <- mcstudy(
  iterations = iterations,
  design = design,
  method = methods,
  eval_function = eval_function
)

mcplot(
  out,
  caption = FALSE,
  marks = 0,
  var_y = "es",
  ylab = "Tau",
  var_col = "Methods",
  var_x = "A_length",
  var_shape = "level_effect",
  var_facet = "B_length"
)

mctable(out, format = "html", wider = "level_effect", label_header = c("No effect", "Effect"))

mctable(out, wider = "level_effect") %>% round(1)

kable(tab,
      escape = F,
      row.names = FALSE,
      digits = 2) %>%
  kable_classic()


tab_d <- as.data.frame(tab)
for (i in 1:nrow(tab)) {
  tab_d[i, -(1:2)] <- convert_r_d(r = convert_tau_r(tab_d[i, -(1:2)]),
                                nA = tab_d[[1]][i],
                                nB = tab_d[[2]][2])
}



library(kableExtra)
#names(tab_d)[3:10] <- rep(c("Tau<sub>AB</sub>", "Tau<sub>trendA</sub>", "Tau<sub>trendA+B</sub>",
#                            "Tau<sub>adj</sub>"), 2)

names(tab_d)[3:10] <-
  rep(
    c(
      "d Tau<sub>AB</sub>",
      "d Tau<sub>trendA</sub>",
      "d Tau<sub>trendA+B</sub>",
      "d Tau<sub>adj</sub>"
    ),
    2
  )

kable(tab_d,
      escape = F,
      row.names = FALSE,
      digits = 2) %>%
  kable_classic() %>%
  add_header_above(c(
    " " = 2,
    "No effect" = 4,
    "Effect" = 4
  ))

convert_tau_r(0.8)

x <- as.list(c(1:10))
rapply(x, function(x) x*x)
bench::mark(
  rapply(x, function(x) x*x),
  unlist(lapply(x, function(x) x*x)),
  vapply(x, function(x) x*x,FUN.VALUE = numeric(1)),
  sapply(x, function(x) x*x),
  check = FALSE
)
rapply(x, function(x) x*x)
