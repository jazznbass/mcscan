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



eval_function <- get_mcfn("mean")
methods <- list(
  'Tau AB' = get_mcfn("tau_u_AB_es"),
  'Tau trendA' = get_mcfn("tau_u_trendA_es"),
  'Tau trendA+B' = get_mcfn("tau_u_trendA_trendB_es"),
  'Tau adj' = get_mcfn("tau_u_base_es")
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
  ylab = "Tau",
  var_col = "Methods",
  var_x = "A_length",
  var_shape = "level_effect",
  var_facet = "B_length"
)

tab <- mctable(out) %>%
  pivot_wider(names_from = "level_effect", values_from = "y") %>%
  pivot_wider(names_from = "Methods", values_from = 5:6) %>%
  select(-n_sims)



tab_d <- as.data.frame(tab)
for (i in 1:nrow(tab)) {
  tab_d[i, 3:10] <- convert_r_d(r = convert_tau_r(tab_d[i, 3:10]),
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

tau_d(0.8, 3, 10)
