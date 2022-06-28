library(mcscan)

design <- design_template(
  n = n_sims,
  start_value = 50,
  s = 10,
  level = 1,
  trend = rnorm(n_sims, 0, abs(0.1/(A_length+B_length)) ),
  slope = rnorm(n_sims, 0, abs(0.1/B_length)),
  phase_design = list(A = A_length, B = B_length),
  distribution = "gaussian"
)

iterations <- list(
  A_length = seq(3, 20, by = 2),
  B_length = c(10, 15, 20, 30, 40, 50),
  n_sims = 50
)

eval_function <- "mean"
methods <- list(
  "Tau-U A vs. B" = tau_u_AB_es,
  "Tau-U trendA" = tau_u_trendA_es,
  "Tau-U trendA + trendB" = tau_u_trendA_trendB_es,
  "Tau-U adjusted" = tau_u_base_es
)

out <- mcstudy(
  iterations = iterations,
  design = design,
  method = methods,
  eval_function = eval_function
)

mcplot(out, caption = FALSE, reverse = FALSE, ylim = NA, marks = 0,
       ylab = "Tau", var_col = NULL)
