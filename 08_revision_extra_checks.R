# =============================================================================
# 03_revision_extra_checks.R · Food Policy revision extras
# Purpose: run extra reviewer-requested checks after 01_data_prep.R
# Inputs expected from 01_data_prep.R: trade_df, K (usually 2), and variables:
#   d_ln_dqty, d_ln_milkp, d_ln_tra, d_ln_trd, d_ln_alf,
#   d_ln_faop, d_ln_fuel, covid, month_fe, year_fe, unit_id, time_idx
# =============================================================================

source(here::here("01_data_prep.R"))

suppressPackageStartupMessages({
  library(fixest)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(readr)
  library(modelsummary)
})

# ---- 0. Global settings ------------------------------------------------------
K <- if (exists("K")) K else 2
OUT_DIR <- here::here("outputs", "revision_extra")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Preferred standard error for revised tables: Newey-West/HAC.
# If your installed fixest cannot compute NW, the helper falls back to heteroskedastic-robust.
get_vcov <- function(model = NULL, lag = 2) {
  # fixest supports vcov = "NW" when panel.id is supplied. Use lag through setFixest_vcov if desired.
  "NW"
}
VCOV_MAIN <- "NW"     # Use "hetero" if NW fails in your environment.
VCOV_FALLBACK <- "hetero"

# ---- 1. Create explicit lags to simplify cumulative tests and IV variants ----
make_lags <- function(data, vars, K = 2, id = "unit_id", time = "time_idx") {
  stopifnot(all(c(id, time, vars) %in% names(data)))
  data %>%
    arrange(.data[[id]], .data[[time]]) %>%
    group_by(.data[[id]]) %>%
    group_modify(~{
      dd <- .x
      for (v in vars) {
        for (k in 0:K) {
          nm <- paste0(v, "_l", k)
          dd[[nm]] <- if (k == 0) dd[[v]] else dplyr::lag(dd[[v]], k)
        }
      }
      dd
    }) %>%
    ungroup()
}

reg_df <- trade_df %>%
  make_lags(vars = c("d_ln_tra", "d_ln_trd", "d_ln_alf"), K = K) %>%
  mutate(tariff_interact_0 = d_ln_tra_l0 * d_ln_trd_l0)

tra_terms <- paste0("d_ln_tra_l", 0:K)
trd_terms <- paste0("d_ln_trd_l", 0:K)
alf_terms <- paste0("d_ln_alf_l", 0:K)
controls  <- c("d_ln_faop", "d_ln_fuel", "covid")
fe_rhs    <- "month_fe + year_fe"

rhs <- function(x) paste(x, collapse = " + ")
mkf <- function(y, x, fe = fe_rhs) as.formula(paste0(y, " ~ ", rhs(x), " | ", fe))

# ---- 2. Main equations with HAC SEs (M10) -----------------------------------
# Eq. 16 / main Col. 1: tariff changes -> US-origin dairy import volume
m_qd_hac <- feols(
  mkf("d_ln_dqty", c(tra_terms, trd_terms, controls)),
  data = reg_df,
  panel.id = ~ unit_id + time_idx,
  vcov = VCOV_MAIN
)

# Eq. 18 / main Col. 2: reduced-form milk-price response
m_price_hac <- feols(
  mkf("d_ln_milkp", c(tra_terms, trd_terms, controls)),
  data = reg_df,
  panel.id = ~ unit_id + time_idx,
  vcov = VCOV_MAIN
)

# Eq. 19: use contemporaneous interaction only to match the manuscript equation.
# If you want lagged interactions, revise Eq. 19 and the table label accordingly.
m_interact_hac <- feols(
  mkf("d_ln_milkp", c(tra_terms, trd_terms, "tariff_interact_0", controls)),
  data = reg_df,
  panel.id = ~ unit_id + time_idx,
  vcov = VCOV_MAIN
)

# Eq. 20 / main Col. 4: baseline over-identified IV with both tariffs
m_iv_full <- feols(
  as.formula(paste0(
    "d_ln_milkp ~ ", rhs(c(alf_terms, controls)), " | ", fe_rhs, " | ",
    "d_ln_dqty ~ ", rhs(c(tra_terms, trd_terms))
  )),
  data = reg_df,
  panel.id = ~ unit_id + time_idx,
  vcov = VCOV_MAIN
)

# ---- 3. Cumulative effects and joint Wald tests (M3) -------------------------
get_vcov_matrix <- function(model, vcov_type = VCOV_MAIN) {
  tryCatch(vcov(model, vcov = vcov_type), error = function(e) vcov(model, vcov = VCOV_FALLBACK))
}

cum_effect <- function(model, terms, label, vcov_type = VCOV_MAIN) {
  b <- coef(model)
  V <- get_vcov_matrix(model, vcov_type)
  idx <- match(terms, names(b))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) {
    return(tibble(label = label, estimate = NA_real_, se = NA_real_, t = NA_real_, p = NA_real_, n_terms = 0L))
  }
  est <- sum(b[idx])
  se  <- sqrt(sum(V[idx, idx, drop = FALSE]))
  t   <- est / se
  p   <- 2 * pnorm(abs(t), lower.tail = FALSE)
  tibble(label = label, estimate = est, se = se, t = t, p = p, n_terms = length(idx))
}

wald_joint <- function(model, terms, label, vcov_type = VCOV_MAIN) {
  b <- coef(model)
  V <- get_vcov_matrix(model, vcov_type)
  idx <- match(terms, names(b))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) {
    return(tibble(label = label, chi2 = NA_real_, df = 0L, F_approx = NA_real_, p = NA_real_))
  }
  bi <- b[idx]
  Vi <- V[idx, idx, drop = FALSE]
  stat <- as.numeric(t(bi) %*% solve(Vi) %*% bi)
  df <- length(idx)
  tibble(label = label, chi2 = stat, df = df, F_approx = stat / df, p = pchisq(stat, df, lower.tail = FALSE))
}

cum_table <- bind_rows(
  cum_effect(m_qd_hac,    tra_terms, "Eq.16: Σ gamma_A, alfalfa tariff -> dairy imports"),
  cum_effect(m_qd_hac,    trd_terms, "Eq.16: Σ gamma_D, dairy tariff -> dairy imports"),
  cum_effect(m_price_hac, tra_terms, "Eq.18: Σ theta_A, alfalfa tariff -> milk price"),
  cum_effect(m_price_hac, trd_terms, "Eq.18: Σ theta_D, dairy tariff -> milk price"),
  cum_effect(m_interact_hac, tra_terms, "Eq.19: Σ theta_A with interaction"),
  cum_effect(m_interact_hac, trd_terms, "Eq.19: Σ theta_D with interaction")
)

wald_table <- bind_rows(
  wald_joint(m_qd_hac,    tra_terms, "Eq.16: H0 gamma_A0=gamma_A1=gamma_A2=0"),
  wald_joint(m_qd_hac,    trd_terms, "Eq.16: H0 gamma_D0=gamma_D1=gamma_D2=0"),
  wald_joint(m_price_hac, tra_terms, "Eq.18: H0 theta_A0=theta_A1=theta_A2=0"),
  wald_joint(m_price_hac, trd_terms, "Eq.18: H0 theta_D0=theta_D1=theta_D2=0")
)

write_csv(cum_table, file.path(OUT_DIR, "M3_cumulative_effects.csv"))
write_csv(wald_table, file.path(OUT_DIR, "M3_joint_wald_tests.csv"))

# ---- 4. IV diagnostics: beta_Q, KP, AR inverted CI, tauD-only IV (M1, M2, O3) --
extract_beta_Q <- function(model) {
  b <- coef(model)
  nm <- grep("d_ln_dqty", names(b), value = TRUE)[1]
  if (is.na(nm)) return(tibble(term = NA_character_, estimate = NA_real_, se = NA_real_))
  V <- get_vcov_matrix(model, VCOV_MAIN)
  tibble(term = nm, estimate = unname(b[nm]), se = sqrt(V[nm, nm]))
}

safe_fitstat <- function(model, types = c("kpr", "sargan", "ivwald1")) {
  out <- list()
  for (tt in types) {
    out[[tt]] <- tryCatch(fitstat(model, type = tt), error = function(e) NA)
  }
  out
}

iv_diag_full <- tibble(model = "Full IV: tauA lags + tauD lags") %>%
  bind_cols(extract_beta_Q(m_iv_full))

# M2(a): just-identified IV using contemporaneous dairy tariff only
m_iv_d0 <- feols(
  as.formula(paste0(
    "d_ln_milkp ~ ", rhs(c(alf_terms, controls)), " | ", fe_rhs, " | ",
    "d_ln_dqty ~ d_ln_trd_l0"
  )),
  data = reg_df,
  panel.id = ~ unit_id + time_idx,
  vcov = VCOV_MAIN
)

# M2(b): over-identified IV using only dairy-tariff lags
m_iv_d_lags <- feols(
  as.formula(paste0(
    "d_ln_milkp ~ ", rhs(c(alf_terms, controls)), " | ", fe_rhs, " | ",
    "d_ln_dqty ~ ", rhs(trd_terms)
  )),
  data = reg_df,
  panel.id = ~ unit_id + time_idx,
  vcov = VCOV_MAIN
)

iv_beta_table <- bind_rows(
  iv_diag_full,
  tibble(model = "Just-id IV: tauD_t only") %>% bind_cols(extract_beta_Q(m_iv_d0)),
  tibble(model = "Over-id IV: tauD lags 0:K only") %>% bind_cols(extract_beta_Q(m_iv_d_lags))
)
write_csv(iv_beta_table, file.path(OUT_DIR, "M2_IV_betaQ_table.csv"))

# KP/Sargan objects are printed; inspect in console because fixest formatting varies by version.
cat("\n--- IV diagnostics: full IV ---\n"); print(safe_fitstat(m_iv_full))
cat("\n--- IV diagnostics: tauD_t only ---\n"); print(safe_fitstat(m_iv_d0))
cat("\n--- IV diagnostics: tauD lags only ---\n"); print(safe_fitstat(m_iv_d_lags))

# M1: Anderson-Rubin confidence set by inverting a robust Wald test.
# For each beta0, regress (Y - beta0*Q) on instruments + controls + FE;
# beta0 is retained if excluded instruments are jointly insignificant at alpha.
ar_invert_fixest <- function(data, y, q, z_terms, exog_terms, fe_rhs,
                             beta_grid, alpha = 0.05, vcov_type = VCOV_MAIN) {
  res <- map_dfr(beta_grid, function(beta0) {
    dd <- data %>% mutate(.ar_y = .data[[y]] - beta0 * .data[[q]])
    fml <- as.formula(paste0(".ar_y ~ ", rhs(c(z_terms, exog_terms)), " | ", fe_rhs))
    mod <- feols(fml, data = dd, panel.id = ~ unit_id + time_idx, vcov = vcov_type)
    wt <- wald_joint(mod, z_terms, label = "AR excluded instruments", vcov_type = vcov_type)
    tibble(beta0 = beta0, chi2 = wt$chi2, df = wt$df, p = wt$p, accept_95 = wt$p > alpha)
  })
  acc <- res %>% filter(accept_95)
  ci <- if (nrow(acc) == 0) {
    tibble(ar_lower = NA_real_, ar_upper = NA_real_, ar_note = "empty on supplied grid; expand grid")
  } else if (all(res$accept_95)) {
    tibble(ar_lower = min(beta_grid), ar_upper = max(beta_grid), ar_note = "covers full supplied grid; CI may be unbounded, expand grid")
  } else {
    tibble(ar_lower = min(acc$beta0), ar_upper = max(acc$beta0), ar_note = "bounded on supplied grid")
  }
  list(grid = res, ci = ci)
}

beta_hat <- iv_beta_table$estimate[iv_beta_table$model == "Full IV: tauA lags + tauD lags"][1]
if (is.finite(beta_hat)) {
  # Wide grid; adjust if the accepted set hits the boundary.
  beta_grid <- seq(beta_hat - 1.0, beta_hat + 1.0, length.out = 801)
  ar_full <- ar_invert_fixest(
    data = reg_df,
    y = "d_ln_milkp",
    q = "d_ln_dqty",
    z_terms = c(tra_terms, trd_terms),
    exog_terms = c(alf_terms, controls),
    fe_rhs = fe_rhs,
    beta_grid = beta_grid,
    alpha = 0.05,
    vcov_type = VCOV_MAIN
  )
  write_csv(ar_full$grid, file.path(OUT_DIR, "M1_AR_grid_full_IV.csv"))
  write_csv(ar_full$ci,   file.path(OUT_DIR, "M1_AR_95CI_full_IV.csv"))
  print(ar_full$ci)
}

# ---- 5. ITS period effects table (M4, O1 partly) -----------------------------
# Period definitions: edit dates here if your manuscript uses different windows.
control_start <- as.Date("2017-01-01")
control_end   <- as.Date("2018-05-31")
tw1_start     <- as.Date("2018-06-01")
tw1_end       <- as.Date("2022-12-31")
adj_start     <- as.Date("2023-01-01")
adj_end       <- as.Date("2023-12-31")
tw2_start     <- as.Date("2024-01-01")
tw2_end       <- as.Date("2025-12-31")

make_date <- function(data) {
  if ("date" %in% names(data)) {
    data$date <- as.Date(data$date)
    return(data)
  }
  if (all(c("year_fe", "month_fe") %in% names(data))) {
    yy <- as.integer(as.character(data$year_fe))
    mm <- as.integer(as.character(data$month_fe))
    data$date <- as.Date(sprintf("%04d-%02d-01", yy, mm))
    return(data)
  }
  stop("Need a monthly date variable named date, or year_fe + month_fe to construct date.")
}

its_df <- trade_df %>%
  make_date() %>%
  mutate(
    period = case_when(
      date >= control_start & date <= control_end ~ "Control",
      date >= tw1_start     & date <= tw1_end     ~ "TW1",
      date >= adj_start     & date <= adj_end     ~ "Adjustment",
      date >= tw2_start     & date <= tw2_end     ~ "TW2",
      TRUE ~ NA_character_
    ),
    period = factor(period, levels = c("Control", "TW1", "Adjustment", "TW2")),
    t_trend = as.numeric(date - min(date, na.rm = TRUE)) / 30.44
  )

# Need level log milk price. If your variable name differs, edit here.
if (!"ln_milkp" %in% names(its_df)) {
  if ("milkp" %in% names(its_df)) {
    its_df <- its_df %>% mutate(ln_milkp = log(milkp))
  } else if ("milk_price" %in% names(its_df)) {
    its_df <- its_df %>% mutate(ln_milkp = log(milk_price))
  } else {
    message("ITS skipped: create ln_milkp or raw milkp/milk_price in trade_df.")
  }
}

if ("ln_milkp" %in% names(its_df)) {
  m_its <- feols(
    ln_milkp ~ t_trend + period + d_ln_faop + d_ln_fuel + covid | month_fe,
    data = its_df %>% filter(!is.na(period)),
    panel.id = ~ unit_id + time_idx,
    vcov = VCOV_MAIN
  )
  its_ct <- broom::tidy(m_its, conf.int = TRUE) %>%
    filter(grepl("^period", term)) %>%
    mutate(
      period = gsub("^period", "", term),
      pct_effect = 100 * (exp(estimate) - 1),
      pct_low    = 100 * (exp(conf.low) - 1),
      pct_high   = 100 * (exp(conf.high) - 1)
    )
  write_csv(its_ct, file.path(OUT_DIR, "M4_ITS_period_effects.csv"))
  print(its_ct)
}

# ---- 6. Summary statistics by policy window (O1) -----------------------------
# Edit this mapping to match the raw variable names in your project.
summary_var_map <- c(
  "U.S. alfalfa import qty (1000 tons/month)" = "alf_qty_1000t",
  "U.S. dairy import qty (1000 tons/month)"   = "dairy_qty_1000t",
  "NZ/EU dairy import qty (1000 tons/month)"  = "nzeu_dairy_qty_1000t",
  "Alfalfa CIF unit value (USD/ton)"          = "alf_cif_usd_ton",
  "Farm-gate milk price (CNY/kg)"             = "milkp"
)

available_map <- summary_var_map[summary_var_map %in% names(its_df)]
if (length(available_map) > 0) {
  summary_stats <- its_df %>%
    filter(!is.na(period)) %>%
    group_by(period) %>%
    summarise(across(all_of(unname(available_map)), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(-period, names_to = "var_name", values_to = "mean") %>%
    mutate(variable = names(available_map)[match(var_name, unname(available_map))]) %>%
    select(variable, period, mean) %>%
    pivot_wider(names_from = period, values_from = mean)
  write_csv(summary_stats, file.path(OUT_DIR, "O1_summary_stats_by_window.csv"))
  print(summary_stats)
} else {
  message("O1 skipped: edit summary_var_map to match your raw variable names.")
}

# ---- 7. Lag-length robustness table (M9) ------------------------------------
run_lag_spec <- function(K_use) {
  df_k <- trade_df %>% make_lags(vars = c("d_ln_tra", "d_ln_trd"), K = K_use)
  tra_k <- paste0("d_ln_tra_l", 0:K_use)
  trd_k <- paste0("d_ln_trd_l", 0:K_use)
  m_q <- feols(mkf("d_ln_dqty",  c(tra_k, trd_k, controls)), data = df_k,
               panel.id = ~ unit_id + time_idx, vcov = VCOV_MAIN)
  m_p <- feols(mkf("d_ln_milkp", c(tra_k, trd_k, controls)), data = df_k,
               panel.id = ~ unit_id + time_idx, vcov = VCOV_MAIN)
  bind_cols(
    tibble(lag_length = paste0("0-", K_use), adj_r2_import = fitstat(m_q, "ar2")$ar2, adj_r2_price = fitstat(m_p, "ar2")$ar2),
    cum_effect(m_q, tra_k, "Sigma_gamma_A") %>% select(import_Sigma_gamma_A = estimate, import_se_A = se),
    cum_effect(m_q, trd_k, "Sigma_gamma_D") %>% select(import_Sigma_gamma_D = estimate, import_se_D = se),
    cum_effect(m_p, tra_k, "Sigma_theta_A") %>% select(price_Sigma_theta_A = estimate, price_se_A = se),
    cum_effect(m_p, trd_k, "Sigma_theta_D") %>% select(price_Sigma_theta_D = estimate, price_se_D = se)
  )
}

lag_table <- map_dfr(c(0, 1, 2, 3, 4), run_lag_spec)
write_csv(lag_table, file.path(OUT_DIR, "M9_lag_length_robustness.csv"))
print(lag_table)

# ---- 8. No-year-FE robustness (M7) ------------------------------------------
# Replaces year FE with linear trend; useful for reconciling year-FE RF with ITS.
reg_df <- reg_df %>% mutate(t_trend = as.numeric(time_idx))
fe_month_only <- "month_fe"

m_price_no_year_fe <- feols(
  as.formula(paste0("d_ln_milkp ~ ", rhs(c(tra_terms, trd_terms, controls, "t_trend")), " | ", fe_month_only)),
  data = reg_df,
  panel.id = ~ unit_id + time_idx,
  vcov = VCOV_MAIN
)

no_year_table <- bind_rows(
  cum_effect(m_price_hac,       tra_terms, "Baseline with year FE: Σ theta_A"),
  cum_effect(m_price_hac,       trd_terms, "Baseline with year FE: Σ theta_D"),
  cum_effect(m_price_no_year_fe, tra_terms, "No year FE + trend: Σ theta_A"),
  cum_effect(m_price_no_year_fe, trd_terms, "No year FE + trend: Σ theta_D")
)
write_csv(no_year_table, file.path(OUT_DIR, "M7_no_year_FE_robustness.csv"))
print(no_year_table)

# ---- 9. Welfare sensitivity wrapper (M5) ------------------------------------
# This block assumes you already have a welfare function in your welfare script.
# Replace compute_welfare_once() with your existing function. The output must include:
# scenario, net_loss_musd, and optionally ps_a, ps_d, cs, tariff_revenue.

compute_welfare_once <- function(eta_d, sigma_d, phi) {
  # TODO: replace this body with your existing welfare calculation.
  # Example:
  # welfare_counterfactual(eta_d = eta_d, sigma_d = sigma_d, phi = phi)
  stop("Replace compute_welfare_once() with your welfare-counterfactual function.")
}

run_welfare_sensitivity <- function(base_eta_d, base_sigma_d, base_phi) {
  grid <- tribble(
    ~parameter, ~multiplier,
    "baseline", 1.00,
    "eta_d",    0.75,
    "eta_d",    1.25,
    "sigma_d",  0.75,
    "sigma_d",  1.25,
    "phi",      0.75,
    "phi",      1.25
  ) %>%
    mutate(
      eta_d   = if_else(parameter == "eta_d",   base_eta_d   * multiplier, base_eta_d),
      sigma_d = if_else(parameter == "sigma_d", base_sigma_d * multiplier, base_sigma_d),
      phi     = if_else(parameter == "phi",     base_phi     * multiplier, base_phi)
    )

  out <- pmap_dfr(grid, function(parameter, multiplier, eta_d, sigma_d, phi) {
    compute_welfare_once(eta_d = eta_d, sigma_d = sigma_d, phi = phi) %>%
      mutate(parameter = parameter, multiplier = multiplier,
             eta_d = eta_d, sigma_d = sigma_d, phi = phi)
  })
  out
}

# Uncomment after you connect compute_welfare_once() to your welfare code.
# welfare_sensitivity <- run_welfare_sensitivity(base_eta_d = -1.5, base_sigma_d = 2.0, base_phi = 0.1)
# write_csv(welfare_sensitivity, file.path(OUT_DIR, "M5_welfare_sensitivity.csv"))

# ---- 10. Structural prediction from Proposition 2 (M6) -----------------------
structural_prediction <- function(phi, sigma_a, sigma_d, eta_a, eta_d) {
  phi * sigma_a / ((sigma_d - eta_d) * (sigma_a - eta_a))
}

# Fill with calibrated elasticities from your welfare section, then compare with Σ theta_A.
# pred <- structural_prediction(phi = 0.10, sigma_a = 0.50, sigma_d = 2.00, eta_a = -0.40, eta_d = -1.50)
# empirical <- cum_table %>% filter(label == "Eq.18: Σ theta_A, alfalfa tariff -> milk price")
# tibble(structural_prediction = pred, empirical_cumulative_theta_A = empirical$estimate)

# ---- 11. Export modelsummary tables -----------------------------------------
modelsummary(
  list(
    "Imports: baseline HAC" = m_qd_hac,
    "Milk price: baseline HAC" = m_price_hac,
    "Milk price: interaction" = m_interact_hac,
    "IV full" = m_iv_full,
    "IV tauD only" = m_iv_d_lags
  ),
  output = file.path(OUT_DIR, "revision_models.html"),
  gof_omit = "IC|Log|RMSE"
)

cat("\nRevision-extra outputs saved to: ", OUT_DIR, "\n")
