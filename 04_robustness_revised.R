# =============================================================================
# 04_robustness_revised.R  ·  Twin Tariff Paper — Appendix B Robustness Checks
# =============================================================================
# Purpose:
#   Robustness and reviewer-requested checks for the Food Policy revision.
#   This version fixes the placebo timing issue, avoids i() in the fixest FE part,
#   adds exportable CSV/LaTeX outputs, and adds cumulative effects, lag robustness,
#   Newey–West HAC SEs, ITS period effects, and optional currency/ECM blocks.
#
# Run from project root:
#   source(here::here("04_robustness_revised.R"))
#
# Required upstream objects from 01_data_prep.R / 02_main_regressions.R:
#   trade_df, K, m_col1, m_col2, m_col3, m_col4
#   variables: date, unit_id, time_idx, month_fe, year_fe,
#              d_ln_dqty, d_ln_milkp, d_ln_tra, d_ln_trd,
#              d_ln_alf, d_ln_faop, d_ln_fuel, covid
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(fixest)
  library(dplyr)
  library(tidyr)
  library(readr)
})

source(here::here("01_data_prep.R"))
source(here::here("02_main_regressions.R"))

out_dir <- here::here("outputs", "robustness")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

trade_plain <- as.data.frame(trade_df) |>
  arrange(date)

# ─────────────────────────────────────────────────────────────────────────────
# Helper functions
# ─────────────────────────────────────────────────────────────────────────────

has_vars <- function(data, vars) all(vars %in% names(data))

missing_vars <- function(data, vars) setdiff(vars, names(data))

skip_if_missing <- function(data, vars, block_name) {
  miss <- missing_vars(data, vars)
  if (length(miss) > 0) {
    message("Skipping ", block_name, ": missing variable(s): ", paste(miss, collapse = ", "))
    return(TRUE)
  }
  FALSE
}

coef_df <- function(model, vcov_type = "hetero", model_name = deparse(substitute(model))) {
  ct <- tryCatch(summary(model, vcov = vcov_type)$coeftable,
                 error = function(e) summary(model)$coeftable)
  out <- as.data.frame(ct)
  out$term <- rownames(out)
  rownames(out) <- NULL
  names(out) <- make.names(names(out))
  out$model <- model_name
  out |>
    relocate(model, term)
}

# Flexible lag-term selector for fixest f(x, 0:K) terms.
# It is intentionally broad, but excludes placebo, lead, and interaction terms.
pick_lag_terms <- function(model, var) {
  nm <- names(coef(model))
  hits <- grep(var, nm, value = TRUE, fixed = TRUE)
  hits <- hits[!grepl("lead|plac|placebo|interact|:", hits, ignore.case = TRUE)]
  hits
}

cum_effect <- function(model, var, vcov_type = "hetero") {
  b <- coef(model)
  terms <- pick_lag_terms(model, var)
  terms <- intersect(terms, names(b))
  if (length(terms) == 0) {
    return(data.frame(variable = var, estimate = NA_real_, se = NA_real_,
                      t = NA_real_, p = NA_real_, terms = NA_character_))
  }
  V <- tryCatch(vcov(model, vcov = vcov_type), error = function(e) vcov(model))
  terms <- intersect(terms, rownames(V))
  R <- rep(0, length(b)); names(R) <- names(b)
  R[terms] <- 1
  est <- sum(b[terms], na.rm = TRUE)
  se <- sqrt(drop(t(R) %*% V[names(b), names(b), drop = FALSE] %*% R))
  tval <- est / se
  pval <- 2 * pt(abs(tval), df = nobs(model) - length(b), lower.tail = FALSE)
  data.frame(variable = var, estimate = est, se = se, t = tval, p = pval,
             terms = paste(terms, collapse = " + "))
}

joint_wald <- function(model, pattern, label, vcov_type = "hetero") {
  out <- tryCatch({
    wt <- fixest::wald(model, keep = pattern, vcov = vcov_type)
    data.frame(test = label,
               statistic = wt$stat,
               p_value = wt$p,
               df1 = wt$df1,
               df2 = wt$df2)
  }, error = function(e) {
    data.frame(test = label, statistic = NA_real_, p_value = NA_real_,
               df1 = NA_real_, df2 = NA_real_)
  })
  out
}

get_fitstat <- function(model, type) {
  tryCatch({
    x <- fitstat(model, type = type)[[type]]
    if (is.list(x) && !is.null(x$stat)) as.numeric(x$stat) else as.numeric(x)
  }, error = function(e) NA_real_)
}

write_model_csv <- function(model, file, vcov_type = "hetero", name = NULL) {
  if (is.null(name)) name <- deparse(substitute(model))
  readr::write_csv(coef_df(model, vcov_type, name), file.path(out_dir, file))
}

# =============================================================================
# B1. Placebo test: move the actual tariff path 24 months EARLIER
# =============================================================================
# This is the standard pre-trend placebo: the July-2018 jump is reassigned to
# July-2016. If the model is valid, placebo tariff changes should not predict
# pre-treatment dairy imports or milk prices.
# NOTE: the prior draft used lag(..., 24), which shifts the shock later. For a
# pre-treatment placebo, use lead(..., 24).

cat("\n══ B1. Placebo test: tariff path shifted 24 months earlier ══\n")

if (!skip_if_missing(trade_plain, c("ln_tra", "ln_trd"), "B1 placebo")) {
  trade_df_placebo <- trade_plain |>
    arrange(date) |>
    mutate(
      ln_tra_plac   = dplyr::lead(ln_tra, 24),
      ln_trd_plac   = dplyr::lead(ln_trd, 24),
      d_ln_tra_plac = ln_tra_plac - dplyr::lag(ln_tra_plac),
      d_ln_trd_plac = ln_trd_plac - dplyr::lag(ln_trd_plac)
    ) |>
    panel(panel.id = ~ unit_id + time_idx)

  m_placebo_rf <- feols(
    d_ln_milkp ~ f(d_ln_tra_plac, 0:K) + f(d_ln_trd_plac, 0:K) +
      d_ln_faop + d_ln_fuel + covid + i(month_fe) + i(year_fe),
    data = trade_df_placebo, panel.id = ~ unit_id + time_idx, vcov = "hetero"
  )

  m_placebo_fs <- feols(
    d_ln_dqty ~ f(d_ln_tra_plac, 0:K) + f(d_ln_trd_plac, 0:K) +
      d_ln_faop + d_ln_fuel + covid + i(month_fe) + i(year_fe),
    data = trade_df_placebo, panel.id = ~ unit_id + time_idx, vcov = "hetero"
  )

  placebo_cum <- bind_rows(
    cum_effect(m_placebo_fs, "d_ln_tra_plac") |> mutate(model = "Placebo first stage"),
    cum_effect(m_placebo_fs, "d_ln_trd_plac") |> mutate(model = "Placebo first stage"),
    cum_effect(m_placebo_rf, "d_ln_tra_plac") |> mutate(model = "Placebo reduced form"),
    cum_effect(m_placebo_rf, "d_ln_trd_plac") |> mutate(model = "Placebo reduced form")
  ) |>
    relocate(model)

  placebo_wald <- bind_rows(
    joint_wald(m_placebo_fs, "d_ln_tra_plac", "FS: placebo alfalfa lags = 0"),
    joint_wald(m_placebo_fs, "d_ln_trd_plac", "FS: placebo dairy lags = 0"),
    joint_wald(m_placebo_rf, "d_ln_tra_plac", "RF: placebo alfalfa lags = 0"),
    joint_wald(m_placebo_rf, "d_ln_trd_plac", "RF: placebo dairy lags = 0")
  )

  write_csv(placebo_cum, file.path(out_dir, "B1_placebo_cumulative_effects.csv"))
  write_csv(placebo_wald, file.path(out_dir, "B1_placebo_joint_wald.csv"))
  write_model_csv(m_placebo_rf, "B1_placebo_reduced_form_coefficients.csv", name = "m_placebo_rf")
  write_model_csv(m_placebo_fs, "B1_placebo_first_stage_coefficients.csv", name = "m_placebo_fs")
}


# =============================================================================
# B2. Lead tariff test: add t+1 and t+2 tariff changes
# =============================================================================
cat("\n══ B2. Lead tariff test ══\n")

trade_df_leads <- trade_plain |>
  arrange(date) |>
  mutate(
    d_ln_tra_lead1 = dplyr::lead(d_ln_tra, 1),
    d_ln_tra_lead2 = dplyr::lead(d_ln_tra, 2),
    d_ln_trd_lead1 = dplyr::lead(d_ln_trd, 1),
    d_ln_trd_lead2 = dplyr::lead(d_ln_trd, 2)
  ) |>
  panel(panel.id = ~ unit_id + time_idx)

m_leads_rf <- feols(
  d_ln_milkp ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_tra_lead1 + d_ln_tra_lead2 + d_ln_trd_lead1 + d_ln_trd_lead2 +
    d_ln_faop + d_ln_fuel + covid + i(month_fe) + i(year_fe),
  data = trade_df_leads, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

m_leads_fs <- feols(
  d_ln_dqty ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_tra_lead1 + d_ln_tra_lead2 + d_ln_trd_lead1 + d_ln_trd_lead2 +
    d_ln_faop + d_ln_fuel + covid + i(month_fe) + i(year_fe),
  data = trade_df_leads, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

lead_wald <- bind_rows(
  joint_wald(m_leads_fs, "lead", "FS: all tariff leads = 0"),
  joint_wald(m_leads_rf, "lead", "RF: all tariff leads = 0")
)

write_model_csv(m_leads_rf, "B2_lead_reduced_form_coefficients.csv", name = "m_leads_rf")
write_model_csv(m_leads_fs, "B2_lead_first_stage_coefficients.csv", name = "m_leads_fs")
write_csv(lead_wald, file.path(out_dir, "B2_lead_joint_wald.csv"))

# =============================================================================
# B3. Year-FE attenuation check: replace year FE with a linear time trend
# =============================================================================
cat("\n══ B3. Year-FE attenuation check ══\n")

trade_df_trend <- trade_plain |>
  mutate(time_trend = as.numeric(date - min(date, na.rm = TRUE)) / 365.25) |>
  panel(panel.id = ~ unit_id + time_idx)

m_rf_noyrfe <- feols(
  d_ln_milkp ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid + time_trend + i(month_fe),
  data = trade_df_trend, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

m_fs_noyrfe <- feols(
  d_ln_dqty ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid + time_trend + i(month_fe),
  data = trade_df_trend, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

b3_cum <- bind_rows(
  cum_effect(m_col1, "d_ln_tra") |> mutate(model = "FS baseline"),
  cum_effect(m_col1, "d_ln_trd") |> mutate(model = "FS baseline"),
  cum_effect(m_fs_noyrfe, "d_ln_tra") |> mutate(model = "FS no year FE"),
  cum_effect(m_fs_noyrfe, "d_ln_trd") |> mutate(model = "FS no year FE"),
  cum_effect(m_col2, "d_ln_tra") |> mutate(model = "RF baseline"),
  cum_effect(m_col2, "d_ln_trd") |> mutate(model = "RF baseline"),
  cum_effect(m_rf_noyrfe, "d_ln_tra") |> mutate(model = "RF no year FE"),
  cum_effect(m_rf_noyrfe, "d_ln_trd") |> mutate(model = "RF no year FE")
) |>
  relocate(model)

write_csv(b3_cum, file.path(out_dir, "B3_yearFE_attenuation_cumulative.csv"))
write_model_csv(m_rf_noyrfe, "B3_no_yearFE_reduced_form_coefficients.csv", name = "m_rf_noyrfe")
write_model_csv(m_fs_noyrfe, "B3_no_yearFE_first_stage_coefficients.csv", name = "m_fs_noyrfe")

# =============================================================================
# B4. Optional ECM block
# =============================================================================
cat("\n══ B4. Optional ECM block ══\n")
ecm_path <- here::here("code", "06_ecm.R")
if (file.exists(ecm_path)) {
  source(ecm_path)
  ecm_summary <- tryCatch({
    data.frame(
      object = c("Long-run alfalfa CIF elasticity", "Long-run alfalfa tariff elasticity",
                 "Long-run dairy tariff elasticity", "ECM speed of adjustment"),
      estimate = c(coef(lr_ols)["ln_alf"], coef(lr_ols)["ln_tra"],
                   coef(lr_ols)["ln_trd"], coef(m_ecm)["ect_lag1"])
    )
  }, error = function(e) data.frame(object = "ECM summary failed", estimate = NA_real_))
  write_csv(ecm_summary, file.path(out_dir, "B4_ECM_summary.csv"))
} else {
  message("Skipping B4: code/06_ecm.R not found.")
}

# =============================================================================
# B5. Fixed-effect specification comparison
# =============================================================================
cat("\n══ B5. Fixed-effect specification comparison ══\n")

m_col1_yrfe <- feols(
  d_ln_dqty ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid + i(year_fe),
  data = trade_df, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

m_col2_yrfe <- feols(
  d_ln_milkp ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid + i(year_fe),
  data = trade_df, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

m_col4_yrfe <- feols(
  d_ln_milkp ~ f(d_ln_alf, 0:K) + d_ln_faop + d_ln_fuel + covid + i(year_fe) |
    d_ln_dqty ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K),
  data = trade_df, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

m_col1_mofe <- feols(
  d_ln_dqty ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid + i(month_fe),
  data = trade_df, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

m_col2_mofe <- feols(
  d_ln_milkp ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid + i(month_fe),
  data = trade_df, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

m_col4_mofe <- feols(
  d_ln_milkp ~ f(d_ln_alf, 0:K) + d_ln_faop + d_ln_fuel + covid + i(month_fe) |
    d_ln_dqty ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K),
  data = trade_df, panel.id = ~ unit_id + time_idx, vcov = "hetero"
)

b5_kp <- data.frame(
  specification = c("Both FEs baseline", "Year FE only", "Month FE only"),
  kp_rk_wald_F = c(get_fitstat(m_col4, "kpr"), get_fitstat(m_col4_yrfe, "kpr"), get_fitstat(m_col4_mofe, "kpr")),
  stock_yogo_10pct_cv = 16.38
)

b5_cum <- bind_rows(
  cum_effect(m_col1, "d_ln_tra") |> mutate(model = "FS both FEs"),
  cum_effect(m_col1, "d_ln_trd") |> mutate(model = "FS both FEs"),
  cum_effect(m_col1_yrfe, "d_ln_tra") |> mutate(model = "FS year FE only"),
  cum_effect(m_col1_yrfe, "d_ln_trd") |> mutate(model = "FS year FE only"),
  cum_effect(m_col1_mofe, "d_ln_tra") |> mutate(model = "FS month FE only"),
  cum_effect(m_col1_mofe, "d_ln_trd") |> mutate(model = "FS month FE only"),
  cum_effect(m_col2, "d_ln_tra") |> mutate(model = "RF both FEs"),
  cum_effect(m_col2, "d_ln_trd") |> mutate(model = "RF both FEs"),
  cum_effect(m_col2_yrfe, "d_ln_tra") |> mutate(model = "RF year FE only"),
  cum_effect(m_col2_yrfe, "d_ln_trd") |> mutate(model = "RF year FE only"),
  cum_effect(m_col2_mofe, "d_ln_tra") |> mutate(model = "RF month FE only"),
  cum_effect(m_col2_mofe, "d_ln_trd") |> mutate(model = "RF month FE only")
) |>
  relocate(model)

write_csv(b5_kp, file.path(out_dir, "B5_FE_comparison_KP.csv"))
write_csv(b5_cum, file.path(out_dir, "B5_FE_comparison_cumulative.csv"))

# =============================================================================
# B6. Optional currency-denomination robustness
# =============================================================================
cat("\n══ B6. Optional currency-denomination robustness ══\n")

if (!skip_if_missing(trade_plain,
                     c("farmgate_cny_kg", "usd_per_cny", "alfalfa_price_usd_ton", "d_ln_fx"),
                     "B6 currency robustness")) {
  df_usd <- trade_plain |>
    arrange(date) |>
    mutate(
      ln_milkp_usd   = log(farmgate_cny_kg * usd_per_cny + 1),
      ln_alf_usd     = log(alfalfa_price_usd_ton / 1000 + 1),
      d_ln_milkp_usd = ln_milkp_usd - lag(ln_milkp_usd),
      d_ln_alf_usd   = ln_alf_usd - lag(ln_alf_usd)
    ) |>
    panel(panel.id = ~ unit_id + time_idx)

  # Correct fixest FE syntax: use | month_fe + year_fe, not | i(month_fe) + i(year_fe).
  b6_cny_rf <- feols(
    d_ln_milkp ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) + d_ln_faop + d_ln_fuel + covid |
      month_fe + year_fe,
    data = trade_df, vcov = "hetero"
  )

  b6_cny_alf <- feols(
    d_ln_milkp ~ f(d_ln_alf, 0:K) + f(d_ln_trd, 0:K) + d_ln_faop + d_ln_fuel + covid |
      month_fe + year_fe,
    data = trade_df, vcov = "hetero"
  )

  b6_usd_rf <- feols(
    d_ln_milkp_usd ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) + d_ln_faop + d_ln_fuel + covid |
      month_fe + year_fe,
    data = df_usd, vcov = "hetero"
  )

  b6_usd_fx_rf <- feols(
    d_ln_milkp_usd ~ f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) + d_ln_fx + d_ln_faop + d_ln_fuel + covid |
      month_fe + year_fe,
    data = df_usd, vcov = "hetero"
  )

  b6_usd_alf <- feols(
    d_ln_milkp_usd ~ f(d_ln_alf_usd, 0:K) + f(d_ln_trd, 0:K) + d_ln_faop + d_ln_fuel + covid |
      month_fe + year_fe,
    data = df_usd, vcov = "hetero"
  )

  b6_usd_fx_alf <- feols(
    d_ln_milkp_usd ~ f(d_ln_alf_usd, 0:K) + f(d_ln_trd, 0:K) + d_ln_fx + d_ln_faop + d_ln_fuel + covid |
      month_fe + year_fe,
    data = df_usd, vcov = "hetero"
  )

  b6_cum <- bind_rows(
    cum_effect(b6_cny_alf, "d_ln_alf") |> mutate(model = "CNY alfalfa channel"),
    cum_effect(b6_usd_alf, "d_ln_alf_usd") |> mutate(model = "USD alfalfa channel"),
    cum_effect(b6_usd_fx_alf, "d_ln_alf_usd") |> mutate(model = "USD + FX alfalfa channel")
  ) |>
    relocate(model)

  write_csv(b6_cum, file.path(out_dir, "B6_currency_cumulative_passthrough.csv"))
  write_model_csv(b6_cny_rf, "B6_cny_reduced_form_coefficients.csv", name = "b6_cny_rf")
  write_model_csv(b6_usd_rf, "B6_usd_reduced_form_coefficients.csv", name = "b6_usd_rf")
  write_model_csv(b6_usd_fx_rf, "B6_usd_fx_reduced_form_coefficients.csv", name = "b6_usd_fx_rf")
}

# =============================================================================
# B7. Main cumulative effects and joint Wald tests for Table 4 / Appendix B
# =============================================================================
cat("\n══ B7. Cumulative effects and joint Wald tests ══\n")

b7_cum <- bind_rows(
  cum_effect(m_col1, "d_ln_tra") |> mutate(model = "Col 1 first stage"),
  cum_effect(m_col1, "d_ln_trd") |> mutate(model = "Col 1 first stage"),
  cum_effect(m_col2, "d_ln_tra") |> mutate(model = "Col 2 reduced form"),
  cum_effect(m_col2, "d_ln_trd") |> mutate(model = "Col 2 reduced form"),
  cum_effect(m_col3, "d_ln_alf") |> mutate(model = "Col 3 alfalfa CIF channel"),
  cum_effect(m_col3, "d_ln_trd") |> mutate(model = "Col 3 alfalfa CIF channel")
) |>
  relocate(model)

b7_wald <- bind_rows(
  joint_wald(m_col1, "d_ln_tra", "Col 1: alfalfa tariff lags = 0"),
  joint_wald(m_col1, "d_ln_trd", "Col 1: dairy tariff lags = 0"),
  joint_wald(m_col2, "d_ln_tra", "Col 2: alfalfa tariff lags = 0"),
  joint_wald(m_col2, "d_ln_trd", "Col 2: dairy tariff lags = 0"),
  joint_wald(m_col3, "d_ln_alf", "Col 3: alfalfa CIF lags = 0"),
  joint_wald(m_col3, "d_ln_trd", "Col 3: dairy tariff lags = 0")
)

write_csv(b7_cum, file.path(out_dir, "B7_main_cumulative_effects.csv"))
write_csv(b7_wald, file.path(out_dir, "B7_main_joint_wald_tests.csv"))

# =============================================================================
# B8. Lag-length robustness table: k = 0, 0–1, 0–2, 0–3, 0–4
# =============================================================================
cat("\n══ B8. Lag-length robustness ══\n")

fit_lag_models <- function(k_lag) {
  m_fs <- feols(
    as.formula(paste0("d_ln_dqty ~ f(d_ln_tra, 0:", k_lag, ") + f(d_ln_trd, 0:", k_lag,
                      ") + d_ln_faop + d_ln_fuel + covid + i(month_fe) + i(year_fe)")),
    data = trade_df, panel.id = ~ unit_id + time_idx, vcov = "hetero"
  )
  m_rf <- feols(
    as.formula(paste0("d_ln_milkp ~ f(d_ln_tra, 0:", k_lag, ") + f(d_ln_trd, 0:", k_lag,
                      ") + d_ln_faop + d_ln_fuel + covid + i(month_fe) + i(year_fe)")),
    data = trade_df, panel.id = ~ unit_id + time_idx, vcov = "hetero"
  )
  data.frame(
    lag_length = paste0("0-", k_lag),
    gamma_A_sum = cum_effect(m_fs, "d_ln_tra")$estimate,
    gamma_D_sum = cum_effect(m_fs, "d_ln_trd")$estimate,
    theta_A_sum = cum_effect(m_rf, "d_ln_tra")$estimate,
    theta_D_sum = cum_effect(m_rf, "d_ln_trd")$estimate,
    fs_adj_r2 = get_fitstat(m_fs, "ar2"),
    rf_adj_r2 = get_fitstat(m_rf, "ar2"),
    fs_n = nobs(m_fs),
    rf_n = nobs(m_rf)
  )
}

b8_lag_table <- bind_rows(lapply(0:4, fit_lag_models))
write_csv(b8_lag_table, file.path(out_dir, "B8_lag_length_robustness.csv"))

# =============================================================================
# B9. Newey–West HAC standard errors for main models
# =============================================================================
cat("\n══ B9. Newey–West HAC standard errors ══\n")

nw_summary <- function(model, name, lag = 2) {
  ct <- tryCatch({
    as.data.frame(summary(model, vcov = fixest::NW(lag))$coeftable)
  }, error = function(e) {
    message("NW failed for ", name, "; returning hetero SEs. Error: ", conditionMessage(e))
    as.data.frame(summary(model, vcov = "hetero")$coeftable)
  })
  ct$term <- rownames(ct); rownames(ct) <- NULL
  ct$model <- name
  ct |>
    relocate(model, term)
}

b9_nw <- bind_rows(
  nw_summary(m_col1, "Col 1 first stage", lag = 2),
  nw_summary(m_col2, "Col 2 reduced form", lag = 2),
  nw_summary(m_col3, "Col 3 alfalfa CIF channel", lag = 2)
)
write_csv(b9_nw, file.path(out_dir, "B9_NeweyWest_lag2_coefficients.csv"))

# =============================================================================
# B10. ITS period-effect table: δ_p with SE and 95% CI
# =============================================================================
cat("\n══ B10. ITS period-effect table ══\n")

# Accept either ln_milkp or construct it from milkp/farmgate_cny_kg.
trade_its <- trade_plain
if (!("ln_milkp" %in% names(trade_its))) {
  if ("milkp" %in% names(trade_its)) {
    trade_its <- trade_its |> mutate(ln_milkp = log(milkp))
  } else if ("farmgate_cny_kg" %in% names(trade_its)) {
    trade_its <- trade_its |> mutate(ln_milkp = log(farmgate_cny_kg))
  }
}

if (!skip_if_missing(trade_its, c("ln_milkp", "date"), "B10 ITS")) {
  trade_its <- trade_its |>
    mutate(
      trend = as.numeric(date - min(date, na.rm = TRUE)) / 30.4375,
      policy_period = case_when(
        date >= as.Date("2017-01-01") & date < as.Date("2018-07-01") ~ "Control",
        date >= as.Date("2018-07-01") & date < as.Date("2023-01-01") ~ "TW1",
        date >= as.Date("2023-01-01") & date < as.Date("2024-01-01") ~ "Adjustment",
        date >= as.Date("2024-01-01") ~ "TW2",
        TRUE ~ NA_character_
      ),
      policy_period = factor(policy_period, levels = c("Control", "TW1", "Adjustment", "TW2"))
    ) |>
    panel(panel.id = ~ unit_id + time_idx)

  m_its <- feols(
    ln_milkp ~ i(policy_period, ref = "Control") + trend + d_ln_faop + d_ln_fuel + covid + i(month_fe),
    data = trade_its,
    panel.id = ~ unit_id + time_idx,
    vcov = "hetero"
  )

  its_coef <- coef_df(m_its, "hetero", "m_its") |>
    filter(grepl("policy_period", term)) |>
    mutate(
      period = case_when(
        grepl("TW1", term) ~ "First trade war",
        grepl("Adjustment", term) ~ "Adjustment",
        grepl("TW2", term) ~ "Second trade war",
        TRUE ~ term
      ),
      pct_effect = 100 * (exp(Estimate) - 1),
      ci_low = 100 * (exp(Estimate - 1.96 * `Std..Error`) - 1),
      ci_high = 100 * (exp(Estimate + 1.96 * `Std..Error`) - 1)
    )

  write_csv(its_coef, file.path(out_dir, "B10_ITS_period_effects.csv"))
  write_model_csv(m_its, "B10_ITS_all_coefficients.csv", name = "m_its")
}

# =============================================================================
# O1. Summary statistics by policy window
# =============================================================================
cat("\n══ O1. Summary statistics by policy window ══\n")

# Edit this map if your raw variable names differ.
summary_var_map <- c(
  "U.S. alfalfa import qty (1000 tons/month)" = "alf_qty_1000t",
  "U.S. dairy import qty (1000 tons/month)"   = "dairy_qty_1000t",
  "NZ/EU dairy import qty (1000 tons/month)"  = "nzeu_dairy_qty_1000t",
  "Alfalfa CIF unit value (USD/ton)"          = "alf_cif_usd_ton",
  "Farm-gate milk price (CNY/kg)"             = "milkp"
)

available_map <- summary_var_map[summary_var_map %in% names(trade_plain)]
if (length(available_map) == 0) {
  message("O1 skipped: none of the mapped summary-stat variables were found. Edit summary_var_map.")
} else {
  summary_stats <- trade_plain |>
    mutate(
      policy_window = case_when(
        date >= as.Date("2017-01-01") & date < as.Date("2018-07-01") ~ "Control (2017-mid 2018)",
        date >= as.Date("2018-07-01") & date < as.Date("2023-01-01") ~ "First Trade War (mid 2018-2022)",
        date >= as.Date("2023-01-01") & date < as.Date("2024-01-01") ~ "Adjustment (2023)",
        date >= as.Date("2024-01-01") ~ "Second Trade War (2024-2025)",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(policy_window)) |>
    group_by(policy_window) |>
    summarise(across(all_of(unname(available_map)), ~ mean(.x, na.rm = TRUE)),
              months = n(), .groups = "drop") |>
    pivot_longer(cols = -c(policy_window, months), names_to = "raw_variable", values_to = "mean") |>
    mutate(variable = names(available_map)[match(raw_variable, available_map)]) |>
    select(variable, policy_window, mean, months) |>
    pivot_wider(names_from = policy_window, values_from = mean)

  write_csv(summary_stats, file.path(out_dir, "O1_summary_statistics_by_policy_window.csv"))
}

cat("\nFinished. Outputs written to: ", out_dir, "\n")

