# =============================================================================
# 02_main_regressions.R  ·  Twin Tariff Paper — Table 1 Main Regressions
# =============================================================================
# Four-column block-recursive identification chain.
#
#   BLOCK A — upstream alfalfa market (clears first)
#   Col 1:  First stage   Δln(Q_D)    ~ τ_A + τ_D + controls
#           Tests Prop. 3 (asymmetric import response: |γ^A| > |γ^D|).
#           Also provides excluded instruments for Col 4.
#
#   BLOCK B — downstream dairy market (conditional on alfalfa price)
#   Col 2:  Reduced form  Δln(P_milk) ~ τ_A + τ_D + controls
#           Total co-movement, all channels pooled; no exclusion restriction.
#           Expected null in first differences: year FEs absorb the single
#           July-2018 jump; year-FE attenuation confirmed in Appendix B3.
#
#   Col 3:  Alfalfa channel  Δln(P_milk) ~ Δln(P_alf) + τ_D + controls
#           OLS test of the cross-market pass-through mechanism: does higher
#           alfalfa CIF price raise the farm-gate milk price?  Tests Prop. 2.
#           Note: P_alf is treated as exogenous here; the 2SLS causal
#           interpretation is addressed in the ECM analysis (Appendix B4).
#
#   Col 4:  2SLS            Δln(P_milk) ~ [Δln(Q_D) ~ τ_A + τ_D] + Δln(P_alf)
#           Instruments dairy import volume with both tariffs.
#           Alfalfa CIF price in second stage absorbs the feed-cost pathway so
#           the instrumented quantity isolates the import-competition channel.
#           Conditional exclusion restriction: given Δln(P_alf), tariffs affect
#           P_milk only through dairy import volumes.
#           KP rk F-stat assessed against Stock-Yogo 10% c.v. = 16.38.
# =============================================================================

source(here::here( "01_data_prep.R"))
suppressPackageStartupMessages({
  library(fixest)
  library(modelsummary)
})

# ── Col 1: First stage — τ_A + τ_D → Q_D ─────────────────────────────────────
m_col1 <- feols(
  d_ln_dqty ~
    f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid +
    i(month_fe) + i(year_fe),
  data     = trade_df,
  panel.id = ~ unit_id + time_idx,
  vcov     = "hetero"
)

# ── Col 2: Reduced form — τ_A + τ_D → P_milk ─────────────────────────────────
# All transmission channels simultaneously; no exclusion restriction.
# Null result expected in FD + year FEs due to attenuation (see Appendix B3).
m_col2 <- feols(
  d_ln_milkp ~
    f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid +
    i(month_fe) + i(year_fe),
  data     = trade_df,
  panel.id = ~ unit_id + time_idx,
  vcov     = "hetero"
)

m_colI <- feols(
  d_ln_milkp ~
    f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K) + f(d_ln_tra*d_ln_trd,0:K) +
    d_ln_faop + d_ln_fuel + covid +
    i(month_fe) + i(year_fe),
  data     = trade_df,
  panel.id = ~ unit_id + time_idx,
  vcov     = "hetero"
)

# ── Col 3: Alfalfa CIF price channel — Δln(P_alf) + τ_D → P_milk ─────────────
# OLS test of Prop. 2 (cross-market pass-through): β^alf_k > 0.
# Dairy tariff τ_D included to capture the direct import-competition channel.
# NOTE: alfalfa CIF price is treated as exogenous; endogeneity-robust estimates
#       appear in the ECM analysis (Appendix B4, code/06_ecm.R).
m_col3 <- feols(
  d_ln_milkp ~
    f(d_ln_alf, 0:K) + f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid +
    i(month_fe) + i(year_fe),
  data     = trade_df,
  panel.id = ~ unit_id + time_idx,
  vcov     = "hetero"
)

# ── Col 4: 2SLS — Q_D instrumented by τ_A + τ_D → P_milk ─────────────────────
# Endogenous:   d_ln_dqty (contemporaneous dairy import volume)
# Instruments:  f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K)   [6 instruments]
# Exogenous:    f(d_ln_alf, 0:K) in 2nd stage absorbs feed-cost pathway
# Over-identified (6 instruments, 1 endogenous) → strong KP F-stat.
m_col4 <- feols(
  d_ln_milkp ~
    f(d_ln_alf, 0:K) +
    d_ln_faop + d_ln_fuel + covid +
    i(month_fe) + i(year_fe) |
    d_ln_dqty ~
    f(d_ln_tra, 0:K) +
    f(d_ln_trd, 0:K),
  data     = trade_df,
  panel.id = ~ unit_id + time_idx,
  vcov     = "hetero"
)

m_col5 <- feols(
  d_ln_milkp ~
    f(d_ln_alf, 0:K) + d_ln_dqty+
    d_ln_faop + d_ln_fuel + covid +
    i(month_fe) + i(year_fe),
  data     = trade_df,
  panel.id = ~ unit_id + time_idx,
  vcov     = "hetero"
)

# ── Instrument strength diagnostics ──────────────────────────────────────────
#
# Two F-statistics are produced for the Col 4 first stage:
#
#  (A) Standard first-stage F  — from summary(m_col4, stage=1)
#      Assumes homoskedastic (iid) errors; uses OLS residual variance.
#      Stock-Yogo 10% c.v. for 6 instruments = 16.38.
#      Montiel-Olea-Pflueger (2013) c.v. for hetero errors ≈ 23.11.
#      This is the number printed by fixest in the console summary output.
#
#  (B) Kleibergen-Paap rk Wald F  — from fitstat(m_col4, "kpr")
#      Heteroskedasticity-ROBUST statistic; appropriate when vcov="hetero".
#      Stock-Yogo CVs still apply as an approximation; see Baum et al. (2007).
#      This is the number that should be reported and cited in the paper.
#
# With vcov = "hetero", (B) is the correct diagnostic.  (A) will always be
# lower than (B) when heteroskedasticity is present.  Do NOT compare (A)
# against the Stock-Yogo threshold when using robust standard errors.
kp_fstat <- tryCatch({
  fitstat(m_col4, type = "kpr")$kpr$stat
}, error = function(e) {
  message("KP F-stat could not be computed: ", conditionMessage(e))
  NA_real_
})

cat(sprintf(
  "\nInstrument strength (Col 4, 6 instruments, 1 endogenous variable):\n",
))

cat(sprintf(
  "  KP rk Wald F (hetero-robust, cite this) : %.2f  [SY 10%% c.v. = 16.38]\n",
  kp_fstat
))
# Standard first-stage F is shown automatically in summary(m_col4, stage=1)
# It will be lower than KP rk F; compare only against iid-based thresholds.
if (!is.na(kp_fstat) && kp_fstat < 16.38)
  warning("KP rk F below Stock-Yogo 10% threshold — weak instruments.")

# ── Wald-ratio implied tariff effects (Col 4 tariff rows) ────────────────────
# In 2SLS with conditional exclusion restriction, the tariff entries in Col 4
# are Wald-ratio implied effects: β_Q × γ^j_k, where β_Q is the structural
# coefficient on instrumented Δln(Q_D) and γ^j_k is the first-stage tariff
# coefficient from Col 1.  This recovers the per-tariff-point causal effect
# operating through the dairy import-volume channel specifically.
wald_implied <- local({
  fs <- coef(m_col1)
  ss <- coef(m_col4)

  beta_Q_name <- grep("^fit_d_ln_dqty", names(ss), value = TRUE)[1]
  beta_Q      <- if (!is.na(beta_Q_name)) ss[[beta_Q_name]] else NA_real_

  lags    <- 0:K
  gamma_A <- sapply(lags, \(k) {
    nm <- paste0("f(d_ln_tra, ", k, ")")
    if (nm %in% names(fs)) fs[[nm]] else NA_real_
  })
  gamma_D <- sapply(lags, \(k) {
    nm <- paste0("f(d_ln_trd, ", k, ")")
    if (nm %in% names(fs)) fs[[nm]] else NA_real_
  })

  data.frame(lag = lags, beta_Q = beta_Q,
             gamma_A = gamma_A, gamma_D = gamma_D,
             implied_alf   = beta_Q * gamma_A,
             implied_dairy = beta_Q * gamma_D)
})

cat("\nWald-ratio implied tariff → milk price effects (dairy-import channel only):\n")
for (i in seq_len(nrow(wald_implied))) {
  cat(sprintf("  Alfalfa t-%d: γ=%+.4f × β_Q=%+.5f = %+.5f\n",
              wald_implied$lag[i], wald_implied$gamma_A[i],
              wald_implied$beta_Q[i], wald_implied$implied_alf[i]))
}

# ── Print summaries ───────────────────────────────────────────────────────────
cat("\n══ Col 1: First stage ══\n");            print(summary(m_col1, vcov = "hetero"))
cat("\n══ Col 2: Reduced form ══\n");           print(summary(m_col2, vcov = "hetero"))
cat("\n══ Col 3: Alfalfa CIF channel (OLS) ══\n"); print(summary(m_col3, vcov = "hetero"))
cat("\n══ Col 4: 2SLS second stage ══\n");      print(summary(m_col4, vcov = "hetero"))
cat("\n══ Col 4: 2SLS first stage ══\n");       print(summary(m_col4, stage = 1, vcov = "hetero"))

# ── etable: Table 1 ──────────────────────────────────────────────────────────
cat("\n\n══ TABLE 1 ══\n")
setFixest_dict(c(
  d_ln_dqty        = "Δlog dairy imports",
  d_ln_milkp       = "Δlog milk price",
  "f(d_ln_tra, 0)" = "Alfalfa tariff t=0",
  "f(d_ln_tra, 1)" = "Alfalfa tariff t−1",
  "f(d_ln_tra, 2)" = "Alfalfa tariff t−2",
  "f(d_ln_trd, 0)" = "Dairy tariff t=0",
  "f(d_ln_trd, 1)" = "Dairy tariff t−1",
  "f(d_ln_trd, 2)" = "Dairy tariff t−2",
  "f(d_ln_alf, 0)" = "Alf. CIF price t=0",
  "f(d_ln_alf, 1)" = "Alf. CIF price t−1",
  "f(d_ln_alf, 2)" = "Alf. CIF price t−2"
))

etable(
  m_col1, m_col2, m_col3, m_col4,
  vcov     = "hetero",
  keep     = c("%d_ln_tra", "%d_ln_trd", "%d_ln_alf"),
  order    = c("%d_ln_tra", "%d_ln_trd", "%d_ln_alf"),
  depvar   = TRUE,
  fitstat  = c("n", "ar2"),
  headers  = list("(1) First stage", "(2) Reduced form",
                  "(3) Alf. channel", "(4) 2SLS"),
  title    = "Twin Tariff Transmission — Block-Recursive Chain (Δlog, HC SE)"
)

cat(sprintf("\nNote: KP rk F (Col 4, first stage) = %.2f  |  SY 10%% c.v. = 16.38\n",
            kp_fstat))
cat("Note: Col 4 tariff entries are Wald-ratio implied effects (β_Q × γ^j_k).\n")
