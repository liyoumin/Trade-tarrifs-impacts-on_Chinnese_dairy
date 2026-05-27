# =============================================================================
# 06_ecm.R  ·  Twin Tariff Paper — Cointegration & Error-Correction Model
# =============================================================================
# Motivation: Unit root tests confirm ln_milkp, ln_tra, ln_trd are all I(1).
# The retaliatory tariff is a sustained step-function (25% for 3+ years), so
# the economically relevant question is whether milk prices are *persistently
# higher* while the tariff is elevated — a long-run level question that the
# first-difference spec cannot answer.
#
# If the three I(1) series are cointegrated, a valid error-correction model
# (ECM) recovers both the long-run equilibrium elasticity (γ) and the short-
# run adjustment coefficient (α), without spurious-regression problems.
#
# Steps:
#   1. Structural-break unit root tests (Zivot-Andrews): confirm I(1) even
#      allowing for a one-time regime break at July 2018
#   2. Engle-Granger cointegration test (residual-based ADF)
#   3. Johansen trace & max-eigenvalue tests (full system)
#   4. Two-step ECM: long-run OLS → residual → short-run ΔOLS with ECT
#   5. ARDL bounds test (Pesaran et al. 2001): robust to ambiguous I(1)/I(0)
#   6. Summary comparison: first-diff vs. ECM tariff elasticities
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(lubridate)
  library(fixest)
  library(urca)       # ca.jo(), ur.za(), ur.df()
  library(lmtest)     # bgtest() for residual autocorrelation
  library(sandwich)   # NeweyWest() HAC standard errors
  library(vars)       # VAR lag selection
  library(strucchange)# breakpoints()
})

source(here( "01_data_prep.R"))
df <- as.data.frame(trade_df)

# Keep only the rows where milk price is observed (2009-01 onwards)
df_m <- df |>
  filter(!is.na(ln_milkp), !is.na(ln_tra), !is.na(ln_trd)) |>
  arrange(date)

N <- nrow(df_m)
cat(sprintf("Working sample: N = %d  (%s to %s)\n",
            N, df_m$date[1], df_m$date[N]))

# Convenience: extract as ts objects for urca/vars
milk_ts  <- ts(df_m$ln_milkpc, start = c(year(df_m$date[1]),
                                          month(df_m$date[1])), frequency = 12)
tra_ts   <- ts(df_m$ln_tra,   start = c(year(df_m$date[1]),
                                          month(df_m$date[1])), frequency = 12)
trd_ts   <- ts(df_m$ln_trd,   start = c(year(df_m$date[1]),
                                          month(df_m$date[1])), frequency = 12)
alf_ts   <- ts(df_m$ln_alfpc,   start = c(year(df_m$date[1]),
                                          month(df_m$date[1])), frequency = 12)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: Zivot-Andrews unit root test (allows one structural break)
# Standard ADF may over-accept unit root when a break looks like a trend.
# ZA tests H0: unit root with no break vs. H1: trend-stationary with break.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══ STEP 1: Zivot-Andrews structural-break unit root tests ══\n")
cat("(H0: unit root;  p < 0.05 → reject unit root even allowing for break)\n\n")

za_milk <- ur.za(milk_ts, model = "both", lag = 4)
cat("ln_milkp:\n"); summary(za_milk)

za_tra <- ur.za(tra_ts, model = "both", lag = 4)
cat("ln_tra (alfalfa tariff):\n"); summary(za_tra)

za_trd <- ur.za(trd_ts, model = "both", lag = 4)
cat("ln_trd (dairy tariff):\n"); summary(za_trd)

# Print key stats manually (ur.za summary goes to console automatically)
cat(sprintf("ZA statistic  ln_milkp:  %.3f  (5%% c.v. = -4.80)\n",
            za_milk@teststat))
cat(sprintf("ZA statistic  ln_tra:    %.3f  (5%% c.v. = -4.80)\n",
            za_tra@teststat))
cat(sprintf("ZA statistic  ln_trd:    %.3f  (5%% c.v. = -4.80)\n",
            za_trd@teststat))
cat(sprintf("ZA break position ln_milkp:  obs %d → approx %s\n",
            za_milk@bpoint,
            as.character(df_m$date[min(za_milk@bpoint + 4L, N)])))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: Engle-Granger cointegration (two-variable: milk ~ alfalfa tariff)
# Regress ln_milkp on ln_tra (+ controls), then ADF-test the residual.
# If residual is I(0), the series are cointegrated.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══ STEP 2: Engle-Granger cointegration test ══\n")

# Long-run static regression: milk price on both tariffs + month FEs + trend
# (No year FEs here: we want the *level* relationship, not within-year residuals)
df_m$time_trend <- as.numeric(df_m$date - df_m$date[1]) / 365.25

lr_ols <- lm(
  ln_milkpc ~ ln_tra + ln_trd + ln_alf + d_ln_faop + d_ln_fuel + time_trend +
    factor(month_fe),
  data    = df_m,
  na.action = na.exclude    # residuals() will return NAs at dropped rows,
                            # keeping length == nrow(df_m) for safe assignment
)

cat("\nLong-run OLS (step 1 of Engle-Granger):\n")
print(summary(lr_ols)$coefficients[1:6, ])   # show tariff + trend rows

# Save ECT (error-correction term)
df_m$ect <- residuals(lr_ols)

# ADF test on residuals — use ur.df with no constant (residuals are demeaned)
cat("\nADF test on long-run residuals (H0: residuals have unit root → NO cointegration):\n")
ect_clean <- na.omit(df_m$ect)   # ur.df cannot handle NAs
eg_adf <- ur.df(ect_clean, type = "none", lags = 4)
summary(eg_adf)
cat(sprintf("ADF stat on ECT: %.3f\n", eg_adf@teststat[1]))
cat("Engle-Granger critical values (no constant): -3.37 (1%), -2.84 (5%), -2.57 (10%)\n")
cat("(More negative than c.v. → reject H0 → residuals I(0) → COINTEGRATED)\n")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 3: Johansen trace test (full system: milk, alfalfa tariff, dairy tariff)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══ STEP 3: Johansen cointegration test ══\n")

# Select VAR lag order first
var_data <- cbind(milk_ts, tra_ts, trd_ts, alf_ts)
colnames(var_data) <- c("ln_milkp", "ln_tra", "ln_trd", "ln_alf")
lag_sel <- VARselect(var_data, lag.max = 6, type = "const")
cat("VAR lag selection (BIC/SC):\n")
print(lag_sel$criteria)
p_lag <- lag_sel$selection["SC(n)"]
cat(sprintf("Selected lag order (BIC): %d\n", p_lag))

# Johansen trace test
joh <- ca.jo(var_data, type = "trace", ecdet = "trend",
             K = max(p_lag, 2), spec = "longrun")
cat("\nJohansen trace test:\n")
summary(joh)

joh_ei <- ca.jo(var_data, type = "eigen", ecdet = "trend",
                K = max(p_lag, 2), spec = "longrun")
cat("\nJohansen maximum eigenvalue test:\n")
summary(joh_ei)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: Two-step ECM estimation
# Short-run first-difference equation augmented with lag-1 ECT.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══ STEP 4: Error-Correction Model ══\n")

# Add lag-1 ECT and lags of differences
K_ecm <- 3   # lag depth for short-run dynamics (months)

df_m <- df_m |>
  arrange(date) |>
  mutate(
    ect_lag1      = dplyr::lag(ect,         1),
    d_ln_milkpc    = ln_milkpc - dplyr::lag(ln_milkpc),
    d_ln_tra_0    = d_ln_tra,
    d_ln_tra_1    = dplyr::lag(d_ln_tra,    1),
    d_ln_tra_2    = dplyr::lag(d_ln_tra,    2),
    d_ln_tra_3    = dplyr::lag(d_ln_tra,    3),
    d_ln_trd_0    = d_ln_trd,
    d_ln_trd_1    = dplyr::lag(d_ln_trd,    1),
    d_ln_trd_2    = dplyr::lag(d_ln_trd,    2),
    d_ln_trd_3    = dplyr::lag(d_ln_trd,    3),
    d_ln_alf_0    = d_ln_alf,
    d_ln_alf_1    = dplyr::lag(d_ln_alf,    1),
    d_ln_alf_2    = dplyr::lag(d_ln_alf,    2),
    d_ln_milkp_1  = dplyr::lag(d_ln_milkpc,  1)   # lagged dep var (AR term)
  )

# ECM: Δln_milkp = α*ECT_{t-1} + Σβ_k Δln_tra_{t-k} + Σγ_k Δln_trd_{t-k}
#                + Σδ_k Δln_alf_{t-k} + macro controls + month FEs
ecm_formula <- d_ln_milkpc ~
  ect_lag1 +                                    # error-correction speed
  d_ln_tra_0 + d_ln_tra_1 + d_ln_tra_2 +       # short-run alfalfa tariff
  d_ln_trd_0 + d_ln_trd_1 + d_ln_trd_2 +       # short-run dairy tariff
  d_ln_alf_0 + d_ln_alf_1 + d_ln_alf_2 +       # alfalfa CIF price (exog)
  d_ln_faop + d_ln_fuel + covid +               # macro controls
  factor(month_fe)                              # month seasonality

m_ecm <- lm(ecm_formula, data = df_m, na.action = na.exclude)

# HAC (Newey-West) standard errors to correct serial correlation in residuals
nw_se  <- sqrt(diag(NeweyWest(m_ecm, lag = 6, prewhite = FALSE)))
nw_t   <- coef(m_ecm) / nw_se
nw_p   <- 2 * pt(abs(nw_t), df = df.residual(m_ecm), lower.tail = FALSE)

ecm_table <- data.frame(
  Coef    = round(coef(m_ecm),  4),
  HAC_SE  = round(nw_se,        4),
  t_stat  = round(nw_t,         3),
  p_value = round(nw_p,         4),
  sig     = ifelse(nw_p < 0.01, "***",
            ifelse(nw_p < 0.05, "**",
            ifelse(nw_p < 0.10, "*",  "")))
)

cat("\nECM short-run coefficients (Newey-West HAC SEs, lag=6):\n")
key_rows <- c("ect_lag1",
              "d_ln_tra_0","d_ln_tra_1","d_ln_tra_2",
              "d_ln_trd_0","d_ln_trd_1","d_ln_trd_2",
              "d_ln_alf_0","d_ln_alf_1","d_ln_alf_2",
              "d_ln_faop","d_ln_fuel","covid")
print(ecm_table[key_rows, ])

cat(sprintf("\nECM Adj. R² = %.4f  |  N = %d\n",
            summary(m_ecm)$adj.r.squared, nrow(na.omit(df_m[, all.vars(ecm_formula)]))))

# Test residual autocorrelation in ECM
bg_ecm <- bgtest(m_ecm, order = 6)
cat(sprintf("Breusch-Godfrey serial correlation test (lag 6): stat=%.2f, p=%.4f\n",
            bg_ecm$statistic, bg_ecm$p.value))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 5: Long-run elasticity summary + ARDL bounds-test approach
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══ STEP 5: Long-run elasticities from the cointegrating vector ══\n")

lr_coefs <- coef(lr_ols)
cat(sprintf("\n  Long-run alfalfa tariff elasticity (γ_A): %+.4f\n", lr_coefs["ln_tra"]))
cat(sprintf("  Long-run dairy tariff elasticity   (γ_D): %+.4f\n", lr_coefs["ln_trd"]))
cat(sprintf("  Long-run alfalfa CIF price         (γ_P): %+.4f\n", lr_coefs["ln_alf"]))
cat(sprintf("  Time trend (per year):                    %+.4f\n", lr_coefs["time_trend"]))

# LR standard errors via HAC on long-run OLS
lr_nw  <- sqrt(diag(NeweyWest(lr_ols, lag = 12, prewhite = FALSE)))
cat(sprintf("\n  γ_A HAC SE: %.4f  |  t = %.2f  |  p = %.4f\n",
            lr_nw["ln_tra"],
            lr_coefs["ln_tra"] / lr_nw["ln_tra"],
            2*pt(abs(lr_coefs["ln_tra"]/lr_nw["ln_tra"]),
                 df=df.residual(lr_ols), lower.tail=FALSE)))
cat(sprintf("  γ_D HAC SE: %.4f  |  t = %.2f  |  p = %.4f\n",
            lr_nw["ln_trd"],
            lr_coefs["ln_trd"] / lr_nw["ln_trd"],
            2*pt(abs(lr_coefs["ln_trd"]/lr_nw["ln_trd"]),
                 df=df.residual(lr_ols), lower.tail=FALSE)))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 6: Comparison summary table
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══ STEP 6: Specification comparison ══\n")
cat(sprintf("
%-35s  %s
%-35s  %s
%-35s  %s
%-35s  %s
%-35s  %s
",
"Specification",          "Alfalfa tariff coef (t=0)",
"─────────────────────────────────────────────────────────────────",  "",
"First-diff (corrected, HC SE)",  sprintf("%+.4f  [using d_ln_tra]", coef(lm(d_ln_milkp ~ d_ln_tra + d_ln_trd + d_ln_alf + d_ln_faop + d_ln_fuel + covid + factor(month_fe), data=df_m))["d_ln_tra"]),
"ECM short-run (HAC SE)",         sprintf("%+.4f  %s",  ecm_table["d_ln_tra_0","Coef"], ecm_table["d_ln_tra_0","sig"]),
"ECM long-run γ_A (HAC SE)",      sprintf("%+.4f  from cointegrating vector", lr_coefs["ln_tra"])
))
