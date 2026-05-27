# =============================================================================
# 01_data_prep.R  ·  Twin Tariff Paper — Data Preparation
# =============================================================================
# Loads both raw CSVs, merges them, and constructs every log-level and
# first-difference variable used downstream.  All other scripts call
#   source(here::here("code", "01_data_prep.R"))
# at the top.
#
# Output: trade_df  — fixest panel, ready for feols()
#         K         — integer lag depth (2)
# =============================================================================

suppressPackageStartupMessages({
  library(here)       # project-root-relative paths via here::here()
  library(readr)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(fixest)
})

# ── Paths ─────────────────────────────────────────────────────────────────────
DATA_FILE <- here( "dairy_trade_data.csv")
ENRG_FILE <- here( "exchange_rate_and_fuel_price.csv")

stopifnot(file.exists(DATA_FILE))
stopifnot(file.exists(ENRG_FILE))

# ── Load raw datasets ─────────────────────────────────────────────────────────
df_main <- read_csv(DATA_FILE, show_col_types = FALSE) |>
  filter(!is.na(Year), !is.na(Month)) |>          # drop any trailing blank rows
  mutate(date = as.Date(sprintf("%04d-%02d-01", Year, Month)))

df_enrg <- read_csv(ENRG_FILE, show_col_types = FALSE) |>
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%y", "%Y-%m-%d", "%m/%d/%Y")))

# ── Merge ─────────────────────────────────────────────────────────────────────
df <- left_join(df_main, df_enrg, by = "date")

# ── Global lag depth ─────────────────────────────────────────────────────────
K <- 2L

# ── Variable construction ─────────────────────────────────────────────────────
#
# ── Currency denomination note ────────────────────────────────────────────────
#
# MAIN REGRESSION VARIABLES use CNY throughout.
#
# Rationale: Chinese dairy producers earn CNY and pay CNY input costs.  The
# relevant price signals for domestic pass-through analysis are CNY-denominated.
# Using USD-denominated milk prices (farmgate_cny_kg * usd_per_cny) introduces
# an asymmetry: the dependent variable carries Δln(CNY/USD) on the left-hand
# side but NOT on the right-hand side (alfalfa USD border price has no FX term),
# so exchange-rate variation goes into the residual and attenuates pass-through
# estimates.  This matters empirically: the CNY depreciated ≈9.5% against the
# USD during July–December 2018, coinciding with the tariff shock.
#
# The CNY milk price is farmgate_cny_kg (raw, no conversion).
# The CNY alfalfa CIF price is alfalfa_price_usd_ton × cny_per_usd / 1000
#   (USD/ton × CNY/USD = CNY/ton, ÷1000 = CNY/kg).
# The tariff instruments log(1+τ) are unit-free percentages — unaffected.
# The FAO dairy price index (global, USD) is kept in USD as a world-price
#   control; its denomination does not affect identification.
#
# farmgate_usd_kg is retained for the welfare calibration (Table 5) only.
# d_ln_fx (Δln CNY/USD) is constructed for Appendix B robustness checks.
#
# Design note on alfalfa CIF price (d_ln_alf):
#   This enters the 2SLS second stage as an *exogenous* regressor, absorbing
#   the direct feed-cost channel (τ_A → P_a → MC_d → P_milk) so that the
#   instrumented dairy import quantity isolates the import-competition pathway.

trade_df <- df |>
  arrange(date) |>
  mutate(
    # ── USD-denominated milk price (welfare calibration only — not in regressions)
    farmgate_usd_kg = farmgate_cny_kg * usd_per_cny,

    # ── CNY-denominated alfalfa CIF price (USD/ton × CNY/USD ÷ 1000 = CNY/kg)
    alfalfa_cny_ton  = alfalfa_price_usd_ton * cny_per_usd,

    # ── Log levels ────────────────────────────────────────────────────────────
    ln_milkp = log(farmgate_cny_kg       + 1),   # farm-gate milk price (CNY/kg)  ← main
    ln_faop  = log(FAO_dairry_price_index    ),   # FAO dairy price index (no +1: always >0)
    ln_alf   = log(alfalfa_price_usd_ton        + 1),   
    ln_fx    = log(cny_per_usd               ),   # log(CNY/USD) — for FX robustness
    ln_dqty  = log(dairy_qty_ton         + 1),   # dairy import qty (mt)
    ln_aqty  = log(alfalfa_qty           + 1),   # alfalfa import qty (mt)
    ln_fuel  = log(international_fuel_price_usd_per_barrel + 1),
    ln_tra   = log(tariff_rate_on_alfalfa / 100 + 1),  # log(1 + τ_A)
    ln_trd   = log(tariff_rate_on_dairy   / 100 + 1),  # log(1 + τ_D)
    ln_wheyp = log(whey_price + 1),
    ln_wheyq = log(whey_qty   + 1),

    # ── First differences  Δln(x) = ln(x_t) − ln(x_{t-1}) ───────────────────
    # NOTE: the original script had a stray comma on the milk price line that
    # set d_ln_milkp = ln_milkp (the level), not the difference.  Fixed here.
    d_ln_milkp = ln_milkp - lag(ln_milkp),   # <── dependent variable (CNY/kg)
    d_ln_faop  = ln_faop  - lag(ln_faop),
    d_ln_alf   = ln_alf   - lag(ln_alf),      # alfalfa CIF price CNY/kg (2nd-stage exogenous)
    d_ln_fx    = ln_fx    - lag(ln_fx),        
    d_ln_dqty  = ln_dqty  - lag(ln_dqty),    # dairy import qty  (endogenous in 2SLS)
    d_ln_aqty  = ln_aqty  - lag(ln_aqty),
    d_ln_fuel  = ln_fuel  - lag(ln_fuel),
    d_ln_tra   = ln_tra   - lag(ln_tra),      # alfalfa tariff Δln(1+τ_A)  [instrument]
    d_ln_trd   = ln_trd   - lag(ln_trd),      # dairy   tariff Δln(1+τ_D)  [instrument]
    d_ln_wheyp = ln_wheyp - lag(ln_wheyp),
    d_ln_wheyq = ln_wheyq - lag(ln_wheyq),

    # ── Interaction term ──────────────────────────────────────────────────────
    # Nonzero only in months where BOTH tariffs change simultaneously.
    # Not used in the main Table 1 (Column 3 is now the alfalfa CIF channel).
    # Retained for potential robustness checks; see 4_Empirical.tex §4.2.
    interact_tra_trd = d_ln_tra * d_ln_trd,

    # ── Fixed effects ─────────────────────────────────────────────────────────
    month_fe = factor(Month),
    year_fe  = factor(Year),

    # ── COVID disruption indicator ────────────────────────────────────────────
    covid = as.integer(date >= as.Date("2020-03-01") &
                       date <= as.Date("2021-06-01")),

    # ── Policy-period labels (for period treatment-effect analysis) ───────────
    window = case_when(
      date >= as.Date("2017-01-01") & date < as.Date("2018-06-01") ~ "control",
      date >= as.Date("2018-07-01") & date < as.Date("2022-02-01") ~ "trade_war",
      date >= as.Date("2022-03-01") & date < as.Date("2023-09-01") ~ "adjust",
      date >= as.Date("2023-09-01")                                ~ "war2",
      TRUE ~ NA_character_
    ),
    window = factor(window,
                    levels = c("control", "trade_war", "adjust", "war2"),
                    labels = c("Control (2017–2018)",
                               "Trade War (2018–2022)",
                               "Adjustment (2022–2023)",
                               "2nd Trade War (2023–2025)")),

    # ── Panel identifiers ─────────────────────────────────────────────────────
    unit_id  = 1L,
    time_idx = row_number()
  )

# ── Diagnostics (on plain data frame, before panel() call) ───────────────────
message(sprintf(
  "trade_df ready: %d rows | milk-price obs: %d | date: %s to %s",
  nrow(trade_df),
  sum(!is.na(trade_df$d_ln_milkp)),
  format(min(trade_df$date, na.rm = TRUE)),
  format(max(trade_df$date, na.rm = TRUE))
))
# Confirm CNY denomination
message(sprintf(
  "Denomination check — milk price (CNY/kg) range: %.2f–%.2f | alfalfa CIF (CNY/kg) range: %.2f–%.2f",
  min(trade_df$farmgate_cny_kg, na.rm = TRUE),
  max(trade_df$farmgate_cny_kg, na.rm = TRUE),
  min(trade_df$alfalfa_cny_kg,  na.rm = TRUE),
  max(trade_df$alfalfa_cny_kg,  na.rm = TRUE)
))

# Retaliatory months where BOTH tariffs change simultaneously
# (diagnostic: confirms sparse overlap of concurrent tariff changes)
both_change <- as.data.frame(trade_df) |>
  dplyr::filter(date >= as.Date("2018-01-01"),
                !is.na(d_ln_tra), d_ln_tra != 0,
                !is.na(d_ln_trd), d_ln_trd != 0) |>
  dplyr::select(date, d_ln_tra, d_ln_trd)
message(sprintf(
  "Months with simultaneous alfalfa + dairy tariff changes (post-2018): %d",
  nrow(both_change)
))
if (nrow(both_change) > 0) print(both_change)

# Set fixest panel structure — MUST be last: dplyr verbs break fixest_panel class
trade_df <- panel(trade_df, panel.id = ~ unit_id + time_idx)

