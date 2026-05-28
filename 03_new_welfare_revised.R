# =============================================================================
# 03_welfare_substitution_revised.R
# Twin Tariff Dairy Paper — NZ/EU substitution-adjusted dairy DWL + M5 grid
# =============================================================================
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
pkgs <- c("tidyverse", "lubridate", "comtradr", "janitor", "scales", "glue")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(comtradr)
  library(janitor)
  library(scales)
  library(glue)
})

out_dir <- file.path("outputs", "welfare")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── 1. Comtrade key ───────────────────────────────────────────────────────────
# Add this to ~/.Renviron, then restart R:
# COMTRADE_PRIMARY=your_key_here
ct_key <- Sys.getenv("COMTRADE_PRIMARY")
if (ct_key == "") {
  stop("Set COMTRADE_PRIMARY in ~/.Renviron or Sys.setenv(COMTRADE_PRIMARY='...') before running.")
}
comtradr::set_primary_comtrade_key(ct_key)

# ── 2. User settings ─────────────────────────────────────────────────────────
dairy_hs <- c("0401", "0402", "0403", "0404", "0405", "0406")
reporter_china <- "CHN"
us_partner <- "USA"
nz_partner <- "NZL"
eu27 <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
  "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
  "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)
partners_all <- c(us_partner, nz_partner, eu27)
years <- 2015:2024

# Paper windows requested by manuscript/revision notes
baseline_start <- ymd("2017-06-01")
baseline_end   <- ymd("2018-06-01")
post_start     <- ymd("2018-07-01")
post_end       <- ymd("2019-07-01")

# If you want to force the paper's calibrated U.S. decline, keep this number.
# If you want to use the Comtrade-computed window decline, set this to NA_real_.
US_decline_calibrated_ton <- 185385.7

# Dairy tariff and free-trade price used in raw DWL calculation.
# If Pd_ft_usd_ton is NA, the script uses the baseline U.S. unit value.
tau_D <- 0.25
Pd_ft_usd_ton <- 4500

clip01 <- function(x) {
  x <- if_else(is.nan(x) | is.infinite(x), NA_real_, as.numeric(x))
  pmin(pmax(x, 0), 1)
}

first_existing <- function(df, choices) {
  hit <- intersect(choices, names(df))
  if (length(hit) == 0) stop(glue("None of these columns found: {paste(choices, collapse = ', ')}"))
  hit[1]
}

parse_comtrade_month <- function(x) {
  x <- as.character(x)
  case_when(
    str_detect(x, "^\\d{6}$") ~ ymd(paste0(x, "01")),
    str_detect(x, "^\\d{4}-\\d{2}$") ~ ymd(paste0(x, "-01")),
    str_detect(x, "^\\d{4}$") ~ ymd(paste0(x, "-01-01")),
    TRUE ~ suppressWarnings(as.Date(x))
  )
}

standardize_comtrade <- function(df) {
  df <- df %>% janitor::clean_names()

  period_col <- first_existing(df, c("period", "ref_period_id", "period_desc"))
  value_col  <- first_existing(df, c("primary_value", "primaryvalue", "trade_value_usd", "value_usd"))
  weight_col <- first_existing(df, c("net_wgt", "net_weight", "net_weight_kg", "netwgt", "qty"))
  partner_col <- first_existing(df, c("partner_iso", "partner_code", "partner_desc", "partner"))

  df %>%
    mutate(
      date = parse_comtrade_month(.data[[period_col]]),
      partner_raw = as.character(.data[[partner_col]]),
      import_value_usd = as.numeric(.data[[value_col]]),
      net_weight_kg = as.numeric(.data[[weight_col]]),
      import_ton = net_weight_kg / 1000,
      source_region = case_when(
        str_detect(partner_raw, "USA|United States|842") ~ "USA",
        str_detect(partner_raw, "NZL|New Zealand|554") ~ "New Zealand",
        partner_raw %in% eu27 ~ "EU-27",
        TRUE ~ NA_character_
      ),
      unit_value_usd_ton = import_value_usd / import_ton
    ) %>%
    filter(!is.na(date), !is.na(source_region), !is.na(import_ton), import_ton > 0)
}

# ── 3. Pull monthly China dairy imports from UN Comtrade ─────────────────────
# If raw CSV already exists, the script reuses it to avoid repeated API calls.
raw_csv <- file.path(out_dir, "china_dairy_imports_raw_comtrade_HS0401_0406.csv")

get_one_hs_year <- function(yr, hs) {
  message(glue("Pulling year {yr}, HS {hs} ..."))
  tryCatch({
    comtradr::ct_get_data(
      type = "goods",
      frequency = "M",
      commodity_classification = "HS",
      commodity_code = hs,
      flow_direction = "Import",
      reporter = reporter_china,
      partner = partners_all,
      start_date = glue("{yr}-01"),
      end_date   = glue("{yr}-12"),
      tidy_cols = TRUE,
      verbose = FALSE,
      requests_per_second = 5 / 60,
      cache = TRUE
    ) %>% mutate(hs_code = hs)
  }, error = function(e) {
    warning(glue("Failed: year {yr}, HS {hs}. Error: {e$message}"))
    tibble()
  })
}

if (file.exists(raw_csv)) {
  raw_comtrade <- read_csv(raw_csv, show_col_types = FALSE)
} else {
  raw_comtrade <- crossing(year = years, hs_code = dairy_hs) %>%
    mutate(data = map2(year, hs_code, get_one_hs_year)) %>%
    select(data) %>%
    unnest(data)
  write_csv(raw_comtrade, raw_csv)
}

# ── 4. Clean and aggregate monthly by origin ─────────────────────────────────
china_dairy_monthly_by_hs <- standardize_comtrade(raw_comtrade) %>%
  group_by(date, source_region, hs_code) %>%
  summarise(
    import_value_usd = sum(import_value_usd, na.rm = TRUE),
    import_ton = sum(import_ton, na.rm = TRUE),
    unit_value_usd_ton = import_value_usd / import_ton,
    .groups = "drop"
  )

china_dairy_monthly_total <- china_dairy_monthly_by_hs %>%
  group_by(date, source_region) %>%
  summarise(
    import_value_usd = sum(import_value_usd, na.rm = TRUE),
    import_ton = sum(import_ton, na.rm = TRUE),
    unit_value_usd_ton = import_value_usd / import_ton,
    .groups = "drop"
  )

write_csv(china_dairy_monthly_by_hs, file.path(out_dir, "china_dairy_imports_by_origin_HS0401_0406_monthly.csv"))
write_csv(china_dairy_monthly_total, file.path(out_dir, "china_dairy_imports_NZ_EU_US_monthly_total.csv"))

# ── 5. Window-based substitution calculation ─────────────────────────────────
window_origin <- china_dairy_monthly_total %>%
  filter(source_region %in% c("USA", "New Zealand", "EU-27")) %>%
  mutate(
    window = case_when(
      date >= baseline_start & date <= baseline_end ~ "baseline_2017m06_2018m06",
      date >= post_start     & date <= post_end     ~ "post_2018m07_2019m07",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(window)) %>%
  group_by(window, source_region) %>%
  summarise(
    months = n_distinct(date),
    M_ton = sum(import_ton, na.rm = TRUE),
    V_usd = sum(import_value_usd, na.rm = TRUE),
    P_usd_ton = V_usd / M_ton,
    .groups = "drop"
  )

write_csv(window_origin, file.path(out_dir, "M5a_window_origin_imports.csv"))

window_wide <- window_origin %>%
  select(window, source_region, M_ton, V_usd, P_usd_ton) %>%
  pivot_wider(
    names_from = c(window, source_region),
    values_from = c(M_ton, V_usd, P_usd_ton),
    values_fill = 0
  )

# Helper values from window table
get_cell <- function(w, region, var) {
  window_origin %>%
    filter(window == w, source_region == region) %>%
    summarise(x = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    pull(x)
}

M_US_base  <- get_cell("baseline_2017m06_2018m06", "USA", "M_ton")
M_US_post  <- get_cell("post_2018m07_2019m07", "USA", "M_ton")
P_US_base  <- get_cell("baseline_2017m06_2018m06", "USA", "P_usd_ton")

M_alt_base <- window_origin %>%
  filter(window == "baseline_2017m06_2018m06", source_region %in% c("New Zealand", "EU-27")) %>%
  summarise(x = sum(M_ton, na.rm = TRUE), .groups = "drop") %>% pull(x)
M_alt_post <- window_origin %>%
  filter(window == "post_2018m07_2019m07", source_region %in% c("New Zealand", "EU-27")) %>%
  summarise(x = sum(M_ton, na.rm = TRUE), .groups = "drop") %>% pull(x)
V_alt_post <- window_origin %>%
  filter(window == "post_2018m07_2019m07", source_region %in% c("New Zealand", "EU-27")) %>%
  summarise(x = sum(V_usd, na.rm = TRUE), .groups = "drop") %>% pull(x)
P_alt_post <- V_alt_post / M_alt_post

US_decline_window_ton <- pmax(M_US_base - M_US_post, 0)
US_decline_ton <- ifelse(is.na(US_decline_calibrated_ton), US_decline_window_ton, US_decline_calibrated_ton)
Alt_substitution_ton <- pmax(M_alt_post - M_alt_base, 0)
Alt_substitution_effective_ton <- pmin(Alt_substitution_ton, US_decline_ton)
s_sub_raw <- Alt_substitution_ton / US_decline_ton
s_sub <- clip01(s_sub_raw)

Pd_ft_used <- ifelse(is.na(Pd_ft_usd_ton), P_US_base, Pd_ft_usd_ton)
price_premium_usd_ton <- pmax(P_alt_post - P_US_base, 0)

dairy_dwl_raw_M <- 0.5 * tau_D * Pd_ft_used * US_decline_ton / 1e6
residual_dwl_M <- 0.5 * price_premium_usd_ton * Alt_substitution_effective_ton / 1e6
dairy_dwl_adjusted_M <- dairy_dwl_raw_M * (1 - s_sub) + residual_dwl_M
dairy_dwl_reduction_M <- dairy_dwl_raw_M - dairy_dwl_adjusted_M

substitution_window <- tibble(
  baseline_start, baseline_end, post_start, post_end,
  M_US_base, M_US_post, US_decline_window_ton, US_decline_ton,
  M_alt_base, M_alt_post, Alt_substitution_ton, Alt_substitution_effective_ton,
  s_sub_raw, s_sub,
  P_US_base, P_alt_post, Pd_ft_used, price_premium_usd_ton,
  tau_D, dairy_dwl_raw_M, residual_dwl_M, dairy_dwl_adjusted_M, dairy_dwl_reduction_M
)

print(substitution_window)
write_csv(substitution_window, file.path(out_dir, "M5b_substitution_adjustment_window.csv"))

# ── 6. Corrected welfare table: raw and adjusted side by side ────────────────
raw_welfare <- tribble(
  ~scenario,                  ~dairy_tariff_active, ~raw_net_welfare_M,
  "Twin tariff",               TRUE,                -11675.7,
  "Tariff on alfalfa",         FALSE,                 -414.7,
  "Tariff on dairy",           TRUE,                 -8726.9,
  "Prohibitive tariff",        TRUE,                -15894.0,
  "Subsidy on alfalfa",        TRUE,                 -2797.8,
  "Subsidy on milk products",  TRUE,                 -4089.4
)

welfare_adjusted <- raw_welfare %>%
  mutate(
    s_sub = if_else(dairy_tariff_active, s_sub, NA_real_),
    dairy_dwl_raw_M = if_else(dairy_tariff_active, dairy_dwl_raw_M, NA_real_),
    residual_dwl_M = if_else(dairy_tariff_active, residual_dwl_M, NA_real_),
    dairy_dwl_adjusted_M = if_else(dairy_tariff_active, dairy_dwl_adjusted_M, NA_real_),
    dairy_dwl_reduction_M = if_else(dairy_tariff_active, dairy_dwl_reduction_M, 0),
    adjusted_net_welfare_M = raw_net_welfare_M + dairy_dwl_reduction_M
  )

print(welfare_adjusted)
write_csv(welfare_adjusted, file.path(out_dir, "M5c_welfare_substitution_adjusted.csv"))

# ── 7. Substitution-share sensitivity ────────────────────────────────────────
s_grid <- seq(0, 1, by = 0.1)

sensitivity_substitution <- crossing(
  scenario = raw_welfare$scenario,
  s_sub = s_grid
) %>%
  left_join(raw_welfare, by = "scenario") %>%
  mutate(
    alt_effective_ton = pmin(s_sub * US_decline_ton, Alt_substitution_ton),
    dairy_dwl_adjusted_M = dairy_dwl_raw_M * (1 - s_sub) +
      0.5 * price_premium_usd_ton * alt_effective_ton / 1e6,
    dairy_dwl_reduction_M = dairy_dwl_raw_M - dairy_dwl_adjusted_M,
    dairy_dwl_reduction_M = if_else(dairy_tariff_active, dairy_dwl_reduction_M, 0),
    adjusted_net_welfare_M = raw_net_welfare_M + dairy_dwl_reduction_M
  ) %>%
  select(scenario, s_sub, raw_net_welfare_M, dairy_dwl_reduction_M, adjusted_net_welfare_M)

write_csv(sensitivity_substitution, file.path(out_dir, "M5d_sensitivity_substitution_share.csv"))

# ── 8. M5 required elasticity sensitivity grid ───────────────────────────────
# Replace these baseline values with the calibrated values used in your theory/welfare section.
base_params <- tibble(
  eta_d   = -1.50,
  sigma_d =  2.00,
  phi     =  0.30
)

elasticity_grid <- bind_rows(
  tibble(case = "Baseline", eta_d = base_params$eta_d, sigma_d = base_params$sigma_d, phi = base_params$phi),
  tibble(case = "eta_d x 0.75", eta_d = base_params$eta_d * 0.75, sigma_d = base_params$sigma_d, phi = base_params$phi),
  tibble(case = "eta_d x 1.25", eta_d = base_params$eta_d * 1.25, sigma_d = base_params$sigma_d, phi = base_params$phi),
  tibble(case = "sigma_d x 0.75", eta_d = base_params$eta_d, sigma_d = base_params$sigma_d * 0.75, phi = base_params$phi),
  tibble(case = "sigma_d x 1.25", eta_d = base_params$eta_d, sigma_d = base_params$sigma_d * 1.25, phi = base_params$phi),
  tibble(case = "phi x 0.75", eta_d = base_params$eta_d, sigma_d = base_params$sigma_d, phi = base_params$phi * 0.75),
  tibble(case = "phi x 1.25", eta_d = base_params$eta_d, sigma_d = base_params$sigma_d, phi = base_params$phi * 1.25)
)

write_csv(elasticity_grid, file.path(out_dir, "M5e_elasticity_grid_for_exact_welfare.csv"))

# Preferred path: define this function in your main welfare-calibration script, then source it before this file.
# It must return a tibble with columns: scenario, net_welfare_M.
# Example signature:
# compute_welfare_counterfactual <- function(eta_d, sigma_d, phi) { ... }

if (exists("compute_welfare_counterfactual") && is.function(compute_welfare_counterfactual)) {
  elasticity_exact <- elasticity_grid %>%
    mutate(res = pmap(list(eta_d, sigma_d, phi), \(eta_d, sigma_d, phi) {
      compute_welfare_counterfactual(eta_d = eta_d, sigma_d = sigma_d, phi = phi)
    })) %>%
    unnest(res)

  write_csv(elasticity_exact, file.path(out_dir, "M5f_elasticity_sensitivity_EXACT.csv"))

  elasticity_bands <- elasticity_exact %>%
    group_by(scenario) %>%
    summarise(
      baseline_M = net_welfare_M[case == "Baseline"][1],
      low_M = min(net_welfare_M, na.rm = TRUE),
      high_M = max(net_welfare_M, na.rm = TRUE),
      .groups = "drop"
    )

  write_csv(elasticity_bands, file.path(out_dir, "M5g_elasticity_sensitivity_bands_EXACT.csv"))
} else {
  message("compute_welfare_counterfactual() not found. Writing a clearly labeled approximation only.")

  # Approximation for drafting only: it rescales the cross-market excess component using
  # phi/(sigma_d - eta_d). Use EXACT output above for final submission if possible.
  base_excess_M <- with(
    raw_welfare,
    raw_net_welfare_M[scenario == "Twin tariff"] -
      (raw_net_welfare_M[scenario == "Tariff on alfalfa"] + raw_net_welfare_M[scenario == "Tariff on dairy"])
  )

  elasticity_approx <- elasticity_grid %>%
    mutate(
      cross_factor = (phi / base_params$phi) *
        ((base_params$sigma_d - base_params$eta_d) / (sigma_d - eta_d)),
      excess_M = base_excess_M * cross_factor
    ) %>%
    crossing(raw_welfare) %>%
    mutate(
      net_welfare_M = case_when(
        scenario == "Twin tariff" ~
          raw_net_welfare_M + (excess_M - base_excess_M),
        scenario == "Subsidy on alfalfa" ~
          raw_net_welfare_M + 0.25 * (excess_M - base_excess_M),
        scenario == "Subsidy on milk products" ~
          raw_net_welfare_M + 0.50 * (excess_M - base_excess_M),
        TRUE ~ raw_net_welfare_M
      ),
      note = "Approximation only: replace with exact welfare recalculation before final submission."
    )

  write_csv(elasticity_approx, file.path(out_dir, "M5f_elasticity_sensitivity_APPROX_DO_NOT_FINAL.csv"))

  elasticity_bands_approx <- elasticity_approx %>%
    group_by(scenario) %>%
    summarise(
      baseline_M = net_welfare_M[case == "Baseline"][1],
      low_M = min(net_welfare_M, na.rm = TRUE),
      high_M = max(net_welfare_M, na.rm = TRUE),
      .groups = "drop"
    )

  write_csv(elasticity_bands_approx, file.path(out_dir, "M5g_elasticity_sensitivity_bands_APPROX_DO_NOT_FINAL.csv"))
}

# ── 9. Figures ───────────────────────────────────────────────────────────────
p1 <- china_dairy_monthly_total %>%
  filter(source_region %in% c("USA", "New Zealand", "EU-27")) %>%
  mutate(year = year(date)) %>%
  group_by(year, source_region) %>%
  summarise(M_ton = sum(import_ton, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = M_ton / 1000, color = source_region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "China dairy imports by origin",
    subtitle = "HS 0401–0406, monthly Comtrade aggregated to annual totals",
    x = NULL,
    y = "Imports, thousand tons",
    color = "Origin"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "fig_dairy_imports_by_origin.png"), p1, width = 8, height = 5, dpi = 300)

p2 <- substitution_window %>%
  ggplot(aes(x = "2018m07--2019m07", y = s_sub)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "NZ/EU substitution share after first trade-war shock",
    subtitle = "Bounded substitution share = NZ/EU import increase / U.S. dairy import decline",
    x = NULL,
    y = "Substitution share"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "fig_substitution_share_window.png"), p2, width = 7, height = 4.5, dpi = 300)

cat("\nDONE. Main outputs written to: ", out_dir, "\n", sep = "")
cat("  M5b_substitution_adjustment_window.csv\n")
cat("  M5c_welfare_substitution_adjusted.csv\n")
cat("  M5d_sensitivity_substitution_share.csv\n")
cat("  M5e_elasticity_grid_for_exact_welfare.csv\n")
cat("  M5f/M5g exact or approximate elasticity sensitivity files\n")
