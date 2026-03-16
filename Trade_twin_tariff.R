### add whey data
### library(readxl); library(purrr); library(dplyr); library(janitor)
library(readr)   # for parse_number
library(ggplot2); library(lubridate); library(fixest) ;library(zoo)
library(tidyr); library(stringr); library(tidyverse); library(fixest)
library(modelsummary); library(glue); library(did); library(forcats)
library(purrr)
library(dplyr)
library(janitor)
setwd("/Users/macpro/Desktop/Youmin-phd/research/Dairy and Alfalfa/China Figure")
df <- read.csv("dairy_trade_data.csv")
df$date <- as.Date(sprintf("%04d-%02d-01", df$Year, df$Month))
eneragy <- read.csv("exchange_rate_and_fuel_price.csv")
df$farmgate_usd_kg <- trade_df$farmgate_cny_kg * trade_df$usd_per_cny
df$alfalfa_usd_kg <- trade_df$alfalfa_price_usd_ton/1000

00 clean data ###add exchange rate. and fuel price
eneragy <- eneragy %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

df <- df %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d")) %>%
  left_join(eneragy, by = "date")
##### variable creat
trade_df <- df %>%
  arrange(date) %>%
  mutate(
    ln_feed = ifelse(feed_price_usd_ton + 1 > 0, log(feed_price_usd_ton), NA_real_),
    ln_alf =  log(replace_na(alfalfa_usd_kg, 0) + 1),
    ln_energy = log(replace_na(international_fuel_price_usd_per_barrel, 0) + 1),
    ln_dair = ifelse(dairy_usd_per_kg > 0, log(dairy_usd_per_kg), NA_real_),
    ln_dqty = ifelse(dairy_qty_ton + 1 > 0, log(dairy_qty_ton+1), NA_real_),
    ln_aqty = ifelse(alfalfa_qty+1 > 0, log(alfalfa_qty + 1), NA_real_), 
    ln_milkp = log(replace_na(farmgate_usd_kg, 0) + 1),
    ln_faop = ifelse(FAO_dairry_price_index > 0, log(FAO_dairry_price_index), NA_real_),
    ln_tra = log((tariff_rate_on_alfalfa/100) +1),
    ln_trd = log((tariff_rate_on_dairy/100)+1),
    ln_whey = log(whey_price),
    ln_whey_q = log(whey_qty),
    ###first difference
    d_ln_feed = ln_feed - lag(ln_feed),
    d_ln_alf = ln_alf - lag(ln_alf),
    d_ln_dair = ln_dair - lag(ln_dair),
    d_tra = (tariff_rate_on_alfalfa - lag(tariff_rate_on_alfalfa))/100,
    d_tra = (tariff_rate_on_dairy - lag(tariff_rate_on_dairy))/100,
    d_ln_tra = ln_tra -lag(ln_tra),
    d_ln_trd = ln_trd - lag(ln_trd), 
    d_ln_aqty = ln_aqty - lag(ln_aqty),
    d_ln_dqty = ln_dqty - lag(ln_dqty),
    d_ln_wheyp = ln_whey -lag(ln_whey),
    d_ln_wheyq = ln_whey - lag(ln_whey_q),
    d_ln_milkp = ln_milkp, - lag(ln_milkp),
    d_ln_faop = ln_faop - lag(ln_faop),
    d_ln_fuel = ln_energy - lag(ln_energy),
    month_fe  = factor(Month), year_fe   = factor(Year)
  )
K <-2

trade_df <- trade_df %>%
  arrange(date) %>%
  mutate(
    unit_id  = 1L,                 # single time-series unit (China)
    time_idx = row_number()       # numeric time index
  ) %>%
  distinct(unit_id, date, .keep_all = TRUE)

trade_df <- trade_df %>%
  mutate(
    # Interaction: alfalfa tariff × global dairy price competition
    # When FAO dairy price is HIGH, imports are expensive → escape valve more closed
    # (so alfalfa tariff bite should be LARGER when FAO price is high)
    interact_tra_fao = d_ln_tra * d_ln_faop,
    interact_tra_trd = d_ln_tra * d_ln_trd,
  )
trade_df$covid <- ifelse(
  trade_df$date >= as.Date("2020-03-01") & trade_df$date <= as.Date("2021-06-01"),
  1, 0
)

01 fixed effects#### fixed effects model - whey price (3)
m1 <- feols(d_ln_wheyp ~ f(d_ln_tra,0:K) + f(d_ln_aqty, 0:K) + f(d_ln_trd,0:K) + f(d_ln_dqty,0:K) + 
              f(d_ln_dair, 0:K) + f(d_ln_faop,0:K) +
             i(year_fe,"2018") + i(month_fe, "6") ,
            data = trade_df,
            panel.id = ~ unit_id + time_idx
)
summ_m1 <- summary(m1, vcov = NW(4))
summ_m1

01 - 1sls ## dairy import qty (1)
m2 <- feols(d_ln_dqty ~ f(d_ln_trd, 0:K)  + f(d_ln_tra, 0:K)  + interact_tra_trd + f(d_ln_fuel) + covid +
              i(month_fe) + i(year_fe),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
)
summ_m2 <- summary(m2, vcov = NW(4))
summ_m2

# IV: instrument feed dynamics with alfalfa tariff/world price
# import qty (1)
m_3 <- feols(d_ln_dqty ~ f(d_ln_trd, 0:K) + f(d_ln_alf, 0:K) + f(d_ln_tra,0:K) 
             + f(d_ln_dair, 0:K) + interact_tra_fao + interact_tra_trd + f(d_ln_fuel) +
              i(month_fe) + i(year_fe),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
) 
summ_m3 <- summary(m_3, vcov = NW(4))
summ_m3

m_iv1 <- feols(
  d_ln_milkp ~
    f(d_ln_alf,   0:K) +
    f(d_ln_dair,  0:K) +
    f(d_ln_wheyp, 0:K) +
    i(month_fe, ref = 6) +
    i(year_fe,  ref = 2018) |
    unit_id |
    f(d_ln_dqty,0:K) ~
    f(d_ln_tra, 0:K) + f(d_ln_trd, 0:K),
  data = trade_df17
)

summary(m_iv1)
summary(m_iv1, stage = 1)

02 2SLS ###2sls (2)
trade_df <- panel(trade_df, panel.id = ~ unit_id + time_idx)

m_iv <- feols(
  d_ln_milkp ~
    f(d_ln_alf, 0:K) +
    interact_tra_trd + f(d_ln_fuel) + f(d_ln_faop) + f(covid) + i(month_fe) + i(year_fe) |
    f(d_ln_dqty) ~
    f(d_ln_tra,  0:K) +
    f(d_ln_trd,  0:K) + ,
  data = trade_df,
  panel.id = ~ unit_id + time_idx
)

summary(m_iv)
summary(m_iv, stage = 1)


03 Reduced form ### appendix B. (1)
rf_mm <- feols(d_ln_milkp ~ f(d_ln_trd,0:K) + f(d_ln_tra, 0:K) + f(d_ln_fuel) + covid +
              i(year_fe) + i(month_fe),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
) 
rf_mm <- summary(rf_mm, vcov = NW(4))
rf_mm

04 robust check interction form # Reduced form WITH interaction (replaces rf_mm in existing code) - appendix B. (2)
rf_interact <- feols(
  d_ln_milkp ~ f(d_ln_tra, 0:2) + f(d_ln_trd, 0:2) + f(d_ln_dair,0:K) +
    interact_tra_trd + covid + f(d_ln_fuel) + 
    i(month_fe) + i(year_fe),
  data = trade_est,
  panel.id = ~unit_id + time_idx,
  vcov = "hetero"
)

# Compare: reduced form without vs. with interaction
etable(rf_mm, rf_interact, se = "hetero", depvar = TRUE,
       title = "Twin Tariff Interaction Test")

m5 <- feols(d_ln_alf ~ f(d_ln_trd,0:K) + f(d_ln_tra,0:K) + 
              i(month_fe, ref = "6") + i(year_fe, ref = "2018"),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
) 
summ_m5 <- summary(m5, vcov = NW(4))
summ_m5

04 robust check margin price - cost # Run the same reduced-form spec with margin as outcome - appendix b(3)
rf_margin <- feols(
  (d_ln_milkp - d_ln_alf) ~ f(d_ln_tra, 0:2) + f(d_ln_trd, 0:2) + covid + f(d_ln_fuel) +
    i(month_fe) + i(year_fe),
  data = trade_df,
  panel.id = ~unit_id + time_idx,
  vcov = "hetero"
)
etable(rf_mm, rf_margin, se = "hetero", depvar = TRUE,
       title = "Twin Tariff Effects: Milk Price vs. Margin")


m6 <- feols(d_ln_milkp ~ f(d_ln_alf, 0:K) +
              f(d_ln_wheyq,0:K) +
              f(d_ln_aqty,0:K) + f(d_ln_dqty,0:K) +
              i(month_fe, ref = "6") + i(year_fe, ref = "2018"),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
) 
summ_m6 <- summary(m6, vcov = NW(4))
summ_m6

### after 2017 APpendix A (fig 14-16)
trade_df17 <- trade_df %>%
  filter(date >= as.Date("2017-01-01"))
trade_df17 <- trade_df %>%
  filter(date >= as.Date("2017-01-01")) %>%
  panel(panel.id = ~ unit_id + time_idx)


04 robust check# Build indexed series
base_date <- as.Date("2017-01-01")
index_df <- trade_df %>%
  filter(!is.na(alfalfa_usd_kg), !is.na(farmgate_usd_kg)) %>%
  mutate(
    # Index to 100 at base period average
    base_alf = mean(alfalfa_usd_kg[date < as.Date("2018-01-01")], na.rm=TRUE),
    base_milk = mean(farmgate_usd_kg[date < as.Date("2018-01-01")], na.rm=TRUE),
    idx_alf = 100 * alfalfa_usd_kg / base_alf,
    idx_milk = 100 * farmgate_usd_kg / base_milk,
    idx_margin = idx_milk - idx_alf # scissors gap
  )

### scissor fig appendix A fig.15
ggplot(index_df, aes(x = date)) +
  geom_line(aes(y = idx_alf, color = "Alfalfa CIF price (indexed)"), linewidth = 0.8) +
  geom_line(aes(y = idx_milk, color = "Farm-gate milk price (indexed)"), linewidth = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2018-06-01"), linetype = "dashed", alpha = 0.5) +
  annotate("text", x = as.Date("2018-09-01"), y = 180,
           label = "Tariff shock", hjust = 0, size = 3.5) +
  scale_color_manual(values = c("Alfalfa CIF price (indexed)" = "#D95F02",
                                "Farm-gate milk price (indexed)" = "#1B9E77")) +
  labs(title = "The Scissors Effect: Input Cost vs. Output Price",
       subtitle = "Both indexed to 100 at pre-tariff average (2017)",
       x = NULL, y = "Index (pre-tariff = 100)", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")


04 robust check ### Appendix fig 14-16
# Define event time (months relative to first major tariff shock: July 2018)
shock_date <- as.Date("2018-07-01")
df_est <- trade_est %>%
  mutate(
    event_month = interval(shock_date, date) %/% months(1)
  )
# Keep a window of -18 to +36 months around the shock
# (pre-period: 18 months before; post: 36 months after)
df_event <- df_est %>%
  filter(event_month >= -18, event_month <= 36)
# Omit event_month = -1 as the reference period (one month before shock)
df_event <- df_event %>%
  mutate(event_month_f = factor(event_month))

# Event study for milk price
es_milk <- feols(
  d_ln_milkp ~ i(event_month_f, ref = "-1") +
    i(month_fe) + i(year_fe),
  data = df_event,
  panel.id = ~unit_id + time_idx,
  vcov = "hetero"
)
# Event study for alfalfa price (to verify the input channel)
es_alf <- feols(
  d_ln_alf ~ i(event_month_f, ref = "-1") +
    i(month_fe) + i(year_fe),
  data = df_event,
  panel.id = ~unit_id + time_idx,
  vcov = "hetero"
)
# Event study for margin
es_margin <- feols(
  (d_ln_milkp - d_ln_alf) ~ i(event_month_f, ref = "-1") +
    i(month_fe) + i(year_fe),
  data = df_event,
  panel.id = ~unit_id + time_idx,
  vcov = "hetero"
)

library(broom)
plot_event_study <- function(model, title_text) {
  tidy(model, conf.int = TRUE) %>%
    filter(str_detect(term, "event_month_f::")) %>%
    mutate(
      t = as.integer(str_replace(term, "event_month_f::", "")),
      significant = p.value < 0.1
    ) %>%
    ggplot(aes(x = t, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", alpha = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    geom_line(linewidth = 0.8) +
    geom_point(aes(fill = significant), shape = 21, size = 2) +
    scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"),
                      guide = "none") +
    annotate("text", x = 1, y = max(tidy(model)$estimate, na.rm=TRUE),
             label = "Tariff shock →", hjust = 0, size = 3.5, color = "red") +
    labs(title = title_text,
         x = "Months relative to July 2018 tariff shock",
         y = "Estimated coefficient (∆ log)",
         caption = "Shaded band = 95% CI. Reference period = month -1.") +
    theme_minimal(base_size = 13)
}
plot_event_study(es_milk, "Event Study: Farm-gate Milk Price")
plot_event_study(es_alf, "Event Study: Alfalfa Import Price")
plot_event_study(es_margin, "Event Study: Price-Cost Margin (Milk − Alfalfa)")


05 ## 05) sensitive test - appendix B - placbo test ============================================
# --- Minimal helpers that work with fixest + sensemakr -----------------------
library(sensemakr)

# Extract a robust t-stat for a named coefficient from a fixest model
tstat_fixest <- function(mod, coef_name, se_type = c("hetero","iid","cluster","nw","dk")) {
  se_type <- match.arg(se_type)
  ss <- summary(mod, se = se_type)
  ct <- ss$coeftable
  if (is.null(ct) || !coef_name %in% rownames(ct)) {
    stop(sprintf("Coefficient '%s' not found. Available:\n%s",
                 coef_name, paste(rownames(ct), collapse = ", ")))
  }
  unname(ct[coef_name, "t value"])
}

# Core sensitivity readout for *one* coefficient
sense_from_fixest <- function(mod, coef_name, se_type = "hetero",
                              alpha = 0.05, q = 1) {
  tval <- tstat_fixest(mod, coef_name, se_type = se_type)
  dof  <- df.residual(mod)  # residual DoF from fixest
  est  <- unname(coef(mod)[coef_name])
  
  # 1) Partial R^2 of treatment with outcome (conditional on included covariates)
  pr2 <- sensemakr::partial_r2(t = tval, dof = dof)
  
  # 2) Robustness value RV_{q,alpha}
  rv  <- sensemakr::robustness_value(t = tval, dof = dof, q = q, alpha = alpha)
  
  list(
    coef      = coef_name,
    estimate  = est,
    t_value   = tval,
    dof       = dof,
    partial_R2= pr2,
    RV_q_alpha= rv,
    alpha     = alpha,
    q         = q
  )
}

# Convenience: run for all lags of a variable expanded by f(var, 0:K)
sense_lag_bundle <- function(mod, var, K, se_type = "hetero", alpha = 0.05, q = 1) {
  coefs <- paste0(var, "::", 0:K)
  found <- intersect(coefs, names(coef(mod)))
  if (length(found) == 0L) stop("No matching coefficients found in model.")
  out <- lapply(found, \(nm) sense_from_fixest(mod, nm, se_type, alpha, q))
  do.call(rbind, lapply(out, as.data.frame))
}

# Example: mm1, focus on contemporaneous d_ln_trd (lag 0)
res_rf_mm1_tra0 <- sense_from_fixest(rf_mm1, coef_name = "f(d_ln_tra, 0)",
                                  se_type = "hetero", alpha = 0.05, q = 1)
res_rf_mm1_tra0

sensemakr::partial_r2_from_model(rf_mm1, covariates = "f(d_ln_alf, 0)")
sensemakr::partial_r2_from_model(rf_mm1, covariates = "f(d_ln_dair, 0)")

res_mm1_trd0$RV_q_alpha

# contemporaneous trade shock
sense_from_fixest(mm1, "f(d_ln_trd, 0)", se_type = "hetero", q = 1, alpha = 0.05)
sense_lag_bundle <- function(mod, var_prefix, K, se_type = "hetero", alpha = 0.05, q = 1) {
  # Construct expected names like f(d_ln_trd, 0), f(d_ln_trd, 1), ...
  coefs <- paste0("f(", var_prefix, ", ", 0:K, ")")
  found <- intersect(coefs, names(coef(mod)))
  if (length(found) == 0L)
    stop("No matching coefficients found in model. Available:\n",
         paste(names(coef(mod)), collapse = ", "))
  
  out <- lapply(found, function(nm)
    sense_from_fixest(mod, nm, se_type = se_type, alpha = alpha, q = q))
  do.call(rbind, lapply(out, as.data.frame))
}

sense_lag_bundle(mm1, "d_ln_trd", K, se_type = "hetero", q = 1, alpha = 0.05)
sense_from_fixest(iv_mm1, "fit_d_ln_dqty", se_type = "hetero", q = 1, alpha = 0.05)

06 ### 06) time serials interrupet
control_start <- as.Date("2017-01-01"); control_end <- as.Date("2018-06-01")
war_start     <- as.Date("2018-06-01"); war_end     <- as.Date("2022-02-01")
adjust_start   <- as.Date("2022-03-01"); adjust_end   <- as.Date("2023-09-01")
second_start    <- as.Date("2023-09-01"); secon_end    <- as.Date("2025-06-01")

dft <- df_est %>%
  mutate(
    window = case_when(
      date.x >= as.Date("2017-01-01") & date.x < as.Date("2018-06-01") ~ "control",
      date.x >= as.Date("2018-06-01") & date.x < as.Date("2020-02-01") ~ "trade_war",
      date.x >= as.Date("2022-03-01") & date.x < as.Date("2023-09-01") ~ "adjust",
      date.x >= as.Date("2023-03-01") & date.x < as.Date("2025-06-01") ~ "2_trade_war",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(window)) %>%
  group_by(unit_id, window) %>%
  summarise(
    avg_d_ln_priced = mean(d_ln_dair, na.rm = TRUE),
    avg_d_ln_pricea = mean(d_ln_alf, na.rm = TRUE),
    avg_d_ln_dqty   = mean(d_ln_dqty, na.rm = TRUE),
    avg_d_ln_aqty   = mean(d_ln_aqty, na.rm = TRUE), 
    avg_d_ln_milkp   = mean(d_ln_milkp, na.rm = TRUE),
    .groups = "drop"
  )

m_dprice <- feols(avg_d_ln_priced ~ i(window, ref = "control"), data = dft)
m_dqty   <- feols(avg_d_ln_dqty   ~ i(window, ref = "control"), data = dft)
m_aprice <- feols(avg_d_ln_pricea ~ i(window, ref = "control"), data = dft)
m_aqty   <- feols(avg_d_ln_aqty   ~ i(window, ref = "control"), data = dft)
m_milkp  <- feols(avg_d_ln_milkp ~ i(window, ref = "control"), data = dft)
m_milkp <- feols(avg_d_ln_milkp ~ i(window, ref = "control"), data = dft)

##BOX PLOT 
p1 <- ggplot(dft, aes(x = window, y = avg_d_ln_priced)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Avg monthly Δ ln(dairy price)", 
       title = "China imports: average dairy monthly price change by period") +
  theme_minimal(base_size = 12)
p1 <- p1 + stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red")
print(p1)

p2 <- ggplot(dft, aes(x = window, y = avg_d_ln_dqty)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Avg monthly Δ ln(dairy quantity)",
       title = "China imports: average dairy monthly quantity change by period") +
  theme_minimal(base_size = 12)
p2 <- p2 + stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red")
print(p2)

p3 <- ggplot(dft, aes(x = window, y = avg_d_ln_pricea)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Avg monthly Δ ln(alfalfa price)", 
       title = "China imports: average alfalfa monthly price change by period") +
  theme_minimal(base_size = 12)
p3 <- p3 + stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red")
print(p3)

p4 <- ggplot(dft, aes(x = window, y = avg_d_ln_aqty)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Avg monthly Δ ln(alfalfa quantity)",
       title = "China imports: average monthly alfalfa quantity change by period") +
  theme_minimal(base_size = 12)
p4 <- p4 + stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red")
print(p4)

p5 <- ggplot(dft, aes(x = window, y = avg_d_ln_milkp)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Farm-gate average monthly Δ ln(milk price)",
       title = "Chian farm-gate milk price change by period") +
  theme_minimal(base_size = 12)
p5 <- p5 + stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red")
print(p5)

#____________________________________________________________________________-
### 5)ATT time serials interrupt
# --- 1) Choose variables to analyze
vars <- c("d_ln_dair", "d_ln_alf", "d_ln_dqty", "d_ln_aqty", "d_ln_milkp")
var_labels <- c(
  d_ln_dair = "Δ ln(dairy price)",
  d_ln_alf  = "Δ ln(alfalfa price)",
  d_ln_dqty    = "Δ ln(dairy quantity)",
  d_ln_aqty    = "Δ ln(alfalfa quantity)",
  d_ln_milkp   = "Δ ln(farm-gate milk price)"
)
# --- 2) Build month-of-year control baselines (per unit & month-of-year)
base_long <- df_est %>%
  dplyr::filter(date.x >= as.Date("2017-01-01") & date.x < as.Date("2018-06-01")) %>%
  dplyr::mutate(month_lab = factor(lubridate::month(date.x, label = TRUE, abbr = TRUE),
                                   levels = month.abb)) %>%
  dplyr::group_by(unit_id, month_lab) %>%
  dplyr::summarise(dplyr::across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(cols = all_of(vars), names_to = "var", values_to = "base")

# --- 3) Compute TE (per unit-month) and ATE (mean across units)
# TE: value - (unit's control-period mean for same month-of-year)
add_window <- function(df){
  df %>%
    dplyr::mutate(
      window = dplyr::case_when(
        date.x >= as.Date("2017-05-01") & date.x < as.Date("2018-05-01") ~ "control",
        date.x >= as.Date("2018-06-01") & date.x < as.Date("2022-06-01") ~ "trade_war",
        date.x >= as.Date("2022-07-01") & date.x < as.Date("2023-07-01") ~ "adjust",
        date.x >= as.Date("2023-08-01") & date.x < as.Date("2025-06-01") ~ "war2",
        TRUE ~ NA_character_
      ),
      month_lab = factor(lubridate::month(date.x, label = TRUE, abbr = TRUE),
                         levels = month.abb)
    ) %>%
    dplyr::filter(!is.na(window)) %>%
    dplyr::mutate(window = factor(
      window,
      levels = c("control","trade_war","adjust","war2"),
      labels = c("Control (2017-05–2018-05)",
                 "Trade War (2018-06–2022-06)",
                 "Adjust (2022-07–2023-07)",
                 "2nd Trade War (2023-07–2025-07)")
    ))
}

control_lab <- "Control (2017-05–2018-06)"

# ATT: mean TE across units (+ SE and 95% CI)
df_eff <- df_est %>%
  add_window() %>%
  dplyr::select(unit_id, window, month_lab, dplyr::all_of(vars)) %>%
  tidyr::pivot_longer(dplyr::all_of(vars), names_to = "var", values_to = "value") %>%
  dplyr::left_join(base_long, by = c("unit_id","month_lab","var")) %>%
  dplyr::mutate(TE = ifelse(window == control_lab, value, value - base)) %>%
  dplyr::filter(window == control_lab | !is.na(base))

df_te <- df_eff %>%
  dplyr::mutate(TE_plot = ifelse(as.character(window) == control_lab, value, TE)) %>%
  dplyr::group_by(unit_id, window, month_lab, var) %>%
  dplyr::summarise(TE_plot = mean(TE_plot, na.rm = TRUE), .groups = "drop")

df_ate <- df_te %>%
  dplyr::group_by(window, month_lab, var) %>%
  dplyr::summarise(
    ATE  = mean(TE_plot, na.rm = TRUE),
    n    = sum(!is.na(TE_plot)),
    SE   = sd(TE_plot, na.rm = TRUE) / sqrt(pmax(n,1)),
    CI_lo = ATE - 1.96*SE,
    CI_hi = ATE + 1.96*SE,
    .groups = "drop"
  )

df_mean <- df_te %>%
  dplyr::group_by(window, var) %>%
  dplyr::summarise(win_mean = mean(TE_plot, na.rm = TRUE), .groups = "drop")

# --- 4) Plot function: boxplots (TE) + ATE points with 95% CI
plot_TE_ATE <- function(var_code){
  te_dat   <- dplyr::filter(df_te,  var == var_code)
  ate_dat  <- dplyr::filter(df_ate, var == var_code)
  win_mean <- dplyr::filter(df_mean, var == var_code)
  y_lab    <- var_labels[[var_code]]
  
  ggplot() +
    geom_boxplot(
      data = te_dat,
      aes(x = month_lab, y = TE_plot),
      outlier.alpha = 0.3, width = 0.75
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(
      data = win_mean,
      aes(yintercept = win_mean),
      linewidth = 0.6
    ) +
    geom_line(
      data = ate_dat,
      aes(x = month_lab, y = ATE, group = 1),
      linewidth = 0.5
    ) +
    geom_errorbar(
      data = ate_dat,
      aes(x = month_lab, ymin = CI_lo, ymax = CI_hi),
      width = 0.2
    ) +
    geom_point(
      data = ate_dat,
      aes(x = month_lab, y = ATE),
      shape = 21, size = 2, stroke = 0.6, fill = "white"
    ) +
    facet_wrap(~ window, ncol = 2) +
    labs(
      x = NULL, y = y_lab,
      title = paste0("Monthly TE for treated; Actual for control — ", y_lab),
      subtitle = paste0(
        "Control facet shows actual values. TE = treatement − control.
        Solid line = window mean. 95% CI on ATE."
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(vjust = 0.9),
      strip.text = element_text(face = "bold")
    )
}

p_dair <- plot_TE_ATE("d_ln_dair")
p_alf  <- plot_TE_ATE("d_ln_alf")
p_dqty <- plot_TE_ATE("d_ln_dqty")
p_aqty <- plot_TE_ATE("d_ln_aqty")
p_milk <- plot_TE_ATE("d_ln_milkp")
print(p_dair)
print(p_alf)
print(p_dqty)
print(p_aqty)
print(p_milk)
ggsave("TE_ATE_dairy_price.png", p_dair, width = 11, height = 6.5, dpi = 300)

                
### longrun effects
                # (B) Milk price: feed & dairy tariff
m2 <- feols(d_ln_dair ~ f(d_ln_alf, 0:K) + f(d_ln_trd, 0:K) + f(d_ln_tra, 0:K) + f(d_ln_aqty, 0:K) + f(d_ln_feed, 0:K) + 
    f(d_ln_dqty, 0:K) +
    i(month_fe, "6") + i(year_fe),
  data = df_est,
  panel.id = ~ unit_id + time_idx
  )

summ_m2 <- summary(m2, vcov = NW(4))
summ_m2

# (C) import qty
m3 <- feols(d_ln_dqty ~ f(d_ln_trd, 0:K) + f(d_ln_dair, 0:K) + f(d_ln_tra,0:K) +
            f(d_ln_alf, 0:K) +
            i(month_fe, ref = "6") + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
) 

summ_m3 <- summary(m3, vcov = NW(4))
summ_m3
# D) world price shift
m4 <- feols(d_ln_milkp ~ f(d_ln_trd,0:K) + f(d_ln_tra,0:K) + f(d_ln_alf,0:K) + f(d_ln_dair,0:K) + f(d_ln_aqty,0:K)
            + f(d_ln_dqty,0:K) + 
              i(month_fe, ref = "6") + i(year_fe, ref = "2018"),
            data = df_est,
            panel.id = ~ unit_id + time_idx
            ) 
summ_m4 <- summary(m4, vcov = NW(4))
summ_m4

m5 <- feols(ln_faop ~ f(d_trd,0:K) + f(d_tra,0:K) + f(ln_alf,0:K) + f(ln_dair,0:K) + f(d_milkp,0:K) + f(ln_aqty,0:K) + f(ln_dqty,0:K) +
              i(month_fe) + i(year_fe, ref = "2018"),
            data = df_est,
            panel.id = ~ unit_id + time_idx
) 
summ_m5 <- summary(m5, vcov = NW(4))
summ_m5

m6 <- feols(ln_feed ~ f(ln_alf,0:K) + f(d_tra,0:K) + f(ln_dair,0:K) + f(d_milkp,0:K) + f(ln_faop,0:K) + f(ln_alf,0:K)+
              i(month_fe, ref = "6") + i(year_fe, ref = "2018"),
            data = df_est,
            panel.id = ~ unit_id + time_idx
) 
summ_m6 <- summary(m6,vcov = NW(4))

m7 <- feols(d_ln_aqty ~ f(d_ln_tra,0:K) + f(d_ln_trd,0:K) + f(d_ln_alf,0:K) + f(d_ln_dair,0:K) + f(d_ln_feed, 0:K) + f(d_ln_dqty,0:K) +
              i(month_fe, ref = "6") + i(year_fe, ref = "2018"),
            data = df_est,
            panel.id = ~ unit_id + time_idx
)  
summ_m7 <- summary(m7,vcov = NW(4))
summ_m7
etable(m3, m7, m4, se = "hetero", depvar = TRUE)
        
ms_vcov <- function(x) fixest::vcov(x, vcov = "iid")   # or "hetero" once stable
cum_effect <- function(model, var, vcov_type = "iid") {
  idx <- grep(paste0("^f\\(", var, ","), names(coef(model)))
  if (!length(idx)) return(NA_character_)
  co  <- coef(model)[idx]
  V   <- fixest::vcov(model, vcov = vcov_type)
  se  <- sqrt(sum(V[idx, idx]))
  est <- sum(co)
  star <- function(z){
    if (is.na(z)) "" else if (z > qnorm(0.995)) "***"
    else if (z > qnorm(0.975)) "**" else if (z > qnorm(0.95)) "*"
    else if (z > qnorm(0.90)) "." else ""
  }
  z <- if (is.na(se) || se==0) NA_real_ else abs(est/se)
  sprintf("%.3f%s", est, star(z))
}

lr_row <- tibble::tibble(
  term = "Long-run effect: Dairy tariff block (Σ lags)",
  `(1) Feed price (Δ log)`   = cum_effect(m1, "d_trd"),
  `(2) Milk price (Δ log)`   = cum_effect(m2, "d_trd"),
  `(3) Import qty (Δ log)`   = cum_effect(m3, "d_trd"),
  `(4) Farm-gate Milk price (Δ log)`   = cum_effect(m4, "d_trd"),
  `(5) FAO index (Δ log)`    = cum_effect(m5, "d_trd"),
  `(6) Alfalfa price (Δ log)`= cum_effect(m6, "d_trd"),
  `(7) Alfalfa qty (Δ log)`  = cum_effect(m7, "d_trd")
)

models <- list(
  "(1) Feed price (Δ log)"    = m1,
  "(2) Milk price (Δ log)"    = m2,
  "(3) Import qty (Δ log)"    = m3,
  "(4) Milk price (Δ log)"    = m4,
  "(5) FAO index (Δ log)"     = m5,
  "(6) Alfalfa price (Δ log)" = m6,
  "(7) Alfalfa qty (Δ log)"   = m7
)
coef_rename <- c(
  "d_trd"     = "Dairy tariff rate (Δ)",
  "d_tra"     = "Alfalfa tariff rate (Δ)",
  "d_ln_feed" = "Feed price (Δ log)",
  "d_ln_dqty"    = "Dairy import qty (Δ log)",
  "d_ln_milkp"   = "Farm-gate milk price (Δ log)",
  "d_ln_faop"    = "FAO dairy price index (Δ log)",
  "d_ln_alf"  = "Alfalfa import price (Δ log)",
  "d_ln_TRd"     = "Tariff revenue (dairy, Δ log)",
  "d_ln_TRa"     = "Tariff revenue (alfalfa, Δ log)"
)

gof <- c("nobs","r.squared","rmse")  # portable across versions

modelsummary(
  models,
  vcov       = ms_vcov,               # <<< key line
  coef_rename = coef_rename,
  stars      = TRUE,
  gof_map    = gof,
  fmt        = 3,
  statistic  = "({std.error})",
  add_rows   = rbind(
    lr_row,
    tibble::tibble(
      term = "Fixed effects",
      `(1) Feed price (Δ log)`    = "Month & Year",
      `(2) Milk price (Δ log)`    = "Month & Year",
      `(3) Import qty (Δ log)`    = "Month & Year",
      `(4) Milk price (Δ log)`    = "Month & Year",
      `(5) FAO index (Δ log)`     = "Month & Year",
      `(6) Alfalfa price (Δ log)` = "Month & Year",
      `(7) Alfalfa qty (Δ log)`   = "Month & Year"
    )
  ),
  output = "model_table_with_longrun.docx"
)


###summary models
models <- list(
  "(1) Feed cost"      = m1,
  "(2) Dairy price OLS" = m2,
  "(3) Dairy imports qty" = m3,
  "(4) Domestic milk price" = m4,
  "(5) FAO dairy price index" = m5,
  "(6) Alfalfa import price" = m6,
  "(7) Alfalfa import qty"  = m7
)

etable(
  m1, m2, m3, m4, m5, m6, m7,
  title = "Tariff and Feed Effects on China’s Dairy Outcomes",
  se = "hetero",                 # heteroskedasticity-robust SE
  keep = "^d_|^f\\(",            # keep differenced & lag blocks
  ## drop = "month_fe|year_fe",     # hide month/year FE dummies if present on RHS
  order = "type",
  dict = c(
    "d_trd"      = "Dairy tariff rate (Δ)",
    "d_tra"      = "Alfalfa tariff rate (Δ)",
    "d_ln_feed"  = "Feed price (Δ log)",
    "d_dqty"     = "Dairy import qty (Δ log)",
    "d_milkp"    = "Farm-gate milk price (Δ log)",
    "d_faop"     = "FAO dairy price index (Δ log)",
    "d_ln_alf"   = "Alfalfa import price (Δ log)",
    "d_TRd"      = "Tariff revenue (dairy, Δ log)",
    "d_TRa"      = "Tariff revenue (alfalfa, Δ log)"
  ),
  signif.code = c("+"=0.1, "*"=0.05, "**"=0.01, "***"=0.001),  # AEPP often shows + for 10%
  digits = 3,
  fitstat = c("n","r2","rmse"),   # portable set across fixest versions
  notes = "Robust standard errors in parentheses. Month and year fixed effects included where indicated but not shown. All RHS variables are first differences (Δ) or log-differences (Δ log)."
)



#### seperate IV check
m_iv2 <- feols(
  d_ln_milkp ~
    f(d_ln_aqty, 0:K) +
    f(d_ln_dqty,0:K) +
    f(d_ln_wheyq,0:K) +
    i(month_fe, ref="6") +
    i(year_fe,  ref="2018") |
    unit_id |
    f(d_ln_alf) ~
    f(d_ln_tra, 0:K) +
    f(d_ln_trd, 0:K),
  data = trade_df,
  panel.id = ~ unit_id + time_idx
)

summary(m_iv2)
summary(m_iv2, stage = 1)

m_iv3 <- feols(
  d_ln_milkp ~
    f(d_ln_dqty,0:K) +
    f(d_ln_wheyq,0:K) +
    i(month_fe, ref="6") +
    i(year_fe,  ref="2018") |
    f(d_ln_dair) ~
    f(d_ln_trd,  0:K)+ f(d_ln_tra,0:K),
  data = trade_df,
  panel.id = ~ unit_id + time_idx
)

summary(m_iv3)
summary(m_iv3, stage = 1)

m_iv4 <- feols(
  d_ln_milkp ~
    f(d_ln_alf, 0:K) +
    f(d_ln_dqty,0:K) +
    f(d_ln_aqty,0:K) +
    i(month_fe, ref="6") +
    i(year_fe,  ref="2018") |
    f(d_ln_wheyq) ~
    f(d_ln_tra,  0:K) +
    f(d_ln_trd,  0:K) +
    f(d_ln_dair, 0:K) ,
  data = trade_df
)

summary(m_iv4)
summary(m_iv, stage = 1)


df_est <- trade_df %>%
  arrange(date) %>%
  mutate(unit_id = 1L, time_idx = row_number()) %>%
  distinct(unit_id, date, .keep_all = TRUE)
# Escape valve test
rf_escape <- feols(
  d_ln_milkp ~ f(d_ln_tra, 0:2) + f(d_ln_trd, 0:2) +
    f(d_ln_faop, 0:2) +
    interact_tra_fao +
    i(month_fe) + i(year_fe),
  data = trade_df,
  panel.id = ~unit_id + time_idx,
  vcov = "hetero"
)
summary(rf_escape, vcov = "hetero")

