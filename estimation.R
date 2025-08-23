### Estimation of China improt from U.S. monthly dada
### library
library(readxl)
library(purrr)
library(dplyr)
library(janitor)
library(readr)   # for parse_number
library(ggplot2)
library(lubridate)
library(fixest)  
library(zoo)
library(tidyr)
library(stringr)
library(tidyverse)
library(fixest)
library(modelsummary)
library(glue)
install.packages("did")
library(did)
setwd("/Users/macpro/Desktop/Youmin-phd/research/Dairy and Alfalfa/China Figure")

###load
path <- ("fao-dairy-price-indices-aug.xlsx")
cn_m <- read.csv("cn_m.csv")
summary(cn_m)

### raw alfalfa process
alfalfa_raw <- read_csv("alfalfa_hay_m.csv", skip = 4) %>%
  clean_names()
months <- tolower(month.name)
value_idx <- grep("^value_\\d+$", names(alfalfa_raw))
qty_idx   <- grep("^qty_\\d+$",   names(alfalfa_raw))
value_idx <- sort(value_idx)
qty_idx   <- sort(qty_idx)

stopifnot(length(value_idx) >= 12, length(qty_idx) >= 12)
for (i in seq_len(12)) {
  names(alfalfa_raw)[value_idx[i]] <- paste0(months[i], "_value")
  names(alfalfa_raw)[qty_idx[i]]   <- paste0(months[i], "_qty")
}

alfalfa_long <- alfalfa_raw %>%
  mutate(
    year = as.numeric(str_sub(as.character(year), 1, 4))
  ) %>%
  pivot_longer(
    cols = matches("(january|february|march|april|may|june|july|august|september|october|november|december)_(value|qty)"),
    names_to = c("month_name", ".value"),
    names_pattern = "(.*)_(value|qty)"
  ) %>%
  mutate(
    month = match(tolower(month_name), tolower(month.name)),
    date  = make_date(year, month, 1),
    # NOTE: the sheet states "Values in Thousands of Dollars"
    # convert to USD before dividing by quantity (assumed metric tons in UOM)
    alfalfa_price_usd_ton = (value * 1000) / qty
  ) %>%
  select(date, year, month, value_usd_thousand = value, qty_ton = qty, alfalfa_price_usd_ton) %>%
  arrange(date)

names(alfalfa_raw)
alfalfa_long <- alfalfa_long[1:252, ]

###merge with alfalfa
clean_df <- cn_m %>%
  mutate(date = mdy(date)) %>%
  left_join(alfalfa_long, by = c("year","month"))

write_csv(clean_df,"clean_df.csv")

### estimation
### 1) tariff revenue calculation
clean_df <- clean_df %>%
  mutate(
    TRd   = (tariff_rate_on_dairy/100) * dairy_value_usd,
    TRa = (tariff_rate_on_alfalfa/100) * feed_value_usd
  )

annual_revenue <- clean_df %>%
  group_by(year) %>%
  summarise(
    dairy_tariff_revenue   = sum(TRd, na.rm = TRUE),
    alfalfa_tariff_revenue = sum(TRa, na.rm = TRUE)
  )
print(annual_revenue)

###.   2) estimation model
clean_df <- clean_df %>%
  arrange(date.x) %>%
  mutate(
    ln_feed = ifelse(feed_price_usd_ton + 1 > 0, log(feed_price_usd_ton), NA_real_),
    ln_alf =  log(replace_na(alfalfa_price_usd_ton, 0) + 1),
    ln_dair = ifelse(dairy_usd_per_kg > 0, log(dairy_usd_per_kg), NA_real_),
    ln_TRa = ifelse(TRa + 1 > 0, log(TRa+1), NA_real_),
    ln_TRd = ifelse(TRd + 1 > 0, log(TRd+1), NA_real_),
    ln_dqty = ifelse(dairy_qty_ton + 1 > 0, log(dairy_qty_ton+1), NA_real_),
    ln_aqty = ifelse(qty_ton+1 > 0, log(qty_ton + 1), NA_real_), 
    ln_milkp = ifelse(farmgate_cny_kg > 0 , log(farmgate_cny_kg), NA_real_),
    ln_faop = ifelse(FAO_dairry_price_index > 0, log(FAO_dairry_price_index), NA_real_),
    ###first difference
    d_ln_feed = ln_feed - lag(ln_feed),
    d_ln_alf = ln_alf - lag(ln_alf),
    d_ln_dair = ln_dair - lag(ln_dair),
    d_tra = tariff_rate_on_alfalfa - lag(tariff_rate_on_alfalfa),
    d_trd = tariff_rate_on_dairy - lag(tariff_rate_on_dairy),
    d_TRa = ln_TRa - lag(ln_TRa),
    d_TRd = ln_TRd - lag(ln_TRd),
    d_aqty = ln_aqty - lag(ln_aqty),
    d_dqty = ln_dqty - lag(ln_dqty),
    d_milkp = ln_milkp, - lag(ln_milkp),
    d_faop = ln_faop - lag(ln_faop),
    month_fe  = factor(month), year_fe   = factor(year)
  )

K <- 3

### panel data id
df_est <- clean_df %>%
  arrange(date.x) %>%
  mutate(
    unit_id  = 1L,                 # single time-series unit (China)
    time_idx = row_number()       # numeric time index
  ) %>%
  distinct(unit_id, date.x, .keep_all = TRUE)


### (A) Pass-through: alfalfa tariff -> feed costs
m1 <- feols(d_ln_alf ~ f(d_tra,0:K) + f(d_ln_feed,0:K) +
              f(d_aqty,0:K) + f(d_TRa,0:K) +
              i(month_fe) + i(year_fe), 
            data = df_est,
            panel.id = ~ unit_id + time_idx
            )
summ_m1 <- summary(m1, vcov = NW(4))
summ_m1
##set na as 0
vars <- c("d_milkp", "-lag(ln_milkp)")
df_est <- df_est %>% mutate(across(all_of(vars), ~ replace_na(., 0)))
df_est <- df_est %>% mutate(across(where(is.numeric), ~ replace(., is.na(.) & row_number() == 1, 0)))

# (B) Milk price: feed & dairy tariff
m2 <- feols(
  d_ln_dair ~ f(d_ln_alf, 0:K) + f(d_trd, 0:K) + f(d_tra, 0:K) + f(d_aqty, 0:K) + 
    f(d_ln_feed, 0:K) + f(d_dqty, 0:K) + f(d_milkp, 0:K)  + 
    i(month_fe) + i(year_fe),
  data = df_est,
  panel.id = ~ unit_id + time_idx
  )
summ_m2 <- summary(m2, vcov = NW(4))
summ_m2

# IV: instrument feed dynamics with alfalfa tariff/world price
m2_iv <- feols(
     d_ln_dair ~ f(d_trd,0:K) + f(d_dqty, 0:K) + f(d_milkp,0:K) + f(d_TRd,0:K) + i(month_fe) + i(year_fe) |
      f(d_ln_alf,0:K) ~ f(d_tra,0:K) + f(d_ln_feed,0:K) + f(d_aqty,0:K) + f(d_TRa,0:K),
     data = df_est,
     panel.id = ~ unit_id + time_idx
   )

etable(m1, m2, m2_iv, se = "hetero", depvar = TRUE)

# (C) import qty
m3 <- feols(d_dqty ~ f(d_trd, 0:K) + f(d_ln_feed, 0:K) + 
            f(d_aqty, 0:K) + f(d_milkp,0:K) +
            i(month_fe) + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
) 

summ_m5 <- summary(m5, vcov = NW(4))
summ_m5
# D) world price shift
m4 <- feols(d_milkp ~ f(d_trd,0:K) + f(d_tra,0:K) + f(d_ln_alf,0:K) + f(d_ln_dair,0:K) + f(d_faop,0:K) +
              i(month_fe) + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
            ) 

m5 <- feols(d_faop ~ f(d_trd,0:K) + f(d_tra,0:K) + f(d_ln_alf,0:K) + f(d_ln_dair,0:K) + f(d_milkp,0:K) + f(d_ln_feed,0:K) +
              i(month_fe) + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
) 

m6 <- feols(d_ln_alf ~ f(d_ln_alf,0:K) + f(d_tra,0:K) + f(d_ln_dair,0:K) + f(d_milkp,0:K) + f(d_faop,0:K) +
              i(month_fe) + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
) 

m7 <- feols(d_aqty ~ f(d_trd,0:K) + f(d_tra,0:K) + f(d_ln_alf,0:K) + f(d_ln_dair,0:K) + f(d_milkp,0:K) + f(d_faop, 0:K) + f(d_ln_feed,0:K) +
              i(month_fe) + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
) 

### longrun effects
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
  "d_dqty"    = "Dairy import qty (Δ log)",
  "d_milkp"   = "Farm-gate milk price (Δ log)",
  "d_faop"    = "FAO dairy price index (Δ log)",
  "d_ln_alf"  = "Alfalfa import price (Δ log)",
  "d_TRd"     = "Tariff revenue (dairy, Δ log)",
  "d_TRa"     = "Tariff revenue (alfalfa, Δ log)"
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
  se = "hetero",
  keep = "d_",
  drop = "month_fe|year_fe",
  order = "type",
  dict = c(
    "d_trd"     = "Dairy tariff rate",
    "d_tra"     = "Alfalfa tariff rate",
    "d_ln_feed" = "Feed price (Δ log)",
    "d_dqty"    = "Dairy import qty",
    "d_milkp"   = "Farm-gate milk price",
    "d_faop"    = "FAO price index",
    "d_ln_alf"  = "Alfalfa import price"
  ),
  title = "Tariff and Feed Effects on Dairy Outcomes",
  fitstat = c("n","r2","rmse")   # <= safe across versions
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


### 4) treatment effects DID
control_start <- as.Date("2017-06-01"); control_end <- as.Date("2018-06-01")
war_start     <- as.Date("2018-06-01"); war_end     <- as.Date("2019-06-01")
adj_start     <- as.Date("2022-06-01"); adj_end     <- as.Date("2023-06-01")

dft <- df_est %>%
  mutate(
    window = case_when(
      date.x >= as.Date("2017-06-01") & date.x < as.Date("2018-06-01") ~ "control",
      date.x >= as.Date("2018-06-01") & date.x < as.Date("2019-06-01") ~ "trade_war",
      date.x >= as.Date("2022-06-01") & date.x < as.Date("2023-06-01") ~ "adjust",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(window)) %>%
  group_by(unit_id, window) %>%
  summarise(
    avg_d_ln_priced = mean(d_ln_dair, na.rm = TRUE),
    avg_d_ln_pricea = mean(d_ln_alf, na.rm = TRUE),
    avg_d_ln_dqty   = mean(d_dqty, na.rm = TRUE),
    avg_d_ln_aqty   = mean(d_aqty, na.rm = TRUE), 
    .groups = "drop"
  )

m_dprice <- feols(avg_d_ln_priced ~ i(window, ref = "control"), data = dft)
m_dqty   <- feols(avg_d_ln_dqty   ~ i(window, ref = "control"), data = dft)
m_aprice <- feols(avg_d_ln_pricea ~ i(window, ref = "control"), data = dft)
m_aqty   <- feols(avg_d_ln_aqty   ~ i(window, ref = "control"), data = dft)

##BOX PLOT ATT
p1 <- ggplot(dft, aes(x = window, y = avg_d_ln_priced)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Avg monthly Δ ln(price)", 
       title = "China imports: average monthly price change by period") +
  theme_minimal(base_size = 12)

p2 <- ggplot(dft, aes(x = window, y = avg_d_ln_dqty)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Avg monthly Δ ln(quantity)",
       title = "China imports: average monthly quantity change by period") +
  theme_minimal(base_size = 12)
print(p1)
print(p2)

p3 <- ggplot(dft, aes(x = window, y = avg_d_ln_pricea)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Avg monthly Δ ln(price)", 
       title = "China imports: average monthly price change by period") +
  theme_minimal(base_size = 12)

p4 <- ggplot(dft, aes(x = window, y = avg_d_ln_aqty)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Avg monthly Δ ln(quantity)",
       title = "China imports: average monthly quantity change by period") +
  theme_minimal(base_size = 12)

print(p3)
print(p4)


### optional) Large-country (ToT) test ===============
# Build US export unit values to China vs ROW for HS 1214 (alfalfa) and/or HS 04 (dairy)
# This assumes 'usitc' has: date or year/month, partner, hs, value_usd, qty_kg (or qty_ton)
to_cn_vs_row <- function(d, hs_regex){
  req <- c("partner","hs_code","year","month","export_value_usd","export_qty")
  stopifnot(all(req %in% names(d)))
  d %>%
    filter(str_detect(hs_code, hs_regex)) %>%
    mutate(date = make_date(as.integer(year), as.integer(month), 1),
           uv = export_value_usd / export_qty) %>%
    mutate(group = if_else(tolower(partner) %in% c("china","people's republic of china","prc"), "CN","ROW")) %>%
    group_by(date, group) %>%
    summarise(uv = sum(export_value_usd)/sum(export_qty), .groups="drop") %>%
    pivot_wider(names_from = group, values_from = uv) %>%
    filter(!is.na(CN), !is.na(ROW)) %>%
    mutate(log_uv_ratio = log(CN/ROW))
}

uv_alf <- tryCatch(to_cn_vs_row(usitc, "^1214"), error = function(e) NULL)
uv_dai <- tryCatch(to_cn_vs_row(usitc, "^04"),   error = function(e) NULL)
summary(uv_dai)



# text snippet using the cumulative effects we computed:
txt <- glue("
We estimate dynamic reduced-form equations with month and year fixed effects. 
The cumulative pass-through from alfalfa tariffs to feed costs over 0..{K} months is {sprintf('%.3f', cum_m1_taualf$est)} 
(s.e. {sprintf('%.3f', cum_m1_taualf$se)}; p = {sprintf('%.3f', cum_m1_taualf$p)}). 
For farm-gate milk, the long-run effect of dairy tariffs is {sprintf('%.3f', cum_m2_taudair$est)} 
(s.e. {sprintf('%.3f', cum_m2_taudair$se)}; p = {sprintf('%.3f', cum_m2_taudair$p)}), 
and the cost pass-through from feed is {sprintf('%.3f', cum_m2_feed$est)} 
(s.e. {sprintf('%.3f', cum_m2_feed$se)}; p = {sprintf('%.3f', cum_m2_feed$p)}).
")
cat(txt)

