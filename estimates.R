### Estimation of China improt from U.S. monthly dada
### library
library(readxl); library(purrr); library(dplyr); library(janitor)
library(readr)   # for parse_number
library(ggplot2); library(lubridate); library(fixest) ; library(zoo)
library(tidyr); library(stringr); library(tidyverse); library(fixest)
library(modelsummary); library(glue); library(did); library(forcats)
setwd("/Users/macpro/Desktop/Youmin-phd/research/Dairy and Alfalfa/China Figure")

###load
path <- ("fao-dairy-price-indices-aug.xlsx")
cn_m <- read.csv("cn_m.csv")
summary(cn_m)
alfalfa_raw <- read_csv("alfalfa_hay_m.csv", skip = 4) %>%
  clean_names()

## alfalfa_row data process
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

### merge with alfalfa
clean_df <- cn_m %>%
  mutate(date = mdy(date)) %>%
  left_join(alfalfa_long, by = c("year","month"))
summary(clean_df)

###describ of tariff rate 
events <- tribble(
  ~start,        ~end,          ~label,
  as.Date("2018-01-01"), as.Date("2018-12-31"), "2018",
  as.Date("2020-01-01"), as.Date("2020-12-31"), "2020",
  as.Date("2022-01-01"), as.Date("2022-12-31"), "2022",
  as.Date("2024-01-01"), as.Date("2024-12-31"), "2024"
)
ggplot(clean_df, aes(x = date.x)) +
  # vertical "shadow" bands
  geom_rect(
    data = events,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey50", alpha = 0.12
  ) +
  # lines
  geom_line(aes(y = tariff_rate_on_alfalfa, color = "Alfalfa tariff")) +
  geom_line(aes(y = tariff_rate_on_dairy,   color = "Dairy tariff"), size = 0.5) +
  # optional dashed boundaries at the year starts
  geom_vline(
    xintercept = as.Date(c("2018-01-01","2020-01-01","2022-01-01","2024-01-01")),
    linetype = "dashed", linewidth = 0.3
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(values = c("Alfalfa tariff" = "#1B9E77", "Dairy tariff" = "#D95F02")) +
  labs(
    title = "Tariff rate on alfalfa and dairy (2010–2025)",
    x = "Year",
    y = "Tariff rate (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

## describe of import qty and value
scale_factor <- diff(range(clean_df$qty_ton, na.rm = TRUE)) / diff(range(clean_df$alfalfa_price_usd_ton, na.rm = TRUE))
ggplot(clean_df, aes(x = date.x)) +
  geom_rect(
    data = events,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey50", alpha = 0.12
  ) +
  geom_line(aes(y = qty_ton, linetype = "Quantity (MT)")) +
  geom_line(aes(y = alfalfa_price_usd_ton * scale_factor,
                linetype = "Unit value (USD/MT)"),
            linewidth = 0.8) +
  geom_vline(
    xintercept = as.Date(c("2018-01-01","2020-01-01","2022-01-01","2024-01-01")),
    linetype = "dashed", linewidth = 0.3
  ) +
  labs(
    title = "China imports of U.S. alfalfa (Quantity & Unit Value, 2005–2025)",
    x = "Year",
    y = "Alfalfa quantity (MT)",
    linetype = NULL
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Alfalfa unit value (USD/MT)")
  ) +
  scale_linetype_manual(values = c("Quantity (MT)" = "solid",
                                   "Unit value (USD/MT)" = "dashed")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",       
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

dairyp <- clean_df$dairy_usd_per_kg*1000
scale_factor2 <- max(clean_df$dairy_qty_ton, na.rm = TRUE) / max(clean_df$dairy_usd_per_kg, na.rm = TRUE)
ggplot(clean_df, aes(x = date.x)) +
  geom_rect(
    data = events,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey50", alpha = 0.12
  ) +
  geom_line(aes(y = dairy_qty_ton, linetype = "Quantity (MT)")) +
  geom_line(aes(y = dairy_usd_per_kg * scale_factor2,
                linetype = "Unit value (USD/KG)"),
            linewidth = 0.8) +
  geom_vline(
    xintercept = as.Date(c("2018-01-01","2020-01-01","2022-01-01","2024-01-01")),
    linetype = "dashed", linewidth = 0.3
  ) +
  labs(
    title = "China imports of U.S. dairy (Quantity & Unit Value, 2005–2025)",
    x = "Year",
    y = "Dairy quantity (MT)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",       
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )


### tariff revenue calculation
clean_df <- clean_df %>%
  mutate(
    TRd   = (tariff_rate_on_dairy/100) * dairy_value_usd,
    TRa = (tariff_rate_on_alfalfa/100) * value_usd_thousand
  )

ggplot(clean_df, aes(x = date.x)) +
  geom_rect(
    data = events,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey50", alpha = 0.12
  ) +
  geom_line(aes(y = TRa, linetype = "Alfalfa tariff revenue")) +
  geom_line(aes(y = TRd, linetype = "Dairy tariff revenue"), linewidth = 0.8) +
  geom_vline(
    xintercept = as.Date(c("2018-01-01","2020-01-01","2022-01-01","2024-01-01")),
    linetype = "dashed", linewidth = 0.3
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title = "Tariff revenue on U.S. alfalfa and dairy (2005–2025)",
    x = "Year",
    y = "Thousand USD",
    linetype = NULL
  ) +
  scale_linetype_manual(
    values = c("Alfalfa tariff revenue" = "solid",
               "Dairy tariff revenue"   = "dashed")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",       
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

###co-variance
vars <- clean_df[, c("alfalfa_price_usd_ton", "dairy_usd_per_kg", "tariff_rate_on_alfalfa",
                     "tariff_rate_on_dairy", "dairy_qty_ton", "qty_ton", "TRd", "TRa")]
cov_matrix <- cov(vars, use = "complete.obs")
cor_matrix <- cor(vars, use = "complete.obs")
cov_matrix
cor_matrix

ancova_model <- aov(dairy_usd_per_kg ~ factor(tariff_rate_on_alfalfa) + alfalfa_price_usd_ton, data = clean_df)
summary(ancova_model)



### estimation-------------------------------------------------------------------------------------------------------
### data frame
clean_df <- clean_df %>%
  mutate(
    dairy_usd_per_ton = dairy_usd_per_kg * 1000
  )

clean_df <- clean_df %>%
  arrange(date.x) %>%
  mutate(
    ln_feed = ifelse(feed_price_usd_ton + 1 > 0, log(feed_price_usd_ton), NA_real_),
    ln_alf =  log(replace_na(alfalfa_price_usd_ton, 0) + 1),
    ln_dair = ifelse(dairy_usd_per_ton > 0, log(dairy_usd_per_kg), NA_real_),
    ln_TRa = ifelse(TRa + 1 > 0, log(TRa+1), NA_real_),
    ln_TRd = ifelse(TRd + 1 > 0, log(TRd+1), NA_real_),
    ln_dqty = ifelse(dairy_qty_ton + 1 > 0, log(dairy_qty_ton+1), NA_real_),
    ln_aqty = ifelse(qty_ton+1 > 0, log(qty_ton + 1), NA_real_), 
    ln_milkp = log(replace_na(farmgate_cny_kg, 0) + 1),
    ln_faop = ifelse(FAO_dairry_price_index > 0, log(FAO_dairry_price_index), NA_real_),
    ln_tra = log(tariff_rate_on_alfalfa+1),
    ln_trd = log(tariff_rate_on_dairy+1),
    ###first difference
    d_ln_feed = ln_feed - lag(ln_feed),
    d_ln_alf = ln_alf - lag(ln_alf),
    d_ln_dair = ln_dair - lag(ln_dair),
    d_tra = tariff_rate_on_alfalfa - lag(tariff_rate_on_alfalfa),
    d_ln_tra = ln_tra -lag(ln_tra),
    d_ln_trd = ln_trd - lag(ln_trd), 
    d_trd = tariff_rate_on_dairy - lag(tariff_rate_on_dairy),
    d_ln_TRa = ln_TRa - lag(ln_TRa),
    d_ln_TRd = ln_TRd - lag(ln_TRd),
    d_ln_aqty = ln_aqty - lag(ln_aqty),
    d_ln_dqty = ln_dqty - lag(ln_dqty),
    d_ln_milkp = ln_milkp, - lag(ln_milkp),
    d_ln_faop = ln_faop - lag(ln_faop),
    d_milkp = farmgate_cny_kg - lag(farmgate_cny_kg),
    d_milkp_percent = (farmgate_cny_kg - lag(farmgate_cny_kg))/(farmgate_cny_kg),
    month_fe  = factor(month), year_fe   = factor(year)
  )
K <-2

### panel data id
df_est <- clean_df %>%
  arrange(date.x) %>%
  mutate(
    unit_id  = 1L,                 # single time-series unit (China)
    time_idx = row_number()       # numeric time index
  ) %>%
  distinct(unit_id, date.x, .keep_all = TRUE)


## 1) absolute effects
mm0 <- feols(dairy_qty_ton ~ f(tariff_rate_on_dairy, 0:K) + f(dairy_usd_per_kg,0:K) +
               i(month_fe, "6") + i(year_fe),
             data = df_est,
             panel.id = ~unit_id + time_idx
)
md0 <- feols(qty_ton ~ f(tariff_rate_on_alfalfa, 0:K) + f(alfalfa_price_usd_ton,0:K) +
               i(month_fe, "6") + i(year_fe),
             data = df_est,
             panel.id = ~unit_id + time_idx
)
###IV
iv_mm0 <- feols(
  farmgate_cny_kg ~ f(tariff_rate_on_dairy, 0:K) + f(dairy_usd_per_kg,0:K) + i(month_fe, "6") + i(year_fe) |
    f(qty_ton) ~ f(tariff_rate_on_alfalfa, 0:K) + f(alfalfa_price_usd_ton,0:K),
  data = df_est,
  panel.id = ~unit_id + time_idx
)

summary(iv_mm0, se = "hetero")
etable(mm0, md0, iv_mm0, se = "hetero", depvar = TRUE)


rf_mm0 <- feols(
  farmgate_cny_kg ~ f(tariff_rate_on_alfalfa, 0:K) + f(tariff_rate_on_dairy, 0:K) + f(alfalfa_price_usd_ton,0:K) +  f(dairy_usd_per_kg,0:K) +
    i(month_fe, "6") + i(year_fe),
  data = df_est,
  panel.id = ~unit_id + time_idx
)
summary(rf_mm0, se = "hetero")


### (A) Pass-through: alfalfa tariff and price
ma <- feols(ln_aqty ~ f(ln_tra,0:K) + f(ln_alf,0:K) +       
              i(month_fe, "6") + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
)
md <- feols(ln_dqty ~ f(ln_trd,0:K)+ f(ln_dair,0:K)+           
              i(month_fe, "6") + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
)
mm <- feols(ln_milkp ~ f(ln_trd,0:K)+ f(ln_tra,0:K)+ f(ln_dair,0:K)+ f(ln_alf,0:K)+    
              i(month_fe, "6") + i(year_fe),
            data = df_est,
            panel.id = ~ unit_id + time_idx
)
summary(mm)

###IV
iv_mm <- feols(
  ln_milkp ~ ln_aqty +  f(ln_tra, 0:K) + i(month_fe, "6") + i(year_fe) |
    ln_dqty ~ f(ln_trd, 0:K) + f(ln_dair,0:K),
  data = df_est,
  panel.id = ~unit_id + time_idx
)

summary(iv_mm, se = "hetero")
etable(ma, md, iv_mm, se = "hetero", depvar = TRUE)
### reduce formation
rf_mm <- feols(
  ln_milkp ~ f(ln_tra, 0:2) + f(ln_trd, 0:2) +
    i(month_fe, "6") + i(year_fe),
  data = df_est,
  panel.id = ~unit_id + time_idx
)

rf_mm1 <- feols(
  d_milkp_per ~ f(d_ln_tra, 0:2) + f(d_ln_trd, 0:2) +
    i(month_fe, "6") + i(year_fe),
  data = df_est,
  panel.id = ~unit_id + time_idx
)

# delta_log model
ma1 <- feols(d_ln_dqty ~ f(d_ln_trd,0:K) + f(d_ln_dair, 0:K) +  f(d_ln_tra,0:K) +
               i(month_fe) + i(year_fe),
             data = df_est,
             panel.id = ~ unit_id + time_idx
)

md1 <- feols(d_ln_aqty ~ f(d_ln_tra,0:K) + f(d_ln_alf,0:K) +
               i(month_fe) + i(year_fe),
             data = df_est,
             panel.id = ~ unit_id + time_idx
)

mm1 <- feols(d_ln_milkp ~ f(d_ln_trd,0:K)+ f(d_ln_tra,0:K) +  f(d_ln_feed,0:K) + f(d_ln_dqty,0:K)+ f(d_ln_alf,0:K)+
               i(month_fe, "6") + i(year_fe),
             data = df_est,
             panel.id = ~ unit_id + time_idx
)
summary(mm1)


###delta_IV
iv_mm1 <- feols(
  f(d_ln_milkp) ~  f(d_ln_aqty,0:K) + f(d_ln_alf, 0:K)+ i(month_fe) + i(year_fe) |
    f(d_ln_dqty) ~
    f(d_ln_trd, 0:K) + f(d_ln_tra, 0:K) + f(d_ln_dair,0:K), 
  data = df_est,
  panel.id = ~ unit_id + time_idx
)

summary(iv_mm1, se = "hetero")
etable(iv_mm1, stage = 1)
etable(ma1, md1, iv_mm1, se = "hetero", depvar = TRUE)


# Extract coefficients
b_price_on_aqty  <- coef(iv_mm1)["fit_d_ln_aqty"]
b_price_on_dqty  <- coef(iv_mm1)["fit_d_ln_dqty"]

# First-stage alfalfa qty on tariffs
fs_a <- tidy(ma1) %>% filter(grepl("f\\(d_ln_alf", term))
# First-stage dairy qty on tariffs
fs_d <- tidy(md1) %>% filter(grepl("f\\(d_ln_dair", term))

# Compute marginal effect by lag
fs_a <- fs_a %>% mutate(effect_on_milk = estimate * b_price_on_aqty)
fs_d <- fs_d %>% mutate(effect_on_milk = estimate * b_price_on_dqty)

bind_rows(fs_a %>% mutate(tariff="Alfalfa tariff"),
          fs_d %>% mutate(tariff="Dairy tariff")) %>%
  select(tariff, term, estimate, effect_on_milk)


## sensitive test
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

# focus on contemporaneous d_ln_trd (lag 0)
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



### treatment effects - fixed effects model--------------------------------------------------------------------
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


###ATE
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

# ATE: mean TE across units (+ SE and 95% CI)
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

