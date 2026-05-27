# --- Packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(forcats)

# --- 0) Windows & helpers (edit dates if needed)
control_start <- as.Date("2017-05-01"); control_end <- as.Date("2018-05-01")
war_start     <- as.Date("2018-06-01"); war_end     <- as.Date("2020-02-01")
covid_start   <- as.Date("2020-03-01"); covid_end   <- as.Date("2022-03-01")
adj_start     <- as.Date("2022-06-01"); adj_end     <- as.Date("2023-06-01")
war2_start    <- as.Date("2023-08-01"); war2_adj    <- as.Date("2025-05-01")

window_levels <- c("control","trade_war","covid19","adjust","war2")
window_labels <- c("Control (2017-05–2018-05)",
                   "Trade War (2018-06–2020-02)",
                   "COVID-19 (2020-03–2022-02)",
                   "Adjustment (2022-06–2023-05)",
                   "2nd Trade War (2023-08–2025-05)")

add_window <- function(df){
  df %>%
    mutate(
      window = case_when(
        date.x >= as.Date("2017-05-01") & date.x < as.Date("2018-06-01") ~ "control",
        date.x >= as.Date("2018-06-01") & date.x < as.Date("2020-03-01") ~ "trade_war",
        date.x >= as.Date("2020-03-01") & date.x < as.Date("2022-03-01") ~ "covid19",
        date.x >= as.Date("2022-06-01") & date.x < as.Date("2023-06-01") ~ "adjust",
        date.x >= as.Date("2023-08-01") & date.x < as.Date("2025-06-01") ~ "war2",
        TRUE ~ NA_character_
      ),
      month_lab = factor(month(date.x, label = TRUE, abbr = TRUE), 
                         levels = month.abb)
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window,
                           levels = c("control","trade_war","covid19","adjust","war2"),
                           labels = c("Control (2017-05–2018-05)",
                                      "Trade War (2018-06–2020-02)",
                                      "COVID-19 (2020-03–2022-02)",
                                      "Adjustment (2022-06–2023-05)",
                                      "2nd Trade War (2023-08–2025-05)")))
}


# --- 1) Choose variables to analyze
vars <- c("d_ln_dair", "d_ln_alf", "d_dqty", "d_aqty", "d_milkp")

# --- 2) Build month-of-year control baselines (per unit & month-of-year)
base_long1 <- df_est %>%
  filter(date.x >= control_start & date.x <= control_end) %>%
  mutate(month_lab = factor(month(date.x, label = TRUE, abbr = TRUE), levels = month.abb)) %>%
  group_by(unit_id, month_lab) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(vars), names_to = "var", values_to = "base")

# --- 3) Compute TE (per unit-month) and ATE (mean across units)
# TE: value - (unit's control-period mean for same month-of-year)
df_eff1 <- df_est %>%
  add_window() %>%
  select(unit_id, window, month_lab, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "var", values_to = "value") %>%
  left_join(base_long, by = c("unit_id", "month_lab", "var")) %>%
  mutate(TE = value - base) %>%
  filter(!is.na(base))  # drop unit-months lacking control baseline

# Collapse to one TE per unit x window x month (averaging if multiple years in window)
df_te1 <- df_eff1 %>%
  group_by(unit_id, window, month_lab, var) %>%
  summarise(TE = mean(TE, na.rm = TRUE), .groups = "drop")

# ATE: mean TE across units (+ SE and 95% CI)
df_ate1 <- df_te1 %>%
  group_by(window, month_lab, var) %>%
  summarise(
    ATE = mean(TE, na.rm = TRUE),
    n   = sum(!is.na(TE)),
    SE  = sd(TE, na.rm = TRUE) / sqrt(n),
    CI_lo = ATE - 1.96*SE,
    CI_hi = ATE + 1.96*SE,
    .groups = "drop"
  )

# --- 4) Plot function: boxplots (TE) + ATE points with 95% CI
plot_TE_ATE1 <- function(var_name, y_lab){
  te_dat  <- df_te1  %>% dplyr::filter(var == var_name)
  ate_dat <- df_ate1 %>% dplyr::filter(var == var_name)
  
  ggplot() +
    # TE distribution (per unit): boxplot
    geom_boxplot(
      data = te_dat,
      aes(x = month_lab, y = TE),
      outlier.alpha = 0.3, width = 0.75
    ) +
    # Reference line
    geom_hline(yintercept = 0, linetype = "dashed") +
    # ATE line, CI bars, and points (explicit aes each time)
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
      title = paste0("Monthly TE (boxes) and ATE (dot±CI): ", var_name),
      subtitle = "TE = outcome − unit’s control-period mean for the same month-of-year"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(vjust = 0.9),
      strip.text = element_text(face = "bold")
    )
}



# --- 5) Make plots (pick the ones you need)
p_dair1 <- plot_TE_ATE1("d_ln_dair", "Δln(dairy price): TE / ATE")
p_alf1  <- plot_TE_ATE1("d_ln_alf",  "Δln(alfalfa price): TE / ATE")
p_dqty <- plot_TE_ATE("d_dqty",    "Δ(dairy quantity): TE / ATE")
p_aqty <- plot_TE_ATE("d_aqty",    "Δ(alfalfa qty): TE / ATE")
p_milk <- plot_TE_ATE("d_milkp",   "Δln(farm-gate milk price): TE / ATE")

print(p_dair1)
print(p_alf1)
# print(p_alf); print(p_dqty); print(p_aqty); print(p_milk)

# Optional: save
# ggsave("TE_ATE_dairy_price.png", p_dair, width = 11, height = 6.5, dpi = 300)


