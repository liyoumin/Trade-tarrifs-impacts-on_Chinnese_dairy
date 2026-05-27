# =============================================================================
# 05_figures.R  ·  Twin Tariff Paper — All Figures
# =============================================================================
# Produces every figure referenced in the manuscript.
# Saved to figure/ relative to the project root (AEPP_Submission/).
#
# Figure list:
#   fig:5   — Tariff rates 2005–2025          (figure/tariff rate.png)
#   fig:7   — Tariff revenue over time        (figure/Tariff revenue of both.png)
#   fig:ate — Period average treatment effects (figure/farmgate.png)
#   A-alf   — Event study: alfalfa price      (appendix/)
#   A-milk  — Event study: milk price         (appendix/)
#   A-sciss — Scissors effect index plot      (appendix/)
#   A-qty   — Import qty by period boxplots   (appendix/)
# =============================================================================

source(here::here("code", "01_data_prep.R"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(broom)
  library(fixest)
})

FIG_DIR <- here::here("figure")
APP_DIR <- here::here("appendix")
dir.create(FIG_DIR, showWarnings = FALSE)
dir.create(APP_DIR, showWarnings = FALSE)

# Convert fixest_panel to plain data frame: dplyr select/filter/mutate and
# ggplot2 pipelines cannot work with the fixest_panel [.fixest_panel method.
trade_plain <- as.data.frame(trade_df)

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_tt <- theme_minimal(base_size = 12) +
  theme(legend.position   = "top",
        panel.grid.minor  = element_blank(),
        plot.title        = element_text(face = "bold", size = 12),
        plot.subtitle     = element_text(size = 10, color = "grey40"))

save_fig <- function(p, filename, w = 8, h = 5) {
  path <- file.path(FIG_DIR, filename)
  ggsave(path, p, width = w, height = h, dpi = 300)
  message("Saved: ", path)
  invisible(p)
}

save_app <- function(p, filename, w = 10, h = 6) {
  path <- file.path(APP_DIR, filename)
  ggsave(path, p, width = w, height = h, dpi = 300)
  message("Saved: ", path)
  invisible(p)
}

# ─────────────────────────────────────────────────────────────────────────────
# fig:5  Tariff rates 2005–2025
# ─────────────────────────────────────────────────────────────────────────────
p_tariff <- trade_plain |>
  select(date, tariff_rate_on_alfalfa, tariff_rate_on_dairy) |>
  pivot_longer(-date, names_to = "commodity",
               values_to = "tariff_rate") |>
  mutate(commodity = recode(commodity,
                            tariff_rate_on_alfalfa = "Alfalfa (HS 1214)",
                            tariff_rate_on_dairy   = "Dairy (HS 04)")) |>
  filter(!is.na(tariff_rate)) |>
  ggplot(aes(x = date, y = tariff_rate, color = commodity)) +
  geom_step(linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2018-07-01"),
             linetype = "dashed", alpha = 0.6) +
  annotate("text", x = as.Date("2018-09-01"), y = 90,
           label = "Retaliatory tariffs\nJuly 2018", hjust = 0, size = 3.2) +
  scale_color_manual(values = c("Alfalfa (HS 1214)" = "#D95F02",
                                "Dairy (HS 04)"     = "#1B9E77")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 130)) +
  labs(title    = "Chinese Applied Tariff Rates on U.S. Alfalfa and Dairy",
       subtitle = "Monthly, 2005–2025  |  Vertical line: July 2018 retaliatory shock",
       x = NULL, y = "Applied tariff rate (%)", color = NULL) +
  theme_tt

save_fig(p_tariff, "tariff rate.png")

# ─────────────────────────────────────────────────────────────────────────────
# fig:7  Tariff revenue
# ─────────────────────────────────────────────────────────────────────────────
trade_df_rev <- trade_plain |>
  filter(!is.na(tariff_rate_on_alfalfa), !is.na(dairy_qty_ton)) |>
  mutate(
    # Revenue ≈ tariff rate × import value  (USD millions)
    rev_alf   = (tariff_rate_on_alfalfa / 100) *
                  alfalfa_price_usd_ton * alfalfa_qty / 1e6,
    rev_dairy = (tariff_rate_on_dairy   / 100) *
                  dairy_usd_per_kg * 1000 * dairy_qty_ton / 1e6
  ) |>
  select(date, rev_alf, rev_dairy) |>
  pivot_longer(-date, names_to = "market", values_to = "revenue_M") |>
  mutate(market = recode(market,
                         rev_alf   = "Alfalfa tariff revenue",
                         rev_dairy = "Dairy tariff revenue")) |>
  filter(!is.na(revenue_M))

p_rev <- ggplot(trade_df_rev, aes(x = date, y = revenue_M, fill = market)) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_vline(xintercept = as.Date("2018-07-01"), linetype = "dashed", alpha = 0.6) +
  scale_fill_manual(values = c("Alfalfa tariff revenue" = "#D95F02",
                               "Dairy tariff revenue"   = "#1B9E77")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title    = "China's Tariff Revenue on U.S. Dairy and Alfalfa",
       subtitle = "Monthly, 2005–2025  |  Stacked area (USD millions)",
       x = NULL, y = "Tariff revenue (USD million)", fill = NULL) +
  theme_tt

save_fig(p_rev, "Tariff revenue of both.png")

# ─────────────────────────────────────────────────────────────────────────────
# fig:ate  Average treatment effects on farm-gate milk price by policy period
# ─────────────────────────────────────────────────────────────────────────────
# Compute window-mean Δln(milk price) relative to control period mean
control_mean <- trade_plain |>
  filter(window == "Control (2017–2018)", !is.na(d_ln_milkp)) |>
  pull(d_ln_milkp) |> mean(na.rm = TRUE)

ate_df <- trade_plain |>
  filter(!is.na(window), !is.na(d_ln_milkp)) |>
  group_by(window) |>
  summarise(
    ATE  = mean(d_ln_milkp, na.rm = TRUE) - control_mean,
    SE   = sd(d_ln_milkp,   na.rm = TRUE) / sqrt(n()),
    n    = n(),
    .groups = "drop"
  ) |>
  mutate(
    CI_lo = ATE - 1.96 * SE,
    CI_hi = ATE + 1.96 * SE,
    # Control period ATE = 0 by construction
    ATE   = ifelse(window == "Control (2017–2018)", 0, ATE),
    CI_lo = ifelse(window == "Control (2017–2018)", 0, CI_lo),
    CI_hi = ifelse(window == "Control (2017–2018)", 0, CI_hi)
  )

p_ate <- ggplot(ate_df, aes(x = window, y = ATE)) +
  geom_col(fill = "#1B9E77", alpha = 0.75, width = 0.6) +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi),
                width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title    = "Average Treatment Effects on China's Farm-Gate Milk Price",
       subtitle = "Relative to Control period (2017–2018)  |  95% CI",
       x = NULL, y = "ATE (Δlog milk price relative to control)") +
  theme_tt +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

save_fig(p_ate, "farmgate.png")

# ─────────────────────────────────────────────────────────────────────────────
# Appendix: Scissors effect — indexed alfalfa CIF price vs. farm-gate milk price
# ─────────────────────────────────────────────────────────────────────────────
base_mean <- trade_plain |>
  filter(date < as.Date("2018-01-01")) |>
  summarise(base_alf  = mean(alfalfa_price_usd_ton, na.rm = TRUE),
            base_milk = mean(farmgate_usd_kg * 1000, na.rm = TRUE))

index_df <- trade_plain |>
  filter(!is.na(alfalfa_price_cny_kg), !is.na(farmgate_cny_kg)) |>
  mutate(
    idx_alf  = 100 * alfalfa_price_usd_ton  / base_mean$base_alf,
    idx_milk = 100 * (farmgate_usd_kg*1000) / base_mean$base_milk
  )

p_scissors <- ggplot(index_df, aes(x = date)) +
  geom_line(aes(y = idx_alf,  color = "Alfalfa CIF price"),
            linewidth = 0.8) +
  geom_line(aes(y = idx_milk, color = "Farm-gate milk price"),
            linewidth = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2018-07-01"),
             linetype = "dashed", alpha = 0.5) +
  annotate("text", x = as.Date("2018-10-01"), y = 200,
           label = "Tariff shock", hjust = 0, size = 3.2) +
  scale_color_manual(values = c("Alfalfa CIF price"      = "#D95F02",
                                "Farm-gate milk price"   = "#1B9E77")) +
  labs(title    = "The Scissors Effect: Input Cost vs. Output Price",
       subtitle = "Both indexed to 100 at pre-tariff average (2005–2017)",
       x = NULL, y = "Index (pre-tariff = 100)", color = NULL) +
  theme_tt

save_app(p_scissors, "Scissors.png")

# ─────────────────────────────────────────────────────────────────────────────
# Appendix: Event study around July 2018 tariff shock
# ─────────────────────────────────────────────────────────────────────────────
shock_date  <- as.Date("2018-07-01")
df_event <- trade_plain |>
  mutate(
    event_month   = as.integer(interval(shock_date, date) / months(1)),
    event_month_f = factor(event_month)
  ) |>
  filter(event_month >= -18, event_month <= 36)

run_event_study <- function(outcome_var, data) {
  fml <- as.formula(
    paste0(outcome_var,
           " ~ i(event_month_f, ref = '-1') + i(month_fe) + i(year_fe)")
  )
  feols(fml, data = data,
        panel.id = ~ unit_id + time_idx,
        vcov     = "hetero")
}

es_milk   <- run_event_study("d_ln_milkp", df_event)
es_alf    <- run_event_study("d_ln_alf",   df_event)


plot_event_study <- function(model, title_text) {
  tidy(model, conf.int = TRUE) |>
    filter(str_detect(term, "event_month_f::")) |>
    mutate(
      t     = as.integer(str_replace(term, "event_month_f::", "")),
      sig   = p.value < 0.10
    ) |>
    ggplot(aes(x = t, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, color = "firebrick", linetype = "dashed",
               alpha = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    geom_line(linewidth = 0.8) +
    geom_point(aes(fill = sig), shape = 21, size = 2) +
    scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"),
                      guide  = "none") +
    annotate("text", x = 1.5, y = Inf, vjust = 1.5,
             label = "← Tariff shock (month 0)", hjust = 0,
             size = 3, color = "firebrick") +
    labs(title    = title_text,
         subtitle = "Reference period = month −1  |  95% CI shaded",
         x        = "Months relative to July 2018 tariff shock",
         y        = "Coefficient (Δlog)") +
    theme_tt
}

p_es_milk <- plot_event_study(es_milk, "Event Study: Farm-Gate Milk Price")
p_es_alf  <- plot_event_study(es_alf,  "Event Study: Alfalfa CIF Import Price")

save_app(p_es_milk, "Milk_event.png",      w = 10, h = 5)
save_app(p_es_alf,  "alf_trend log.png",   w = 10, h = 5)

cat("\nAll figures saved.\n")

