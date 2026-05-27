# =============================================================================
# 07_appendix_B_checks.R
# Appendix B checks:
#   Item 7  — Interaction sensitivity / leave-one-policy-month jackknife
#   Item 13 — Block-recursion check
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(fixest)
  library(tibble)
})

# ── Source prepared data ──────────────────────────────────────────────────────
prep_file <- if (file.exists(here("code", "01_data_prep.R"))) {
  here("code", "01_data_prep.R")
} else {
  here("01_data_prep.R")
}

# Convert fixest panel back to regular data frame for easier filtering
df_app <- as.data.frame(trade_df)

# Output folder
out_dir <- here("appendix_outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ── Helper functions ──────────────────────────────────────────────────────────

get_coef_row <- function(model, term, spec_name, dropped_month = NA_character_) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  
  se_col <- grep("Std", names(ct), value = TRUE)[1]
  p_col  <- grep("^Pr", names(ct), value = TRUE)[1]
  
  row <- ct[ct$term == term, ]
  
  if (nrow(row) == 0) {
    return(tibble(
      specification = spec_name,
      dropped_month = dropped_month,
      term = term,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      nobs = nobs(model)
    ))
  }
  
  tibble(
    specification = spec_name,
    dropped_month = dropped_month,
    term = term,
    estimate = row$Estimate,
    std_error = row[[se_col]],
    p_value = row[[p_col]],
    nobs = nobs(model)
  )
}

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}

write_simple_latex <- function(df, caption, label, file) {
  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Specification & Coefficient & Std. error & $p$-value & Sign preserved \\\\",
    "\\midrule"
  )
  
  body <- apply(df, 1, function(x) {
    paste0(
      x[["Specification"]], " & ",
      x[["Coefficient"]], " & ",
      x[["Std. error"]], " & ",
      x[["p-value"]], " & ",
      x[["Sign preserved"]], " \\\\"
    )
  })
  
  lines <- c(
    lines,
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{flushleft}",
    "\\footnotesize Notes: Heteroskedasticity-robust standard errors are used. Significance levels: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
    "\\end{flushleft}",
    "\\end{table}"
  )
  
  writeLines(lines, file)
}

# =============================================================================
# Item 7. Interaction sensitivity: leave-one-policy-month jackknife
# =============================================================================

# Re-create contemporaneous interaction term
df_app <- df_app |>
  mutate(interact_tra_trd = d_ln_tra * d_ln_trd)

# Identify months where both tariffs changed simultaneously
policy_months <- df_app |>
  filter(
    date >= as.Date("2018-01-01"),
    !is.na(interact_tra_trd),
    interact_tra_trd != 0
  ) |>
  distinct(date) |>
  arrange(date)

print(policy_months)

# Baseline interaction model:
# Milk price on alfalfa tariff lags, dairy tariff lags, and contemporaneous interaction
fit_interaction_model <- function(dat) {
  feols(
    d_ln_milkp ~
      f(d_ln_tra, 0:K) +
      f(d_ln_trd, 0:K) +
      interact_tra_trd +
      d_ln_faop + d_ln_fuel + covid +
      i(month_fe) + i(year_fe),
    data     = dat,
    panel.id = ~ unit_id + time_idx,
    vcov     = "hetero"
  )
}

m_interact_base <- fit_interaction_model(df_app)

interaction_base <- get_coef_row(
  model = m_interact_base,
  term = "interact_tra_trd",
  spec_name = "Baseline",
  dropped_month = NA_character_
)

# Leave-one-policy-month jackknife
policy_dates <- as.Date(policy_months$date)

interaction_loo <- bind_rows(lapply(seq_along(policy_dates), function(i) {
  d <- policy_dates[i]
  
  dat_i <- df_app |>
    filter(date != d)
  
  m_i <- fit_interaction_model(dat_i)
  
  get_coef_row(
    model = m_i,
    term = "interact_tra_trd",
    spec_name = paste0("Drop ", format(d, "%Y-%m")),
    dropped_month = as.character(d)
  )
}))

interaction_results <- bind_rows(interaction_base, interaction_loo) |>
  mutate(
    baseline_sign = sign(estimate[specification == "Baseline"][1]),
    sign_preserved = case_when(
      is.na(estimate) ~ NA_character_,
      specification == "Baseline" ~ "Yes",
      sign(estimate) == baseline_sign ~ "Yes",
      TRUE ~ "No"
    ),
    estimate_star = paste0(fmt_num(estimate, 3), stars(p_value))
  ) |>
  select(
    specification, dropped_month, term,
    estimate, std_error, p_value, estimate_star,
    sign_preserved, nobs
  )

print(interaction_results)

# Export CSV
write_csv(
  interaction_results,
  file.path(out_dir, "table_interaction_jackknife.csv")
)

# Export LaTeX table
interaction_latex <- interaction_results |>
  transmute(
    Specification = specification,
    Coefficient = estimate_star,
    `Std. error` = fmt_num(std_error, 3),
    `p-value` = fmt_num(p_value, 3),
    `Sign preserved` = sign_preserved
  )

write_simple_latex(
  df = interaction_latex,
  caption = "Leave-one-policy-month sensitivity check for the tariff interaction",
  label = "tab:interaction_jackknife",
  file = file.path(out_dir, "table_interaction_jackknife.tex")
)

# =============================================================================
# Item 13. Block-recursion check
# =============================================================================

# Theory implication:
# If the model is block-recursive, dairy tariffs should not strongly predict
# alfalfa import quantities after controlling for alfalfa tariffs and controls.

m_block <- feols(
  d_ln_aqty ~
    f(d_ln_tra, 0:K) +
    f(d_ln_trd, 0:K) +
    d_ln_faop + d_ln_fuel + covid +
    i(month_fe) + i(year_fe),
  data     = df_app,
  panel.id = ~ unit_id + time_idx,
  vcov     = "hetero"
)

summary(m_block, vcov = "hetero")

# Extract tariff coefficients
block_terms <- c(
  paste0("f(d_ln_tra, ", 0:K, ")"),
  paste0("f(d_ln_trd, ", 0:K, ")")
)

block_labels <- c(
  paste0("$\\Delta \\ln(1+\\tau^A_{t-", 0:K, "})$"),
  paste0("$\\Delta \\ln(1+\\tau^D_{t-", 0:K, "})$")
)

block_results <- bind_rows(lapply(seq_along(block_terms), function(i) {
  get_coef_row(
    model = m_block,
    term = block_terms[i],
    spec_name = block_labels[i],
    dropped_month = NA_character_
  )
})) |>
  mutate(
    estimate_star = paste0(fmt_num(estimate, 3), stars(p_value))
  ) |>
  select(
    variable = specification,
    term,
    estimate,
    std_error,
    p_value,
    estimate_star,
    nobs
  )

print(block_results)

# Joint Wald test: dairy tariff coefficients jointly equal zero
# This directly tests whether dairy tariffs predict alfalfa imports.
dairy_wald <- tryCatch(
  {
    wald(m_block, keep = "d_ln_trd", vcov = "hetero")
  },
  error = function(e) {
    message("Wald test failed: ", conditionMessage(e))
    NULL
  }
)

print(dairy_wald)

# Export CSV
write_csv(
  block_results,
  file.path(out_dir, "table_block_recursion.csv")
)

# Export LaTeX table for block-recursion check
block_latex_lines <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Block-recursion check: alfalfa imports and dairy tariffs}",
  "\\label{tab:block_recursion}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Variable & Coefficient & Std. error & $p$-value \\\\",
  "\\midrule"
)

block_body <- block_results |>
  mutate(
    coef_fmt = estimate_star,
    se_fmt = fmt_num(std_error, 3),
    p_fmt = fmt_num(p_value, 3)
  ) |>
  transmute(
    line = paste0(variable, " & ", coef_fmt, " & ", se_fmt, " & ", p_fmt, " \\\\")
  ) |>
  pull(line)

block_latex_lines <- c(
  block_latex_lines,
  block_body,
  "\\midrule",
  paste0("Month fixed effects & Yes & & \\\\"),
  paste0("Year fixed effects & Yes & & \\\\"),
  paste0("Controls & Yes & & \\\\"),
  paste0("Observations & ", nobs(m_block), " & & \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{flushleft}",
  "\\footnotesize Notes: The dependent variable is $\\Delta \\ln Q_{A,t}$, the monthly log change in China's alfalfa import quantity from the United States. The block-recursion assumption predicts that dairy-tariff coefficients should be small and statistically insignificant. Heteroskedasticity-robust standard errors are used. Significance levels: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
  "\\end{flushleft}",
  "\\end{table}"
)

writeLines(
  block_latex_lines,
  file.path(out_dir, "table_block_recursion.tex")
)

# Also save full model table by fixest etable
etable(
  m_block,
  vcov = "hetero",
  keep = c("%d_ln_tra", "%d_ln_trd"),
  title = "Block-recursion check: alfalfa imports and dairy tariffs",
  tex = TRUE,
  file = file.path(out_dir, "etable_block_recursion.tex"),
  replace = TRUE
)

# =============================================================================
# Console summary
# =============================================================================

cat("\nAppendix B outputs saved to:\n")
cat(out_dir, "\n\n")

cat("Item 7 outputs:\n")
cat("  table_interaction_jackknife.csv\n")
cat("  table_interaction_jackknife.tex\n\n")

cat("Item 13 outputs:\n")
cat("  table_block_recursion.csv\n")
cat("  table_block_recursion.tex\n")
cat("  etable_block_recursion.tex\n")
