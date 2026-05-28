# Chinese Twin Agricultural Tariffs and China's Dairy Supply

This repository contains the data and R code used for the paper **Chinese Twin Agricultural Tariffs Impact on Dairy Supply**. The project studies how China's retaliatory tariffs on an upstream feed input, U.S. alfalfa hay, and a downstream output, U.S. dairy products, jointly affected Chinese dairy imports, farm-gate milk prices, and welfare during the U.S.-China trade-war period.

The core idea is a **twin-tariff** mechanism: an upstream alfalfa tariff can raise feed costs for domestic dairy producers, while a downstream dairy tariff changes import competition and substitution toward alternative origins such as New Zealand and the EU. The code builds monthly trade and price series, estimates dynamic tariff pass-through models, checks robustness, and runs welfare/substitution counterfactuals.

## Repository Contents

```text
.
|-- 01_data_prep.R                         # Data merge, cleaning, log variables, lags, policy windows
|-- 02_main_regressions.R                  # Main fixed-effects and IV regression table
|-- 3_new_welfare_revised.R                # NZ/EU substitution-adjusted welfare analysis
|-- 04_robustness_revised.R                # Appendix robustness checks
|-- 05_figures.R                           # Main and appendix figures
|-- 06_ecm.R                               # Cointegration and error-correction model checks
|-- 07_sensitive_check.R                   # Leave-one-policy-month and sensitivity checks
|-- 08_revision_extra_checks.R             # Revision checks: HAC, AR, ITS, lag robustness
|-- dairy_trade_data.csv                   # Master monthly dataset
|-- exchange_rate_and_fuel_price.csv       # FX and fuel controls
|-- UN_comtrade_HS0401_0406.csv            # Raw/composite dairy import data by HS code
|-- *_NZ_EU_*.csv                          # Alternative-origin dairy import data
|-- HS*.csv                                # HS-level dairy/alfalfa trade extracts
|-- figure/                                # Manuscript figures
|-- output/                                # Main regression and welfare outputs
`-- outputs/                               # Robustness, revision, and welfare output folders
```

## Data

The empirical dataset is monthly and primarily covers **January 2005 to November 2025** in the local `dairy_trade_data.csv` file. Rows without valid `Year` and `Month` are dropped during preparation.

| File | Description |
| --- | --- |
| `dairy_trade_data.csv` | Master monthly series for U.S.-origin dairy quantities and values, alfalfa quantities and CIF unit values, tariffs, farm-gate milk prices, FAO dairy prices, and HS dairy aggregates. |
| `exchange_rate_and_fuel_price.csv` | CNY/USD exchange rates, USD/CNY conversion, international fuel price, and U.S. gasoline proxy variables. |
| `UN_comtrade_HS0401_0406.csv` | UN Comtrade import records for China dairy products, HS 0401-0406. |
| `dairy_imports_NZ_EU_monthly_by_HS0401_0406.csv` | Monthly dairy imports from New Zealand and EU origins by HS code. |
| `Dairy_imports_NZ_EU_monthly_total.csv` | Monthly alternative-origin dairy totals used in substitution and welfare calculations. |
| `HS_dairy_monthly_by_hs_2005_2026.csv` | HS-level monthly dairy quantity, value, and unit-value series. |
| `HS04_yearly.csv` | Annual HS 04 dairy summary. |

Key constructed variables in `01_data_prep.R`:

| Variable | Meaning |
| --- | --- |
| `ln_milkp`, `d_ln_milkp` | Log and first-difference log farm-gate milk price, CNY/kg. |
| `ln_dqty`, `d_ln_dqty` | Log and first-difference log U.S.-origin dairy import quantity. |
| `ln_aqty`, `d_ln_aqty` | Log and first-difference log U.S.-origin alfalfa import quantity. |
| `ln_alf`, `d_ln_alf` | Log and first-difference log alfalfa CIF unit value. |
| `ln_tra`, `d_ln_tra` | Log applied alfalfa tariff, `log(1 + tau_A)`, and monthly change. |
| `ln_trd`, `d_ln_trd` | Log applied dairy tariff, `log(1 + tau_D)`, and monthly change. |
| `d_ln_faop` | Monthly change in the FAO dairy price index. |
| `d_ln_fuel` | Monthly change in the fuel-price control. |
| `covid` | COVID disruption indicator. |
| `month_fe`, `year_fe` | Calendar fixed effects. |

## Policy Windows

The project uses four policy windows for descriptive tables, interrupted-time-series checks, and period-level interpretation:

| Window | Period | Interpretation |
| --- | --- | --- |
| Control | 2017-01 to 2018-06 | Pre-retaliation baseline. |
| First Trade War | 2018-07 to 2022-02 | Initial retaliatory tariff period and Phase One exclusion period. |
| Adjustment | 2022-03 to 2023-09 | Tariff exclusions, domestic adjustment, and origin diversification. |
| Second Trade War | 2023-09 to 2025-11 | Renewed escalation and adaptation period in the current data. |

## Empirical Strategy

The main empirical scripts estimate monthly growth-response models with two lags (`K = 2`) and fixed effects. The regression logic follows a block-recursive supply-chain interpretation.

1. **First stage / import adjustment**

   `02_main_regressions.R` estimates how alfalfa and dairy tariff changes affect U.S.-origin dairy import quantity:

   ```text
   Delta log(Q_D) ~ lags(Delta log(1 + tau_A)) + lags(Delta log(1 + tau_D))
                    + global dairy price + fuel + covid + month FE + year FE
   ```

2. **Reduced-form milk-price response**

   The reduced form estimates the total association between tariff changes and farm-gate milk-price growth.

3. **Alfalfa price channel**

   The alfalfa CIF unit value is included to test whether higher imported feed costs pass through into China's farm-gate milk price.

4. **Instrumental variables / 2SLS**

   Dairy import quantity is treated as endogenous and instrumented using alfalfa and dairy tariff changes. Alfalfa CIF price is included in the second stage to absorb the feed-cost channel, so the instrumented quantity coefficient isolates the import-competition pathway.

The scripts report heteroskedasticity-robust or Newey-West/HAC standard errors depending on the revision block.

## Robustness Checks

The robustness scripts implement reviewer and appendix checks, including:

| Script | Checks |
| --- | --- |
| `04_robustness_revised.R` | Placebo tariff timing, tariff leads, year-FE attenuation, FE comparisons, currency denomination checks, lag-length robustness, Newey-West coefficients, ITS period effects, and optional ECM block. |
| `06_ecm.R` | Zivot-Andrews unit-root tests, Engle-Granger residual ADF test, Johansen cointegration tests, two-step ECM, HAC standard errors, and long-run tariff elasticities. |
| `07_sensitive_check.R` | Leave-one-policy-month sensitivity and interaction sensitivity checks. |
| `08_revision_extra_checks.R` | HAC versions of the main equations, cumulative effects, joint Wald tests, Anderson-Rubin confidence sets, dairy-tariff-only IV variants, ITS summary statistics, lag-length checks, and welfare-sensitivity scaffolding. |

Generated robustness outputs are stored mainly in `outputs/robustness/` and `output/`.

## Welfare and Substitution Analysis

`3_new_welfare_revised.R` implements a partial-equilibrium welfare decomposition with alternative-origin substitution. The script:

1. Aggregates China dairy imports from the U.S., New Zealand, and EU-27.
2. Compares baseline and post-tariff windows.
3. Measures U.S.-origin dairy import decline.
4. Calculates the substitution share absorbed by New Zealand/EU suppliers.
5. Adjusts dairy deadweight loss for substitution and residual price premia.
6. Produces welfare tables and sensitivity grids.

This script uses `comtradr` if raw Comtrade data must be downloaded. To pull fresh data, set a Comtrade key in `.Renviron`:

```r
COMTRADE_PRIMARY= requested API
```

If cached CSVs are already present, the workflow can reuse them instead of querying the API.

## Figures and Outputs

Important generated outputs include:

| Path | Description |
| --- | --- |
| `figure/tariff rate.png` | Applied tariff rates for U.S. alfalfa and dairy. |
| `figure/Tariff revenue of both.png` | Estimated tariff revenue on U.S. alfalfa and dairy. |
| `output/IV_betaQ_table.csv` | IV estimates for the dairy-import quantity channel. |
| `output/cumulative_effects.csv` | Cumulative tariff effects and standard errors. |
| `output/joint_wald_tests.csv` | Joint Wald tests for lagged tariff effects. |
| `output/lag_length_robustness.csv` | Alternative lag-length estimates. |
| `output/sensitivity_substitution_share.csv` | Welfare sensitivity to substitution assumptions. |
| `output/welfare_table_2019_only_substitution_adjusted.csv` | Substitution-adjusted welfare table. |
| `outputs/robustness/*.csv` | Appendix robustness outputs. |

## How to Reproduce

Run the scripts from the repository root in R or RStudio.

```r
source("01_data_prep.R")
source("02_main_regressions.R")
source("04_robustness_revised.R")
source("05_figures.R")
source("06_ecm.R")
source("07_sensitive_check.R")
source("08_revision_extra_checks.R")
source("03_new_welfare_revised.R")
```

Recommended package set:

```r
install.packages(c(
  "here", "readr", "dplyr", "tidyr", "lubridate", "fixest",
  "modelsummary", "ggplot2", "stringr", "broom", "purrr",
  "tibble", "tidyverse", "comtradr", "janitor", "scales",
  "glue", "urca", "lmtest", "sandwich", "vars", "strucchange"
))
```

Notes:

- The scripts use `here::here()`, so open the project from the repository root before running.
- The current folder contains macOS sidecar files such as `.DS_Store` and `._*`; these are not required for reproduction and should be excluded from Git.
- `.RData` and `.Rhistory` are local session files and should generally not be committed.
- Some comments in older scripts refer to a `code/` folder. In the current submission bundle, the R scripts are located in the repository root.

## Research Summary

The analysis supports the paper's twin-tariff argument: upstream input tariffs and downstream product tariffs should not be evaluated as isolated shocks when the commodities are vertically linked. In the Chinese dairy system, alfalfa tariffs operate through feed-cost pass-through, while dairy tariffs operate more through import substitution and quantity reallocation. The welfare code evaluates whether the joint regime generates losses larger than the sum of single-tariff counterfactuals and whether upstream support policies can reduce those losses.
 
