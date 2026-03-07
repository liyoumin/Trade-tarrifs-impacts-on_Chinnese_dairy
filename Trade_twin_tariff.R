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

##### variable creat
trade_df <- df %>%
  arrange(date) %>%
  mutate(
    ln_feed = ifelse(feed_price_usd_ton + 1 > 0, log(feed_price_usd_ton), NA_real_),
    ln_alf =  log(replace_na(alfalfa_price_usd_ton, 0) + 1),
    ln_dair = ifelse(dairy_usd_per_kg > 0, log(dairy_usd_per_kg), NA_real_),
    ln_dqty = ifelse(dairy_qty_ton + 1 > 0, log(dairy_qty_ton+1), NA_real_),
    ln_aqty = ifelse(alfalfa_qty+1 > 0, log(alfalfa_qty + 1), NA_real_), 
    ln_milkp = log(replace_na(farmgate_cny_kg, 0) + 1),
    ln_faop = ifelse(FAO_dairry_price_index > 0, log(FAO_dairry_price_index), NA_real_),
    ln_tra = log(tariff_rate_on_alfalfa+1),
    ln_trd = log(tariff_rate_on_dairy+1),
    ln_whey = log(whey_price),
    ln_whey_q = log(whey_qty),
    ###first difference
    d_ln_feed = ln_feed - lag(ln_feed),
    d_ln_alf = ln_alf - lag(ln_alf),
    d_ln_dair = ln_dair - lag(ln_dair),
    d_tra = tariff_rate_on_alfalfa - lag(tariff_rate_on_alfalfa),
    d_tra = tariff_rate_on_dairy - lag(tariff_rate_on_dairy),
    d_ln_tra = ln_tra -lag(ln_tra),
    d_ln_trd = ln_trd - lag(ln_trd), 
    d_ln_aqty = ln_aqty - lag(ln_aqty),
    d_ln_dqty = ln_dqty - lag(ln_dqty),
    d_ln_wheyp = ln_whey -lag(ln_whey),
    d_ln_wheyq = ln_whey - lag(ln_whey_q),
    d_ln_milkp = ln_milkp, - lag(ln_milkp),
    d_ln_faop = ln_faop - lag(ln_faop),
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


#### fixed effects model
m1 <- feols(d_ln_wheyp ~ f(ln_tra,0:K) + f(ln_aqty, 0:K) + f(ln_trd,0:K) + f(ln_dqty,0:K) + f(ln_whey_q, 0:K) +
              i(month_fe, "6") + i(year_fe),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
)
summ_m1 <- summary(m1, vcov = NW(4))
summ_m1
## whey import qty
m2 <- feols(d_ln_wheyq ~ f(d_ln_alf, 0:K) + f(d_ln_trd, 0:K)  + f(d_ln_tra, 0:K)  + 
              i(month_fe, "6") + i(year_fe),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
)
summ_m2 <- summary(m2, vcov = NW(4))
summ_m2

# IV: instrument feed dynamics with alfalfa tariff/world price
# import qty
m_3 <- feols(d_ln_dqty ~ f(d_ln_trd, 0:K) + f(d_ln_dair, 0:K) + f(d_ln_tra,0:K) 
             + f(d_ln_dair, 0:K) +
              i(month_fe, ref = "6") + i(year_fe),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
) 
summ_m3 <- summary(m_3, vcov = NW(4))
summ_m3

###2sls
trade_df <- panel(trade_df, panel.id = ~ unit_id + time_idx)

m_iv <- feols(
  d_ln_milkp ~
    f(d_ln_alf, 0:K) +
    f(d_ln_wheyq,0:K) +
    f(d_ln_aqty,0:K) +
    i(month_fe, ref="6") +
    i(year_fe,  ref="2018") |
    f(d_ln_dqty) ~
    f(d_ln_tra,  0:K) +
    f(d_ln_trd,  0:K) +
    f(d_ln_dair, 0:K) ,
  data = trade_df
)

summary(m_iv)
summary(m_iv, stage = 1)

m4 <- feols(d_ln_milkp ~ f(d_ln_trd,0:K) + f(d_ln_tra,0:K) + 
            + f(d_ln_aqty,0:K) + f(d_ln_dqty,0:K) +
              i(month_fe, ref = "6") + i(year_fe, ref = "2018"),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
) 
summ_m4 <- summary(m4, vcov = NW(4))
summ_m4

m5 <- feols(d_ln_alf ~ f(d_ln_trd,0:K) + f(d_ln_tra,0:K) + 
              i(month_fe, ref = "6") + i(year_fe, ref = "2018"),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
) 
summ_m5 <- summary(m5, vcov = NW(4))
summ_m5

m6 <- feols(d_ln_milkp ~ f(d_ln_alf, 0:K) +
              f(d_ln_wheyq,0:K) +
              f(d_ln_aqty,0:K) + f(d_ln_dqty,0:K) +
              i(month_fe, ref = "6") + i(year_fe, ref = "2018"),
            data = trade_df,
            panel.id = ~ unit_id + time_idx
) 
summ_m6 <- summary(m6, vcov = NW(4))
summ_m6
### after 2017
trade_df17 <- trade_df %>%
  filter(date >= as.Date("2017-01-01"))
trade_df17 <- trade_df %>%
  filter(date >= as.Date("2017-01-01")) %>%
  panel(panel.id = ~ unit_id + time_idx)

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

#### seperate IV

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
  data = trade_df
)

summary(m_iv2)
summary(m_iv2, stage = 1)

m_iv3 <- feols(
  d_ln_milkp ~
    f(d_ln_aqty, 0:K) +
    f(d_ln_dqty,0:K) +
    f(d_ln_wheyq,0:K) +
    i(month_fe, ref="6") +
    i(year_fe,  ref="2018") |
    f(d_ln_dair) ~
    f(d_ln_trd,  0:K),
  data = trade_df
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
