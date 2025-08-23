## 1) Libraries
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
install.packages("fixest")
library(fixest)
setwd("/Users/macpro/Desktop/Youmin-phd/research/Dairy and Alfalfa/China Figure")
# === 1) Load and prepare ===
path <- ("fao-dairy-price-indices-aug.xlsx")
fao <- read_xlsx(path)
# Dairy feed (monthly)
feed1 <- read_csv("dairy feed monthly.csv")
# Dairy (monthly)
dairy <- read_csv("dairy monthly.csv")
# yearly alfalfa and 
panel <- read_xlsx("Panel data.xlsx")

###export csv for github
write.csv(cn_m,"/Users/macpro/Desktop/Youmin-phd/research/Dairy and Alfalfa/China Figure/cn_m.csv", row.names = FALSE)

# Robust “monthly export table” parser ('dairy monthly.csv')

raw <- suppressWarnings(read_csv(path, col_names = FALSE, show_col_types = FALSE))
stopifnot(nrow(raw) >= 6)
# 1) Identify the two header rows
# Heuristic: the first row that contains multiple month labels is the TOP row,
# and the very next row is the BOTTOM row with "Value"/"Qty"/"Year"/"UOM".
month_tokens <- c(tolower(month.name), tolower(month.abb))
find_top <- function(mat){
  for(r in seq_len(nrow(mat))){
    vals <- tolower(as.character(unlist(mat[r, ]))); vals[is.na(vals)] <- ""
    hits <- sum(vapply(month_tokens, function(m) any(str_detect(vals, paste0("^", m, "\\b"))), logical(1)))
    if(hits >= 6) return(r)  # "enough months"
  }
  NA_integer_
}
r_top <- find_top(raw)
if(is.na(r_top)) stop("Could not locate the month header row. Please print first 6 rows to inspect.")
r_bot <- r_top + 1L
hdr_top <- raw[r_top, ] %>% unlist() %>% as.character()
hdr_bot <- raw[r_bot, ] %>% unlist() %>% as.character()
hdr_top[is.na(hdr_top)] <- ""; hdr_bot[is.na(hdr_bot)] <- ""
# 2) Normalize month names in top header: "Jan" -> "January", keep others as-is
norm_month <- function(x){
  x2 <- str_trim(tolower(x))
  for(i in seq_along(month.abb)){
    x2 <- str_replace_all(x2, paste0("^", tolower(month.abb[i]), "\\b"), tolower(month.name[i]))
  }
  str_to_title(x2)  # back to Title Case
}
top_norm <- norm_month(hdr_top)
bot_norm <- str_trim(hdr_bot)
# 3) Locate the Year column by bottom header tokens
# The bottom row usually has a cell == "Year"; if not, search contains "Year"
year_idx <- which(tolower(bot_norm) == "year")
if(length(year_idx) == 0){
  year_idx <- which(str_detect(tolower(bot_norm), "year"))
}
if(length(year_idx) == 0) stop("Could not find 'Year' column in the second header row.")
# 4) Identify monthly VALUE/QTY columns by position:
# For each column j, if top_norm[j] is a valid month name and bot_norm[j] matches value/qty, keep it.
month_idx <- match(top_norm, month.name)  # NA if not a month
is_value  <- str_detect(tolower(bot_norm), "^value\\b")
is_qty    <- str_detect(tolower(bot_norm), "^(qty|quantity)\\b")
is_uom    <- str_detect(tolower(bot_norm), "\\buom\\b")
# Build an index map: for each month m, which column is Value and which is Qty?
cols <- tibble(col = seq_along(top_norm),
               m = month_idx,
               value = is_value,
               qty = is_qty,
               uom = is_uom)
month_map <- map_dfr(1:12, function(m){
  vcol <- cols %>% filter(m == !!m, value) %>% slice(1) %>% pull(col)
  qcol <- cols %>% filter(m == !!m, qty)   %>% slice(1) %>% pull(col)
  tibble(m = m, vcol = if(length(vcol)) vcol else NA_integer_,
         qcol = if(length(qcol)) qcol else NA_integer_)
})
# Sanity: if we found none, bail with a helpful message
if(all(is.na(month_map$vcol)) && all(is.na(month_map$qcol))){
  stop("Found no month columns with 'Value'/'Qty'. Inspect the header rows for exact tokens.")
}
# 5) Slice data rows and extract
dat <- raw[-(1:r_bot), , drop = FALSE]
# Pull base fields
year_raw <- dat[[ year_idx[1] ]]
# Optional UOM: take the first UOM column if present
uom_idx <- which(is_uom)
uom_raw <- if(length(uom_idx)) dat[[ uom_idx[1] ]] else NA
# Build the long tibble
out <- map_dfr(1:12, function(m){
  vcol <- month_map$vcol[month_map$m == m]
  qcol <- month_map$qcol[month_map$m == m]
  if(is.na(vcol) || is.na(qcol)) return(tibble())
  tibble(
    year_raw  = as.character(year_raw),
    month     = m,
    value_usd = suppressWarnings(as.numeric(gsub(",", "", as.character(dat[[ vcol ]] )))),
    qty_ton   = suppressWarnings(as.numeric(gsub(",", "", as.character(dat[[ qcol ]] )))),
    uom       = if(!all(is.na(uom_raw))) as.character(uom_raw) else NA_character_
  )
})
if(nrow(out) == 0) stop("Parsed no rows after pairing months. Check if Value/Qty tokens differ (e.g., 'Quantity').")
out %>%
  mutate(
    year = as.integer(str_extract(year_raw, "\\d{4}")),
    date = make_date(year, month, 1),
    uv_usd_per_ton = ifelse(!is.na(qty_ton) & qty_ton > 0, value_usd/qty_ton, NA_real_)
  ) %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  select(date, year, month, uom, value_usd, qty_ton, uv_usd_per_ton)
}

parse_trade_monthly_csv <- parse_dairy_monthly_csv_pos
dairy_us2cn_m <- parse_trade_monthly_csv("dairy monthly.csv") 
head(dairy_us2cn_m, 12)
summary(dairy_us2cn_m$uv_usd_per_ton)

# Expand annual series to monthly (piecewise constant)
annual_to_monthly <- function(df, year_var, keep_vars){
  df %>%
    clean_names() %>%
    rename(year = {{year_var}}) %>%
    mutate(year = as.integer(year)) %>%
    filter(!is.na(year)) %>%
    crossing(month = 1:12) %>%
    mutate(date = make_date(year, month, 1)) %>%
    arrange(date) %>%
    dplyr::select(date, year, month, all_of(keep_vars))
}

### CN - China imports from U.S. yearly
cn <- read_excel("Panel data.xlsx", sheet = "China import from U.S.") %>% clean_names()
cn <- cn %>%
  select(where(~ !all(is.na(.))))

### China milk farm gate price data
milk_sheet <- read_excel("Panel data.xlsx", sheet = "China farm- gate milk pric", col_names = FALSE)
milk_sheet <- milk_sheet %>%
  select(where(~ !all(is.na(.))))
milk_raw <- read_excel("Panel data.xlsx", 
                       sheet = "China farm- gate milk pric", 
                       col_names = FALSE)
# --- Step 1: keep only first 13 rows (Jan–Dec + avg annual)
milk_tbl <- milk_raw[1:13, 1:(1 + length(2009:2025))]  # first column = Month, others = years
# adjust if extra columns beyond 2019/2020
# --- Step 2: set first col = Month names
names(milk_tbl) <- c("month_name", as.character(2009:2025))
milk_tbl <- milk_tbl %>% filter(month_name %in% month.name)

milk_long <- milk_tbl %>%
  pivot_longer(-month_name, names_to = "year", values_to = "farmgate_cny_100kg") %>%
  mutate(
    year  = as.integer(year),
    month = match(month_name, month.name),
    date  = make_date(year, month, 1),
    farmgate_cny_kg = as.numeric(farmgate_cny_100kg) / 100  # convert from per 100kg to per kg
  ) %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  select(date, year, month, farmgate_cny_kg)

feed_raw <- read_csv("Standard Query_37713.csv", skip = 4, show_col_types = FALSE) %>%
  clean_names()
partner_col <- names(feed_raw)[map_lgl(feed_raw, ~ any(str_detect(as.character(.x), "^China$"), na.rm = TRUE))]
product_col <- names(feed_raw)[map_lgl(feed_raw, ~ any(str_detect(as.character(.x), "Feed,\\s*Ingrd\\s*&\\s*Fod"), na.rm = TRUE))]
year_col <- names(feed_raw)[map_lgl(feed_raw, ~ any(str_detect(as.character(.x), "^[12][0-9]{3}-[12][0-9]{3}$"), na.rm = TRUE))]
uom_col <- names(feed_raw)[map_lgl(feed_raw, ~ any(str_detect(as.character(.x), "^MT$"), na.rm = TRUE))]
stopifnot(length(partner_col)==1, length(product_col)==1, length(year_col)==1, length(uom_col)==1)
month_cols <- names(feed_raw)[
  str_detect(names(feed_raw), "^(value|qty)_(\\d+)$|^(value|qty)\\.\\.\\.(\\d+)$")
]
length(month_cols)          # should be 24
month_cols[1:6]             # first few names
months_vec <- rep(1:12, each = 2)             # 1,1,2,2,...,12,12
type_vec   <- rep(c("value","qty"), 12)       # value,qty,value,qty,...
feed_vq <- feed_raw %>%
  select(all_of(month_cols)) %>%
  mutate(row_id = dplyr::row_number()) %>%
  pivot_longer(cols = -row_id, names_to = "colname", values_to = "raw") %>%
  group_by(row_id) %>%
  mutate(pos = dplyr::row_number()) %>%
  ungroup() %>%
  mutate(
    month = months_vec[pos],
    type  = type_vec[pos]
  ) %>%
  select(row_id, month, type, raw) %>%
  pivot_wider(names_from = type, values_from = raw)
partner_col <- names(feed_raw)[map_lgl(feed_raw, ~ any(str_detect(as.character(.x), "^China$"), na.rm = TRUE))]
product_col <- names(feed_raw)[map_lgl(feed_raw, ~ any(str_detect(as.character(.x), "Feed,\\s*Ingrd\\s*&\\s*Fod"), na.rm = TRUE))]
year_col    <- names(feed_raw)[map_lgl(feed_raw, ~ any(str_detect(as.character(.x), "^[12][0-9]{3}-[12][0-9]{3}$"), na.rm = TRUE))]
stopifnot(length(partner_col)==1, length(product_col)==1, length(year_col)==1)
ids <- feed_raw %>%
  transmute(
    row_id   = dplyr::row_number(),
    partner  = .data[[partner_col]],
    product  = .data[[product_col]],
    year_str = .data[[year_col]]
  )
feed_m <- ids %>%
  inner_join(feed_vq, by = "row_id") %>%
  filter(partner == "China", str_detect(product, "Feed,\\s*Ingrd\\s*&\\s*Fod")) %>%
  mutate(
    year  = readr::parse_number(year_str),
    qty   = readr::parse_number(as.character(qty)),     # MT
    value = readr::parse_number(as.character(value)),   # thousands USD
    value = value * 1000,                               # USD
    date  = make_date(year, month, 1),
    feed_price_usd_ton = if_else(qty > 0, value / qty, NA_real_)
  ) %>%
  arrange(date) %>%
  select(date, year, month,
         feed_qty_mt = qty,
         feed_value_usd = value,
         feed_price_usd_ton)
# USITC partner export data (to build UV to CN vs ROW)
usitc <- tryCatch({
  read_excel("USITC data- dairy and alfalfa.xlsx") %>% clean_names()
}, error = function(e) NULL)

### 2) build monthly panel
cn_keep <- c("alfalfa_tariff_rate","dairy_tariff_rate",
              "alfalfa_price_mt","dairy_price_kg",
              "feed_ingrd_fod_price_ton")

annual_to_monthly <- function(df, year_var, keep_vars = NULL){
  df2 <- df %>%
    clean_names() %>%
    rename(year = {{year_var}}) %>%
    mutate(year = suppressWarnings(as.integer(year))) %>%
    filter(!is.na(year))
  if (!is.null(keep_vars)) {
    df2 <- df2 %>% select(year, any_of(keep_vars))   # any_of avoids errors
  }
  df2 %>%
    crossing(month = 1:12) %>%
    mutate(date = make_date(year, month, 1)) %>%
    arrange(date)
}

pick_col <- function(df, candidates){
  intersect(candidates, names(df))[1] %||% NA_character_
}

map_cols <- list(
  tau_alf              = pick_col(cn, c("alfalfa_tariff_rate","alfalfa_tariff")),
  tau_dair             = pick_col(cn, c("dairy_tariff_rate","dairy_tariff")),
  p_alf_world_usd_ton  = pick_col(cn, c("alfalfa_price_mt","alfalfa_unit_value","alfalfa_price_usd_ton")),
  p_dair_imp_usd_kg    = pick_col(cn, c("dairy_price_kg","dairy_price_usd_kg","dairy_unit_value")),
  feed_price_usd_ton   = pick_col(cn, c(
    "feed_ingrd_fod_price_ton",
    "feed_ingrd_fod_price_tonne",
    "feed_ingrd_fod_price__ton",
    "feed_price_usd_ton","feed_price_ton"))
)
 
### merge 
parse_dairy_monthly_csv_pos <- function(path){


df_tr <- cn_m %>%
  left_join(dairy_us2cn_m %>% select(date, dairy_value_usd = value_usd), by = "date") %>%
  mutate(TR_dair_m = tau_dair * dairy_value_usd)

df_tr %>%
  ggplot(aes(date, TR_dair_m/1e6)) +
  geom_line() +
  labs(y = "Tariff revenue on dairy (USD million)", x = NULL,
       title = "China monthly tariff revenue on U.S. dairy (estimated)") +
  theme_minimal()

df_tr <- df_tr %>%
  left_join(milk_long, by = c("date"))







# === extra) Event-style percent changes for the paper text ===
pct <- function(a,b) round((a/b - 1)*100, 1)
y <- function(y) df %>% filter(year.x == y) %>% slice(1)
pct_stats <- tibble(
  metric = c("alfalfa_qty_2017_2019", "dairy_qty_2017_2019",
             "alfalfa_uv_2017_2019", "dairy_uv_2017_2019",
             "alfalfa_qty_2022_2024", "dairy_qty_2022_2024",
             "alfalfa_uv_2022_2025", "dairy_uv_2022_2025"),
  pct_change = c(
    pct(y(2019)$alfalfa_qty_mt, y(2017)$alfalfa_qty_mt),
    pct(y(2019)$dairy_qty_mt,   y(2017)$dairy_qty_mt),
    pct(y(2019)$alfalfa_uv_usd_mt, y(2017)$alfalfa_uv_usd_mt),
    pct(y(2019)$dairy_uv_usd_mt,   y(2017)$dairy_uv_usd_mt),
    pct(y(2025)$alfalfa_qty_mt, y(2022)$alfalfa_qty_mt),
    pct(y(2025)$dairy_qty_mt,   y(2022)$dairy_qty_mt),
    pct(y(2025)$alfalfa_uv_usd_mt, y(2022)$alfalfa_uv_usd_mt),
    pct(y(2025)$dairy_uv_usd_mt,   y(2022)$dairy_uv_usd_mt)
  )
)
print(pct_stats)

# === extra) Plots (single chart each; highlight 2018–2019 and 2022–2024) ===
# Alfalfa: quantity (solid) + unit value (dashed, right axis)
ggplot(df, aes(Year, alfalfa_qty_mt)) +
  geom_line() +
  geom_vline(xintercept = c(2018, 2019, 2022, 2024), linetype = "dotted") +
  labs(title = "China imports of U.S. alfalfa (2010–2025)",
       y = "Quantity (MT)", x = "Year") +
  theme_minimal()

ggplot(df, aes(Year, alfalfa_uv_usd_mt)) +
  geom_line(linetype = "dashed") +
  geom_vline(xintercept = c(2018, 2019, 2022, 2024), linetype = "dotted") +
  labs(title = "Unit value of China’s alfalfa imports from U.S.",
       y = "USD per metric ton", x = "Year") +
  theme_minimal()

# Dairy: quantity and unit value in separate charts
ggplot(df, aes(Year, dairy_qty_mt)) +
  geom_line() +
  geom_vline(xintercept = c(2018, 2019, 2022, 2024), linetype = "dotted") +
  labs(title = "China imports of U.S. dairy (2010–2025)",
       y = "Quantity (MT)", x = "Year") +
  theme_minimal()

ggplot(df, aes(Year, dairy_uv_usd_mt)) +
  geom_line(linetype = "dashed") +
  geom_vline(xintercept = c(2018, 2019, 2022, 2024), linetype = "dotted") +
  labs(title = "Unit value of China’s dairy imports from U.S.",
       y = "USD per metric ton", x = "Year") +
  theme_minimal()

# === extra) Simple log-linear regressions (illustrative) ===
m1 <- lm(log(alfalfa_qty_mt) ~ alfalfa_tariff + I(Year %in% 2018:2019) + I(Year %in% 2022:2024), data = df)
m2 <- lm(log(alfalfa_uv_usd_mt) ~ alfalfa_tariff + I(Year %in% 2018:2019) + I(Year %in% 2022:2024), data = df)
m3 <- lm(log(dairy_qty_mt) ~ dairy_tariff + I(Year %in% 2018:2019) + I(Year %in% 2022:2024), data = df)
m4 <- lm(log(dairy_uv_usd_mt) ~ dairy_tariff + I(Year %in% 2018:2019) + I(Year %in% 2022:2024), data = df)

summary(m1); summary(m2); summary(m3); summary(m4)


# --- ToT proxy (relative to a pre-tariff benchmark, e.g., 2010–2017 mean) ---
bench <- df |>
  filter(Year %in% 2010:2017) |>
  summarise(Pfa0 = mean(Pfa, na.rm=TRUE),
            Pfd0 = mean(Pfd, na.rm=TRUE))

df <- df |>
  mutate(ToT_a = pmax(0, bench$Pfa0 - Pfa) * M_a,   # only count price falls
         ToT_d = pmax(0, bench$Pfd0 - Pfd) * M_d,
         ToT_total = ToT_a + ToT_d)

# --- Summaries for highlighted windows ---
sum_win <- df |>
  mutate(window = case_when(
    Year %in% 2018:2019 ~ "Tariff shock (18–19)",
    Year %in% 2022:2024 ~ "Adjustment (22–24)",
    TRUE ~ "Other"
  )) |>
  group_by(window) |>
  summarise(
    TR_a = sum(TR_a, na.rm=TRUE),
    TR_d = sum(TR_d, na.rm=TRUE),
    TR_total = sum(TR_total, na.rm=TRUE),
    ToT_total = sum(ToT_total, na.rm=TRUE)
  )
print(sum_win)

