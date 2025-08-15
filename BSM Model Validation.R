
suppressPackageStartupMessages({
  required_pkgs <- c("dplyr","lubridate","tibble","stringr")
  to_install <- setdiff(required_pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(required_pkgs, require, character.only = TRUE))
})

Helpers
bs_put <- function(S, K, r, sigma, T){
  sigma <- pmax(sigma, 1e-8); T <- pmax(T, 1e-8)
  d1 <- (log(S/K) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
}

1) SPX intraday
stopifnot(exists("spx_data_bs"))
spx <- spx_data_bs %>% rename_with(~tolower(.x))

stopifnot("date" %in% names(spx))
price_col <- intersect(names(spx), c("spx","price","close","last"))
stopifnot(length(price_col) == 1)


date_raw <- spx$date
if (is.numeric(date_raw)) {
  
  if (max(date_raw, na.rm = TRUE) > 1e9) {
    dt <- as.POSIXct(date_raw, origin = "1970-01-01", tz = "UTC")          
  } else {
    dt <- as.POSIXct(date_raw * 86400, origin = "1899-12-30", tz = "UTC")
  }
} else {
  
  dt <- suppressWarnings(lubridate::ymd_hms(date_raw, tz = "UTC"))
  if (all(is.na(dt))) dt <- as.POSIXct(date_raw, tz = "UTC")
}

spx <- spx %>%
  mutate(dt = dt,
         trading_day = as.Date(dt),
         price = as.numeric(.data[[price_col]])) %>%
  arrange(dt)

val_day <- unique(spx$trading_day)
stopifnot(length(val_day) == 1)

ocm <- spx %>%
  summarise(
    date        = first(trading_day),
    open_time   = min(dt),
    close_time  = max(dt),
    S0          = first(price),
    session_hrs = as.numeric(difftime(close_time, open_time, units = "hours")),
    .groups = "drop"
  )
T_0dte_open <- ifelse(is.finite(ocm$session_hrs), ocm$session_hrs, 6.5) / (365*24)

2) Vol inputs
stopifnot(exists("Vol_data_bs"))
v <- Vol_data_bs %>% rename_with(~tolower(.x))
vix1d_col <- intersect(names(v), c("vix1d","vix_1d","vix1d_value"))[1]
vixm_col  <- intersect(names(v), c("vix","vix30","vix_monthly","vix_index"))[1]
stopifnot(!is.na(vix1d_col), !is.na(vixm_col))

vix1d <- as.numeric(v[[vix1d_col]]); vix1d <- ifelse(vix1d > 1, vix1d/100, vix1d)
vix    <- as.numeric(v[[vixm_col]]);  vix    <- ifelse(vix    > 1, vix/100,   vix)

3. Risk free rates
stopifnot(exists("risk_free_bs"))
rf <- risk_free_bs %>% rename_with(~tolower(.x))

get_rate <- function(df, names_try){
  hit <- intersect(names(df), names_try)
  if (length(hit)) as.numeric(df[[hit[1]]]) else NA_real_
}
r_1d <- get_rate(rf, c("r_1d","rate_1d","rf_1d","r1d","rate"))
r_1m <- get_rate(rf, c("r_1m","rate_1m","rf_1m","r1m","rate"))
if (is.na(r_1d)) r_1d <- 0
if (is.na(r_1m)) r_1m <- r_1d
r_1d <- ifelse(r_1d > 1, r_1d/100, r_1d)
r_1m <- ifelse(r_1m > 1, r_1m/100, r_1m)

4. Maturity strikes
T_m_open <- 22/365
S0 <- ocm$S0
K_0dte <- S0
K_m    <- S0

5. Premiums
opt_multiplier <- 100
put_0dte_open <- bs_put(S0, K_0dte, r_1d, vix1d, T_0dte_open)
put_m_open    <- bs_put(S0, K_m,    r_1m,  vix,   T_m_open)

premia_table <- tibble(
  date              = ocm$date,
  S0                = S0,
  vol_0dte          = vix1d,
  vol_monthly       = vix,
  r_1d              = r_1d,
  r_1m              = r_1m,
  T_0dte_open       = T_0dte_open,
  T_m_open          = T_m_open,
  put_0dte_open     = put_0dte_open,
  put_m_open        = put_m_open,
  prem_0dte_contract= put_0dte_open * opt_multiplier,
  prem_m_contract   = put_m_open    * opt_multiplier
)

premia_table


