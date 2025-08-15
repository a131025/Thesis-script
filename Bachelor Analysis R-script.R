
required_pkgs <- c("dplyr","tidyr","purrr","lubridate","tibble","stringr","readr","stats","writexl")
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(required_pkgs, require, character.only = TRUE))


event_days <- as.Date(c("2025-01-10","2025-03-12","2025-03-19","2025-06-18"))
event_days_available <- intersect(event_days, unique(spx$date))
if (length(event_days_available) < length(event_days)) {
  message("Skipping missing SPX dates: ",
          paste(setdiff(event_days, event_days_available), collapse = ", "))
}

portfolio_notional <- 400e6      
hedge_target_frac  <- 0.30       
capital_budget_frac<- 0.01      
capital_budget     <- capital_budget_frac * portfolio_notional
hedged_notional_target <- hedge_target_frac * portfolio_notional

opt_multiplier <- 100            
es_multiplier  <- 50             

use_intraday_session_length <- TRUE  
fallback_session_hours <- 6.5        

use_third_friday_expiry <- TRUE      
fixed_monthly_days <- 30

use_delta_sizing <- FALSE           

mark_monthly_eod_mtm <- TRUE        

fx_usd_to_portfolio <- 1.0           

Helpers
detect_col <- function(df, candidates) {
  hit <- intersect(names(df), candidates)
  if (length(hit) == 0) stop(paste("Missing any of:", paste(candidates, collapse=", ")))
  hit[1]
}

bs_put <- function(S, K, r, sigma, T) {
  sigma <- pmax(sigma, 1e-8); T <- pmax(T, 1e-8)
  d1 <- (log(S/K) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
}
bs_delta_put <- function(S, K, r, sigma, T) {
  sigma <- pmax(sigma, 1e-8); T <- pmax(T, 1e-8)
  d1 <- (log(S/K) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  pnorm(d1) - 1
}

third_friday <- function(d) {
  first <- lubridate::floor_date(d, "month")
  wd <- lubridate::wday(first, week_start = 1)
  to_friday <- (5 - wd) %% 7
  first_friday <- first + lubridate::days(to_friday)
  tf <- first_friday + lubridate::weeks(2)
  tf
}

Data Processing

price_col <- detect_col(spx_data, c("Price","price","Close","close","Last","last","SPX","spx"))
time_col  <- detect_col(spx_data, c("Datetime","datetime","DateTime","Timestamp","timestamp","Time","time","Date","date"))

spx <- spx_data %>%
  rename_with(~tolower(.x)) %>%
  mutate(
    dt    = as.POSIXct(datetime * 86400, origin = "1899-12-30", tz = "UTC"),
    date  = as.Date(dt),
    price = as.numeric(spx)
  ) %>%
  arrange(dt)

vix1d <- vix1d_newdata %>%
  rename_with(~stringr::str_to_lower(.x))
date_col_vix1d <- detect_col(vix1d, c("date","dt","datetime","timestamp","day","time"))
vix1d <- vix1d %>%
  mutate(date = as.Date(.data[[date_col_vix1d]], origin = "1899-12-30")) %>%
  rename(vix1d = any_of(c("vix1d","vix_1d","vix1d_value","vix1d_index"))) %>%
  transmute(date, vix1d = as.numeric(vix1d))

vixm <- vix_monthlydata %>%
  rename_with(~stringr::str_to_lower(.x))

date_col_vixm <- detect_col(vixm, c("date","dt","datetime","timestamp","day","time"))

vixm <- vixm %>%
  mutate(date = as.Date(.data[[date_col_vixm]], origin = "1899-12-30")) %>%
  rename(vix = any_of(c("vix","vix30","vix_monthly","vix_index"))) %>%
  transmute(date, vix = as.numeric(vix))

rf <- rf_rates_data %>%
  rename_with(~stringr::str_to_lower(.x))

date_col_rf <- detect_col(rf, c("date","dt","datetime","timestamp","day","time"))

rf <- rf %>%
  mutate(date = as.Date(.data[[date_col_rf]], origin = "1899-12-30")) %>%
  rename(
    r_1d = any_of(c("r_1d","rf_1d","r1d","rf1d","rate_1d")),
    r_1m = any_of(c("r_1m","rf_1m","r1m","rf1m","rate_1m"))
  ) %>%
  mutate(
    across(c(r_1d, r_1m), as.numeric),
    across(c(r_1d, r_1m), ~ ifelse(.x > 1, .x/100, .x))
  ) %>%
  distinct(date, .keep_all = TRUE)

md <- margin_data %>% 
  rename_with(~stringr::str_to_lower(.x))

if (!"date" %in% names(md)) {
  alt <- intersect(names(md), c("dt","datetime","timestamp","day","time"))
  if (length(alt)) md <- md %>% mutate(date = .data[[alt[1]]])
}

md <- md %>%
  mutate(date = as.Date(date, origin = "1899-12-30"))

num_cols <- names(md)[sapply(md, is.numeric)]
stopifnot(length(num_cols) >= 1)
margin_col <- setdiff(num_cols, "date")[1]

es_margin <- md %>%
  transmute(date = as.Date(date),
            es_margin_per_contract = .data[[margin_col]])

ocm <- spx %>%
  filter(date %in% event_days_available) %>%
  group_by(date) %>%
  summarise(
    open_time = min(dt),
    close_time = max(dt),
    S0   = price[which.min(dt)],      
    ST   = price[which.max(dt)],      
    Smin = min(price, na.rm = TRUE),  
    min_time = dt[which.min(price)],
    session_hours = as.numeric(difftime(close_time, open_time, units = "hours")),
    .groups = "drop"
  )

ocm <- ocm %>%
  dplyr::mutate(
    T_0dte_open = dplyr::if_else(
      is.finite(session_hours),
      session_hours/(365*24),
      6.5/(365*24)
    )
  )

ocm <- spx %>%
  filter(date %in% event_days_available) %>%
  group_by(date) %>%
  summarise(
    open_time = min(dt),
    close_time = max(dt),
    S0   = first(price),                
    ST   = last(price),                 
    Smin = min(price, na.rm = TRUE),    
    min_time = dt[which.min(price)],    
    session_hours = as.numeric(difftime(close_time, open_time, units = "hours")),
    .groups = "drop"
  ) %>%
  mutate(
    
    T_0dte_open = session_hours / (365 * 24)
  )
third_friday <- function(d) {
  first <- lubridate::floor_date(d, "month")
  wd <- lubridate::wday(first, week_start = 1)    
  to_friday <- (5 - wd) %% 7
  first_friday <- first + lubridate::days(to_friday)
  first_friday + lubridate::weeks(2)
}

use_third_friday_expiry <- TRUE   
fixed_monthly_days <- 30

monthly_times <- ocm %>%
  dplyr::transmute(date, open_time, close_time) %>%
  dplyr::mutate(
    expiry_date = if (use_third_friday_expiry) {
      tf <- third_friday(date)
      after <- date > tf
      tf <- ifelse(after, third_friday(date %m+% months(1)), tf)
      as.Date(tf, origin = "1970-01-01")
    } else {
      as.Date(open_time) + lubridate::days(fixed_monthly_days)
    },
    
    expiry_dt_open  = as.POSIXct(expiry_date) + lubridate::hours(16),
    T_m_open = pmax(as.numeric(difftime(expiry_dt_open, open_time, units = "days"))/365, 1e-8)
  )

monthly_times <- ocm %>%
  dplyr::transmute(date, open_time, close_time) %>%
  dplyr::mutate(
    expiry_date = if (use_third_friday_expiry) {
      tf <- third_friday(date)
      after <- date > tf
      tf <- ifelse(after, third_friday(date %m+% months(1)), tf)
      as.Date(tf, origin = "1970-01-01")
    } else {
      as.Date(open_time) + lubridate::days(fixed_monthly_days)
    },
    expiry_dt_open  = as.POSIXct(expiry_date) + lubridate::hours(16),
    expiry_dt_close = as.POSIXct(expiry_date) + lubridate::hours(16),
    T_m_open  = pmax(as.numeric(difftime(expiry_dt_open,  open_time,  units = "days"))/365, 1e-8),
    T_m_close = pmax(as.numeric(difftime(expiry_dt_close, close_time, units = "days"))/365, 1e-8)
  )
third_friday <- function(d){
  first <- lubridate::floor_date(d, "month")
  wd <- lubridate::wday(first, week_start = 1)  
  first_friday <- first + lubridate::days((5 - wd) %% 7)
  first_friday + lubridate::weeks(2)
}

monthly_times <- ocm %>%
  dplyr::transmute(date, open_time, close_time) %>%
  dplyr::mutate(
    expiry_date  = as.Date(third_friday(date)),
    expiry_dt    = as.POSIXct(expiry_date) + lubridate::hours(16),
    T_m_open     = pmax(as.numeric(difftime(expiry_dt,  open_time,  units="days"))/365, 1e-8),
    T_m_close    = pmax(as.numeric(difftime(expiry_dt,  close_time, units="days"))/365, 1e-8)
  )


inputs <- ocm %>%
  dplyr::left_join(monthly_times %>% dplyr::select(date, T_m_open, T_m_close), by = "date") %>%
  dplyr::left_join(vix1d, by = "date") %>%
  dplyr::left_join(vixm,  by = "date") %>%
  dplyr::left_join(rf,    by = "date") %>%
  dplyr::mutate(
    vix1d = ifelse(vix1d > 1, vix1d/100, vix1d),
    vix   = ifelse(vix   > 1, vix/100,   vix),
    r_1d  = coalesce(r_1d, 0),
    r_1m  = coalesce(r_1m, 0),
    K_0dte = S0,
    K_m    = S0
  )

names(inputs)
inputs %>% dplyr::select(date, T_m_open, T_m_close)

Premiums
premia <- inputs %>%
  dplyr::mutate(
    put_0dte_open = bs_put(S0, K_0dte, r_1d, vix1d, T_0dte_open),
    put_m_open    = bs_put(S0, K_m,   r_1m,  vix,   T_m_open),
    prem_0dte_contract = put_0dte_open * opt_multiplier,
    prem_m_contract    = put_m_open    * opt_multiplier,
    
    
    delta_put_0dte = bs_delta_put(S0, K_0dte, r_1d, vix1d, T_0dte_open),
    delta_put_m    = bs_delta_put(S0, K_m,    r_1m,  vix,   T_m_open)
  )

Option Sizing
size_tbl <- premia %>%
  mutate(
   
    contracts_needed_notional = floor(hedged_notional_target / (S0 * opt_multiplier)),
    
    
    contracts_needed_delta = floor(hedged_notional_target / (abs(delta_put_m) * S0 * opt_multiplier)),
    
    contracts_needed = if (use_delta_sizing) contracts_needed_delta else contracts_needed_notional,
    
    contracts_afford_0dte = floor((capital_budget / fx_usd_to_portfolio) / prem_0dte_contract),
    contracts_afford_m    = floor((capital_budget / fx_usd_to_portfolio) / prem_m_contract),
    
    n_0dte = pmin(contracts_needed, contracts_afford_0dte),
    n_m    = pmin(contracts_needed, contracts_afford_m),
    
    cost_0dte_usd = n_0dte * prem_0dte_contract,
    cost_m_usd    = n_m    * prem_m_contract,
    cost_0dte     = cost_0dte_usd * fx_usd_to_portfolio,
    cost_m        = cost_m_usd    * fx_usd_to_portfolio,
    cost_0dte_pct = cost_0dte / portfolio_notional,
    cost_m_pct    = cost_m    / portfolio_notional
  )

use_delta_sizing <- TRUE

fut_inputs <- premia %>% select(date, S0, ST, Smin) %>% left_join(es_margin, by = "date")

fut_size <- fut_inputs %>%
  mutate(
    contracts_needed_es = floor(hedged_notional_target / (S0 * es_multiplier)),
    contracts_afford_es = floor((capital_budget / fx_usd_to_portfolio) / es_margin_per_contract),
    n_es = pmin(contracts_needed_es, contracts_afford_es),
    margin_used_es_usd = n_es * es_margin_per_contract,
    margin_used_es     = margin_used_es_usd * fx_usd_to_portfolio,
    cost_es_pct        = margin_used_es / portfolio_notional
  )


pnl_tbl <- size_tbl %>%
  left_join(fut_size %>% select(date, n_es, margin_used_es, cost_es_pct), by = "date") %>%
  mutate(
    port_ret  = (ST - S0)/S0,
    port_pnl  = port_ret * portfolio_notional,
    
    
    payoff_put_0dte_usd = pmax(K_0dte - ST, 0) * opt_multiplier,
    pnl_0dte            = (n_0dte * payoff_put_0dte_usd * fx_usd_to_portfolio) - cost_0dte,
    
    T_m_close_use = pmax(T_m_close, 1e-8),
    value_m_close_usd = if (mark_monthly_eod_mtm) {
      bs_put(ST, K_m, r_1m, vix, T_m_close_use) * opt_multiplier
    } else {
      pmax(K_m - ST, 0) * opt_multiplier
    },
    pnl_m = (n_m * value_m_close_usd * fx_usd_to_portfolio) - cost_m,
    
    
    pnl_es = n_es * (S0 - ST) * es_multiplier * fx_usd_to_portfolio,
    
    net_pnl_0dte = port_pnl + pnl_0dte,
    net_pnl_m    = port_pnl + pnl_m,
    net_pnl_es   = port_pnl + pnl_es
  )


elapsed <- ocm %>%
  select(date, session_hours, min_time, open_time) %>%
  mutate(
    hours_elapsed = as.numeric(difftime(min_time, open_time, units = "hours"))
  ) %>%
  rename(session_hours_mdd = session_hours)

inputs_mdd <- inputs %>%
  select(date, r_1d, r_1m, vix1d, vix, K_0dte, K_m, T_m_open, Smin, S0)

mdd_tbl <- pnl_tbl %>%
  select(-matches("^T_m_open$|^r_1d$|^r_1m$|^vix1d$|^vix$|^K_0dte$|^K_m$|^Smin$|^S0$")) %>%
  left_join(elapsed, by = "date") %>%
  left_join(inputs_mdd, by = "date") %>%
  mutate(
    T_remain_0dte = pmax((session_hours_mdd / (365 * 24)) - hours_elapsed / (365 * 24), 1e-8),
    T_remain_m    = pmax(T_m_open - hours_elapsed / (365 * 24), 1e-8),
    
    hedge_val_min_0dte = n_0dte * bs_put(Smin, K_0dte, r_1d, vix1d, T_remain_0dte) *
      opt_multiplier * fx_usd_to_portfolio,
    hedge_val_min_m    = n_m    * bs_put(Smin, K_m,   r_1m,  vix,   T_remain_m) *
      opt_multiplier * fx_usd_to_portfolio,
    hedge_val_min_es   = n_es   * (S0 - Smin) * es_multiplier * fx_usd_to_portfolio,
    
    dd_base_amt = pmax(0, (S0 - Smin) / S0) * portfolio_notional,
    dd_0dte_amt = pmax(0, dd_base_amt - hedge_val_min_0dte),
    dd_m_amt    = pmax(0, dd_base_amt - hedge_val_min_m),
    dd_es_amt   = pmax(0, dd_base_amt - hedge_val_min_es),
    
    dd_red_0dte_pct = ifelse(dd_base_amt > 0, 1 - dd_0dte_amt / dd_base_amt, 0),
    dd_red_m_pct    = ifelse(dd_base_amt > 0, 1 - dd_m_amt / dd_base_amt,    0),
    dd_red_es_pct   = ifelse(dd_base_amt > 0, 1 - dd_es_amt / dd_base_amt,   0)
  )
names(ocm)
test <- pnl_tbl %>%
  left_join(elapsed, by = "date")

names(test)
names(
  pnl_tbl %>%
    left_join(elapsed, by = "date") %>%
    left_join(inputs %>% select(date, r_1d, r_1m, vix1d, vix, K_0dte, K_m, T_m_open, Smin, S0), by = "date")
)

sizes <- size_tbl[, c("date", "n_0dte", "n_m", "cost_0dte", "cost_m")]
futs  <- fut_size[, c("date", "n_es", "margin_used_es")]


spx_intraday <- merge(spx, sizes, by = "date", all.x = TRUE)
spx_intraday <- merge(spx_intraday, futs, by = "date", all.x = TRUE)


S0_map <- aggregate(price ~ date, data = spx_intraday, FUN = function(x) x[1])
names(S0_map)[2] <- "S0"
spx_intraday <- merge(spx_intraday, S0_map, by = "date", all.x = TRUE)


vars_needed <- c("date", "K_0dte", "K_m", "r_1d", "r_1m", "vix1d", "vix", "T_m_open")
spx_intraday <- merge(spx_intraday, inputs[, vars_needed], by = "date", all.x = TRUE)


spx_intraday$port_ret  <- (spx_intraday$price - spx_intraday$S0) / spx_intraday$S0
spx_intraday$port_val  <- portfolio_notional * (1 + spx_intraday$port_ret)

session_length_map <- aggregate(dt ~ date, data = spx_intraday, FUN = function(x) diff(range(x)) / 3600)
names(session_length_map)[2] <- "session_hours"
spx_intraday <- merge(spx_intraday, session_length_map, by = "date", all.x = TRUE)

open_time_map <- aggregate(dt ~ date, data = spx_intraday, FUN = min)
names(open_time_map)[2] <- "open_time"
spx_intraday <- merge(spx_intraday, open_time_map, by = "date", all.x = TRUE)
spx_intraday$hours_elapsed <- as.numeric(difftime(spx_intraday$dt, spx_intraday$open_time, units = "hours"))

spx_intraday$T_remain_0dte <- pmax(
  as.numeric(spx_intraday$session_hours) / (365 * 24) - 
    as.numeric(spx_intraday$hours_elapsed) / (365 * 24),
  1e-8
)

spx_intraday$T_remain_m <- pmax(
  as.numeric(spx_intraday$T_m_open) - 
    as.numeric(spx_intraday$hours_elapsed) / (365 * 24),
  1e-8
)

spx_intraday$hedge_val_0dte <- spx_intraday$n_0dte * bs_put(spx_intraday$price, spx_intraday$K_0dte,
                                                            spx_intraday$r_1d, spx_intraday$vix1d,
                                                            spx_intraday$T_remain_0dte) *
  opt_multiplier * fx_usd_to_portfolio


spx_intraday$hedge_val_m <- spx_intraday$n_m * bs_put(spx_intraday$price, spx_intraday$K_m,
                                                      spx_intraday$r_1m, spx_intraday$vix,
                                                      spx_intraday$T_remain_m) *
  opt_multiplier * fx_usd_to_portfolio

spx_intraday$hedge_val_es <- spx_intraday$n_es * (spx_intraday$S0 - spx_intraday$price) *
  es_multiplier * fx_usd_to_portfolio

spx_intraday$val_0dte <- spx_intraday$port_val + spx_intraday$hedge_val_0dte - spx_intraday$cost_0dte
spx_intraday$val_m    <- spx_intraday$port_val + spx_intraday$hedge_val_m    - spx_intraday$cost_m
spx_intraday$val_es   <- spx_intraday$port_val + spx_intraday$hedge_val_es

var_results <- data.frame(date = unique(spx_intraday$date))
for (d in var_results$date) {
  day_data <- spx_intraday[spx_intraday$date == d, ]
  r_base <- diff(day_data$port_val) / portfolio_notional
  r_0dte <- diff(day_data$val_0dte) / portfolio_notional
  r_m    <- diff(day_data$val_m)    / portfolio_notional
  r_es   <- diff(day_data$val_es)   / portfolio_notional
  
  var_results[var_results$date == d, "var_unhedged"] <- var(r_base, na.rm = TRUE)
  var_results[var_results$date == d, "var_0dte"]     <- var(r_0dte, na.rm = TRUE)
  var_results[var_results$date == d, "var_monthly"]  <- var(r_m,    na.rm = TRUE)
  var_results[var_results$date == d, "var_futures"]  <- var(r_es,   na.rm = TRUE)
}

var_results$var_red_0dte    <- 1 - var_results$var_0dte    / var_results$var_unhedged
var_results$var_red_monthly <- 1 - var_results$var_monthly / var_results$var_unhedged
var_results$var_red_futures <- 1 - var_results$var_futures / var_results$var_unhedged

var_results


cost_effectiveness <- pnl_tbl %>%
  transmute(date, S0, ST,
            cost_0dte, cost_m,
            cost_es = margin_used_es,
            net_pnl_0dte, net_pnl_m, net_pnl_es,
            port_pnl) %>%
  
  left_join(
    mdd_tbl %>%
      select(date, dd_base_amt, dd_0dte_amt, dd_m_amt, dd_es_amt,
             dd_red_0dte_pct, dd_red_m_pct, dd_red_es_pct),
    by = "date"
  ) %>%
  mutate(
    loss_base_amt        = pmax(0, -port_pnl),
    loss_hedged_0dte_amt = pmax(0, -net_pnl_0dte),
    loss_hedged_m_amt    = pmax(0, -net_pnl_m),
    loss_hedged_es_amt   = pmax(0, -net_pnl_es),
    
    loss_avoided_0dte = pmax(0, loss_base_amt - loss_hedged_0dte_amt),
    loss_avoided_m    = pmax(0, loss_base_amt - loss_hedged_m_amt),
    loss_avoided_es   = pmax(0, loss_base_amt - loss_hedged_es_amt),
    
    mdd_avoided_0dte = pmax(0, dd_base_amt - dd_0dte_amt),
    mdd_avoided_m    = pmax(0, dd_base_amt - dd_m_amt),
    mdd_avoided_es   = pmax(0, dd_base_amt - dd_es_amt),
    
    cost_per_euro_avoided_0dte = if_else(loss_avoided_0dte > 0,
                                         cost_0dte / loss_avoided_0dte, NA_real_),
    cost_per_euro_avoided_m    = if_else(loss_avoided_m > 0,
                                         cost_m / loss_avoided_m, NA_real_),
    cost_per_euro_avoided_es   = if_else(loss_avoided_es > 0,
                                         cost_es / loss_avoided_es, NA_real_),
    
    cost_per_euro_MDD_avoided_0dte = if_else(mdd_avoided_0dte > 0,
                                             cost_0dte / mdd_avoided_0dte, NA_real_),
    cost_per_euro_MDD_avoided_m    = if_else(mdd_avoided_m > 0,
                                             cost_m / mdd_avoided_m, NA_real_),
    cost_per_euro_MDD_avoided_es   = if_else(mdd_avoided_es > 0,
                                             cost_es / mdd_avoided_es, NA_real_)
  )


premia_table <- premia %>%
  transmute(date, S0, ST,
            vol_0dte = vix1d,
            vol_monthly = vix,
            r_1d, r_1m,
            T_0dte_open, T_m_open,
            put_0dte_open, put_m_open,
            prem_0dte_contract, prem_m_contract)

sizing_table <- size_tbl %>%
  transmute(date, S0,
            contracts_needed,
            n_0dte, cost_0dte, cost_0dte_pct,
            n_m, cost_m, cost_m_pct)

futures_sizing <- fut_size %>%
  transmute(date, S0,
            contracts_needed_es, n_es,
            margin_used_es, cost_es_pct)

results_eod <- pnl_tbl %>%
  transmute(date, S0, ST, port_pnl,
            n_0dte, pnl_0dte, net_pnl_0dte,
            n_m, pnl_m, net_pnl_m,
            n_es, pnl_es, net_pnl_es)

mdd_summary <- mdd_tbl %>%
  transmute(date, S0, Smin, hours_elapsed,
            dd_base_amt, dd_0dte_amt, dd_m_amt, dd_es_amt,
            dd_red_0dte_pct, dd_red_m_pct, dd_red_es_pct)

premia_table
sizing_table
futures_sizing
results_eod
mdd_summary
var_reduction
cost_effectiveness

install.packages("writexl")
library(writexl)

write_xlsx(
  list(
    "Premia_at_Open"      = premia_table,
    "Option_Sizing"       = sizing_table,
    "Futures_Sizing"      = futures_sizing,
    "PnL_EndOfDay"        = results_eod,
    "MDD_TimeValue"       = mdd_summary,
    "Variance_Reduction"  = var_reduction,
    "Cost_Effectiveness"  = cost_effectiveness
  ),
  path = "spx_hedge_results_full_0809iwanttodie_again_2.xlsx"
)

install.packages("ggplot2")
library(ggplot2)


pnl_long <- results_eod
  select(date, net_pnl_0dte, net_pnl_m, net_pnl_es) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "net_pnl")

ggplot(pnl_long, aes(x = date, y = net_pnl, fill = strategy)) +
  geom_col(position = "dodge") +
  labs(title = "Net PnL by Strategy", y = "PnL (Portfolio Currency)") +
  theme_minimal()

mdd_long <- mdd_summary %>%
  select(date, dd_red_0dte_pct, dd_red_m_pct, dd_red_es_pct) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "dd_reduction")

ggplot(mdd_long, aes(x = date, y = dd_reduction, fill = strategy)) +
  geom_col(position = "dodge") +
  labs(title = "MDD Reduction %", y = "Reduction (%)") +
  theme_minimal()

ggplot(var_reduction, aes(x = strategy, y = var_red_vs_unhedged, fill = strategy)) +
  geom_col() +
  labs(title = "Variance Reduction vs Unhedged", y = "Variance Reduction (%)") +
  theme_minimal()


cost_long <- cost_effectiveness %>%
  select(date, cost_0dte, cost_m, cost_es,
         loss_avoided_0dte, loss_avoided_m, loss_avoided_es) %>%
  pivot_longer(-date, names_to = c(".value", "strategy"),
               names_pattern = "(.*)_(.*)")

ggplot(cost_long, aes(x = loss_avoided, y = cost, color = strategy)) +
  geom_point(size = 3) +
  geom_text(aes(label = date), hjust = -0.2) +
  labs(title = "Cost vs Loss Avoided", x = "Loss Avoided", y = "Cost") +
  theme_minimal()
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

strategy_levels <- c("0DTE Put","Monthly Put","ES Futures")
palette_set2 <- scale_fill_brewer(palette = "Set2")

mdd_long <- mdd_summary %>%
  transmute(
    date,
    `0DTE Put`    = dd_red_0dte_pct,
    `Monthly Put` = dd_red_m_pct,
    `ES Futures`  = dd_red_es_pct
  ) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "mdd_red") %>%
  mutate(strategy = factor(strategy, levels = strategy_levels))

p_mdd <- ggplot(mdd_long, aes(x = date, y = mdd_red, fill = strategy)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, na.rm = TRUE) +
  geom_text(aes(label = percent(mdd_red, accuracy = 0.1)),
            position = position_dodge(width = 0.75),
            vjust = -0.35, size = 4, na.rm = TRUE) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
  palette_set2 +
  labs(title = "Max Drawdown Reduction at Intraday Low",
       x = "Event Day", y = "Drawdown Reduction (%)", fill = "Strategy") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
p_mdd  

p_mdd_facet <- ggplot(mdd_long, aes(x = strategy, y = mdd_red, fill = strategy)) +
  geom_col(width = 0.7, na.rm = TRUE) +
  geom_text(aes(label = percent(mdd_red, accuracy = 0.1)),
            vjust = -0.4, size = 4, na.rm = TRUE) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
  palette_set2 +
  labs(title = "Max Drawdown Reduction by Strategy",
       x = NULL, y = "Reduction (%)") +
  facet_wrap(~date, scales = "free_x") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))
p_mdd_facet

p_mdd_facet <- ggplot(mdd_long, aes(x = strategy, y = mdd_red, fill = strategy)) +
  geom_col(width = 0.7, na.rm = TRUE) +
  geom_text(aes(label = percent(mdd_red, accuracy = 0.1)),
            vjust = -0.4, size = 4, na.rm = TRUE) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
  palette_set2 +
  labs(title = "Max Drawdown Reduction by Strategy",
       x = NULL, y = "Reduction (%)") +
  facet_wrap(~date, scales = "free_x") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))
p_mdd_facet

mdd_heat <- mdd_long %>%
  mutate(mdd_red_pct = round(mdd_red*100, 1))

p_mdd_heat <- ggplot(mdd_heat, aes(x = date, y = strategy, fill = mdd_red)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(mdd_red_pct, "%")), size = 3) +
  scale_fill_gradient(low = "white", high = "darkgreen", labels = percent_format(accuracy = 1)) +
  labs(title = "Max Drawdown Reduction (%) - Heatmap",
       x = "Event Day", y = "Strategy", fill = "Reduction") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_mdd_heat


mdd_heat <- mdd_summary %>%
  transmute(
    date = as.character(date),
    `0DTE Put`    = dd_red_0dte_pct,
    `Monthly Put` = dd_red_m_pct,
    `ES Futures`  = dd_red_es_pct
  ) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "mdd_red") %>%
  mutate(mdd_lab = paste0(round(mdd_red*100, 1), "%"))

p_mdd_heat_rot <- ggplot(mdd_heat, aes(x = strategy, y = date, fill = mdd_red)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = mdd_lab), size = 3.8) +
  scale_fill_gradient(low = "white", high = "darkgreen",
                      labels = percent_format(accuracy = 1), limits = c(0,1)) +
  labs(title = "Max Drawdown Reduction (%)",
       x = "Strategy", y = "Event Day", fill = "Reduction") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
p_mdd_heat_rot
install.packages("plotly")
library(plotly)

strategies <- c("0DTE Put","Monthly Put","ES Futures")
dates_chr  <- mdd_summary$date |> as.character()

Z <- mdd_summary %>%
  transmute(
    `0DTE Put`    = dd_red_0dte_pct,
    `Monthly Put` = dd_red_m_pct,
    `ES Futures`  = dd_red_es_pct
  ) %>%
  as.matrix()   

p3d <- plot_ly(
  x = ~seq_along(strategies),   
  y = ~seq_along(dates_chr),    
  z = ~Z,
  type = "surface",
  colors = colorRamp(c("white","darkgreen"))
) %>%
  layout(
    title = "Max Drawdown Reduction – 3D Surface",
    scene = list(
      xaxis = list(title = "Strategy", tickmode = "array",
                   tickvals = 1:length(strategies), ticktext = strategies),
      yaxis = list(title = "Event Day", tickmode = "array",
                   tickvals = 1:length(dates_chr),  ticktext = dates_chr),
      zaxis = list(title = "Reduction", tickformat = ".0%")
    )
  )
p3d

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales)
})

strategy_levels <- c("0DTE Put","Monthly Put","ES Futures")

mdd_heat <- mdd_summary %>%
  transmute(
    date = as.character(date),
    `0DTE Put`    = dd_red_0dte_pct,
    `Monthly Put` = dd_red_m_pct,
    `ES Futures`  = dd_red_es_pct
  ) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "value") %>%
  mutate(strategy = factor(strategy, levels = strategy_levels),
         label = percent(value, 0.1))

p_mdd_heat <- ggplot(mdd_heat, aes(x = strategy, y = date, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.8) +
  scale_fill_gradient(low = "white", high = "#1b9e77",
                      labels = percent_format(accuracy = 1), limits = c(0,1)) +
  labs(title = "Max Drawdown Reduction (%)",
       x = "Strategy", y = "Event Day", fill = "Reduction") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
p_mdd_heat

stopifnot(exists("var_reduction"))

var_heat <- var_reduction %>%
  mutate(date_chr = as.character(.data[["date"]])) %>%
  transmute(
    date = .data[["date_chr"]],
    `0DTE Put`    = var_red_0dte,
    `Monthly Put` = var_red_monthly,
    `ES Futures`  = var_red_futures
  ) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "value") %>%
  mutate(label = percent(value, 0.1))

ggplot(var_heat, aes(x = strategy, y = date, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.8) +
  scale_fill_gradient(low = "white", high = "#2b8cbe",
                      labels = percent_format(accuracy = 1), limits = c(0,1)) +
  labs(title = "Intraday Variance Reduction (%)",
       x = "Strategy", y = "Event Day", fill = "Reduction") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

cost_loss_heat <- cost_effectiveness %>%
  transmute(
    date = as.character(date),
    `0DTE Put`    = cost_per_euro_avoided_0dte,
    `Monthly Put` = cost_per_euro_avoided_m,
    `ES Futures`  = cost_per_euro_avoided_es
  ) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "value") %>%
  filter(is.finite(value), value >= 0) %>%
  mutate(strategy = factor(strategy, levels = strategy_levels),
         label = ifelse(is.finite(value), label_number_si(accuracy = 0.01)(value), ""))

p_cost_loss_heat <- ggplot(cost_loss_heat, aes(x = strategy, y = date, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.6) +
  scale_fill_gradient(low = "#1a9850", high = "#d73027",  # green→red (low cost = good)
                      labels = label_number_si(accuracy = 0.01)) +
  labs(title = "Cost per € of End-of-Day Loss Avoided (lower = better)",
       x = "Strategy", y = "Event Day", fill = "€ / € avoided") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
p_cost_loss_heat

cost_mdd_heat <- cost_effectiveness %>%
  transmute(
    date = as.character(date),
    `0DTE Put`    = cost_per_euro_MDD_avoided_0dte,
    `Monthly Put` = cost_per_euro_MDD_avoided_m,
    `ES Futures`  = cost_per_euro_MDD_avoided_es
  ) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "value") %>%
  filter(is.finite(value), value >= 0) %>%
  mutate(strategy = factor(strategy, levels = strategy_levels),
         label = ifelse(is.finite(value), label_number_si(accuracy = 0.01)(value), ""))


ggplot(cost_loss_heat, aes(x = strategy, y = date, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.6) +
  scale_fill_gradient(
    low = "#1a9850", high = "#d73027",
    labels = scales::label_number(accuracy = 0.01,
                                  scale_cut = scales::cut_si(""))
  ) +
  labs(
    title = "Cost per $ of End-of-Day Loss Avoided",
    x = "Strategy", y = "Event Day", fill = "$ / $ avoided"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

cost_mdd_heat <- cost_effectiveness %>%
  mutate(date_chr = as.character(.data[["date"]])) %>%
  transmute(
    date = .data[["date_chr"]],
    `0DTE Put`    = cost_per_euro_MDD_avoided_0dte,
    `Monthly Put` = cost_per_euro_MDD_avoided_m,
    `ES Futures`  = cost_per_euro_MDD_avoided_es
  ) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "value") %>%
  filter(is.finite(value), value >= 0) %>%
  mutate(label = scales::label_number(accuracy = 0.01,
                                      scale_cut = scales::cut_si(""))(value))

ggplot(cost_mdd_heat, aes(x = strategy, y = date, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.6) +
  scale_fill_gradient(
    low = "#1a9850", high = "#d73027",
    labels = scales::label_number(accuracy = 0.01,
                                  scale_cut = scales::cut_si(""))
  ) +
  labs(
    title = "Cost per $ of MDD Avoided",
    x = "Strategy", y = "Event Day", fill = "$ / $ MDD avoided"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

d <- as.Date("2025-06-18")
row <- cost_effectiveness[cost_effectiveness$date==d, ]

with(row, {
  c0  <- cost_0dte
  la0 <- loss_avoided_0dte
  ratio0 <- c0 / la0
  
  cm  <- cost_m
  lam <- loss_avoided_m
  ratiom <- cm / lam
  
  ces <- cost_es
  laes <- loss_avoided_es
  ratioes <- ces / laes
  
  cbind(
    strategy = c("0DTE","Monthly","Futures"),
    cost = c(c0, cm, ces),
    loss_avoided = c(la0, lam, laes),
    euro_per_euro_avoided = c(ratio0, ratiom, ratioes)
  )
})

fmt_euro <- scales::label_number(accuracy = 0.01) 

cost_loss_heat <- cost_effectiveness %>%
  mutate(date_chr = as.character(date)) %>%
  transmute(
    date = date_chr,
    `0DTE Put`    = if_else(loss_avoided_0dte > 0, cost_0dte / loss_avoided_0dte, NA_real_),
    `Monthly Put` = if_else(loss_avoided_m    > 0, cost_m    / loss_avoided_m,    NA_real_),
    `ES Futures`  = if_else(loss_avoided_es   > 0, cost_es   / loss_avoided_es,   NA_real_)
  ) %>%
  tidyr::pivot_longer(-date, names_to = "strategy", values_to = "value") %>%
  dplyr::mutate(label = ifelse(is.na(value), "N/A", fmt_euro(value)))

ggplot(cost_loss_heat, aes(strategy, date, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.6) +
  scale_fill_gradient(low = "#1a9850", high = "#d73027", labels = fmt_euro, na.value = "grey85") +
  labs(title = "Capital Required per $ of End-of-Day Loss Avoided",
       subtitle = "Grey = N/A (no base loss or no improvement)",
       x = "Strategy", y = "Event Day", fill = "$ / $ avoided") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

eff_loss_heat <- cost_effectiveness %>%
  mutate(date_chr = as.character(date)) %>%
  transmute(
    date = date_chr,
    `0DTE Put`    = if_else(cost_0dte > 0, loss_avoided_0dte / cost_0dte, NA_real_),
    `Monthly Put` = if_else(cost_m    > 0, loss_avoided_m    / cost_m,    NA_real_),
    `ES Futures`  = if_else(cost_es   > 0, loss_avoided_es   / cost_es,   NA_real_)
  ) %>%
  tidyr::pivot_longer(-date, names_to = "strategy", values_to = "value") %>%
  dplyr::mutate(label = ifelse(is.na(value), "N/A", scales::label_percent(accuracy = 0.1)(value)))



cost_mdd_heat <- cost_effectiveness %>%
  mutate(date_chr = as.character(.data[["date"]])) %>%
  transmute(
    date = .data[["date_chr"]],
    `0DTE Put`    = cost_per_euro_MDD_avoided_0dte,
    `Monthly Put` = cost_per_euro_MDD_avoided_m,
    `ES Futures`  = cost_per_euro_MDD_avoided_es
  ) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "value") %>%
  filter(is.finite(value), value >= 0) %>%
  mutate(label = sprintf("%.2f", value))   
ggplot(cost_mdd_heat, aes(x = strategy, y = date, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.6) +
  scale_fill_gradient(
    low = "#1a9850", high = "#d73027",
    labels = scales::label_number(accuracy = 0.01) 
  ) +
  labs(
    title = "Capital Required per $ of MDD Avoided",
    x = "Strategy", y = "Event Day", fill = "$ / $ MDD avoided"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

day_meta <- spx %>%
  filter(date %in% event_days_available) %>%
  group_by(date) %>%
  summarise(
    open_time     = min(dt),
    close_time    = max(dt),
    S0            = first(price),
    session_hours = as.numeric(difftime(close_time, open_time, units = "hours")),
    .groups = "drop"
  )

sizes <- size_tbl %>%
  select(date, n_0dte, n_m, cost_0dte, cost_m)

futs  <- fut_size %>%
  select(date, n_es)

need_inputs <- inputs %>%
  select(date, K_0dte, K_m, r_1d, r_1m, vix1d, vix, T_m_open)

spx_intraday <- spx %>%
  filter(date %in% event_days_available) %>%
  left_join(day_meta,    by = "date") %>%
  left_join(sizes,       by = "date") %>%
  left_join(futs,        by = "date") %>%
  left_join(need_inputs, by = "date") %>%
  mutate(

    hours_elapsed   = as.numeric(difftime(dt, open_time, units = "hours")),
    
    T_remain_0dte   = pmax((session_hours/(365*24)) - hours_elapsed/(365*24), 1e-8),
    T_remain_m      = pmax(T_m_open - hours_elapsed/(365*24), 1e-8),
    
    port_ret        = (price - S0)/S0,
    port_val        = portfolio_notional * (1 + port_ret),
  
    val_0dte_hedge  = n_0dte * bs_put(price, K_0dte, r_1d, vix1d, T_remain_0dte) * opt_multiplier * fx_usd_to_portfolio,
    val_m_hedge     = n_m    * bs_put(price, K_m,    r_1m,  vix,   T_remain_m)    * opt_multiplier * fx_usd_to_portfolio,
    val_es_hedge    = n_es   * (S0 - price) * es_multiplier * fx_usd_to_portfolio,
  
    val_unhedged    = port_val,
    val_0dte        = port_val + val_0dte_hedge - cost_0dte,
    val_m           = port_val + val_m_hedge    - cost_m,
    val_es          = port_val + val_es_hedge  
    
paths_long <- spx_intraday %>%
  select(date, dt,
         `Unhedged` = val_unhedged,
         `0DTE Put` = val_0dte,
         `Monthly Put` = val_m,
         `Futures` = val_es) %>%
  pivot_longer(-c(date, dt), names_to = "Strategy", values_to = "Value")

ggplot(paths_long, aes(x = dt, y = Value, color = Strategy)) +
  geom_line(size = 0.7) +
  facet_wrap(~ date, scales = "free_x", ncol = 2) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, big.mark = ",")) +
  labs(title = "Intraday Portfolio Paths",
       x = NULL, y = "Value") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")


names(spx_intraday)

var_reduction_daily <- spx_intraday %>%
  group_by(date) %>%
  summarise(
    var_unhedged = var(port_base, na.rm = TRUE),
    var_0dte     = var(val_0dte, na.rm = TRUE),
    var_m        = var(val_m, na.rm = TRUE),
    var_es       = var(val_es, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    red_0dte = 1 - (var_0dte / var_unhedged),
    red_m    = 1 - (var_m / var_unhedged),
    red_es   = 1 - (var_es / var_unhedged)
  ) %>%
  select(date, red_0dte, red_m, red_es)

p
var_reduction_long <- var_reduction_daily %>%
  mutate(date_chr = as.character(date)) %>%
  pivot_longer(
    cols = starts_with("red_"),
    names_to = "strategy",
    values_to = "value"
  ) %>%
  mutate(
    strategy = recode(strategy,
                      red_0dte = "0DTE Put",
                      red_m    = "Monthly Put",
                      red_es   = "ES Futures"),
    value_pct = value * 100,  
    label = paste0(round(value_pct, 1), "%")  
  )


ggplot(var_reduction_long, aes(x = strategy, y = date_chr, fill = value_pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.6) +
  scale_fill_gradient(
    low = "#f7fbff", high = "#08306b",
    labels = function(x) paste0(round(x, 1), "%")
  ) +
  labs(
    title = "Variance Reduction (%)",
    x = "Strategy", y = "Event Day", fill = "Reduction (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))