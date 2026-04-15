# =============================================================================
# SECTOR MOMENTUM STRATEGY — CROSS-SECTIONAL RELATIVE STRENGTH
# Universe: SPY, QQQ, XLF, XLK, XLE, XLV
# Strategy: Long top-N momentum sectors, vol-targeted, drawdown-controlled
# Benchmark: SPY Buy & Hold
# =============================================================================

library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(dplyr)
library(xts)

####### 0. PARAMETERS — all in one place for easy sensitivity analysis ########
START_DATE       <- "2018-01-01"
LOOKBACK         <- 90        # momentum lookback (trading days)
TOP_N            <- 2         # number of sectors held at any time
TARGET_VOL       <- 0.15      # annualized vol target (15%)
VOL_WINDOW       <- 30        # rolling vol estimation window
MAX_DRAWDOWN     <- -0.10     # drawdown circuit breaker
TRANSACTION_COST <- 0.0010    # 10 bps round-trip (realistic for ETFs)
SPLIT_DATE       <- "2021-01-01"

#################### 1. DATA ACQUISITION & CLEANING ###########################
tickers <- c("SPY", "QQQ", "XLF", "XLK", "XLE", "XLV")

suppressWarnings(
  getSymbols(tickers, from = START_DATE, auto.assign = TRUE)
)

# Adjusted close prices — accounts for dividends and splits
prices <- do.call(merge, lapply(tickers, function(x) Ad(get(x))))
colnames(prices) <- tickers

# Sanity check — flag and fill any missing data rather than silently dropping
na_counts <- colSums(is.na(prices))
if (any(na_counts > 0)) {
  warning("Missing data detected: ", paste(names(na_counts[na_counts > 0]),
                                           na_counts[na_counts > 0], sep = "=", collapse = ", "))
  prices <- na.locf(prices, na.rm = TRUE)  # forward-fill gaps (e.g. holidays)
}

# Arithmetic returns for P&L, log returns available if needed
returns <- na.omit(Return.calculate(prices, method = "discrete"))


########## 2. MOMENTUM SIGNAL — 90-DAY CROSS-SECTIONAL PRICE MOMENTUM #########
# ROC = (P_t / P_{t-n}) - 1, ranked cross-sectionally each day
momentum    <- ROC(prices, n = LOOKBACK, type = "discrete")
momentum_xts <- xts(momentum, order.by = index(prices))

# Cross-sectional rank (1 = weakest, 6 = strongest)
ranks <- t(apply(coredata(momentum_xts), 1, rank, na.last = "keep"))
ranks <- xts(ranks, order.by = index(momentum_xts))
colnames(ranks) <- tickers


####### 3. PORTFOLIO CONSTRUCTION — EQUAL-WEIGHT TOP-N MOMENTUM SECTORS ########
# Strip to matrix first — ifelse doesn't play well with xts internals
weights_raw <- ifelse(coredata(ranks) >= (ncol(ranks) - TOP_N + 1), 1, 0)

# Normalize to sum to 1 (equal weight among selected)
row_sums <- rowSums(weights_raw, na.rm = TRUE)
row_sums[row_sums == 0] <- NA
weights_norm <- weights_raw / row_sums

# Re-attach the xts index AFTER all matrix operations are done
weights_xts <- xts(weights_norm, order.by = index(ranks))
weights_xts <- lag(weights_xts, k = 1)


####################### 4. STRATEGY RETURNS ###################################
common_dates    <- index(weights_xts)[index(weights_xts) %in% index(returns)]
weights_aligned <- weights_xts[common_dates]
returns_aligned <- returns[common_dates]

# Gross daily strategy return
gross_returns <- xts(
  rowSums(weights_aligned * returns_aligned, na.rm = TRUE),
  order.by = common_dates
)


#################### 5. TRANSACTION COST MODEL ################################
# Estimate daily turnover — penalize only on position changes
weight_changes  <- abs(diff(weights_aligned))
daily_turnover  <- xts(rowSums(weight_changes, na.rm = TRUE),
                       order.by = index(weight_changes))
daily_tc        <- daily_turnover * TRANSACTION_COST

# Align and deduct costs
common_dates_tc <- index(gross_returns)[index(gross_returns) %in% index(daily_tc)]
net_returns     <- gross_returns[common_dates_tc] - daily_tc[common_dates_tc]


######## 6. VOLATILITY TARGETING — SCALE TO 15% ANNUALIZED VOL ################
# Annualize daily rolling vol (sqrt(252) convention)
rolling_vol_daily <- runSD(net_returns, n = VOL_WINDOW)
rolling_vol_ann   <- rolling_vol_daily * sqrt(252)

# Clamp scalar to [0.5, 2.0] — avoid extreme leverage or near-zero exposure
vol_scalar      <- TARGET_VOL / rolling_vol_ann
vol_scalar      <- pmin(pmax(vol_scalar, 0.5), 2.0)
scaled_returns  <- net_returns * vol_scalar


######## 7. DRAWDOWN CIRCUIT BREAKER — FLAT IF DD EXCEEDS THRESHOLD ############
# Uses GROSS cumulative returns to measure drawdown, kills exposure on breach
cum_returns  <- cumprod(1 + na.omit(scaled_returns))
drawdown     <- cum_returns / cummax(cum_returns) - 1

# Zero out returns on days where drawdown exceeds threshold
# Re-enter when drawdown recovers above threshold
scaled_returns[index(scaled_returns) %in% index(drawdown[drawdown < MAX_DRAWDOWN])] <- 0


################# 8. BENCHMARK & COMBINED SERIES ##############################
spy_returns      <- returns[, "SPY"]
combined_returns <- na.omit(merge(scaled_returns, spy_returns))
colnames(combined_returns) <- c("Momentum Strategy", "SPY B&H")


################## 9. PERFORMANCE REPORTING ##################################
cat("\n========== FULL PERIOD PERFORMANCE ==========\n")
print(table.AnnualizedReturns(combined_returns))

cat("\n========== RISK METRICS ==========\n")
cat("Max Drawdown:\n")
print(maxDrawdown(combined_returns))
cat("\nSharpe Ratio (Annualized):\n")
print(SharpeRatio.annualized(combined_returns, Rf = 0.05/252))  # risk-free ~5%
cat("\nCalmar Ratio:\n")
print(CalmarRatio(combined_returns))
cat("\nSortino Ratio:\n")
print(SortinoRatio(combined_returns))

cat("\n========== MONTHLY RETURNS TABLE ==========\n")
print(table.CalendarReturns(combined_returns[, "Momentum Strategy"]))

# Cumulative performance, drawdown, and monthly returns
charts.PerformanceSummary(
  combined_returns,
  main     = "Sector Momentum Strategy vs SPY",
  colorset = c("#003366", "#CC0000"),
  legend.loc = "topleft"
)


########## WALK-FORWARD VALIDATION — IN-SAMPLE vs OUT-OF-SAMPLE ################
train <- combined_returns[index(combined_returns) <  SPLIT_DATE]
test  <- combined_returns[index(combined_returns) >= SPLIT_DATE]

cat("\n========== IN-SAMPLE (", START_DATE, "to", SPLIT_DATE, ") ==========\n")
print(table.AnnualizedReturns(train))

cat("\n========== OUT-OF-SAMPLE (", SPLIT_DATE, "to present) ==========\n")
print(table.AnnualizedReturns(test))

cat("\n========== SHARPE DEGRADATION (in vs out) ==========\n")
sharpe_in  <- SharpeRatio.annualized(train,  Rf = 0.05/252)[1,]
sharpe_out <- SharpeRatio.annualized(test,   Rf = 0.05/252)[1,]
degradation <- (sharpe_out - sharpe_in) / abs(sharpe_in) * 100
cat(sprintf("  Strategy Sharpe degradation: %.1f%%\n", degradation["Momentum Strategy"]))
cat(sprintf("  SPY Sharpe degradation:      %.1f%%\n", degradation["SPY B&H"]))