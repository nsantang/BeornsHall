# ============================================================================
#  DISCOUNTED CASH FLOW VALUATION MODEL
#  Two-Stage DCF | CAPM-Derived WACC | Gordon Growth Terminal Value
#  Sensitivity Analysis | Publication-Quality Output
# ============================================================================

#  Description:
#    Full-featured DCF engine that fetches live financial data via the
#    Alpha Vantage API, computes a CAPM-derived discount rate, runs a
#    two-stage free cash flow projection, and produces a multi-panel
#    presentation-quality report including a WACC x TGR sensitivity
#    heatmap and an EV bridge chart.

#  Dependencies (install once):
install.packages(c("httr","jsonlite","dplyr","ggplot2","scales",
                       "lubridate","tidyquant","patchwork","ggtext",
                       "glue","gt"))
install.packages("tidyquant", dependencies = TRUE)

# ============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(scales)
  library(lubridate)
  library(tidyquant)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(gt)
})

# ── Configuration ─────────────────────────────────────────────────────────────

AV_API_KEY          <-  "G5AOFWKF540MIQ2H"   
MARKET_RISK_PREMIUM <- 0.055                 # Damodaran (2024): US ERP
TERMINAL_GROWTH     <- 0.025                 # Long-run nominal GDP growth
FORECAST_YEARS_DEF  <- 5L                    # Default forecast horizon

# ── Colour Palette (JPMorgan-inspired; works in light & dark exports) ─────────

COLS <- list(
  navy      = "#00274C",
  blue      = "#0061A0",
  midblue   = "#4A90C4",
  lightblue = "#BDD7EE",
  teal      = "#1D7A6B",
  green     = "#2E7D32",
  amber     = "#E65100",
  red       = "#B71C1C",
  grey90    = "#1A1A2E",
  grey60    = "#555566",
  grey30    = "#AAAABC",
  grey10    = "#F4F5F7",
  white     = "#FFFFFF"
)

# ── Theme ─────────────────────────────────────────────────────────────────────

theme_jpm <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text               = element_text(colour = COLS$grey90, family = "sans"),
      plot.background    = element_rect(fill = COLS$white, colour = NA),
      panel.background   = element_rect(fill = COLS$white, colour = NA),
      panel.grid.major   = element_line(colour = "#E8E9EC", linewidth = 0.35),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = COLS$grey60, size = rel(0.85)),
      axis.title         = element_text(colour = COLS$grey60, size = rel(0.9)),
      axis.line.x        = element_line(colour = "#CCCCDD", linewidth = 0.4),
      axis.ticks         = element_blank(),
      plot.title         = element_text(face = "bold", size = rel(1.15),
                                        colour = COLS$navy, margin = margin(b = 4)),
      plot.subtitle      = element_text(size = rel(0.88), colour = COLS$grey60,
                                        margin = margin(b = 10)),
      plot.caption       = element_text(size = rel(0.72), colour = COLS$grey30,
                                        hjust = 0, margin = margin(t = 8)),
      legend.position    = "bottom",
      legend.key.size    = unit(0.45, "cm"),
      legend.text        = element_text(size = rel(0.82), colour = COLS$grey60),
      legend.title       = element_blank(),
      strip.text         = element_text(face = "bold", colour = COLS$navy,
                                        size = rel(0.9))
    )
}

# ── Helper: safe API fetch ─────────────────────────────────────────────────────

av_get <- function(params) {
  params[["apikey"]] <- AV_API_KEY
  url <- modify_url("https://www.alphavantage.co/query", query = params)
  resp <- GET(url, timeout(20))
  stop_for_status(resp)
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
  if (!is.null(data[["Note"]]))
    stop("Alpha Vantage rate limit hit. Wait 60s or upgrade your API key.")
  if (!is.null(data[["Information"]]))
    stop("Alpha Vantage: ", data[["Information"]])
  data
}

# ── 1. Market Data ─────────────────────────────────────────────────────────────

#' Fetch daily adjusted prices and compute historical CAGR
get_price_cagr <- function(symbol, years) {
  message(glue("  Fetching {years}-year price history for {symbol}..."))
  prices <- tq_get(symbol, get = "stock.prices",
                   from = Sys.Date() - years(years + 1))
  prices <- prices %>% arrange(date)
  cutoff <- max(prices$date) %m-% years(years)
  window  <- prices %>% filter(date >= cutoff)
  start_p <- window$adjusted[1]
  end_p   <- window$adjusted[nrow(window)]
  list(
    cagr   = (end_p / start_p)^(1 / years) - 1,
    latest = tail(prices$adjusted, 1),
    prices = prices
  )
}

#' Fetch 10-year US Treasury yield (risk-free rate)
get_risk_free_rate <- function() {
  message("  Fetching 10-year Treasury yield...")
  
  data <- av_get(list(
    `function` = "TREASURY_YIELD",
    interval = "daily",
    maturity = "10year"
  ))
  
  as.numeric(data$data$value[1]) / 100
}

#' Fetch company overview (beta, name, sector, market cap, P/E)
get_company_overview <- function(symbol) {
  message(glue("  Fetching company overview for {symbol}..."))
  
  data <- av_get(list(
    `function` = "OVERVIEW",
    symbol = symbol
  ))
  list(
    name     = data$Name,
    sector   = data$Sector,
    beta     = as.numeric(data$Beta),
    mktcap   = as.numeric(data$MarketCapitalization),
    pe       = as.numeric(data$PERatio),
    ev_ebitda= as.numeric(data$EVToEBITDA),
    fwd_pe   = as.numeric(data$ForwardPE),
    div_yield= as.numeric(data$DividendYield),
    eps      = as.numeric(data$EPS)
  )
}

# ── 2. Financials ──────────────────────────────────────────────────────────────

#' Fetch annual cash flow statements (last 5 years)
get_cash_flows <- function(symbol) {
  message(glue("  Fetching cash flow statements for {symbol}..."))
  
  data <- av_get(list(
    `function` = "CASH_FLOW",
    symbol = symbol
  ))
  
  reports <- data$annualReports
  
  if (is.null(reports) || length(reports) == 0) {
    return(tibble())
  }
  
  reports %>%
    dplyr::as_tibble() %>%
    dplyr::transmute(
      date  = as.Date(fiscalDateEnding),
      OCF   = as.numeric(operatingCashflow),
      CapEx = as.numeric(capitalExpenditures),
      FCF   = dplyr::if_else(
        !is.na(OCF) & !is.na(CapEx),
        OCF - abs(CapEx),
        NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::desc(date))
}


#' Fetch annual balance sheet (most recent year)
get_balance_sheet <- function(symbol) {
  message(glue("  Fetching balance sheet for {symbol}..."))
  
  data <- av_get(list(
    `function` = "BALANCE_SHEET",
    symbol = symbol
  ))
  
  reports <- data$annualReports
  
  if (is.null(reports) || length(reports) == 0) {
    return(NULL)
  }
  
  latest <- reports[1, ]
  
  list(
    total_debt         = as.numeric(latest$shortLongTermDebtTotal),
    cash               = as.numeric(latest$cashAndCashEquivalentsAtCarryingValue),
    shares_outstanding = as.numeric(latest$commonStockSharesOutstanding),
    total_assets       = as.numeric(latest$totalAssets),
    total_equity       = as.numeric(latest$totalShareholderEquity)
  )
}


# ── 3. WACC ────────────────────────────────────────────────────────────────────

#' Compute CAPM cost of equity
#'   Re = Rf + β × ERP
compute_wacc <- function(beta, rf, mrp = MARKET_RISK_PREMIUM,
                         bs, overview) {
  cost_of_equity <- rf + beta * mrp

  # Simple WACC if we can compute debt/equity split from balance sheet
  equity_val <- overview$mktcap
  debt_val   <- bs$total_debt
  total_cap  <- equity_val + debt_val

  # Assume pre-tax cost of debt ≈ 10Y Treasury + 150bps credit spread
  # and marginal tax rate of 21% (US statutory)
  kd      <- rf + 0.015
  tax     <- 0.21
  w_e     <- equity_val / total_cap
  w_d     <- debt_val   / total_cap
  wacc    <- w_e * cost_of_equity + w_d * kd * (1 - tax)

  list(
    cost_of_equity = cost_of_equity,
    cost_of_debt   = kd,
    wacc           = wacc,
    w_equity       = w_e,
    w_debt         = w_d,
    tax_rate       = tax,
    beta           = beta,
    rf             = rf
  )
}

# ── 4. DCF Engine ──────────────────────────────────────────────────────────────

#' Project FCFs and compute present values
project_fcfs <- function(fcf_base, growth_rate, discount_rate, n_years) {
  years  <- seq_len(n_years)
  fcfs   <- fcf_base * cumprod(rep(1 + growth_rate, n_years))
  pv_fcf <- fcfs / (1 + discount_rate)^years
  tibble(year = years, fcf_projected = fcfs, pv_fcf = pv_fcf)
}

#' Gordon Growth terminal value (present value)
calc_terminal_value <- function(last_fcf, g = TERMINAL_GROWTH, r, n) {
  if (g >= r) stop("Terminal growth rate must be strictly less than discount rate.")
  tv    <- (last_fcf * (1 + g)) / (r - g)
  pv_tv <- tv / (1 + r)^n
  list(tv = tv, pv_tv = pv_tv)
}

#' Full DCF valuation
run_dcf <- function(fcf_base, growth_rate, discount_rate, terminal_growth,
                    n_years, net_debt, shares_outstanding) {
  proj   <- project_fcfs(fcf_base, growth_rate, discount_rate, n_years)
  tv_res <- calc_terminal_value(tail(proj$fcf_projected, 1),
                                g = terminal_growth,
                                r = discount_rate,
                                n = n_years)
  sum_pv_fcf <- sum(proj$pv_fcf)
  ev         <- sum_pv_fcf + tv_res$pv_tv
  equity_val <- ev - net_debt
  iv_per_share <- equity_val / shares_outstanding

  list(
    projections    = proj,
    pv_tv          = tv_res$pv_tv,
    terminal_value = tv_res$tv,
    sum_pv_fcf     = sum_pv_fcf,
    enterprise_value = ev,
    equity_value   = equity_val,
    iv_per_share   = iv_per_share,
    tv_pct_of_ev   = tv_res$pv_tv / ev
  )
}

# ── 5. Sensitivity Analysis ────────────────────────────────────────────────────

#' WACC × Terminal Growth Rate sensitivity grid
sensitivity_grid <- function(fcf_base, growth_rate, base_wacc, base_tgr,
                              n_years, net_debt, shares_outstanding,
                              wacc_range = seq(-0.02, 0.02, by = 0.005),
                              tgr_range  = seq(-0.01, 0.015, by = 0.005)) {
  grid <- expand.grid(
    wacc_delta = wacc_range,
    tgr_delta  = tgr_range
  ) %>%
    mutate(
      wacc = base_wacc + wacc_delta,
      tgr  = base_tgr  + tgr_delta
    ) %>%
    filter(tgr < wacc) %>%
    rowwise() %>%
    mutate(
      iv = tryCatch(
        run_dcf(fcf_base, growth_rate, wacc, tgr,
                n_years, net_debt, shares_outstanding)$iv_per_share,
        error = function(e) NA_real_
      )
    ) %>%
    ungroup()
  grid
}

# ── 6. Plots ───────────────────────────────────────────────────────────────────

#' Panel A: Historical + projected FCF bar chart
plot_fcf <- function(historical_cf, projections, symbol) {
  hist <- historical_cf %>%
    filter(!is.na(FCF)) %>%
    arrange(date) %>%
    tail(5) %>%
    mutate(type = "Historical", year_label = format(date, "%Y"))

  proj <- projections %>%
    mutate(
      date       = max(hist$date) %m+% years(year),
      year_label = format(date, "%Y"),
      FCF        = fcf_projected,
      type       = "Projected"
    ) %>%
    select(date, year_label, FCF, type)

  combined <- bind_rows(hist %>% select(date, year_label, FCF, type), proj)

  ggplot(combined, aes(x = year_label, y = FCF / 1e9,
                       fill = type, alpha = type)) +
    geom_col(width = 0.65, colour = NA) +
    geom_text(aes(label = sprintf("$%.1fB", FCF / 1e9),
                  vjust = ifelse(FCF >= 0, -0.4, 1.3)),
              size = 3, colour = COLS$grey60, fontface = "plain") +
    scale_fill_manual(values = c("Historical" = COLS$midblue,
                                 "Projected"  = COLS$blue)) +
    scale_alpha_manual(values = c("Historical" = 0.65, "Projected" = 1)) +
    scale_y_continuous(labels = label_dollar(suffix = "B", scale = 1),
                       expand = expansion(mult = c(0.05, 0.18))) +
    labs(
      title    = "Free Cash Flow: Historical & Projected",
      subtitle = glue("{symbol} | Shaded bars = DCF forecast period"),
      x = NULL, y = "FCF (USD billions)",
      caption  = "Source: Alpha Vantage. FCF = Operating Cash Flow less Capital Expenditures."
    ) +
    theme_jpm() +
    theme(legend.position = "bottom",
          panel.grid.major.x = element_blank())
}

#' Panel B: Enterprise Value bridge (waterfall)
plot_ev_bridge <- function(dcf_result, symbol) {
  pv_fcf <- dcf_result$sum_pv_fcf
  pv_tv  <- dcf_result$pv_tv
  ev     <- dcf_result$enterprise_value
  nd     <- ev - dcf_result$equity_value
  eq     <- dcf_result$equity_value

  # Build waterfall segments
  bridge <- tibble(
    label  = c("PV of FCFs", "Terminal Value", "Enterprise Value",
                "Less: Net Debt", "Equity Value"),
    value  = c(pv_fcf, pv_tv, ev, -nd, eq),
    start  = c(0, pv_fcf, 0, eq, 0),
    end    = c(pv_fcf, ev, ev, ev, eq),
    type   = c("add", "add", "total", "sub", "total")
  ) %>%
    mutate(label = factor(label, levels = label))

  fill_map <- c(add = COLS$teal, sub = COLS$amber, total = COLS$navy)
  alpha_map <- c(add = 0.85, sub = 0.85, total = 1)

  ggplot(bridge, aes(x = label)) +
    geom_rect(aes(xmin = as.integer(label) - 0.35,
                  xmax = as.integer(label) + 0.35,
                  ymin = start / 1e9,
                  ymax = end   / 1e9,
                  fill = type, alpha = type),
              colour = NA) +
    geom_segment(data = bridge %>% filter(type != "total"),
                 aes(x    = as.integer(label) + 0.35,
                     xend = as.integer(label) + 0.65,
                     y    = end / 1e9,
                     yend = end / 1e9),
                 colour = COLS$grey30, linewidth = 0.35, linetype = "dashed") +
    geom_text(aes(y     = (start + end) / 2e9,
                  label = sprintf("$%.1fB", abs(value) / 1e9)),
              size = 3.1, colour = COLS$white, fontface = "bold") +
    scale_x_discrete() +
    scale_y_continuous(labels = label_dollar(suffix = "B", scale = 1),
                       expand = expansion(mult = c(0.05, 0.10))) +
    scale_fill_manual(values  = fill_map)  +
    scale_alpha_manual(values = alpha_map) +
    labs(
      title    = "Enterprise Value to Equity Value Bridge",
      subtitle = glue("{symbol} | Gordon Growth terminal value"),
      x = NULL, y = "USD billions",
      caption  = "Enterprise Value = Sum of PV(FCFs) + PV(Terminal Value). Equity Value = EV – Net Debt."
    ) +
    theme_jpm() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank())
}

#' Panel C: Sensitivity heatmap
plot_sensitivity <- function(sens_grid, base_wacc, base_tgr, base_iv, symbol) {
  plot_data <- sens_grid %>%
    mutate(
      wacc_label = paste0(round(wacc * 100, 1), "%"),
      tgr_label  = paste0(round(tgr  * 100, 1), "%"),
      is_base    = abs(wacc - base_wacc) < 0.001 & abs(tgr - base_tgr) < 0.001,
      pct_diff   = (iv - base_iv) / base_iv
    )

  wacc_order <- plot_data %>% distinct(wacc) %>% arrange(desc(wacc)) %>%
    pull(wacc) %>% {paste0(round(. * 100, 1), "%")}
  tgr_order  <- plot_data %>% distinct(tgr)  %>% arrange(tgr)  %>%
    pull(tgr)  %>% {paste0(round(. * 100, 1), "%")}

  plot_data <- plot_data %>%
    mutate(wacc_label = factor(wacc_label, levels = wacc_order),
           tgr_label  = factor(tgr_label,  levels = tgr_order))

  ggplot(plot_data, aes(x = tgr_label, y = wacc_label, fill = pct_diff)) +
    geom_tile(colour = COLS$white, linewidth = 0.6) +
    geom_text(aes(label    = if_else(is.na(iv), "—",
                                     paste0("$", round(iv, 2))),
                  fontface = if_else(is_base, "bold", "plain"),
                  colour   = if_else(is_base, COLS$white, COLS$grey90)),
              size = 3.2) +
    geom_tile(data = plot_data %>% filter(is_base),
              fill = NA, colour = COLS$navy, linewidth = 1.1) +
    scale_fill_gradient2(
      low      = COLS$red,
      mid      = COLS$grey10,
      high     = COLS$green,
      midpoint = 0,
      na.value = "#EEEEEE",
      labels   = label_percent(),
      name     = "vs. base case"
    ) +
    scale_colour_identity() +
    labs(
      title    = "Sensitivity: Intrinsic Value per Share",
      subtitle = glue("{symbol} | WACC (rows) × Terminal Growth Rate (columns) | Base case outlined"),
      x        = "Terminal Growth Rate",
      y        = "WACC",
      caption  = "Red = lower intrinsic value vs. base case. Green = higher. Box = base case."
    ) +
    theme_jpm() +
    theme(
      panel.grid  = element_blank(),
      axis.line.x = element_blank(),
      legend.key.width = unit(1.5, "cm")
    )
}

#' Panel D: FCF present value decomposition (contribution by year + TV)
plot_pv_decomposition <- function(dcf_result, symbol) {
  proj <- dcf_result$projections %>%
    mutate(label = paste0("Year ", year), component = "PV of FCFs")

  tv_row <- tibble(
    year = max(proj$year) + 1, pv_fcf = dcf_result$pv_tv,
    label = "Terminal\nValue", component = "Terminal Value"
  )

  all <- bind_rows(proj %>% select(year, pv_fcf, label, component), tv_row) %>%
    mutate(
      pct   = pv_fcf / dcf_result$enterprise_value,
      label = factor(label, levels = label)
    )

  fill_map <- c("PV of FCFs" = COLS$midblue, "Terminal Value" = COLS$navy)

  ggplot(all, aes(x = label, y = pv_fcf / 1e9, fill = component)) +
    geom_col(width = 0.6, colour = NA) +
    geom_text(aes(label = sprintf("%.1f%%", pct * 100),
                  vjust = -0.5),
              size = 3, colour = COLS$grey60) +
    scale_fill_manual(values = fill_map) +
    scale_y_continuous(labels = label_dollar(suffix = "B", scale = 1),
                       expand = expansion(mult = c(0.02, 0.15))) +
    labs(
      title    = "Present Value Decomposition",
      subtitle = glue("{symbol} | % labels = share of Enterprise Value"),
      x = NULL, y = "Present Value (USD billions)",
      caption  = "Terminal value typically 60–80% of EV in a well-calibrated model."
    ) +
    theme_jpm() +
    theme(panel.grid.major.x = element_blank())
}

# ── 7. Summary Console Output ──────────────────────────────────────────────────

print_summary <- function(symbol, overview, wacc_res, dcf_result,
                          growth_rate, terminal_growth, n_years, market_price) {
  iv  <- dcf_result$iv_per_share
  upside <- if (!is.na(market_price)) (iv - market_price) / market_price else NA

  cat("\n")
  cat(strrep("═", 62), "\n")
  cat(sprintf("  DCF VALUATION SUMMARY — %s\n", toupper(symbol)))
  cat(sprintf("  %s | %s\n", overview$name, overview$sector))
  cat(strrep("═", 62), "\n\n")

  cat("  ── KEY ASSUMPTIONS ──────────────────────────────────────\n")
  cat(sprintf("  Risk-Free Rate (10Y UST)      %6.2f%%\n", wacc_res$rf * 100))
  cat(sprintf("  Beta                          %6.2f\n",  wacc_res$beta))
  cat(sprintf("  Market Risk Premium           %6.2f%%\n", MARKET_RISK_PREMIUM * 100))
  cat(sprintf("  Cost of Equity (CAPM)         %6.2f%%\n", wacc_res$cost_of_equity * 100))
  cat(sprintf("  Cost of Debt (after-tax)      %6.2f%%\n", wacc_res$cost_of_debt * (1 - wacc_res$tax_rate) * 100))
  cat(sprintf("  WACC                          %6.2f%%\n", wacc_res$wacc * 100))
  cat(sprintf("  FCF Growth Rate (Stage 1)     %6.2f%%\n", growth_rate * 100))
  cat(sprintf("  Terminal Growth Rate          %6.2f%%\n", terminal_growth * 100))
  cat(sprintf("  Forecast Horizon              %6d  years\n", n_years))

  cat("\n  ── VALUATION OUTPUT ─────────────────────────────────────\n")
  cat(sprintf("  PV of FCFs                    %s\n",
              scales::dollar(dcf_result$sum_pv_fcf, scale = 1e-9,
                             suffix = "B", accuracy = 0.1)))
  cat(sprintf("  PV of Terminal Value          %s  (%.0f%% of EV)\n",
              scales::dollar(dcf_result$pv_tv, scale = 1e-9,
                             suffix = "B", accuracy = 0.1),
              dcf_result$tv_pct_of_ev * 100))
  cat(sprintf("  Enterprise Value              %s\n",
              scales::dollar(dcf_result$enterprise_value, scale = 1e-9,
                             suffix = "B", accuracy = 0.1)))
  cat(sprintf("  Equity Value                  %s\n",
              scales::dollar(dcf_result$equity_value, scale = 1e-9,
                             suffix = "B", accuracy = 0.1)))
  cat(sprintf("  Intrinsic Value / Share       %s\n",
              scales::dollar(iv, accuracy = 0.01)))

  if (!is.na(market_price)) {
    direction <- if (upside > 0) "UPSIDE" else "DOWNSIDE"
    cat(sprintf("  Market Price                  %s\n",
                scales::dollar(market_price, accuracy = 0.01)))
    cat(sprintf("  Implied %-8s             %+.1f%%\n", direction, upside * 100))
    verdict <- dplyr::case_when(
      upside >  0.20 ~ "STRONG BUY",
      upside >  0.05 ~ "BUY",
      upside > -0.05 ~ "HOLD",
      upside > -0.20 ~ "UNDERPERFORM",
      TRUE           ~ "SELL"
    )
    cat(sprintf("  Indicative Signal             %s\n", verdict))
  }

  cat("\n  ── COMPARABLES (MARKET DATA) ────────────────────────────\n")
  cat(sprintf("  Market Cap                    %s\n",
              scales::dollar(overview$mktcap, scale = 1e-9,
                             suffix = "B", accuracy = 0.1)))
  if (!is.na(overview$pe))
    cat(sprintf("  Trailing P/E                  %.1fx\n",   overview$pe))
  if (!is.na(overview$fwd_pe))
    cat(sprintf("  Forward P/E                   %.1fx\n",   overview$fwd_pe))
  if (!is.na(overview$ev_ebitda))
    cat(sprintf("  EV/EBITDA                     %.1fx\n",   overview$ev_ebitda))
  if (!is.na(overview$div_yield) && overview$div_yield > 0)
    cat(sprintf("  Dividend Yield                %.2f%%\n",  overview$div_yield * 100))

  cat("\n")
  cat(strrep("─", 62), "\n")
  cat("  DISCLAIMER: For educational and illustrative purposes only.\n")
  cat("  Not investment advice. Past performance ≠ future results.\n")
  cat(strrep("─", 62), "\n\n")
}

# ── 8. Main Execution ──────────────────────────────────────────────────────────

main <- function() {
  cat("\n")
  cat(strrep("━", 62), "\n")
  cat("  DISCOUNTED CASH FLOW VALUATION ENGINE  v2.0\n")
  cat(strrep("━", 62), "\n\n")

  # ── User inputs ──────────────────────────────────────────────────────────────
  symbol <- toupper(trimws(readline("  Ticker symbol (e.g. AAPL): ")))
  if (nchar(symbol) == 0) stop("Ticker cannot be empty.")

  yrs_input <- readline(
    glue("  Forecast horizon in years [default {FORECAST_YEARS_DEF}]: "))
  n_years <- if (yrs_input == "") FORECAST_YEARS_DEF else as.integer(yrs_input)
  if (is.na(n_years) || n_years < 1 || n_years > 15)
    stop("Forecast horizon must be between 1 and 15 years.")

  price_input <- readline(
    "  Current market price (optional, press Enter to skip): ")
  market_price <- if (price_input == "") NA_real_ else as.numeric(price_input)

  tgr_input <- readline(
    glue("  Terminal growth rate % [default {TERMINAL_GROWTH * 100}]: "))
  terminal_growth <- if (tgr_input == "") TERMINAL_GROWTH else
    as.numeric(tgr_input) / 100

  # ── Fetch data ───────────────────────────────────────────────────────────────
  cat("\n  Pulling data from Alpha Vantage & Yahoo Finance...\n\n")

  price_data <- get_price_cagr(symbol, n_years)
  growth_rate <- price_data$cagr
  if (is.na(market_price)) market_price <- price_data$latest

  overview    <- get_company_overview(symbol)
  rf          <- get_risk_free_rate()
  cash_flows  <- get_cash_flows(symbol)
  bs          <- get_balance_sheet(symbol)

  # Fallback: if Beta missing, prompt user
  beta <- overview$beta
  if (is.na(beta)) {
    beta_in <- readline("  Beta not available; enter manually (e.g. 1.2): ")
    beta    <- as.numeric(beta_in)
  }

  # ── WACC ─────────────────────────────────────────────────────────────────────
  wacc_res <- compute_wacc(beta, rf, MARKET_RISK_PREMIUM, bs, overview)
  discount_rate <- wacc_res$wacc

  # Allow override
  wacc_override <- readline(
    sprintf("  Computed WACC = %.2f%%. Press Enter to accept or type new %%: ",
            discount_rate * 100))
  if (nchar(trimws(wacc_override)) > 0)
    discount_rate <- as.numeric(wacc_override) / 100

  # ── Base FCF ─────────────────────────────────────────────────────────────────
  fcf_base <- cash_flows$FCF[1]
  if (is.na(fcf_base)) stop("Most recent FCF is NA. Check ticker or API quota.")
  # Average last 2 years to smooth one-off items
  fcf_base <- mean(cash_flows$FCF[1:min(2, nrow(cash_flows))], na.rm = TRUE)

  net_debt <- bs$total_debt - bs$cash
  shares   <- bs$shares_outstanding

  # ── Run DCF ──────────────────────────────────────────────────────────────────
  cat("\n  Running DCF model...\n")
  dcf_result <- run_dcf(
    fcf_base        = fcf_base,
    growth_rate     = growth_rate,
    discount_rate   = discount_rate,
    terminal_growth = terminal_growth,
    n_years         = n_years,
    net_debt        = net_debt,
    shares_outstanding = shares
  )

  # ── Sensitivity ───────────────────────────────────────────────────────────────
  cat("  Building sensitivity grid...\n")
  sens <- sensitivity_grid(
    fcf_base           = fcf_base,
    growth_rate        = growth_rate,
    base_wacc          = discount_rate,
    base_tgr           = terminal_growth,
    n_years            = n_years,
    net_debt           = net_debt,
    shares_outstanding = shares
  )

  # ── Console summary ───────────────────────────────────────────────────────────
  print_summary(symbol, overview, wacc_res, dcf_result,
                growth_rate, terminal_growth, n_years, market_price)

  # ── Build plots ───────────────────────────────────────────────────────────────
  cat("  Rendering charts...\n\n")

  p_fcf   <- plot_fcf(cash_flows, dcf_result$projections, symbol)
  p_bridge<- plot_ev_bridge(dcf_result, symbol)
  p_sens  <- plot_sensitivity(sens, discount_rate, terminal_growth,
                               dcf_result$iv_per_share, symbol)
  p_decomp<- plot_pv_decomposition(dcf_result, symbol)

  # ── Compose into one report page ─────────────────────────────────────────────
  header_text <- glue(
    "**{overview$name}** ({symbol})  ·  {overview$sector}  ·  ",
    "Intrinsic Value: **{scales::dollar(dcf_result$iv_per_share, accuracy = 0.01)}/share**  ·  ",
    "WACC: **{round(discount_rate * 100, 2)}%**  ·  ",
    "Stage-1 Growth: **{round(growth_rate * 100, 1)}%**  ·  ",
    "TGR: **{round(terminal_growth * 100, 1)}%**"
  )

  report <- (p_fcf | p_decomp) / (p_bridge | p_sens) +
    plot_annotation(
      title    = "DISCOUNTED CASH FLOW VALUATION REPORT",
      subtitle = header_text,
      caption  = glue(
        "Model: Two-Stage DCF | WACC via CAPM | Gordon Growth Terminal Value | ",
        "Data: Alpha Vantage, Yahoo Finance | ",
        "As of {format(Sys.Date(), '%B %d, %Y')} | For educational purposes only"
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 16,
                                     colour = COLS$navy, hjust = 0,
                                     margin = margin(b = 4)),
        plot.subtitle = element_markdown(size = 10, colour = COLS$grey60,
                                          hjust = 0, margin = margin(b = 12)),
        plot.caption  = element_text(size = 8, colour = COLS$grey30,
                                     hjust = 0, margin = margin(t = 10)),
        plot.background = element_rect(fill = COLS$white, colour = NA),
        plot.margin   = margin(20, 24, 16, 24)
      )
    )

  # ── Save output ───────────────────────────────────────────────────────────────
  out_file <- glue("{symbol}_DCF_Report_{format(Sys.Date(), '%Y%m%d')}.pdf")
  ggsave(out_file, plot = report, width = 16, height = 11,
         units = "in", dpi = 300, device = "pdf")
  cat(sprintf("  Report saved → %s\n\n", out_file))

  # Also display in RStudio viewer
  print(report)

  invisible(list(dcf = dcf_result, wacc = wacc_res,
                 sensitivity = sens, plots = report))
}

# ── Entry point ────────────────────────────────────────────────────────────────

if (interactive()) {
  results <- main()
} else {
  message("Run source('dcf_valuation.R') in an interactive R session.")
}
