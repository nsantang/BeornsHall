suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(lubridate)
  library(tidyquant)
})

if (!interactive()) stop("This script requires an interactive R session to read input.")

api_key <- "FUUHSV33HH7481LB"

# --- Helper Functions ---

get_price_growth_rate <- function(stock_prices, years) {
  stock_prices <- stock_prices %>% arrange(date)
  start_date <- max(stock_prices$date) %m-% years(years)
  filtered <- stock_prices %>% filter(date >= start_date)
  start_price <- filtered$adjusted[1]
  end_price <- filtered$adjusted[nrow(filtered)]
  cagr <- (end_price / start_price)^(1 / years) - 1
  return(cagr)
}

get_beta <- function(symbol, api_key) {
  url <- paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=", symbol, "&apikey=", api_key)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  if (!is.null(data$Beta)) return(as.numeric(data$Beta))
  warning("Beta not found.")
  return(NA)
}

get_risk_free_rate <- function(api_key) {
  url <- paste0("https://www.alphavantage.co/query?function=TREASURY_YIELD&interval=daily&maturity=10year&apikey=", api_key)
  response <- httr::GET(url)
  data <- jsonlite::fromJSON(rawToChar(response$content))
  if (!is.null(data$data)) {
    latest <- data$data
    return(as.numeric(latest$value[1]) / 100)
  }
  warning("Risk-free rate not found.")
  return(NA)
}

get_discount_rate <- function(symbol, api_key, market_risk_premium = 0.055) {
  beta <- get_beta(symbol, api_key)
  rf <- get_risk_free_rate(api_key)
  if (is.na(beta) || is.na(rf)) return(NA)
  return(rf + beta * market_risk_premium)
}

get_cash_flows <- function(symbol, api_key) {
  url <- paste0("https://www.alphavantage.co/query?function=CASH_FLOW&symbol=", symbol, "&apikey=", api_key)
  response <- GET(url)
  stop_for_status(response)
  data <- fromJSON(suppressMessages(content(response, "text")))
  annual_reports <- data$annualReports
  df <- annual_reports %>%
    select(fiscalDateEnding, operatingCashflow, capitalExpenditures) %>%
    mutate(
      OCF = as.numeric(operatingCashflow),
      CapEx = as.numeric(capitalExpenditures),
      FCF = if_else(!is.na(OCF) & !is.na(CapEx), OCF - CapEx, NA_real_)
    ) %>%
    arrange(desc(fiscalDateEnding))
  return(df)
}

get_balance_sheet_data <- function(symbol, api_key) {
  url <- paste0("https://www.alphavantage.co/query?function=BALANCE_SHEET&symbol=", symbol, "&apikey=", api_key)
  response <- GET(url)
  stop_for_status(response)
  data <- fromJSON(suppressMessages(content(response, "text")))
  annual_reports <- data$annualReports
  df <- annual_reports %>%
    select(fiscalDateEnding, totalLiabilities, cashAndCashEquivalentsAtCarryingValue, commonStockSharesOutstanding) %>%
    mutate(
      TotalLiabilities = as.numeric(totalLiabilities),
      Cash = as.numeric(cashAndCashEquivalentsAtCarryingValue),
      NetDebt = if_else(!is.na(TotalLiabilities) & !is.na(Cash), TotalLiabilities - Cash, NA_real_),
      SharesOutstanding = as.numeric(commonStockSharesOutstanding)
    ) %>%
    arrange(desc(fiscalDateEnding))
  return(df)
}

forecast_fcf <- function(fcf_last, growth_rate, years) {
  fcf_last * cumprod(rep(1 + growth_rate, years))
}

discount_fcfs <- function(fcfs, discount_rate) {
  years <- seq_along(fcfs)
  fcfs / (1 + discount_rate)^years
}

calc_terminal_value <- function(last_fcf, g = 0.02, r, n) {
  tv <- (last_fcf * (1 + g)) / (r - g)
  tv / (1 + r)^n
}

# --- User Inputs ---

symbol <- readline(prompt = "Enter Ticker Symbol (e.g. AAPL): ")
if (symbol == "") stop("Ticker symbol cannot be empty.")

years_input <- readline(prompt = "Enter Forecast Horizon (years) [default 5]: ")
forecast_years <- ifelse(years_input == "", 5, as.integer(years_input))

# --- Get Data and Rates ---

cat("\nFetching stock prices for growth rate estimation...\n")
stock_prices <- tq_get(symbol, get = "stock.prices")

growth_rate <- get_price_growth_rate(stock_prices, forecast_years)
cat(sprintf("Estimated Historical CAGR over last %d years: %.2f%%\n", forecast_years, growth_rate * 100))

discount_rate <- get_discount_rate(symbol, api_key)
if (is.na(discount_rate)) {
  discount_rate_input <- readline(prompt = "Discount rate unavailable, please enter manually (%) [e.g. 8]: ")
  discount_rate <- as.numeric(discount_rate_input) / 100
}
cat(sprintf("Using discount rate: %.2f%%\n", discount_rate * 100))

# --- Fetch Financial Statements ---

cat("\nFetching cash flow and balance sheet data...\n")
cash_flows <- get_cash_flows(symbol, api_key)
bs_data <- get_balance_sheet_data(symbol, api_key)

fcf_recent <- cash_flows$FCF[1]
if (is.na(fcf_recent)) stop("Most recent FCF data is not available.")

# --- DCF Calculations ---

fcf_forecast <- forecast_fcf(fcf_recent, growth_rate, forecast_years)
pv_fcfs <- discount_fcfs(fcf_forecast, discount_rate)
pv_terminal <- calc_terminal_value(tail(fcf_forecast, 1), g = 0.02, r = discount_rate, n = forecast_years)

net_debt <- bs_data$NetDebt[1]
shares_out <- bs_data$SharesOutstanding[1]

enterprise_value <- sum(pv_fcfs) + pv_terminal
equity_value <- enterprise_value - net_debt
intrinsic_value_per_share <- equity_value / shares_out

# --- Output Results ---

summary_table <- data.frame(
  Metric = c("Enterprise Value (B)", "Net Debt (B)", "Equity Value (B)", "Shares Outstanding", "Intrinsic Value per Share (USD)"),
  Value = c(
    round(enterprise_value / 1e9, 2),
    round(net_debt / 1e9, 2),
    round(equity_value / 1e9, 2),
    format(shares_out, big.mark = ","),
    round(intrinsic_value_per_share, 2)
  )
)

cat("\n====== DCF VALUATION SUMMARY ======\n")
print(summary_table)

# --- Plot Free Cash Flow Forecast ---

years_seq <- seq(as.Date(cash_flows$fiscalDateEnding[1]), by = "year", length.out = forecast_years)
df_plot <- data.frame(Year = format(years_seq, "%Y"), FCF = fcf_forecast / 1e9)

cat("\nGenerating Free Cash Flow Forecast Plot...\n")

plot_obj <- ggplot(df_plot, aes(x = Year, y = FCF, group = 1)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2", size = 3) +
  labs(title = paste("FCF Forecast for", symbol),
       y = "Free Cash Flow (Billions USD)",
       x = "Year") +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01, big.mark = ",")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


print(plot_obj)
