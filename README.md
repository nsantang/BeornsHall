# BeornsHall

**About Me**
Quantitative analyst with a background in financial modeling, systematic strategies, and machine learning. All projects are independently built and validated against real data. Currently transitioning from marketing analytics into quantitative finance and investment research.

**This is my person portfolio. Please don't take or misappropriate anything on here!**

- **MomentumStrat_RiskAnalysis:** Cross-sectional sector momentum strategy built in R. Ranks SPY, QQQ, XLF, XLK, XLE, and XLV by 90-day price momentum, holds the top 2 sectors, and rebalances daily. Includes volatility targeting (15% annualized), turnover-based transaction costs, and a 10% drawdown circuit breaker. Validated with an in-sample/out-of-sample split at 2021. Benchmarked against SPY buy-and-hold.
- **DCF_ValuationModel:** Two-stage discounted cash flow engine built in R. Fetches live financial data via the Alpha Vantage API, derives a discount rate using CAPM, and projects free cash flows across a high-growth and stable-growth stage. Terminal value is calculated using the Gordon Growth Model. Includes a WACC × terminal growth rate sensitivity heatmap and an enterprise value bridge chart. Outputs a multi-panel, publication-quality report.
- **NCAA_NET_RankingModel:** Dual-model NCAA basketball ranking evaluator built in R. Scrapes live NET rankings directly from NCAA.com, converts win-loss records across overall, road, neutral, and all four quadrants into win-rate ratios, then trains both a linear regression and an XGBoost model (500 rounds, inverse-rank transformation for top-team accuracy) to predict a team's expected rank. Flags teams as properly ranked, overrated, or underrated within a 5-spot tolerance. Demonstrated on Syracuse as a sample team.
