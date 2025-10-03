install.packages(c("tidyverse", "rvest", "xgboost", "Matrix"), repos = "https://cloud.r-project.org")

library(tidyverse)
library(rvest)
library(xgboost)
library(Matrix)  # needed for sparse matrix

# Scrape NCAA NET rankings table
url <- "https://www.ncaa.com/rankings/basketball-men/d1/ncaa-mens-basketball-net-rankings"
webpage <- read_html(url)
table <- webpage %>% html_node("table") %>% html_table()

data <- as_tibble(table) %>%
  rename(Quad_1 = 'Quad 1',
         Quad_2 = 'Quad 2',
         Quad_3 = 'Quad 3',
         Quad_4 = 'Quad 4')

# Remove Previous column if exists
if ("Previous" %in% names(data)) data <- data %>% select(-Previous)

# Convert record strings to ratio
process_record <- function(record_str) {
  nums <- as.numeric(unlist(strsplit(record_str, "-")))
  total <- sum(nums)
  if (total == 0) return(0)
  return(nums[1] / total)
}

record_cols <- c("Record", "Road", "Neutral", "Quad_1", "Quad_2", "Quad_3", "Quad_4")
for (col in record_cols) {
  if (col %in% names(data)) data[[col]] <- sapply(data[[col]], process_record)
}

# Ensure Rank exists
if (!"Rank" %in% names(data)) stop("Rank column not found!")

# ===========================
# Linear Regression Model
# ===========================
lm_model <- lm(Rank ~ Record + Road + Neutral + Quad_1 + Quad_2 + Quad_3 + Quad_4, data = data)

check_school_rank_lm <- function(school_stats, actual_rank = NA) {
  pred <- predict(lm_model, newdata = school_stats)
  
  if (!is.na(actual_rank)) {
    cat("=== Linear Regression ===\n")
    cat("Predicted Rank:", round(pred, 1), "| Actual Rank:", actual_rank, "\n")
    
    diff <- round(pred - actual_rank, 1)
    
    if (abs(diff) <= 5) {
      cat("The school is properly ranked!\n")
    } else if (diff > 5) {
      cat("The school may be overrated (Difference:", abs(diff), "Spots)\n")
    } else {
      cat("The school may be underrated (Difference:", abs(diff), "Spots)\n")
    }
  }
}

# ===========================
# XGBoost Model
# ===========================
# Transform Rank for better top-team prediction
data <- data %>% mutate(inv_rank = 1 / Rank)

# Prepare data for XGBoost
feature_cols <- c("Record", "Road", "Neutral", "Quad_1", "Quad_2", "Quad_3", "Quad_4")
dtrain <- xgb.DMatrix(data = as.matrix(data[, feature_cols]), label = data$inv_rank)

# Train XGBoost model
set.seed(123)
xgb_model <- xgboost(
  data = dtrain,
  nrounds = 500,
  max_depth = 3,
  eta = 0.01,
  objective = "reg:squarederror",
  verbose = 0
)

check_school_rank_xgb <- function(school_stats, actual_rank = NA) {
  dtest <- xgb.DMatrix(data = as.matrix(school_stats))
  pred_inv <- predict(xgb_model, dtest)
  pred_rank <- 1 / pred_inv  # convert back to rank
  
  if (!is.na(actual_rank)) {
    cat("=== XGBoost ===\n")
    cat("Predicted Rank:", round(pred_rank, 1), "| Actual Rank:", actual_rank, "\n")
    
    diff <- round(pred_rank - actual_rank, 1)
    
    if (abs(diff) <= 5) {
      cat("The school is properly ranked!\n")
    } else if (diff > 5) {
      cat("The school may be overrated (Difference:", abs(diff), "Spots)\n")
    } else {
      cat("The school may be underrated (Difference:", abs(diff), "Spots)\n")
    }
  }
}

# ===========================
# Syracuse
# ===========================
cuse <- data %>% filter(School == "Syracuse")
cuse_rank <- cuse$Rank[1]
cuse_stats <- cuse %>% select(Record, Road, Neutral, Quad_1, Quad_2, Quad_3, Quad_4)

# Run both models
check_school_rank_lm(cuse_stats, actual_rank = cuse_rank)
check_school_rank_xgb(cuse_stats, actual_rank = cuse_rank)