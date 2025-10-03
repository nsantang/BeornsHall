# Install required packages
install.packages(c("tidyverse", "rvest", "gbm"), repos = "https://cloud.r-project.org")

library(tidyverse)
library(rvest)
library(gbm)

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

# Transform Rank to inverse for better modeling of top teams
data <- data %>% mutate(inv_rank = 1 / Rank)

# Fit Gradient Boosting Model
set.seed(123)
gbm_model <- gbm(
  inv_rank ~ Record + Road + Neutral + Quad_1 + Quad_2 + Quad_3 + Quad_4,
  data = data,
  distribution = "gaussian",
  n.trees = 2000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 5,
  cv.folds = 5,
  verbose = FALSE
)

# Function to check school rank
check_school_rank_gbm <- function(school_stats, actual_rank = NA) {
  pred_inv <- predict(gbm_model, newdata = school_stats, n.trees = gbm_model$n.trees)
  pred_rank <- 1 / pred_inv  # Convert back to rank
  
  if (!is.na(actual_rank)) {
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
  return(pred_rank)
}


cuse <- data %>% filter(School == "Syracuse")
cuse_rank <- cuse$Rank[1]
cuse_stats <- cuse %>% select(Record, Road, Neutral, Quad_1, Quad_2, Quad_3, Quad_4)

check_school_rank_gbm(cuse_stats, actual_rank = cuse_rank)