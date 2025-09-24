# Load required packages
if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cloud.r-project.org")
if (!require("rvest")) install.packages("rvest", repos = "https://cloud.r-project.org")

library(tidyverse)
library(rvest)

url <- "https://www.ncaa.com/rankings/basketball-men/d1/ncaa-mens-basketball-net-rankings"
webpage <- read_html(url)
table <- webpage %>% html_node("table") %>% html_table()

data <- as_tibble(table)

if ("Previous" %in% names(data)) {
  data <- data %>% select(-Previous)
}
process_record <- function(record_str) {
  # Converts strings like "12-5" to a ratio 12/(12+5)
  nums <- as.numeric(unlist(strsplit(record_str, "-")))
  total <- sum(nums)
  if (total == 0) return(0)
  return(nums[1] / total)
}

record_cols <- c("Record", "Road", "Neutral", "Quad 1", "Quad 2", "Quad 3", "Quad 4")

for (col in record_cols) {
  if (col %in% names(data)) {
    data[[col]] <- sapply(data[[col]], process_record)
  }
}
if (!"Rank" %in% names(data)) stop("Rank column not found!")

model <- lm(Rank ~ Record + Road + Neutral + `Quad 1` + `Quad 2` + `Quad 3` + `Quad 4`, 
            data = data)
summary(model)

check_school_rank <- function(school_stats, actual_rank = NA) {
  pred <- predict(model, newdata = school_stats)
  if (!is.na(actual_rank)) {
    cat("Predicted Rank:", round(pred, 1), "| Actual Rank:", actual_rank, "\n")
    diff <- round(pred - actual_rank, 1)
    if (abs(diff) <= 5) {
      cat("The school is properly ranked!\n")
    } else {
      cat("The school may be over- or under-ranked (Difference:", diff, ")\n")
    }
  } else {
    cat("Predicted Rank:", round(pred, 1), "\n")
  }
  return(pred)
}

syracuse_stats <- tibble(
  Record = 0.655172,
  Road = 0.4,
  Neutral = 0.333333,
  `Quad 1` = 0.222222,
  `Quad 2` = 0.666667,
  `Quad 3` = 0.9,
  `Quad 4` = 1.0
)

check_school_rank(syracuse_stats, actual_rank = 83)

nc_stats <- tibble(
  Record = 0.853103,
  Road = 0.777778,
  Neutral = 0.5,
  `Quad 1` = 0.6,
  `Quad 2` = 0.75,
  `Quad 3` = 1.0,
  `Quad 4` = 1.0
)

check_school_rank(nc_stats, actual_rank = 10)
