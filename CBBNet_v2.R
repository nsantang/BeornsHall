install.packages(c("tidyverse", "rvest"), repos = "https://cloud.r-project.org")

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

check_school_rank <- function(school_stats, actual_rank = NA) {
  pred <- predict(model, newdata = school_stats)
  
  if (!is.na(actual_rank)) {
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

cuse <- data[data$School=='Syracuse',]

cuse_rank <- cuse$Rank
cuse_stats <- cuse %>%
  select(Record, Road, Neutral, `Quad 1`, `Quad 2`, `Quad 3`, `Quad 4`)

check_school_rank(cuse_stats, actual_rank = cuse_rank)
