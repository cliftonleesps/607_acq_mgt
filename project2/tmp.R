library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)




customers <- read_csv("https://raw.githubusercontent.com/cliftonleesps/607_acq_mgt/main/project2/customer_churn.csv", n_max=6)
customers[['Division']][2] <- 'A'
customers[['Division']][4] <- 'B'
customers[['Division']][6] <- 'C'

customers_clean <- tibble(
  "division" = "division",
  "type" = "description",
  "month" = "month",
  "number_customers" = 0
)
months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

for (division in LETTERS[1:3]) {
  for (description in c("Gained", "Lost")) {
    # select one row at a time
    row <- customers %>% filter (Division == division & Description == description)
    #glimpse(row)
    for (month in months) {
      customers_clean <- customers_clean %>% add_row(
        "division" = division,
        "type" = description,
        "month" = month,
        "number_customers" = row[[month]]
      )
    }
  } # description
} # division
customers_clean <- customers_clean[-1,]

view(customers_clean)
