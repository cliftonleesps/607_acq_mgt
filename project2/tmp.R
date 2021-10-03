library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(purrr)


### customer churn


customers <- read_csv("https://raw.githubusercontent.com/cliftonleesps/607_acq_mgt/main/project2/customer_churn.csv", n_max=6)
customers <- as_tibble(customers)
customers[['Division']][2] <- 'A'
customers[['Division']][4] <- 'B'
customers[['Division']][6] <- 'C'

# Create a longer format with the tidyr pivot_longer function
customers_long_format <- pivot_longer(customers,cols=3:14,names_to="month", values_to="num_customers")

customers_aggregate <- tibble (division = '', churn = 0)

for (division in LETTERS[1:3]) {
    # sum up the customers lost and gained
    customers_lost <- customers_long_format %>% filter(Division==division & Description == "Lost") %>% select(num_customers) %>% sum()
    customers_gained <- customers_long_format %>% filter(Division==division & Description == "Gained") %>% select(num_customers) %>% sum()
    
    # add a row to the aggregate tibble
    customers_aggregate <- add_row(
                                    customers_aggregate,
                                    division = division,
                                    churn = abs(customers_lost/customers_gained)
                                   )
} # division

customers_aggregate <- customers_aggregate[-1,]

ggplot(customers_aggregate) + 
  geom_col(mapping=aes(x=division, y=churn),width=0.15) + ylim(0,1) + 
  ggtitle("Customer Churn By Division") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Churn Percentage") + 
  xlab("Division")




## income by religion

income <- read_csv("https://raw.githubusercontent.com/cliftonleesps/607_acq_mgt/main/project2/income_distribution_by_religious_group.csv", 
                   col_names=c("religion","less_than_30","30_to_49","50_to_99","100_or_more","total"), 
                   skip=1)

# Shorten the names
income[1] = c(
              "Buddhist",
              "Catholic",
              "Evan. Prot.",
              "Hindu",
              "Hist. Black Prot",
              "JW",
              "Jewish",
              "Main. Prot.","Mormon",
              "Muslim",
              "Orth. Christian",
              "Unaffiliated")

# Remove the percent signs and convert to integer for some columns
for (c in c("less_than_30","30_to_49","50_to_99","100_or_more")) {
  income[[c]] = as.numeric(
                          as.character(
                                      str_replace(income[[c]], "%", "")
                                      )
                          )
}

ggplot(data=income) + 
  geom_col(mapping=aes(x=religion,
                       y=less_than_30,
                       fill=religion),
           width=0.15, position="dodge") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("")

# make a temporary data frame by melting religion with the salary range columns
dfm <- melt(income[,c("religion","less_than_30", "30_to_49", "50_to_99", "100_or_more")])

# make a bar plot
ggplot(dfm,aes(x = religion,y = value)) +
  geom_bar( aes(fill = variable),
            stat = "identity",
            position = "stack") +
            labs(title="Religion & Income Levels") +
            ylab("Percent") +
            xlab("Religion") +
            # scale_fill_discrete(name="Income Ranges") + 
            scale_fill_manual("Income Ranges",values=c("#5E7CC2", "#00C1D9", "#00DBB9","#91EF8D"),labels=c("< 30k", "30k to 49k", "50k to 99k",">100k")) +
            theme(plot.title = element_text(hjust = 0.5))





