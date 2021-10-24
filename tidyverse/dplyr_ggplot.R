library(tidyverse)


csv_2010 <- "https://raw.githubusercontent.com/cliftonleesps/607_acq_mgt/main/tidyverse/MERGED2010_11_PP.small.csv"
csv_2018 <- "https://raw.githubusercontent.com/cliftonleesps/607_acq_mgt/main/tidyverse/MERGED2018_19_PP.small.csv"

na_fields <- c("", "NA", "NULL", "PrivacySuppressed")


# Use the readr::read_csv function
college_2010 <- read_csv(csv_2010, na = na_fields)
college_2018 <- read_csv(csv_2018, na = na_fields)


# Use the dplyr::inner_join function to join by Office of Postsecondary Education ID as primary key.
# Created by the Department of Education.
all_colleges <- inner_join(college_2010, 
                           college_2018,
                           by = "OPEID"
                           )


# create a new column diff_debt
# Filter out any rows where the diff_debt == NA
all_colleges <- all_colleges %>%
  mutate( diff_debt = GRAD_DEBT_MDN.y - GRAD_DEBT_MDN.x) %>%
  filter(!is.na(diff_debt) & !is.na(COSTT4_A.y)) %>%
  arrange(desc(COSTT4_A.y), desc(diff_debt))


# Select only a few columns
display_table <- all_colleges %>% select("INSTNM.y","COSTT4_A.y","GRAD_DEBT_MDN.y","diff_debt")

display_table <- display_table[1:500,]

# rename columns
colnames(display_table) <- c("Institution Name","Annual Tuition Cost (2018)","Median Debt at Graduation","Difference in Median Debt from 2010")

# Use the ggplot function to display our results
ggplot(data = display_table) +
  geom_point(
    mapping = aes(x = `Annual Tuition Cost (2018)`, y = `Difference in Median Debt from 2010`)
  ) +
  theme_bw() +
  labs(
      title = "Annual Tuition (2018) Compared to Change in Median Debt",
      x = "Annual Tuition 2018 ($)", 
      y = "Change in Median Debt ($)")

