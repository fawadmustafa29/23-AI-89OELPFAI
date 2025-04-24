library(tidyverse)

# Load dataset
customer_churn <- read_csv("customer_churn.csv")

# Step 1: Count customers who have churned
churned_count <- customer_churn %>%
  filter(Churn == "Yes") %>%
  nrow()
print(churned_count)

# Step 2: Create ChargeGap column
customer_churn <- customer_churn %>%
  mutate(TotalCharges = as.numeric(TotalCharges)) %>%
  mutate(ChargeGap = TotalCharges - (MonthlyCharges * Tenure))

# Step 3: Filter long-term active customers
long_term_active <- customer_churn %>%
  filter(Tenure > 24, Churn == "No")

# Step 4: Average MonthlyCharges by ContractType
avg_monthly_by_contract <- customer_churn %>%
  group_by(Contract) %>%
  summarise(AverageMonthlyCharges = mean(MonthlyCharges, na.rm = TRUE))
print(avg_monthly_by_contract)

# Step 5: Categorize customers by AgeGroup
customer_churn <- customer_churn %>%
  mutate(AgeGroup = case_when(
    Age < 25 ~ "Youth",
    Age >= 25 & Age <= 55 ~ "Adult",
    Age > 55 ~ "Senior"
  ))

# Step 6: Top 5 cities with highest number of churned customers
top_cities <- customer_churn %>%
  filter(Churn == "Yes") %>%
  count(City, sort = TRUE) %>%
  top_n(5, n)
print(top_cities)

# Step 7: Extract Name and City for specific high-spending churned customers
high_spending_churn <- customer_churn %>%
  filter(TotalCharges > 3000, Contract == "Month-to-month", Churn == "Yes") %>%
  select(Name, City)
print(high_spending_churn)

# Step 8: Table of average tenure and total revenue by ContractType
revenue_summary <- customer_churn %>%
  group_by(Contract) %>%
  summarise(
    AvgTenure = mean(Tenure, na.rm = TRUE),
    TotalRevenue = sum(TotalCharges, na.rm = TRUE)
  )
print(revenue_summary)

