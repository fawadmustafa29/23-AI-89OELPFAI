df <- read_excel("customer_churn.xlsx")
df %>%
  filter(Churn == "Yes") %>%
  summarise(Churned_Customers = n())
df <- df %>%
  mutate(ChargeGap = TotalCharges - (MonthlyCharges * Tenure),
         ChargeGap = if_else(is.na(ChargeGap), 0, ChargeGap))
df %>%
  filter(Tenure > 24, Churn == "No")
df %>%
  group_by(ContractType) %>%
  summarise(Average_MonthlyCharges = mean(MonthlyCharges, na.rm = TRUE))
df <- df %>%
  mutate(AgeGroup = case_when(
    Age < 25 ~ "Youth",
    Age >= 25 & Age <= 55 ~ "Adult",
    Age > 55 ~ "Senior"
  ))
