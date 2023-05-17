# relevant liabrary
library(readxl)
library(tidyverse)


# loading the data
data <- read_excel("Data for analysis.xlsx", 
                                sheet = "Final")
data <- na.omit(data)

# summary statistics for the banks

leverdUnleverd <- data %>% 
  group_by(`Bank Name`) %>% 
  summarise(
    N = n(),
    Min = min(`Assets/Equity`),
    Mean = mean(`Assets/Equity`),
    q1 = quantile(`Assets/Equity`,probs = 0.25),
    Median = median(`Assets/Equity`),
    q3 = quantile(`Assets/Equity`,probs = 0.75),
    Max = max(`Assets/Equity`)
    
  )

# 1st and 4th quantile of the overall asset/equity
  
data %>% 
  summarise(
    quantile = quantile(`Assets/Equity`, probs = c(0.25,0.75))
  )

# finding the levered and unlevered firms

leverdUnleverd %>% 
  filter(q1<=10.7 | q1>=21.3) %>% 
  arrange(q1)



# partioning the data in the highly levered and lower levered dataset

dataPart <- data %>% 
  # select(`Bank Name`, Year, LL, HL) %>% 
  mutate(
    LL = case_when(
    `Bank Name`== "Truist Financial"~1,
    `Bank Name`== "M&T Bank"~1,
    `Bank Name`== "PNC"~1,
    `Bank Name`== "Fifth Third Bancorp"~1,
    `Bank Name`== "Bank of New York Mellon"~1,
    `Bank Name`== "Bank of America"~1,
    `Bank Name`== "US Bancorp"~1,
    `Bank Name`== "JPMorgan Chase"~1,
    TRUE ~ 0),
    HL = case_when(
      `Bank Name`== "Deutsche Bank"~1,
      TRUE ~ 0)) %>% 
  group_by(`Bank Name`) %>% 
  mutate(BankID = )

dataPart %>% 
  # group_by(LL,HL) %>% 
  filter(HL==1 | LL==1) %>%
  group_by(LL,HL) %>%
  summarise(
    obs=n(),
    assets=mean(`Total Assets`/1000),
    costOfDebtFinancing = mean(`Interest Expense%`),
    growthRateOfDebtFinancing =mean(`Equity Growth`),
    growthRateOfLending = mean(`Loan Growth`),
    assetRisk = sd(`Total Assets`),
    ROA = mean(`Return on Assets`)
    
    
  ) %>% t()


# Need to draw the figures (4 figures)




# questions 1: Elasticity of bank activity with respect to bank capital
# log (total asset) == log (total equity)
library(plm)




plm(formula = log(data$`Total Assets`)~log(data$`Total Common Equity`)|`Bank Name`,data = data,model = "pooling")











  




