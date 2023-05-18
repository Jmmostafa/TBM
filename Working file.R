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
      TRUE ~ 0)) 

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


# variable summary stat (x_it) plus macro variables
# N, mean, std.dev, min, max

dataPart %>% 
  summarise(
    N=n(),
    MeanEq = mean(log(`Total Common Equity`)),
    MeanAss = mean(log(`Total Common Equity`)),
    MeanEq = mean(log(`Total Common Equity`))
    
  )






# Need to draw the figures (4 figures)




# questions 1: Elasticity of bank activity with respect to bank capital
# log (total asset) == log (total equity)
library(plm)
pData <- pdata.frame(data[,c(2,3,6:34)], index = c('BankID'))
# pData$row.names <- make.unique(pData$row.names)



linearModel1 <- lm(formula = log(pData$`Total Assets`)~log(pData$`Total Common Equity`),data = pData)

plm1_i <- plm(formula = log(`Total.Assets`)~log(`Total.Common.Equity`), 
    data = pData, 
    model = 'within', 
    effect = "time")
summary(plm1_i)

plm1_ii <- plm(formula = log(`Total.Assets`)~log(`Total.Common.Equity`)+`Return.on.Assets`, 
    data = pData, 
    model = 'within', 
    effect = "individual")
summary(plm1_ii)








  




