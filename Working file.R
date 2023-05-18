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

library(ggplot2)

# Asset and total equity (i)

data %>% 
  group_by(Year) %>% 
  summarise(
    meanAE=median(`Assets/Equity`),
    q1 = quantile(`Assets/Equity`, probs = 0.25),
    q3 = quantile(`Assets/Equity`, probs = 0.75)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -Year,names_to = "variable",values_to = "value") %>% 
  ggplot(mapping = aes(x=Year, y= value, col =variable))+
  geom_line(size =1.2)+
  ylim(c(8,30))+
  theme_minimal()

# RWA and Tier 1 capital ratio (iii)

data %>% 
  group_by(Year) %>% 
  summarise(
    meanAE=median(`RWA/Tier 1`),
    q1 = quantile(`RWA/Tier 1`, probs = 0.25),
    q3 = quantile(`RWA/Tier 1`, probs = 0.75)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -Year,names_to = "variable",values_to = "value") %>% 
  ggplot(mapping = aes(x=Year, y= value, col =variable))+
  geom_line(size =1.2)+
  # ylim(c(8,30))+
  theme_minimal()
  

# Total fair value of the asset and the market value of the equity (need the market value of equity) (iv)

data %>% 
  group_by(Year) %>% 
  summarise(
    meanAE=median(`Total Fair Value Assets/Total Equity`),
    q1 = quantile(`Total Fair Value Assets/Total Equity`, probs = 0.25),
    q3 = quantile(`Total Fair Value Assets/Total Equity`, probs = 0.75)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -Year,names_to = "variable",values_to = "value") %>% 
  ggplot(mapping = aes(x=Year, y= value, col =variable))+
  geom_line(size =1.2)+
  # ylim(c(8,30))+
  theme_minimal()





# questions 1: Elasticity of bank activity with respect to bank capital
# log (total asset) == log (total equity)
library(plm)
pData <- pdata.frame(data[,c(2,3,6:34)], index = c('BankID'))
# pData$row.names <- make.unique(pData$row.names)

linearModel1 <- lm(formula = log(data$`Total Assets`)~log(data$`Total Common Equity`),data = data)

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

# question 2: Does equity react to the cycle?

plm2_i <- plm(formula = log(`Total.Common.Equity`)~lag(log(`Total.Common.Equity`))
              +`Return.on.Assets`+`GAAP.IFRS`, 
               data = pData, 
               model = 'within',
              effect = 'twoways')
summary(plm2_i)

# second parts

plm2_ii <- plm(formula = log(`Total.Common.Equity`)~lag(log(`Total.Common.Equity`))
               +Crisis
               +Crisis:lag(log(`Total.Common.Equity`))
               +`Return.on.Assets`
               +`Return.on.Assets`:Crisis
               +`GAAP.IFRS`, 
               data = pData, 
               model = 'within') # need to have the data based on ruling around the effects
summary(plm2_ii)




# further parts with macrodata (i: common equity)


# (a)

plm2_iii <- plm(formula = `Equity.Growth`~ `GDP.growth`,
               data = pData, 
               model = 'within',
               effect = 'twoways') # need to have the data based on ruling around the effects
summary(plm2_iii)

# (c)

plm2_iii <- plm(formula = `Equity.Growth`~ `GDP.growth`+ `GDP.growth`:Crisis,
               data = pData, 
               model = 'within',
               effect = 'twoways') # need to have the data based on ruling around the effects
summary(plm2_iii)

# (b)

plm2_iii <- plm(formula = `Equity.Growth`~ `Stock.Market.Growth`,
               data = pData, 
               model = 'within',
               effect = 'twoways') # need to have the data based on ruling around the effects
summary(plm2_iii)

# (d)

plm2_iii <- plm(formula = `Equity.Growth`~ `Stock.Market.Growth`+`Stock.Market.Growth`:Crisis,
               data = pData, 
               model = 'within',
               effect = 'twoways') # need to have the data based on ruling around the effects
summary(plm2_iii)



# further parts with macrodata (ii: Tier 1 equity)

# (a)

plm2_iii <- plm(formula = `Tier.1.Growth`~ `GDP.growth`,
               data = pData, 
               model = 'within',
               effect = 'twoways') # need to have the data based on ruling around the effects
summary(plm2_iii)

# (c)

plm2_iii <- plm(formula = `Tier.1.Growth`~ `GDP.growth`+ `GDP.growth`:Crisis,
               data = pData, 
               model = 'within',
               effect = 'twoways') # need to have the data based on ruling around the effects
summary(plm2_iii)

# (b)

plm2_iii <- plm(formula = `Tier.1.Growth`~ `Stock.Market.Growth`,
               data = pData, 
               model = 'within',
               effect = 'twoways') # need to have the data based on ruling around the effects
summary(plm2_iii)

# (d)

plm2_iii <- plm(formula = `Tier.1.Growth`~ `Stock.Market.Growth`+`Stock.Market.Growth`:Crisis,
               data = pData, 
               model = 'within',
               effect = 'twoways') # need to have the data based on ruling around the effects
summary(plm2_iii)





# question 3: Impact of bank capitalization on funding costs

(a)

plm3_i <- plm(formula = `Interest.Expense.`~lag(`Assets.Equity`)
              +lag(`Interest.Expense.`)
              +`Return.on.Assets`
              +`GAAP.IFRS`,
               data = pData, 
               model = 'within',
              effect = 'twoways')
summary(plm3_i)

(b)

plm3_i <- plm(formula = `Interest.Expense.`~lag(`RWA.Tier.1`)
              +lag(`Interest.Expense.`)
              +`Return.on.Assets`
              +`GAAP.IFRS`,
              data = pData, 
              model = 'within',
              effect = 'twoways')
summary(plm3_i)



(c)


plm3_i <- plm(formula = `Interest.Expense.`~lag(`Total.Common.Equity`/`Total.Assets`)
              +lag(`Interest.Expense.`)
              +`Return.on.Assets`
              +`GAAP.IFRS`,
              data = pData, 
              model = 'within',
              effect = 'twoways')
summary(plm3_i)
  
# now same model but controlling macroeconomic variables 

# (a) need to do it later again

plm3_i <- plm(formula = `Interest.Expense.`~lag(`Assets.Equity`)
              +lag(`Interest.Expense.`)
              + `GDP.growth`
              + `MP3.growth`
              + `Stock.Market.Growth`
              +`Return.on.Assets`
              +`GAAP.IFRS`,
               data = pData, 
               model = 'within',
              effect = 'twoways')
summary(plm3_i)

# (b)

plm3_i <- plm(formula = `Interest.Expense.`~lag(`RWA.Tier.1`)
              +lag(`Interest.Expense.`)
              +`Return.on.Assets`
              +`GAAP.IFRS`,
              data = pData, 
              model = 'within',
              effect = 'twoways')
summary(plm3_i)



# (c)


plm3_i <- plm(formula = `Interest.Expense.`~lag(`Total.Common.Equity`/`Total.Assets`)
              +lag(`Interest.Expense.`)
              +`Return.on.Assets`
              +`GAAP.IFRS`,
              data = pData, 
              model = 'within',
              effect = 'twoways')
summary(plm3_i)
  

# Need to complete the funding analysis issues as soon as possible (tomorrow will be even better)


