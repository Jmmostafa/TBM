# relevant liabrary
library(readxl)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(plm)
library(urca)

# loading the data and grooming the data
data <- read_excel("Data for analysis.xlsx", 
                                        sheet = "Final", 
                                        col_types = c(rep("text",5), rep('numeric',32)))
data <- as.data.frame(na.omit(data))




# --------------------------------------------------------------------------------
# Preparing the initial graphs ---------------------------------------------------
# --------------------------------------------------------------------------------


# 1st one ------------------------------------------------------------------------


data %>% 
  #group_by(countryOfOrigin) %>% 
  ggplot(mapping = aes(x=TA_TE, y=FundingCost))+
  geom_point()+
  geom_smooth(method = 'lm', se=FALSE)+
  #facet_wrap(facets = ~countryOfOrigin, scales = 'free_y', ncol = 2)+
  labs(title = "Cost of Debt Funding",
       # subtitle = "Categories are based on bank regions",
       caption = "Data source: Bloomberg") +
  xlab('Total Asset/Total Equity') + ylab('Funding Cost')+
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0))


data %>% 
  group_by(countryOfOrigin) %>% 
  ggplot(mapping = aes(x=TA_TE, y=FundingCost,col=countryOfOrigin))+
  geom_point()+
  geom_smooth(method = 'lm', se=FALSE)+
  facet_wrap(facets = ~countryOfOrigin, scales = 'free_y', ncol = 2)+
  labs(title = "Cost of Debt Funding",
       subtitle = "Categories are based on bank regions",
       caption = "Data source: Bloomberg") +
  xlab('Total Asset/Total Equity') + ylab('Funding Cost')+
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0))


# 2nd one ------------------------------------------------------------------------


data %>% 
  # group_by(countryOfOrigin) %>% 
  ggplot(mapping = aes(x=TA_TE, y=TFundGrow))+
  geom_point()+
  geom_smooth(method = 'lm', se=FALSE)+
  # facet_wrap(facets = ~countryOfOrigin, scales = 'free_y', ncol = 2)+
  labs(title = "Debt Funding",
       # subtitle = "Categories are based on bank regions",
       caption = "Data source: Bloomberg") +
  xlab('Total Asset/Total Equity') + ylab('Non-equity funding Growth')+
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0))


data %>% 
  group_by(countryOfOrigin) %>% 
  ggplot(mapping = aes(x=TA_TE, y=TFundGrow,col=countryOfOrigin))+
  geom_point()+
  geom_smooth(method = 'lm', se=FALSE)+
  facet_wrap(facets = ~countryOfOrigin, scales = 'free_y', ncol = 2)+
  labs(title = "Debt Funding",
       subtitle = "Categories are based on bank regions",
       caption = "Data source: Bloomberg") +
  xlab('Total Asset/Total Equity') + ylab('Non-equity funding Growth')+
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0))




# 3rd ----------------------------------------------------------------------------


data %>% 
  # group_by(countryOfOrigin) %>% 
  ggplot(mapping = aes(x=TA_TE, y=LG))+
  geom_point()+
  geom_smooth(method = 'lm', se=FALSE)+
  # facet_wrap(facets = ~countryOfOrigin, scales = 'free_y', ncol = 2)+
  labs(title = "Lending",
       # subtitle = "Categories are based on bank regions",
       caption = "Data source: Bloomberg") +
  xlab('Total Asset/Total Equity') + ylab('Non-equity Financing Growth')+
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0))


data %>% 
  group_by(countryOfOrigin) %>% 
  ggplot(mapping = aes(x=TA_TE, y=LG,col=countryOfOrigin))+
  geom_point()+
  geom_smooth(method = 'lm', se=FALSE)+
  facet_wrap(facets = ~countryOfOrigin, scales = 'free_y', ncol = 2)+
  labs(title = "Lending",
       subtitle = "Categories are based on bank regions",
       caption = "Data source: Bloomberg") +
  xlab('Total Asset/Total Equity') + ylab('Non-equity Financing Growth')+
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0))



# summary statistics for the banks (need to add all the variables)

leverdUnleverd <- data %>% 
  group_by(bankName) %>% 
  summarise(
    N = n(),
    Min = min(TA_TE),
    Mean = mean(TA_TE),
    q1 = quantile(TA_TE,probs = 0.25),
    Median = median(TA_TE),
    q3 = quantile(TA_TE,probs = 0.75),
    Max = max(TA_TE),
    SD = sd(TA_TE)
    )

write.table(leverdUnleverd,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)


# 1st and 4th quantile of the overall asset/equity

data %>% 
  summarise(
    quantile = quantile(TA_TE, probs = c(0.25,0.75))
  )

# finding the levered and unlevered firms

leverdUnleverd %>% 
  filter(q1<=10.11 | q1>=20.12) %>% 
  arrange(q1)


# partioning the data in the highly levered and lower levered dataset

dataHL <- data %>% 
  # select(bankName, Year, LL, HL) %>% 
  mutate(
    LL = case_when(
      bankName== "Santander Holdings"~1,
      bankName== "Capial One Financial"~1,
      bankName== "Truist Financial"~1,
      bankName== "M&T Bank"~1,
      bankName== "PNC"~1,
      bankName== "Fifth Third Bancorp"~1,
      bankName== "Bank of New York Mellon"~1,
      bankName== "KeyCorp"~1,
      bankName== "Bank of America"~1,
      bankName== "US Bancorp"~1,
      bankName== "Citigroup Inc"~1,
      TRUE ~ 0),
    HL = case_when(
      bankName== "Barclays"~1,
      bankName== "Commerzbank"~1,
      bankName== "Deutsche Bank"~1,
      TRUE ~ 0)) 

HL_LLsumm <- dataHL %>% 
  # group_by(LL,HL) %>% 
  filter(HL==1 | LL==1) %>%
  group_by(LL,HL) %>%
  summarise(
    obs=n(),
    assets=mean(TA/1000),
    costOfDebtFinancing = mean(FundingCost),
    growthRateOfDebtFinancing =mean(TFundGrow),
    growthRateOfLending = mean(LG),
    assetRisk = sd(TAG),
    ROA = mean(ROA))%>% 
  t() %>% 
  round(digits = 2)

HL_LLsummFull <- dataHL %>% 
  # group_by(LL,HL) %>% 
  # filter(HL==1 | LL==1) %>%
  # group_by(LL,HL) %>%
  summarise(
    obs=n(),
    assets=mean(TA/1000),
    costOfDebtFinancing = mean(FundingCost),
    growthRateOfDebtFinancing =mean(TFundGrow),
    growthRateOfLending = mean(LG),
    assetRisk = sd(TAG),
    ROA = mean(ROA))%>% 
  t() %>% 
  round(digits = 2)

write.table(HL_LLsummFull,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)




# testing the difference ---------------------------------------------------------

dataHL %>% 
  # group_by(LL,HL) %>% 
  filter(HL==1 | LL==1) %>%
  group_by(LL,HL) %>%
  summarise(
    assets=mean(TA/1000)) %>% t.test()

dataHL %>% 
  # group_by(LL,HL) %>% 
  filter(HL==1 | LL==1) %>%
  group_by(LL,HL) %>%
  summarise(
    costOfDebtFinancing = mean(FundingCost)) %>% t.test()

dataHL %>% 
  # group_by(LL,HL) %>% 
  filter(HL==1 | LL==1) %>%
  group_by(LL,HL) %>%
  summarise(
    growthRateOfDebtFinancing =mean(TFundGrow)) %>% t.test()

dataHL %>% 
  # group_by(LL,HL) %>% 
  filter(HL==1 | LL==1) %>%
  group_by(LL,HL) %>%
  summarise(
    growthRateOfLending = mean(LG)) %>% t.test()

dataHL %>% 
  # group_by(LL,HL) %>% 
  filter(HL==1 | LL==1) %>%
  group_by(LL,HL) %>%
  summarise(
    ROA = mean(ROA)) %>% t.test()




# --------------------------------------------------------------------------------
# Summary Statistics  ------------------------------------------------------------
# --------------------------------------------------------------------------------


# summary statistics for all the variables

summData <- as.matrix(data[,6:dim(data)[2]]) # just numeric variables
summaryResults <- list()

allMin <- apply(summData, MARGIN = 2, FUN = min, na.rm=TRUE)
allMean <- apply(summData, MARGIN = 2, FUN = mean, na.rm=TRUE)
allMax <- apply(summData, MARGIN = 2, FUN = max, na.rm=TRUE)
allSD <- apply(summData, MARGIN = 2, FUN = sd, na.rm=TRUE)
allCorr <- round(cor(summData[,-c(1:4)]),digits = 4) # maybe a heatmap
summaryResults$All <- round(data.frame(allMin,allMean,allMax,allSD), digits = 3)

logAssEqSumm <- data %>% 
  select(TA,TCE) %>% 
  summarise(
    MeanTA=mean(log(TA/1000)),
    SdTA=sd(log(TA/1000)),
    MinTA=min(log(TA/1000)),
    MaxTA=max(log(TA/1000)),
    MeanTCE=mean(log(TCE/1000)),
    SdTCE=sd(log(TCE/1000)),
    MinTCE=min(log(TCE/1000)),
    MaxTCE=max(log(TCE/1000))) 

write.table(logAssEqSumm,
            file = 'clipboard',
                    sep = " ",
                    qmethod = 'double',
                    row.names = TRUE,
                    col.names = TRUE)



write.table(summaryResults$All,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)

# Overall Summary ----------------------------------------------------------------




data %>% 
  summarise(
    N=n(),
    MeanEq = mean(log(TCE)),
    SDEq = sd(log(TCE)),
    MinEq = min(log(TCE)),
    MaxEq = max(log(TCE)),
    
  )




# country wise panel summary ---------------------------------------------------

data %>% 
  group_by(countryOfOrigin,year) %>% 
  summarise(
    N=n(),
    avgCoF = mean(FundingCost),
    avgLG = mean(LG),
    avgFG = mean(TFundGrow)
    
  )



# Need to draw the figures (4 figures)

# Asset and total equity (i)

dataFig <- data
dataFig$year <- as.numeric(dataFig$year)

fig_A <- dataFig %>% 
  group_by(year) %>% 
  summarise(
    `Median`=median(TA_TE),
    `25th Percentile` = quantile(TA_TE, probs = 0.25),
    `75th Percentile` = quantile(TA_TE, probs = 0.75)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -year,names_to = "variable",values_to = "value") %>% 
  ggplot(mapping = aes(x=year, y=value, col=variable))+
  geom_line()+
  ylim(c(8,30))+
  theme_bw()+
  xlab('')+
  ylab('')+
  labs(title = "Definition of leverage",
       subtitle = "Total Asset/Total Equity",
       caption = "Data source: Bloomberg") +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0))
  

# RWA and TierCap (i)

fig_B <- dataFig %>% 
  group_by(year) %>% 
  summarise(
    `Median`=median(RWA_TierCap),
    `25th Percentile` = quantile(RWA_TierCap, probs = 0.25),
    `75th Percentile` = quantile(RWA_TierCap, probs = 0.75)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -year,names_to = "variable",values_to = "value") %>% 
  ggplot(mapping = aes(x=year, y=value, col=variable))+
  geom_line()+
  theme_bw()+
  xlab('')+
  ylab('')+
  labs(title = " ",
       subtitle = "Risk Weighted Asset/Tier 1 Capital") +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0))

# Total fair value of the asset and the market value of the equity (need the market value of equity) (iv)

fig_C <- dataFig %>% 
  group_by(year) %>% 
  summarise(
    `Median`=median(FVA_TCE),
    `25th Percentile` = quantile(FVA_TCE, probs = 0.25),
    `75th Percentile` = quantile(FVA_TCE, probs = 0.75)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -year,names_to = "variable",values_to = "value") %>% 
  ggplot(mapping = aes(x=year, y=value, col=variable))+
  geom_line()+
  theme_bw()+
  xlab('')+
  ylab('')+
  labs(title = "",
       subtitle = "Market Value of Asset/Market Value of Equity") +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0))


# using patch work package for combining multiple graph at once ------------------


library(gridExtra)
library(patchwork)
library(cowplot)

fig_A|fig_B/fig_C



# --------------------------------------------------------------------------------
# # questions 1: Elasticity of bank activity with respect to bank capital --------
# --------------------------------------------------------------------------------

# log (total asset) == log (total equity) ------(i)
# log (total asset) == log (total equity)+ROA ------(ii)
# log (total asset) == log (total equity)+ROA + firm fixed effect ------(iii)
# log (total asset) == log (total equity)+ROA + (firm + year) fixed effect ------(iv)

q1modeli <- plm(formula = log(TA)~log(TCE),model = 'within',data = data)
q1modelii <- plm(formula = log(TA)~log(TCE)+ROA+TAG+factor(IFRS),model = 'within',data = data)
q1modeliii <- plm(formula = log(TA)~log(TCE)+ROA+TAG+factor(IFRS),model = 'within',index = 'bankID', data = data)
q1modeliv <- plm(formula = log(TA)~log(TCE)+ROA+TAG+factor(IFRS),model = 'within',index = c('bankID','year'),data = data)
q1modelv <- plm(formula = log(TA)~log(TCE)+ROA,model = 'within',effect = 'twoways',index = c('bankID','year'),data = data) # we haven't reported that one....


# compiling the results
result1 <- stargazer(q1modeli,q1modelii,q1modeliii,q1modeliv, q1modeliv, type = 'text',
          omit.summary.stat = 'mean',header = TRUE,
          digits = 3)


write.table(result1,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)

# coeftest
lmtest::coeftest(q1modeli,vcovHC(q1modeli,type = 'HC0',cluster = 'group'))

# serial correlation method
pbgtest(q1modeli,order = 1)


# handful try --------------------------------------------------------------------


# # unit root test of the variables
# ur.df(as.data.frame(q1modeli$residuals),type = 'none', lags = NULL, model='fixed') %>% summary()
# punitroot(q1modeli$residuals,test='adf')
# 
# library(tseries)
# x <- as.data.frame(q1modeli$residuals)
# pp.test(x$`q1modeli$residuals`, alternative = 'stationary') %>% summary()
# 


# --------------------------------------------------------------------------------
#  2. How does equity react to changes in the business and financial cycle? ---------
# --------------------------------------------------------------------------------

# growth rate of common equity == gdp growth+lag(common equity growth)+ROA+IFRS, index=('bankID') ------(i)
# growth rate of common equity == stock market growth + lag(common equity growth)+ROA+IFRS, index=('bankID')------(ii)
# growth rate of common equity == gdp growth:crisis_II+lag(common equity growth)+ROA+IFRS, index=('bankID') ------(iii)
# growth rate of common equity == stock market growth:crisis_II+lag(common equity growth)+ROA+IFRS, index=('bankID') ------(iv)


# 2nd part -----------------------------------------------------------------------


# growth rate of Tier 1 capital == gdp growth+lag( growth rate of Tier 1 capita)+ROA+IFRS, index=('bankID') ------(i)
# growth rate of Tier 1 capital == stock market growth +lag( growth rate of Tier 1 capita)+ROA+IFRS, index=('bankID') ------(ii)
# growth rate of Tier 1 capital == gdp growth:crisis_II + lag( growth rate of Tier 1 capita)+ROA+IFRS, index=('bankID')------(iii)
# growth rate of Tier 1 capital == stock market growth:crisis_II +lag( growth rate of Tier 1 capita)+ROA+IFRS, index=('bankID') ------(iv)


q2_1modeli <- plm(formula = TCEG~lag(TCEG)+GDPGrowth+ROA+TAG+factor(IFRS),
                  index = 'bankID',model = 'within',data = data)

q2_1modelii <- plm(formula = TCEG~lag(TCEG)+SMGSnP500+ROA+TAG+factor(IFRS),
                   index = 'bankID',model = 'within',data = data)

q2_1modeliii <- plm(formula = TCEG~lag(TCEG)+(GDPGrowth*crisisGfc)+ROA+TAG+factor(IFRS),
                    index = 'bankID',model = 'within',data = data)

q2_1modeliv <- plm(formula = TCEG~lag(TCEG)+(SMGSnP500*crisisGfc)+ROA+TAG+factor(IFRS),
                   index = 'bankID',model = 'within',data = data)

stargazer(q2_1modeli,q2_1modelii,q2_1modeliii,q2_1modeliv, type = 'text',
          omit.summary.stat = 'mean',
          digits=3)


result2_1 <- stargazer(q2_1modeli,q2_1modelii,q2_1modeliii,q2_1modeliv, type = 'text',
                       omit.summary.stat = 'mean',
                       digits=3)

write.table(result2_1,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)

pbgtest(q2_1modeli,order = 1)
pbgtest(q2_1modelii,order = 1)
pbgtest(q2_1modeliii,order = 1)
pbgtest(q2_1modeliv,order = 1)


# 2nd part -----------------------------------------------------------------------

q2_2modeli <- plm(formula = TierCapGrowth~lag(TierCapGrowth)+GDPGrowth+ROA+TAG+factor(IFRS),
                  index = 'bankID',model = 'within',data = data)

q2_2modelii <- plm(formula = TierCapGrowth~lag(TierCapGrowth)+SMGSnP500+ROA+TAG+factor(IFRS),
                   index = 'bankID',model = 'within',data = data)

q2_2modeliii <- plm(formula = TierCapGrowth~lag(TierCapGrowth)+(GDPGrowth*crisisGfc)+ROA+TAG+factor(IFRS),
                    index = 'bankID',model = 'within',data = data)

q2_2modeliv <- plm(formula = TierCapGrowth~lag(TierCapGrowth)+(SMGSnP500*crisisGfc)+ROA+TAG+factor(IFRS),
                   index = 'bankID',model = 'within',data = data)

stargazer(q2_2modeli,q2_2modelii,q2_2modeliii,q2_2modeliv, type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

result2_2 <- stargazer(q2_2modeli,q2_2modelii,q2_2modeliii,q2_2modeliv, type = 'text',
                       omit.summary.stat = 'mean',
                       digits = 3)

write.table(result2_2,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)


pbgtest(q2_2modeli,order = 1)
pbgtest(q2_2modelii,order = 1)
pbgtest(q2_2modeliii,order = 1)
pbgtest(q2_2modeliv,order = 1)


# --------------------------------------------------------------------------------
# question 3: Impact of bank capitalisation on funding costs The -----------------
# --------------------------------------------------------------------------------

# without macroVar ---------------------------------------------------------------

q3_1modeli <- plm(formula = FundingCost~lag(FundingCost)+lag(TA_TE)
                  +ROA +TAG+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q3_1modelii <- plm(formula = FundingCost~lag(FundingCost)+lag(RWA_TierCap)
                   +ROA+TAG+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'with',data = data)

# 2nd part (with macro variables) -------------------------------------------------


q3_2modeli <- plm(formula = FundingCost~lag(FundingCost)+lag(TA_TE)
                  +GDPGrowth+SMGSnP500
                  +ROA+ TAG +factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q3_2modelii <- plm(formula = FundingCost~lag(FundingCost)+lag(RWA_TierCap)
                   +GDPGrowth+SMGSnP500
                   +ROA+TAG+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',data = data)

q3_2modeliii <- plm(formula = FundingCost~lag(FundingCost)+lag((TCE/TA)*100)
                   +GDPGrowth+SMGSnP500
                   +ROA+TAG+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',data = data)


stargazer(q3_1modeli, q3_1modelii, q3_2modeli, q3_2modelii, q3_2modeliii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

result3 <- stargazer(q3_1modeli, q3_1modelii, q3_2modeli, q3_2modelii, q3_2modeliii,
                     type = 'text',
                     omit.summary.stat = 'mean',
                     digits = 3)

write.table(result3,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)




pbgtest(q3_1modeli,order = 1)
pbgtest(q3_1modelii,order = 1)
pbgtest(q3_2modeli,order = 1)
pbgtest(q3_2modelii,order = 1)
pbgtest(q3_2modeliii,order = 1)


# Try GMM method -----------------------------------------------------------------

q3_1modelii_i <- plm(formula = FundingCost~lag(FundingCost)+lag(RWA_TierCap)
                   +ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',data = data)
q3_1modelii_ii <- plm(formula = FundingCost~lag(FundingCost)+lag(RWA_TierCap)
                   +ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'random',data = data)
phtest(q3_1modelii_i,q3_1modelii_ii)


pgmm(FundingCost ~  lag(RWA_TierCap, 0:1)
     + ROA | lag(FundingCost, 5:10),
     data = data, effect = "twoways", model = "twostep")



# Attention: Need to use GMM for all  the method ---------------------------------

pgmm(FundingCost ~  lag(RWA_TierCap)
     + ROA | lag(FundingCost),
     data = data, effect = "twoways", model = "twostep") %>% summary()



# --------------------------------------------------------------------------------
# 4. Do less leveraged banks get more funding? The ---------------------------------
# --------------------------------------------------------------------------------

# without macroVar ---------------------------------------------------------------

q4_1modeli <- plm(formula = TFundGrow~lag(TFundGrow)+lag(TA_TE)+ROA+TAG+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)
q4_1modelii <- plm(formula = TFundGrow~lag(TFundGrow)+lag(RWA_TierCap)+ROA+TAG+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',
                   data = data)

# 2nd part (with macro variables) -------------------------------------------------


q4_2modeli <- plm(formula = TFundGrow~lag(TFundGrow)+lag(TA_TE)
                  +GDPGrowth+SMGSnP500+ROA+TAG+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q4_2modelii <- plm(formula = TFundGrow~lag(TFundGrow)+lag(RWA_TierCap)
                   +GDPGrowth+SMGSnP500+ROA+TAG+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',data = data)

stargazer(q4_1modeli, q4_1modelii, q4_2modeli, q4_2modelii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

result4 <- stargazer(q4_1modeli, q4_1modelii, q4_2modeli, q4_2modelii,
                     type = 'text',
                     omit.summary.stat = 'mean',
                     digits = 3)


write.table(result4,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)




pbgtest(q4_1modeli,order = 1)
pbgtest(q4_1modelii,order = 1)
pbgtest(q4_2modeli,order = 1)
pbgtest(q4_2modelii,order = 1)



# --------------------------------------------------------------------------------
# 5. Do less leveraged banks supply more credit? ---------------------------------
# --------------------------------------------------------------------------------


# without macroVar ---------------------------------------------------------------

q5_1modeli <- plm(formula = LG~lag(LG)+lag(TA_TE)+ROA+TAG+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)
q5_1modelii <- plm(formula = LG~lag(LG)+lag(RWA_TierCap)+TAG+ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',
                   data = data)

# 2nd part (with macro variables) -------------------------------------------------


q5_2modeli <- plm(formula = LG~lag(LG)+lag(TA_TE)+ROA+TAG+factor(IFRS)
                  +GDPGrowth+SMGSnP500,
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q5_2modelii <- plm(formula = LG~lag(LG)+lag(RWA_TierCap)+ROA+TAG+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'fd',data = data)

q5_2modeliii <- plm(formula = LG~lag(LG)+lag(TCE/TA*100)+ROA+TAG+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'fd',data = data)

result5 <- stargazer(q5_1modeli, q5_1modelii, q5_2modeli, q5_2modelii,q5_2modeliii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)


write.table(result5,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)




pbgtest(q5_1modeli,order = 1)
pbgtest(q5_1modelii,order = 1)
pbgtest(q5_2modeli,order = 1)
pbgtest(q5_2modelii,order = 1)
pbgtest(q5_2modeliii,order = 1)


# --------------------------------------------------------------------------------
# 7. Is it a simple rebalancing effect? (changes in equity growth) ---------------------------------
# --------------------------------------------------------------------------------


# without macroVar ---------------------------------------------------------------

q7_1modeli <- plm(formula = TCEG~lag(TCEG)+lag(TA_TE)+ROA+TAG+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)
q7_1modelii <- plm(formula = TCEG~lag(TCEG)+lag(RWA_TierCap)+TAG+ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',
                   data = data)

# 2nd part (with macro variables) -------------------------------------------------


q7_2modeli <- plm(formula = TCEG~lag(TCEG)+lag(TA_TE)+ROA+TAG+factor(IFRS)
                  +GDPGrowth+SMGSnP500,
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q7_2modelii <- plm(formula = TCEG~lag(TCEG)+lag(RWA_TierCap)+ROA+TAG+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'within',data = data)

q7_2modeliii <- plm(formula = TCEG~lag(TCEG)+lag(TA/TCE*100)+ROA+TAG+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'within',data = data)

result7 <- stargazer(q7_1modeli, q7_1modelii, q7_2modeli, q7_2modelii,q7_2modeliii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)


pbgtest(q7_1modeli,order = 1)
pbgtest(q7_1modelii,order = 1)
pbgtest(q7_2modeli,order = 1)
pbgtest(q7_2modelii,order = 1)
pbgtest(q7_2modeliii,order = 1)


# --------------------------------------------------------------------------------
# 7.maket value and accounting value ---------------------------------
# --------------------------------------------------------------------------------


# without macroVar ---------------------------------------------------------------

q8_1modeli <- plm(formula = FundingCost~lag(FundingCost)+lag(FVA_TCE)+ROA+TAG+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)
q8_1modelii <- plm(formula = TFundGrow~lag(TFundGrow)+lag(FVA_TCE)+TAG+ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',
                   data = data)
q8_1modeliii <- plm(formula = LG~lag(LG)+lag(FVA_TCE)+TAG+ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',
                   data = data)


# 2nd part (with macro variables) -------------------------------------------------


q8_2modeli <- plm(formula = FundingCost~lag(FundingCost)+lag(FVA_TCE)+ROA+TAG+factor(IFRS)
                  +GDPGrowth+SMGSnP500,
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q8_2modelii <- plm(formula = TFundGrow~lag(TFundGrow)+lag(FVA_TCE)+ROA+TAG+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'within',data = data)

q8_2modeliii <- plm(formula = LG~lag(LG)+lag(FVA_TCE)+ROA+TAG+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'within',data = data)

result8 <- stargazer( q8_2modeli, q8_2modelii,q8_2modeliii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

result8 <- stargazer(q8_1modeli, q8_1modelii,q8_1modeliii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)


pbgtest(q8_1modeli,order = 1)
pbgtest(q8_1modelii,order = 1)
pbgtest(q8_1modeliii,order = 1)
pbgtest(q8_2modeli,order = 1)
pbgtest(q8_2modelii,order = 1)
pbgtest(q8_2modeliii,order = 1)



# --------------------------------------------------------------------------------
# 4.8. The effect of bank capital in the monetary transmission mechanism ------
# --------------------------------------------------------------------------------


# without macroVar ---------------------------------------------------------------

q6_1modeli <- plm(formula = LG~lag(LG)+lag(TA_TE)+(lag(TA_TE)*MPGrowth),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q6_1modelii <- plm(formula = LG~lag(LG)+lag(RWA_TierCap)+(lag(RWA_TierCap)*MPGrowth),
                   index = c('bankID','year'),
                   model = 'within',
                   data = data)

# 2nd part (with macro variables) -------------------------------------------------


q6_2modeli <- plm(formula = LG~lag(LG)+lag(TA_TE)+(lag(TA_TE)*MPGrowth)+
                    ROA+TAG+factor(IFRS)+
                    GDPGrowth+SMGSnP500,
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q6_2modelii <- plm(formula = LG~lag(LG)+lag(RWA_TierCap)+(lag(RWA_TierCap)*MPGrowth)+
                     ROA+TAG+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'within',data = data)

stargazer(q6_1modeli, q6_1modelii, q6_2modeli, q6_2modelii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

result6 <- stargazer(q6_1modeli, q6_1modelii, q6_2modeli, q6_2modelii,
                     type = 'text',
                     omit.summary.stat = 'mean',
                     digits = 3)

write.table(result6,
            file = 'clipboard',
            sep = " ",
            qmethod = 'double',
            row.names = TRUE,
            col.names = TRUE)



pbgtest(q6_1modeli,order = 1)
pbgtest(q6_1modelii,order = 1)
pbgtest(q6_2modeli,order = 1)
pbgtest(q6_2modelii,order = 1)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# further anlysis for my parts
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dataWork <- as.data.frame(data)
# dataWork$year <- as.Date(dataWork$year,format = '%Y')
# dataWork$year <- format(as.Date(dataWork$year), format = "%Y")


graph <- dataWork %>% 
  group_by(year) %>% 
  summarise(
    totalLoan=mean(TLoan/1000),
    totalCE=mean(TCE/1000),
    totalASs=mean(TA/1000),
    totalfun=mean(TFunding/1000),
    
  ) 

plot(
  x=graph$year,
  y=graph$totalLoan,
  type='l',
  xlab = "",
  ylab = "Billion (USD)",
  main = 'Sample Financial Overview (1989 - 2022)',
  col='red',
  ylim=c(0,1100)
)

lines(
  x=graph$year,
  y=graph$totalCE,
  col='blue'
)
lines(
  x=graph$year,
  y=graph$totalASs,
  col='green'
)
lines(
  x=graph$year,
  y=graph$totalfun,
  col='yellow'
)

legend("topleft", # orientation of legend
       legend = c('Average Loan','Average Equity', 'Average Asset', 'Average Debt Funding'),
       bg = "NA", # no background
       bty = "n", # no box around legend
       cex = 0.75, # character expansion factor, 0.5 = half of the standard size
       col = c("red","blue",'green','yellow'), # colour of the lines
       lty = 1) 

























































