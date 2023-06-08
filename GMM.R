

# --------------------------------------------------------------------------------
# # Dynamic GMM Method for the models --------------------------------------------
# --------------------------------------------------------------------------------

pgmm(FundingCost ~  lag(RWA_TierCap)
     + ROA | lag(FundingCost),
     data = data, effect = "twoways", model = "twostep") %>% summary()



# --------------------------------------------------------------------------------
# # questions 1: Elasticity of bank activity with respect to bank capital --------
# --------------------------------------------------------------------------------

# log (total asset) == log (total equity) ------(i)
# log (total asset) == log (total equity)+ROA ------(ii)
# log (total asset) == log (total equity)+ROA + firm fixed effect ------(iii)
# log (total asset) == log (total equity)+ROA + (firm + year) fixed effect ------(iv)

gmmq1modeli <- pgmm(formula = log(TA)~log(TCE) |log(TA), model = 'onestep',data = data, effect = "twoways")
gmmq1modelii <- plm(formula = log(TA)~log(TCE)+ROA,model = 'within',data = data)
gmmq1modeliii <- plm(formula = log(TA)~log(TCE)+ROA,model = 'within',index = c('bankID'), data = data)
gmmq1modeliv <- plm(formula = log(TA)~log(TCE)+ROA,model = 'within',index = c('bankID','year'),data = data)
gmmq1modelv <- plm(formula = log(TA)~log(TCE)+ROA,model = 'within',effect = 'twoways',index = c('bankID','year'),data = data)


# compiling the results
gmmresult1 <- stargazer(gmmq1modeli,gmmq1modelii,gmmq1modeliii,gmmq1modeliv,gmmq1modelv, type = 'text',
                        omit.summary.stat = 'mean',header = FALSE,
                        digits = 3)

write.table(result1, 
            file = 'clipboard',
            sep = "/",
            row.names = TRUE,
            col.names = TRUE)

# coeftest
lmtest::coeftest(q1modeli,vcovHC(q1modeli,type = 'HC0',cluster = 'group'))

# serial correlation method
pbgtest(q1modeli,order = 1)


# handful try --------------------------------------------------------------------


# unit root test of the variables
ur.df(as.data.frame(q1modeli$residuals),type = 'none', lags = NULL, model='fixed') %>% summary()
punitroot(q1modeli$residuals,test='adf')

library(tseries)
x <- as.data.frame(q1modeli$residuals)
pp.test(x$`q1modeli$residuals`, alternative = 'stationary') %>% summary()



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


q2_1modeli <- plm(formula = TCEG~lag(TCEG)+GDPGrowth+ROA+IFRS,
                  index = 'bankID',model = 'within',data = data)

q2_1modelii <- plm(formula = TCEG~lag(TCEG)+SMGSnP500+ROA+IFRS,
                   index = 'bankID',model = 'within',data = data)

q2_1modeliii <- plm(formula = TCEG~lag(TCEG)+GDPGrowth:crisisGfc+ROA+IFRS,
                    index = 'bankID',model = 'within',data = data)

q2_1modeliv <- plm(formula = TCEG~lag(TCEG)+SMGSnP500:crisisGfc+ROA+IFRS,
                   index = 'bankID',model = 'within',data = data)

stargazer(q2_1modeli,q2_1modelii,q2_1modeliii,q2_1modeliv, type = 'text',
          omit.summary.stat = 'mean',
          digits=3)
pbgtest(q2_1modeli,order = 1)
pbgtest(q2_1modelii,order = 1)
pbgtest(q2_1modeliii,order = 1)
pbgtest(q2_1modeliv,order = 1)


# 2nd part -----------------------------------------------------------------------

q2_2modeli <- plm(formula = TierCapGrowth~lag(TierCapGrowth)+GDPGrowth+ROA+IFRS,
                  index = 'bankID',model = 'within',data = data)

q2_2modelii <- plm(formula = TierCapGrowth~lag(TierCapGrowth)+SMGSnP500+ROA+IFRS,
                   index = 'bankID',model = 'within',data = data)

q2_2modeliii <- plm(formula = TierCapGrowth~lag(TierCapGrowth)+GDPGrowth:crisisGfc+ROA+IFRS,
                    index = 'bankID',model = 'within',data = data)

q2_2modeliv <- plm(formula = TierCapGrowth~lag(TierCapGrowth)+SMGSnP500:crisisGfc+ROA+IFRS,
                   index = 'bankID',model = 'within',data = data)

stargazer(q2_2modeli,q2_2modelii,q2_2modeliii,q2_2modeliv, type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

pbgtest(q2_2modeli,order = 1)
pbgtest(q2_2modelii,order = 1)
pbgtest(q2_2modeliii,order = 1)
pbgtest(q2_2modeliv,order = 1)


# --------------------------------------------------------------------------------
# question 3: Impact of bank capitalisation on funding costs The -----------------
# --------------------------------------------------------------------------------

# without macroVar ---------------------------------------------------------------

q3_1modeli <- plm(formula = FundingCost~lag(FundingCost)+lag(TA_TE)
                  +ROA +factor(IFRS),
                  index = c('bankID','year'),
                  model = 'fd',
                  data = data)

q3_1modelii <- plm(formula = FundingCost~lag(FundingCost)+lag(RWA_TierCap)
                   +ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'fd',data = data)

# 2nd part (with macro variables) -------------------------------------------------


q3_2modeli <- plm(formula = FundingCost~lag(FundingCost)+lag(TA_TE)
                  +GDPGrowth+SMGSnP500
                  +ROA +factor(IFRS),
                  index = c('bankID','year'),
                  model = 'fd',
                  data = data)

q3_2modelii <- plm(formula = FundingCost~lag(FundingCost)+lag(RWA_TierCap)
                   +GDPGrowth+SMGSnP500
                   +ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'fd',data = data)

q3_2modeliii <- plm(formula = FundingCost~lag(FundingCost)+lag((TCE/TA)*100)
                    +GDPGrowth+SMGSnP500
                    +ROA+factor(IFRS),
                    index = c('bankID','year'),
                    model = 'fd',data = data)


stargazer(q3_1modeli, q3_1modelii, q3_2modeli, q3_2modelii, q3_2modeliii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

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

q4_1modeli <- plm(formula = TFundGrow~lag(TFundGrow)+lag(TA_TE)+ROA+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)
q4_1modelii <- plm(formula = TFundGrow~lag(TFundGrow)+lag(RWA_TierCap)+ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',
                   data = data)

# 2nd part (with macro variables) -------------------------------------------------


q4_2modeli <- plm(formula = TFundGrow~lag(TFundGrow)+lag(TA_TE)
                  +GDPGrowth+SMGSnP500+ROA+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q4_2modelii <- plm(formula = TFundGrow~lag(TFundGrow)+lag(RWA_TierCap)
                   +GDPGrowth+SMGSnP500+ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'fd',data = data)

stargazer(q4_1modeli, q4_1modelii, q4_2modeli, q4_2modelii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

pbgtest(q4_1modeli,order = 1)
pbgtest(q4_1modelii,order = 1)
pbgtest(q4_2modeli,order = 1)
pbgtest(q4_2modelii,order = 1)



# --------------------------------------------------------------------------------
# 5. Do less leveraged banks supply more credit? ---------------------------------
# --------------------------------------------------------------------------------


# without macroVar ---------------------------------------------------------------

q5_1modeli <- plm(formula = LG~lag(LG)+lag(TA_TE)+ROA+factor(IFRS),
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)
q5_1modelii <- plm(formula = LG~lag(LG)+lag(RWA_TierCap)+ROA+factor(IFRS),
                   index = c('bankID','year'),
                   model = 'within',
                   data = data)

# 2nd part (with macro variables) -------------------------------------------------


q5_2modeli <- plm(formula = LG~lag(LG)+lag(TA_TE)+ROA+factor(IFRS)
                  +GDPGrowth+SMGSnP500,
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q5_2modelii <- plm(formula = LG~lag(LG)+lag(RWA_TierCap)+ROA+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'fd',data = data)

stargazer(q5_1modeli, q5_1modelii, q5_2modeli, q5_2modelii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

pbgtest(q5_1modeli,order = 1)
pbgtest(q5_1modelii,order = 1)
pbgtest(q5_2modeli,order = 1)
pbgtest(q5_2modelii,order = 1)



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
                    ROA+factor(IFRS)+
                    GDPGrowth+SMGSnP500,
                  index = c('bankID','year'),
                  model = 'within',
                  data = data)

q6_2modelii <- plm(formula = LG~lag(LG)+lag(RWA_TierCap)+(lag(RWA_TierCap)*MPGrowth)+
                     ROA+factor(IFRS)
                   +GDPGrowth+SMGSnP500,
                   index = c('bankID','year'),
                   model = 'fd',data = data)

stargazer(q6_1modeli, q6_1modelii, q6_2modeli, q6_2modelii,
          type = 'text',
          omit.summary.stat = 'mean',
          digits = 3)

pbgtest(q6_1modeli,order = 1)
pbgtest(q6_1modelii,order = 1)
pbgtest(q6_2modeli,order = 1)
pbgtest(q6_2modelii,order = 1)



















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



# (c) ==================
dataFurnished <- data
colnames(dataFurnished) <- tolower(gsub(" ", "_", colnames(dataFurnished)))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ use this thought in the whole area ======


plm3_i <- plm(formula = log(dataFurnished$total_common_equity) ~(log(dataFurnished$total_assets)), 
              index = c('bankid','year'),
              data = dataFurnished,
              model = 'within',
              effect = 'individual')
stargazer(plm3_i, plm1_i, type = 'text', omit.summary.stat = 'mean', digits = 3) # to present the data in a nicer way

# Extracting the fixed effects of the individual level

a_i <- fixef(plm3_i, type = 'dmean')
summary(a_i)
data.frame(a_i) %>% stargazer(type = 'text')


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


plm3_i <- plm(formula = `Interest.Expense.`~lag(`Total.Common.Equity`/`Total.Assets`)
              +lag(`Interest.Expense.`)
              +`Return.on.Assets`
              +`GAAP.IFRS`,
              data = pData, 
              model = 'within',
              effect = 'twoways')
stargazer(plm3_i, type = 'text')


# Need to complete the funding analysis issues as soon as possible (tomorrow will be even better)


