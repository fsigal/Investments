library(quantmod)
library(tidyverse)
library(writexl)
library(zoo)

# Import Stock Data from Yahoo Finance and convert to DataFrame
quantmod::getSymbols("ZM")
series <- as.data.frame(ZM)

# 2-weeks variation
series1 <- series %>% mutate(series.Close = ZM.Close) %>% 
  mutate(variation = (series.Close/lag(series.Close, n = 10)-1)) %>%
  mutate(Prev11 =(lag(series.Close, n = 11)/lag(series.Close, n = 12)-1), 
         Prev12 =(lag(series.Close, n = 12)/lag(series.Close, n = 13)-1),
         Prev13 =(lag(series.Close, n = 13)/lag(series.Close, n = 14)-1),
         Prev14 =(lag(series.Close, n = 14)/lag(series.Close, n = 15)-1),
         Prev15 =(lag(series.Close, n = 15)/lag(series.Close, n = 16)-1),
         Prev16 =(lag(series.Close, n = 16)/lag(series.Close, n = 17)-1),
         Prev17 =(lag(series.Close, n = 17)/lag(series.Close, n = 18)-1),
         Prev18 =(lag(series.Close, n = 18)/lag(series.Close, n = 19)-1),
         Prev19 =(lag(series.Close, n = 19)/lag(series.Close, n = 20)-1),
         Prev20 =(lag(series.Close, n = 20)/lag(series.Close, n = 21)-1),
         Prev11b =(lag(series.Close, n = 11)/lag(series.Close, n = 16)-1),
         Prev11c =(lag(series.Close, n = 11)/lag(series.Close, n = 21)-1),
         Prev11d =(lag(series.Close, n = 11)/lag(series.Close, n = 26)-1),
         moving_avg14 = rollmean(variation, k = 14, align = "right", fill = NA),
         moving_avg21 = rollmean(variation, k = 21, align = "right", fill = NA))
series2 = na.omit(series1)
  
# Split between train and test sets (last 20 obs in test)
test <- tail(series2, 24)
train <- head(series2, length(series2$variation)-24)

# Linear Regression
reg_lin1 <- lm(variation ~ Prev11 + Prev12 + Prev13 + Prev14 + Prev15 + Prev16 +
                Prev17 + Prev18 + Prev19 + Prev20 + Prev11b + Prev11c + Prev11d + 
                moving_avg14 + moving_avg21, 
              data = train)
summary(reg_lin1)

pred_lin1 <- predict(reg_lin1, newdata = test )
coef <- cor.test(pred_lin1,test$variation)
coef
error <- cbind(pred_lin1,test$variation)

