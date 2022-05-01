library(fpp3)
library(feasts)
library(lubridate)

Credits <- read.csv("credit.csv")
colnames(Credits)[1] <- gsub('^...','',colnames(Credits)[1])


Credits %>% 
  mutate(Month = c(492:1)) %>% 
  as_tsibble(index = Month) -> Empire_Credits


Empire_Credits %>% 
  autoplot(credit_in_millions)

Empire_Credits %>% 
  model(LinearRegression = TSLM(credit_in_millions ~ trend())) %>%
  report()

### In our linear model investigating the relationship between credit in millions and and trend, 
# 72.5% % of the variation in credit in millions is explained by the trend.

### Transformation

lambda <- Empire_Credits %>%
  features(credit_in_millions, features = guerrero) %>%
  pull(lambda_guerrero)
Empire_Credits %>%
  autoplot(box_cox(credit_in_millions, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed credit in millions with $\\lambda$ = ",
         round(lambda,2))))


### TRAIN & HOLDOUT 

TRAIN <- Empire_Credits[1:492,]
HOLDOUT <- Empire_Credits[481:492,]


### Cross Validation

fit <- TRAIN %>% 
  stretch_tsibble(.init = 48, .step = 24)  %>% 
  model(
    arima012011 = ARIMA(credit_in_millions ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(credit_in_millions ~ pdq(2,1,0) + PDQ(0,1,1)),
    arima = ARIMA(credit_in_millions))


fit  %>% 
  forecast (h = 12) %>%  
  accuracy (TRAIN)  %>% 
  arrange(RMSE)

### Best model 

fit <- TRAIN %>%
  model(arima = ARIMA(credit_in_millions))
report(fit)
fit %>% select(arima) %>% gg_tsresiduals(lag=36)

## Forecast 

forecast(fit, h= 12) %>% 
  filter(.model == 'arima') %>% 
  autoplot(TRAIN)

## Predictions 

preds <- forecast(fit, TRAIN)

### Untransformed Predictions

untransformed_preds <- inv_box_cox(preds$.mean, lambda)

### Accuracy 

rmse <- function(preds, untransformed_preds) {
  sqrt(mean((preds - untransformed_preds)^2))
}


RMSE(untransformed_preds,TRAIN$credit_in_millions)


## The RMSE for our non transformed predictions is 3.79. This means that for each individual prediction, 
#  we expect our model to miss at a rate of 3.79 Imperial Credits (in millions of credits?) 
## This high RMSE value (considering credit values range from about 1-2 million)
## indicates that our model is a poor one, as the misses it makes are extremely consequential.  









