library(fpp3)
library(feasts)
library(lubridate)

Credits <- read.csv("credit.csv")

Credits %>% 
  mutate(Month = c(492:1)) %>% 
  as_tsibble(index = Month) -> Empire_Credits


Empire_Credits %>% 
  autoplot(credit_in_millions)


lambda <- Empire_Credits %>%
  features(credit_in_millions, features = guerrero) %>%
  pull(lambda_guerrero)

Empire_Credits %>%
  autoplot(box_cox(credit_in_millions, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed Empire Credits with $\\lambda$ = ",
         round(lambda,4))))

#Box Cox basically didn't do anything, so we should not use that.


holdout <- Empire_Credits %>% 
  filter(Month %in% c(tail(Empire_Credits$Month, 12)))

training <- Empire_Credits %>% 
  filter(Month %in% c(head(Empire_Credits$Month, 480)))

training %>% 
  stretch_tsibble(.init = 48, .step = 24) %>% 
  model(
        arima210 = ARIMA(credit_in_millions ~ pdq(2,1,0)),
        arima110 = ARIMA(credit_in_millions ~ pdq(1,1,0)),
        arima310 = ARIMA(credit_in_millions ~ pdq(3,1,0)),
        arima111 = ARIMA(credit_in_millions ~ pdq(1,1,1)),
        ETSmodel = ETS(credit_in_millions)) %>% 
  forecast(h=12) %>% 
  accuracy(training) -> training_cv

training_cv %>% 
  arrange(RMSE)

#ETS appears to be the best 

training %>% model(
  ETSmodel = ETS(credit_in_millions)) -> bestfit

report(bestfit)

gg_tsresiduals(bestfit)

bestfit %>% 
  forecast(h=12) -> bestforecast

training %>% 
  autoplot(credit_in_millions)+
  autolayer(bestforecast)

pred <- bestfit %>% 
  forecast(h=12)

y_pred <- pred

rmse <- function(y_actual, y_pred) {
  sqrt(mean((y_actual - y_pred)^2))
}

rmse(holdout$credit_in_millions, y_pred$.mean)

Empire_Credits %>% model(
  ETSmodel = ETS(credit_in_millions)) -> bestfit1

bestfit1 %>% 
  forecast(h=12)

