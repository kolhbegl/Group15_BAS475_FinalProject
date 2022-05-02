library(fpp3)
library(feasts)
library(lubridate)
library(tseries)
library(dplyr)
library(forecast)
library(patchwork)
Credits <- read.csv("credit.csv")

Credits %>% 
  mutate(Month = c(492:1)) %>% 
  as_tsibble(index = Month) -> Empire_Credits


Empire_Credits %>% 
  autoplot(credit_in_millions) +
  labs(title = "Empire Credits Original")

# Pull Lamdba
Empire_Credits %>% 
  select(credit_in_millions) %>% 
  features(credit_in_millions, features = guerrero) %>%
  pull(lambda_guerrero) -> lambda

# Box-Cox Transformation
Empire_Credits %>%
  select(credit_in_millions) %>% 
  autoplot(box_cox(credit_in_millions,lambda )) +
  labs(y = "",
       title = 
         "Transformed Credit with Lambda",
         round(lambda,2))


holdout <- Empire_Credits %>% 
  filter(Month %in% c(tail(Empire_Credits$Month, 12)))

training <- Empire_Credits %>% 
  filter(Month %in% c(head(Empire_Credits$Month, 480)))


#Need to difference
gg_tsdisplay(training)

#Ignore for now
training %>%
  gg_tsdisplay(difference(credit_in_millions, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

training %>%
  gg_tsdisplay(difference(credit_in_millions, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

training %>% 
  gg_tsdisplay(difference(credit_in_millions, 12) %>% difference(),
               plot_type='partial', lag=12)

#Models
training %>% 
  stretch_tsibble(.init = 48, .step = 24) %>% 
  model(arima210 = ARIMA(credit_in_millions ~ pdq(2,1,0)),
        arima013 = ARIMA(credit_in_millions ~ pdq(0,1,3)),
        arima311 = ARIMA(credit_in_millions ~ pdq(3,1,1)),
        arima011 = ARIMA(credit_in_millions ~ pdq(0,1,1)),
        stepwise = ARIMA(credit_in_millions),
        search = ARIMA(credit_in_millions, stepwise=FALSE),
        ETS = ETS(credit_in_millions)) %>% 
  forecast(h = 12) %>%
  accuracy(training) -> TrainingModels

TrainingModels %>% arrange(RMSE)

training %>% 
  model(ETS = ETS(credit_in_millions)) -> BestModel

report(BestModel)
gg_tsresiduals(BestModel)

BestModel %>% 
  forecast(h=12) -> CreditsForecast

training %>% 
  autoplot(credit_in_millions) +
  autolayer(CreditsForecast)

y_pred <- BestModel %>% 
  forecast(h=12)

rmse <- function(y_actual, y_pred) {
  sqrt(mean((y_actual - y_pred)^2))
}

rmse(holdout$credit_in_millions, y_pred$.mean)

Empire_Credits %>% model(
  ETSmodel = ETS(credit_in_millions)) %>% 
  forecast(h=12)

#Says Arima011 is best prediction, but ETS RMSE is the same
auto.arima(training, seasonal = TRUE)
