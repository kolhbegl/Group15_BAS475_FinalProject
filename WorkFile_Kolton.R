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
  model(lmodel = TSLM(credit_in_millions~Month),
        arima111110 = ARIMA(credit_in_millions ~ pdq(1,1,1) + PDQ(1,1,0)),
        arima210 = ARIMA(credit_in_millions ~ pdq(2,1,0)),
        arima110 = ARIMA(credit_in_millions ~ pdq(1,1,0)),
        arima310 = ARIMA(credit_in_millions ~ pdq(3,1,0)),
        arima111 = ARIMA(credit_in_millions ~ pdq(1,1,1)),
        ETSmodel = ETS(credit_in_millions)) %>% 
  forecast(h=12) %>% 
  accuracy(training) -> training_cv

Empire_Credits %>% 
  stretch_tsibble(.init = 48, .step = 24) %>% 
  model(mean = MEAN(credit_in_millions),
        NaiveM = NAIVE(credit_in_millions),
        NaiveDrift = NAIVE(credit_in_millions~drift()),
        lmodel = TSLM(credit_in_millions~Month),
        arima111110 = ARIMA(credit_in_millions ~ pdq(1,1,1) + PDQ(1,1,0)),
        arima210 = ARIMA(credit_in_millions ~ pdq(2,1,0)),
        arima110 = ARIMA(credit_in_millions ~ pdq(1,1,0)),
        arima310 = ARIMA(credit_in_millions ~ pdq(3,1,0)),
        arima111 = ARIMA(credit_in_millions ~ pdq(1,1,1)),
        ETSmodel = ETS(credit_in_millions),
        
        ) %>% 
  forecast(h=12) %>% 
  accuracy(Empire_Credits) %>% 
  arrange(RMSE)



training_cv %>% 
  arrange(RMSE)

#ETS appears to be the best 

training %>% model(
  NaiveDrift = NAIVE(credit_in_millions~drift())
  ) -> bestfit

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
  NaiveDrift = NAIVE(credit_in_millions~drift())
             ) %>% 
  forecast(h=12)


####################################################

Empire_Credits %>% 
  mutate(Credit_Diff = difference(credit_in_millions)) -> Empire_Credits2


Empire_Credits2 <- Empire_Credits2[-1,]

Empire_Credits2 %>% 
  gg_tsdisplay(Credit_Diff)

Empire_Credits2 %>% 
  features(Credit_Diff, unitroot_kpss)

Empire_Credits2 %>% 
  autoplot(Credit_Diff)

holdout2 <- Empire_Credits2 %>% 
  filter(Month %in% c(tail(Empire_Credits2$Month, 12)))

training2 <- Empire_Credits %>% 
  filter(Month %in% c(head(Empire_Credits2$Month, 479)))

training2 %>% 
  stretch_tsibble(.init = 48, .step = 24) %>% 
  model(
        arima111110 = ARIMA(Credit_Diff ~ pdq(1,1,1) + PDQ(1,1,0)),
        arima210 = ARIMA(Credit_Diff ~ pdq(2,1,0)),
        arima110 = ARIMA(Credit_Diff ~ pdq(1,1,0)),
        arima310 = ARIMA(Credit_Diff ~ pdq(3,1,0)),
        arima111 = ARIMA(Credit_Diff ~ pdq(1,1,1)),
        ETSmodel = ETS(Credit_Diff)) %>% 
  forecast(h=12) %>% 
  accuracy(training2) %>% 
  arrange(RMSE)

training2 %>% model(
  ETSmodel = ETS(Credit_Diff)) -> bestfit

report(bestfit)

gg_tsresiduals(bestfit)

bestfit %>% 
  forecast(h=12) -> bestforecast


pred <- bestfit %>% 
  forecast(h=12)

y_pred <- pred

rmse(holdout2$Credit_Diff, y_pred$.mean)


Empire_Credits2 %>% model(
  ETSmodel = ETS(Credit_Diff)) %>% 
  forecast(h=12)





