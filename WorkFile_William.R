library(fpp3)
library(feasts)
library(lubridate)

Credits <- read.csv("credit.csv")

Credits %>% 
  mutate(Month = c(492:1)) %>% 
  as_tsibble(index = Month) -> Empire_Credits

Empire_Credits %>%
  gg_tsdisplay()

Empire_Credits %>% 
  autoplot(ï..credit_in_millions)

#Box Cox data
lambda <- Empire_Credits %>%
  features(ï..credit_in_millions, features = guerrero) %>%
  pull(lambda_guerrero)
Empire_Credits %>%
  mutate(transform = box_cox(ï..credit_in_millions, lambda)) -> box_coxEC

Empire_Credits %>%
  autoplot(box_cox(ï..credit_in_millions, lambda))

box_coxEC %>%
  gg_tsdisplay(transform)

#Log data 
Empire_Credits %>% 
  mutate(transform = log(ï..credit_in_millions)) -> logEC

Empire_Credits %>% 
  autoplot(log(ï..credit_in_millions))

box_coxEC %>%
  gg_tsdisplay(transform)

box_coxEC %>%
  features(transform, unitroot_kpss) 

box_coxEC %>%
  features(transform, unitroot_ndiffs)

box_coxEC %>%
  mutate(diff_EC = difference(transform)) -> box_coxEC


training <- box_coxEC %>%
  filter(box_coxEC$Month <=480)

Holdout <- box_coxEC %>%
  filter(box_coxEC$Month >480)



training %>%
  stretch_tsibble(.init = 48, .step = 24)%>%
  filter(!is.na(diff_EC)) %>%
  model(arima210 = ARIMA(diff_EC ~ pdq(2,1,0)), 
        arima013 = ARIMA(diff_EC~ pdq(0,1,0)),
        stepwise = ARIMA(diff_EC),
        search = ARIMA(diff_EC, stepwise = FALSE),
        ETS = ETS(diff_EC)) %>%
  forecast( h = 12) %>%
  accuracy(training) -> FIT
FIT %>% 
  arrange(RMSE)


training %>% model(
  search = ARIMA(diff_EC, stepwise = FALSE)) -> best
best %>% 
  forecast(h = 12) -> bestfc


sqrt(mean((Holdout$diff_EC - bestfc$.mean)^2))











