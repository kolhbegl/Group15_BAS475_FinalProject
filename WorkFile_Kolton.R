library(fpp3)
library(feasts)
library(lubridate)

Credits <- read.csv("credit.csv")

Credits %>% 
  mutate(Month = c(492:1)) %>% 
  as_tsibble(index = Month) -> Empire_Credits1


Empire_Credits1 %>% 
  autoplot(credit_in_millions)

Empire_Credits2 <- Empire_Credits1

Empire_Credits3 <- Empire_Credits1

Empire_Credits4 <- Empire_Credits1







