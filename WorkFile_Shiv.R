library(fpp3)
library(feasts)
library(lubridate)

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









