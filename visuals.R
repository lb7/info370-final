library(ggplot2)
library(tidyr)

n <- mydata %>%
  mutate(
    rating.f = predict(percentFemale_Rating), 
    rating.m = predict(percentMale_Rating),
    revenue.f = predict(percentFemale_Revenue),
    revenue.m = predict(percentMale_Revenue)
  )

rating.f <- ggplot(n, aes(percent_female, averageRating)) + 
  geom_point() + 
  geom_line(aes(percent_female, rating.f))

rating.m <- ggplot(n, aes(percent_male, averageRating)) + 
  geom_point() + 
  geom_line(aes(percent_male, rating.m))

revenue.f <- ggplot(n, aes(percent_female, revenue)) + 
  geom_point() + 
  geom_line(aes(percent_female, revenue.f))

revenue.m <- ggplot(n, aes(percent_male, revenue)) + 
  geom_point() + 
  geom_line(aes(percent_male, revenue.m))