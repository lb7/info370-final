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
  geom_jitter() + 
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

cols <- c(Male='navy', Female='magenta4', Female_Fit='violet', Male_Fit='skyblue')

v.ratings <- ggplot(n) +
  geom_jitter(aes(percent_female, averageRating, color='Female'), alpha=0.7) + 
  geom_jitter(aes(percent_male, averageRating, color='Male'), alpha=0.2) +
  geom_line(aes(percent_female, rating.f, color='Female_Fit'), size=1.2) +
  geom_line(aes(percent_male, rating.m, color='Male_Fit'), size=1.2) +
  labs(title='Rating by Gender Composition', x='Percentage', y='Average Rating') +
  scale_color_manual(name='Legend', values = cols)

v.revenue <- ggplot(n) +
  geom_jitter(aes(percent_female, revenue, color='Female'), alpha=0.7) + 
  geom_jitter(aes(percent_male, revenue, color='Male'), alpha=0.2) +
  geom_line(aes(percent_female, revenue.f, color='Female_Fit'), size=1.2) +
  geom_line(aes(percent_male, revenue.m, color='Male_Fit'), size=1.2) +
  labs(title='Revenue by Gender Composition', x='Percentage', y='Revenue ($)') +
  scale_color_manual(name='Legend', values = cols)

