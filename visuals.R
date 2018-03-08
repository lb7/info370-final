library(ggplot2)
library(dplyr)
library(tidyr)

source('model.R')

# Add predictions to easily visualize
n <- mydata %>%
  mutate(
    rating.f = predict(percentFemale_Rating), 
    rating.m = predict(percentMale_Rating),
    revenue.f = predict(percentFemale_Revenue),
    revenue.m = predict(percentMale_Revenue)
  )

# Set up colors for legend
cols <- c(Male='navy', Female='magenta4', Female_Fit='violet', Male_Fit='skyblue')

# Plot gender composition vs ratings
v.ratings <- ggplot(n) +
  geom_jitter(aes(percent_female, averageRating, color='Female'), alpha=0.7) + 
  geom_jitter(aes(percent_male, averageRating, color='Male'), alpha=0.2) +
  geom_line(aes(percent_female, rating.f, color='Female_Fit'), size=1.2) +
  geom_line(aes(percent_male, rating.m, color='Male_Fit'), size=1.2) +
  labs(title='Rating by Gender Composition', x='Percentage', y='Average Rating') +
  scale_color_manual(name='Legend', values = cols)

# Plot gender composition vs revenue
v.revenue <- ggplot(n) +
  geom_jitter(aes(percent_female, revenue, color='Female'), alpha=0.7) + 
  geom_jitter(aes(percent_male, revenue, color='Male'), alpha=0.2) +
  geom_line(aes(percent_female, revenue.f, color='Female_Fit'), size=1.2) +
  geom_line(aes(percent_male, revenue.m, color='Male_Fit'), size=1.2) +
  labs(title='Revenue by Gender Composition', x='Percentage', y='Revenue ($)') +
  scale_color_manual(name='Legend', values = cols)

