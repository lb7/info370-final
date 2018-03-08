library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(reshape)

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


# Setup for actor gender spread
MyData <- read.csv(file="clean.csv", header=TRUE, sep=",")
gender <- c("N/a", "Female", "Male")
a <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("actor.num", "gender", "value"))

  # Actor 1
  act1 <-table(MyData$actor1_gender)
  act1.m <-melt(act1)
  act1.m$gender <- gender
  act1.m$actor.num <- 1
  act1.m.sub<- subset(act1.m, select = -c(Var.1))
  a <- rbind(a, act1.m.sub)
  
  # Actor 2
  act2 <-table(MyData$actor2_gender)
  act2.m <-melt(act2)
  act2.m$gender <- gender
  act2.m$actor.num <- 2
  act2.m.sub<- subset(act2.m, select = -c(Var.1))
  a <- rbind(a, act2.m.sub)
  
  # Actor 3
  act3 <-table(MyData$actor3_gender)
  act3.m <-melt(act3)
  act3.m$gender <- gender
  act3.m$actor.num <- 3
  act3.m.sub<- subset(act3.m, select = -c(Var.1))
  a <- rbind(a, act3.m.sub)
  
  # Actor 4
  act4 <-table(MyData$actor4_gender)
  act4.m <-melt(act4)
  act4.m$gender <- gender
  act4.m$actor.num <- 4
  act4.m.sub<- subset(act4.m, select = -c(Var.1))
  a <- rbind(a, act4.m.sub)
  
  # Actor 5
  act5 <-table(MyData$actor5_gender)
  act5.m <-melt(act5)
  act5.m$gender <- gender
  act5.m$actor.num <- 5
  act5.m.sub<- subset(act5.m, select = -c(Var.1))
  a <- rbind(a, act5.m.sub)

  final.actor <- a
  
# Plot gender breakdown for 5 main actors/actresses
v.actor.gender <- ggplot() + 
      geom_bar(aes(y = value, x = actor.num, fill = gender), data = final.actor, stat="identity") +
      labs(title='Gender Breakdown of Cast Actors', x='Actor Casting Position', y='Count ')

# Setup for director gender spread
a.dir <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("gender", "value"))
dir <-table(MyData$director_gender)
dir.m <-melt(dir)
dir.m$gender <- gender
dir.final<- subset(dir.m, select = -c(Var.1))

# Plot gender breakdown of directors
v.dir.gender <- ggplot() + 
  geom_bar(aes(y = value, x = gender, fill = gender), data = dir.final, stat="identity") +
  labs(title='Gender Breakdown of Directors', x='Gender', y='Count')


