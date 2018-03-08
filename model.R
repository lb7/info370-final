library(dplyr)
library(tidyr)
library(ggplot2)

# Import the dataset
mydata <- read.table("./clean.csv", header=TRUE, sep=",") %>% 
  drop_na(averageRating) %>% 
  filter(revenue > 50000, revenue < 1.0e9)

# Check for the values that are null
sapply(mydata,function(x) sum(is.na(x)))

# Note that actor gender is 1 == female and 2 == male AND 0 == UNKNOWN

# Check unique values per column
sapply(mydata, function(x) length(unique(x)))

#### GLM MODELS
glm_avgRating <- glm(averageRating ~ actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + director_gender + startYear,
                     data=mydata, family=gaussian(link="identity"))

glm_Revenue <- glm(revenue ~ actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + director_gender + startYear,
                     data=mydata, family=gaussian(link="identity"))

glm_Popularity <- glm(as.numeric(popularity) ~ actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + director_gender + startYear,
                   data=mydata, family=gaussian(link="identity"))

# Analysis Functions
summary(glm_avgRating) # display results
confint(glm_avgRating) # 95% CI for the coefficients
exp(coef(glm_avgRating)) # exponentiated coefficients
exp(confint(glm_avgRating)) # 95% CI for exponentiated coefficients
vcov(glm_avgRating) # covariance matrix for model parameters 

summary(glm_Revenue) # display results
confint(glm_Revenue) # 95% CI for the coefficients
exp(coef(glm_Revenue)) # exponentiated coefficients
exp(confint(glm_Revenue)) # 95% CI for exponentiated coefficients
vcov(glm_Revenue) # covariance matrix for model parameters 

summary(glm_Popularity) # display results
confint(glm_Popularity) # 95% CI for the coefficients
exp(coef(glm_Popularity)) # exponentiated coefficients
exp(confint(glm_Popularity)) # 95% CI for exponentiated coefficients
vcov(glm_Popularity) # covariance matrix for model parameters 

# NOTE: seeing pretty high P values and std error for gender when examining the statistical models.

# Hierarchical Models

# This is going to be a long progression for each predictive attribute

## AVERAGE RATING HIERARCHICAL MODELING
rating_hierarch_director <- lm(averageRating ~ director_gender, data=mydata)
rating_hierarch_director_actor1 <- lm(averageRating ~ director_gender + actor1_gender, data=mydata)
rating_hierarch_director_actor2 <- lm(averageRating ~ director_gender + actor1_gender + actor2_gender, data=mydata)
rating_hierarch_director_actor3 <- lm(averageRating ~ director_gender + actor1_gender + actor2_gender + actor3_gender, data=mydata)
rating_hierarch_director_actor4 <- lm(averageRating ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender, data=mydata)
rating_hierarch_director_actor5 <- lm(averageRating ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender, data=mydata)
rating_hierarch_director_year <- lm(averageRating ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + startYear, data=mydata)
# Summarize all models
summary(rating_hierarch_director)
summary(rating_hierarch_director_actor1)
summary(rating_hierarch_director_actor2)
summary(rating_hierarch_director_actor3)
summary(rating_hierarch_director_actor4)
summary(rating_hierarch_director_actor5)
summary(rating_hierarch_director_year)

# Run anova on all of them
rating_hierarch_anova <- anova(rating_hierarch_director,
      rating_hierarch_director_actor1,
      rating_hierarch_director_actor2,
      rating_hierarch_director_actor3,
      rating_hierarch_director_actor4,
      rating_hierarch_director_actor5,
      rating_hierarch_director_year
)

## REVENUE HIERARCHICAL MODELING
revenue_hierarch_director <- lm(revenue ~ director_gender, data=mydata)
revenue_hierarch_director_actor1 <- lm(revenue ~ director_gender + actor1_gender, data=mydata)
revenue_hierarch_director_actor2 <- lm(revenue ~ director_gender + actor1_gender + actor2_gender, data=mydata)
revenue_hierarch_director_actor3 <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender, data=mydata)
revenue_hierarch_director_actor4 <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender, data=mydata)
revenue_hierarch_director_actor5 <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender, data=mydata)
revenue_hierarch_director_year <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + startYear, data=mydata)

# Summarize all models
summary(revenue_hierarch_director)
summary(revenue_hierarch_director_actor1)
summary(revenue_hierarch_director_actor2)
summary(revenue_hierarch_director_actor3)
summary(revenue_hierarch_director_actor4)
summary(revenue_hierarch_director_actor5)
summary(revenue_hierarch_director_year)

# Run anova on all of them
revenue_hierarch_anova <- anova(revenue_hierarch_director,
      revenue_hierarch_director_actor1,
      revenue_hierarch_director_actor2,
      revenue_hierarch_director_actor3,
      revenue_hierarch_director_actor4,
      revenue_hierarch_director_actor5,
      revenue_hierarch_director_year
)

## POPULARITY HIERARCHICAL MODELING
popularity_hierarch_director <- lm(as.numeric(popularity) ~ director_gender, data=mydata)
popularity_hierarch_director_actor1 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender, data=mydata)
popularity_hierarch_director_actor2 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender, data=mydata)
popularity_hierarch_director_actor3 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender, data=mydata)
popularity_hierarch_director_actor4 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender, data=mydata)
popularity_hierarch_director_actor5 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender, data=mydata)
popularity_hierarch_director_year <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + startYear, data=mydata)

# Summarize all models
summary(popularity_hierarch_director)
summary(popularity_hierarch_director_actor1)
summary(popularity_hierarch_director_actor2)
summary(popularity_hierarch_director_actor3)
summary(popularity_hierarch_director_actor4)
summary(popularity_hierarch_director_actor5)
summary(popularity_hierarch_director_year)

# Run anova on all of them
popularity_hierarch_anova <- anova(popularity_hierarch_director,
      popularity_hierarch_director_actor1,
      popularity_hierarch_director_actor2,
      popularity_hierarch_director_actor3,
      popularity_hierarch_director_actor4,
      popularity_hierarch_director_actor5,
      popularity_hierarch_director_year
)

# Examine anovas
rating_hierarch_anova
revenue_hierarch_anova
popularity_hierarch_anova

## Take a look at percentages
# Sum the number of 1 occurances
percentages_cols = c("budget", "startYear","revenue", "averageRating", "popularity", "numVotes","actor1_gender","actor2_gender","actor3_gender","actor4_gender","actor5_gender") 
percentages_cols = c("actor1_gender","actor2_gender","actor3_gender","actor4_gender","actor5_gender") 
percentagesData = mydata[percentages_cols]

# Count occurances of gender
percentagesData$count.1 <- apply(percentagesData, 1, function(x) length(which(x==1)))
percentagesData$count.2 <- apply(percentagesData, 1, function(x) length(which(x==2)))
mydata$count_female = percentagesData$count.1
mydata$count_male = percentagesData$count.2

mydata$percent_female = mydata$count_female / 5
mydata$percent_male = mydata$count_male / 5

## Main Cast Percentage Female results
percentFemale_Rating <- lm(averageRating ~ percent_female, data=mydata)
percentFemale_Revenue <- lm(revenue ~ percent_female, data=mydata)
percentFemale_Popularity <- lm(as.numeric(popularity) ~ percent_female, data=mydata)
percentFemale_Votes <- lm(numVotes ~ percent_female, data=mydata)

summary(percentFemale_Rating)
summary(percentFemale_Revenue)
summary(percentFemale_Popularity)
summary(percentFemale_Votes)

## Main Cast Percentage Female results
percentMale_Rating <- lm(averageRating ~ percent_male, data=mydata)
percentMale_Revenue <- lm(revenue ~ percent_male, data=mydata)
percentMale_Popularity <- lm(as.numeric(popularity) ~ percent_male, data=mydata)
percentMale_Votes <- lm(numVotes ~ percent_male, data=mydata)

summary(percentMale_Rating)
summary(percentMale_Revenue)
summary(percentMale_Popularity)
summary(percentMale_Votes)

# NOTE: more impact on popularity of the movie when you consider how these things go...
# General Appearance is that we don't have a lot of influence over ratings, revenue or votes
# based on gender. Popularity sees the most impact but even that isn't really a whole lot

## Director Gender vs Director Name
# Convert Directors to categorical
mydata$director_code <- as.numeric(factor(mydata$director_name , levels=unique(mydata$director_name)))

# Gen models for just director information
director_Rating <- lm(averageRating ~ director_gender + director_code, data=mydata)
director_Revenue <- lm(revenue ~ director_gender + director_code, data=mydata)
director_Popularity <- lm(as.numeric(popularity) ~ director_code + director_gender, data=mydata)
director_Votes <- lm(numVotes ~ director_code + director_gender, data=mydata)

summary(director_Rating)
summary(director_Popularity)
summary(director_Revenue)
summary(director_Votes)

# NOTE: Impact of gender for director seems to be less than who the director actually is

## Convert actors to categorical
mydata$actor1_code <- as.numeric(factor(mydata$actor1_name , levels=unique(mydata$actor1_name)))

mydata$actor2_code <- as.numeric(factor(mydata$actor2_name , levels=unique(mydata$actor2_name)))

mydata$actor3_code <- as.numeric(factor(mydata$actor3_name , levels=unique(mydata$actor3_name)))

mydata$actor4_code <- as.numeric(factor(mydata$actor4_name , levels=unique(mydata$actor4_name)))

mydata$actor5_code <- as.numeric(factor(mydata$actor5_name , levels=unique(mydata$actor5_name)))

# Fit models for just actors
actor_Rating <- lm(averageRating ~ actor1_gender + actor1_code + actor2_gender + actor2_code + actor3_gender + actor3_code + actor4_gender + actor4_code + actor5_gender + actor5_code, data=mydata)
actor_Revenue <-  lm(revenue ~ actor1_gender + actor1_code + actor2_gender + actor2_code + actor3_gender + actor3_code + actor4_gender + actor4_code + actor5_gender + actor5_code, data=mydata)
actor_Popularity <-  lm(as.numeric(popularity) ~ actor1_gender + actor1_code + actor2_gender + actor2_code + actor3_gender + actor3_code + actor4_gender + actor4_code + actor5_gender + actor5_code, data=mydata)
actor_Votes <-  lm(numVotes ~ actor1_gender + actor1_code + actor2_gender + actor2_code + actor3_gender + actor3_code + actor4_gender + actor4_code + actor5_gender + actor5_code, data=mydata)

# Summarize
summary(actor_Rating)
summary(actor_Revenue)
summary(actor_Popularity)
summary(actor_Votes)

mydata$actor_pop <- predict(actor_Popularity)

plot(as.numeric(mydata$popularity), as.numeric(mydata$actor_pop))

library(ggplot2)
mydata$director_gender
femDir <- mydata[mydata$director_gender == 1,]
ggplot(femDir, aes(x=startYear))+geom_freqpoly(aes(colour=director_gender), size=3, alpha=I(.6))
  mDir <- mydata[mydata$director_gender == 2,]
ggplot(mDir, aes(x=startYear))+geom_freqpoly(aes(colour=director_gender), size=3, alpha=I(.6))

                                                     