# Import the dataset
mydata <- read.table("./clean.csv", header=TRUE, sep=",")

# Check for the values that are null
sapply(mydata,function(x) sum(is.na(x)))

# Note that actor gender is 1 == female and 2 == male AND 0 == UNKNOWN

# Check unique values per column
sapply(mydata, function(x) length(unique(x)))

#### GLM MODELS
glm_avgRating <- glm(averageRating ~ actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + director_gender + budget + startYear,
                     data=mydata, family=gaussian(link="identity"))

glm_Revenue <- glm(revenue ~ actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + director_gender + budget + startYear,
                     data=mydata, family=gaussian(link="identity"))

glm_Popularity <- glm(as.numeric(popularity) ~ actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + director_gender + budget + startYear,
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
rating_hierarch_director_budget <- lm(averageRating ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + startYear + budget, data=mydata)

# Summarize all models
summary(rating_hierarch_director)
summary(rating_hierarch_director_actor1)
summary(rating_hierarch_director_actor2)
summary(rating_hierarch_director_actor3)
summary(rating_hierarch_director_actor4)
summary(rating_hierarch_director_actor5)
summary(rating_hierarch_director_year)
summary(rating_hierarch_director_budget)

# Run anova on all of them
rating_hierarch_anova <- anova(rating_hierarch_director,
      rating_hierarch_director_actor1,
      rating_hierarch_director_actor2,
      rating_hierarch_director_actor3,
      rating_hierarch_director_actor4,
      rating_hierarch_director_actor5,
      rating_hierarch_director_year,
      rating_hierarch_director_budget
)

## REVENUE HIERARCHICAL MODELING
revenue_hierarch_director <- lm(revenue ~ director_gender, data=mydata)
revenue_hierarch_director_actor1 <- lm(revenue ~ director_gender + actor1_gender, data=mydata)
revenue_hierarch_director_actor2 <- lm(revenue ~ director_gender + actor1_gender + actor2_gender, data=mydata)
revenue_hierarch_director_actor3 <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender, data=mydata)
revenue_hierarch_director_actor4 <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender, data=mydata)
revenue_hierarch_director_actor5 <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender, data=mydata)
revenue_hierarch_director_year <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + startYear, data=mydata)
revenue_hierarch_director_budget <- lm(revenue ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + startYear + budget, data=mydata)

# Summarize all models
summary(revenue_hierarch_director)
summary(revenue_hierarch_director_actor1)
summary(revenue_hierarch_director_actor2)
summary(revenue_hierarch_director_actor3)
summary(revenue_hierarch_director_actor4)
summary(revenue_hierarch_director_actor5)
summary(revenue_hierarch_director_year)
summary(revenue_hierarch_director_budget)

# Run anova on all of them
revenue_hierarch_anova <- anova(revenue_hierarch_director,
      revenue_hierarch_director_actor1,
      revenue_hierarch_director_actor2,
      revenue_hierarch_director_actor3,
      revenue_hierarch_director_actor4,
      revenue_hierarch_director_actor5,
      revenue_hierarch_director_year,
      revenue_hierarch_director_budget
)

## POPULARITY HIERARCHICAL MODELING
popularity_hierarch_director <- lm(as.numeric(popularity) ~ director_gender, data=mydata)
popularity_hierarch_director_actor1 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender, data=mydata)
popularity_hierarch_director_actor2 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender, data=mydata)
popularity_hierarch_director_actor3 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender, data=mydata)
popularity_hierarch_director_actor4 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender, data=mydata)
popularity_hierarch_director_actor5 <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender, data=mydata)
popularity_hierarch_director_year <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + startYear, data=mydata)
popularity_hierarch_director_budget <- lm(as.numeric(popularity) ~ director_gender + actor1_gender + actor2_gender + actor3_gender + actor4_gender + actor5_gender + startYear + budget, data=mydata)

# Summarize all models
summary(popularity_hierarch_director)
summary(popularity_hierarch_director_actor1)
summary(popularity_hierarch_director_actor2)
summary(popularity_hierarch_director_actor3)
summary(popularity_hierarch_director_actor4)
summary(popularity_hierarch_director_actor5)
summary(popularity_hierarch_director_year)
summary(popularity_hierarch_director_budget)

# Run anova on all of them
popularity_hierarch_anova <- anova(popularity_hierarch_director,
      popularity_hierarch_director_actor1,
      popularity_hierarch_director_actor2,
      popularity_hierarch_director_actor3,
      popularity_hierarch_director_actor4,
      popularity_hierarch_director_actor5,
      popularity_hierarch_director_year,
      popularity_hierarch_director_budget
)

# Examine anovas
rating_hierarch_anova
revenue_hierarch_anova
popularity_hierarch_anova

