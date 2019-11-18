#Elizabeth Oakes
#17.11.2019
#Analyzing some data about student alcohol consumption

#setting working directory
setwd("C:/Users/E/Documents/IODS-project/data")

#packages and libraries
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(ggplot2)
library(boot)

#loading the data
alc <- read.csv("alc.csv", header = T, sep = ",")
glimpse(alc)
colnames(alc)

#let's look at all the variables to decide where to place focus
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

#Some summary statistics by variable
alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade <- mean(G3))

#box plot of high use and grades by sex
g1 <- ggplot(alc, aes(x = high_use, y = G3,  col = sex))
g1 + geom_boxplot() + ylab("grade") + ggtitle("Student grades by alcohol consumption and sex")

#bar plot of high use and goout
social <- ggplot(alc, aes(x = goout, fill = high_use))
social + geom_bar(position = "dodge") + ggtitle("Student alcohol consumption and socializing")

#bar plot of romantic and high use
rombar <- ggplot(alc, aes(x = romantic, fill = high_use))
rombar + geom_bar(position = "dodge") + ggtitle("Student alcohol consumption and romantic relationship")

#bar plot of free time and high use
free <- ggplot(alc, aes(x = freetime, fill = high_use))
free + geom_bar(position = "dodge") +ggtitle("Student alcohol consumption and free time")

#let's make a regression model with the variables for grades and socializing
#first we need to make goout a factor
alc <- mutate(alc, fac_goout = as.factor(alc$goout))
levels(alc$fac_goout)
# find the model with glm()
m <- glm(high_use ~ G3 + fac_goout + sex, data = alc, family = "binomial")
# print out a summary of the model
summary(m)
# print out the coefficients of the model
coef(m)
# compute odds ratios (OR)
OR <- coef(m) %>% exp
# compute confidence intervals (CI)
CI <- confint(m) %>% exp
# print out the odds ratios with their confidence intervals
cbind(OR, CI)

#And we can compare the model predictions to the actual data
# predict() the probability of high_use
probabilities <- predict(m, type = "response")
# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)
# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, G3, fac_goout, sex, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()









# initialise a plot of high_use and absences
g2 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))

# define the plot as a boxplot and draw it
g2 + geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")


#building regression models with certain variables
# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# print out a summary of the model
m
summary(m)
# print out the coefficients of the model
coef(m)

#doing some odds ratio and confidence intervals for this model
# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)


#making and tabulating some predictions with predict()
# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)


#Yet more ways of looking at a model which I think fails to be predictive?
# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()


#predicting error in the model, I don't understand this
# okay, a low number output is good, as it approaches 1, the predictive value gets less.
# prob = 1 means everybody has high alcohol use (T)
# prob = 0 means nobody has high alcohol use (F)
# prob = as written now, it's using alcohol use from actual data.
# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
#prob can also equal 0 or 1 for some effect...
loss_func(class = alc$high_use, prob = alc$probability)


#validating the model
# K-fold cross-validation, can use K = nrow(alc) for train, this is test, I think...
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)
#get some info about how that function works
?cv.glm
# average number of wrong predictions in the cross validation
cv$delta[1]
