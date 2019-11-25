# Elizabeth Oakes
# 24.11.2019
# Exercise 4

#install packages and libraries
library(MASS)
library(corrplot)

#load the data
data("Boston")

#have a look
dim(Boston)
str(Boston)
summary(Boston)

#visualize variables and their relationships
pairs(Boston)

#same with correlations
correlations <- cor(Boston) %>% round(digits = 2)
correlations
corrplot(correlations, method="circle", type = "upper", cl.pos = "b", tl.pos = "d", tl.cex = 0.6)

#standardize the data and have a look, get it to the right form
boston_scaled <- scale(Boston)
boston_scaled <- as.data.frame(boston_scaled)
summary(boston_scaled)

#make crime a categorical variable
bins <- quantile(boston_scaled$crim)
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, label = c("low", "med_low", "med_high", "high"))

#print table of your new variable
table(crime)

#remove original crime variable and replace with new categorical one
boston_scaled <- dplyr::select(boston_scaled, -crim)
boston_scaled <- data.frame(boston_scaled, crime)

#create a training set with 80% of the data and a test set with the rest
n <- nrow(boston_scaled)
ind <- sample(n,  size = n * 0.8)
train <- boston_scaled[ind,]
test <- boston_scaled[-ind,]
correct_classes <- test$crime
test <- dplyr::select(test, -crime)

#fit the linear discriminant analysis on the training data and have a look
lda.fit <- lda(crime ~ ., data = train)
lda.fit

#make a biplot of the results of this lda
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

classes <- as.numeric(train$crime)

plot(lda.fit, col = classes, pch = classes, dimen = 2)
lda.arrows(lda.fit, myscale = 1)

#try the model on the test data and tabulate how it does
#instructions say something about removing crime variable here (think you already did), may need to adjust
lda.pred <- predict(lda.fit, newdata = test)
table(correct = correct_classes, predicted = lda.pred$class)

#reload and rescale the data in preparation for k-means analysis
data("Boston")
boston_scaled <- scale(Boston)
boston_scaled <- as.data.frame(boston_scaled)

#calculate the distance between variables in two ways, euclidian and manhattan
dist_eu <- dist(boston_scaled)
summary(dist_eu)
?dist
dist_man <- dist(boston_scaled, method = "manhattan")
summary(dist_man)

#k-means time
km <-kmeans(boston_scaled, centers = 3)
pairs(boston_scaled[6:10], col = km$cluster)

set.seed(123)

# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(boston_scaled, k)$tot.withinss})

# visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line')

# k-means clustering
km <-kmeans(boston_scaled, centers = 2)

# plot the Boston dataset with clusters
pairs(boston_scaled, col = km$cluster)

