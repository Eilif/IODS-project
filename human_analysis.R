#Elizabeth Oakes
#2.12.2019
#Exercise five dimensionality reduction

#load packages and libraries
install.packages("FactoMineR")
library(dplyr)
library(magrittr)
library(corrplot)
library(ggplot2)
library(tidyr)
library(GGally)
library(FactoMineR)

#read in data
human <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human2.txt", header = TRUE, sep = ",")

#summarize the variables
summary(human)
dim(human)

# visualize the variables
ggpairs(human)

# compute the correlation matrix and visualize it with corrplot
cor(human) %>% corrplot(type = "upper")

#unstandardized PCA
pca_human <- prcomp(human)
pca_human

#and its biplot
biplot(pca_human, choices = 1:2, cex = c(0.5, 1), col = c("#f06d0a", "#4002d1"))

# standardize the variables
human_std <- scale(human)

# print out summaries of the standardized variables
summary(human_std)

# perform principal component analysis (with the SVD method)
pca_human2 <- prcomp(human_std)

# draw a biplot of the principal component representation and the original variables
biplot(pca_human2, choices = 1:2, cex = c(0.5, 1), col = c("#b80202", "#0c5d7a"))

# create and print out a summary of pca_human
s <- summary(pca_human)
s

# rounded percetanges of variance captured by each PC
pca_pr <- round(100*s$importance[2, ], digits = 1)

# print out the percentages of variance
pca_pr

# create object pc_lab to be used as axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot
biplot(pca_human, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])


#load tea
data(tea)
str(tea)
dim(tea)

#bar plots of drinking tea
gather(tea) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# column names to keep in the dataset
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

# select the 'keep_columns' to create a new dataset
tea_time <- select(tea, one_of(keep_columns))

# look at the summaries and structure of the data
summary(tea_time)
str(tea_time)
dim(tea_time)

# visualize the dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# multiple correspondence analysis
mca <- MCA(tea_time, graph = FALSE)

# summary of the model
summary(mca)

# visualize MCA
plot(mca, invisible=c("ind"), habillage= "quali")




