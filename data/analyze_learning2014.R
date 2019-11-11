#Elizabeth Oakes
#11.11.2019
#Some data analysis script

#set working directory
setwd("C:/Users/E/Documents/IODS-project/data")

#getting packages and loading libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("devtools")
library(devtools)
install.packages("GGally")
library(GGally)
install.packages("hms")
library(hms)

#read in data and check
learning2014 <- read.csv(file = "learning2014.csv", header = TRUE, sep = ",")
head(learning2014)
dim(learning2014)
str(learning2014)

#Let's compare each interger variable to each other integer variable by gender
massive_plot <- ggpairs(learning2014[-1], mapping = aes(col = gender), lower = list(combo = wrap("facethist", bins = 20)))
massive_plot

#let's visualize the relationship between attitude and points by  #col = gender#
p1 <- ggplot(learning2014, aes(x = atti, y = Points))
p2 <- p1 + geom_point()
p3 <- p2 + geom_smooth(method = "lm")
p4 <- p3 + ggtitle("Student's attitude versus exam points")
p4

#let's visualize the relationship between surface and deep learning by gender
sd1 <- ggplot(learning2014, aes(x = surf, y = deep, col = gender))
sd2 <- sd1 + geom_point()
sd3 <- sd2 + geom_smooth(method = "lm")
sd4 <- sd3 + ggtitle("Student's surface learning versus deep learning")
sd4