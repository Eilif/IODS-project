#Elizabeth Oakes
#10.11.2019
#Some data wrangling script

#set working directory
setwd("C:/Users/E/Documents/IODS-project")

#getting packages and loading libraries
install.packages("dplyr")
library(dplyr)

#read in data
learning2014 <- read.csv(url("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"), header = TRUE, sep = "\t")

#exploring the data, which comes in 60 variables of 183 observations - all integers, except gender, which is a factor.
head(learning2014)
dim(learning2014)
str(learning2014)

#combining and averaging variables
#surface
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
surface_columns <- select(learning2014, one_of(surface_questions))
learning2014$surf <- rowMeans(surface_columns)
#deep
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
deep_columns <- select(learning2014, one_of(deep_questions))
learning2014$deep <- rowMeans(deep_columns)
#strategic
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
strategic_columns <- select(learning2014, one_of(strategic_questions))
learning2014$stra <- rowMeans(strategic_columns)
#attitude
attitude_questions <- c("Da", "Db", "Dc", "Dd", "De", "Df", "Dg", "Dh", "Di", "Dj")
attitude_columns <- select(learning2014, one_of(attitude_questions))
learning2014$atti <- rowMeans(attitude_columns)

#creating a smaller dataset with select variables
keep_columns <- c("gender","Age","atti", "deep", "stra", "surf", "Points")
lrn2014smll <- select(learning2014,one_of(keep_columns))
#getting rid of observations where points = 0
lrn2014smll <- filter(lrn2014smll, Points > 0)
#check yr work
str(lrn2014smll)

#write to the data folder
setwd("C:/Users/E/Documents/IODS-project/data")
write.csv(lrn2014smll, file = "learning2014.csv")

#check yeh didn't jack it
learning2014 <- read.csv("learning2014.csv")
head(learning2014)
str(learning2014)


