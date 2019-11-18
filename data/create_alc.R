#Elizabeth Oakes
#16.11.2019
#data from: https://archive.ics.uci.edu/ml/datasets/Student+Performance

#setting working directory
setwd("C:/Users/E/Documents/IODS-project/data")

#loading the libraries
library(dplyr)

#reading in the two csvs and exploring their structure
student_mat <- read.csv("student-mat.csv", header = TRUE, sep = ";")
dim(student_mat)
str(student_mat)

student_por <- read.csv("student-por.csv", header = TRUE, sep = ";")
dim(student_por)
str(student_por)

#creating a new dataframe by combining students in both datasets as identified by certain columns
join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")
student_alc <- inner_join(student_mat, student_por, by = join_by, suffix = c(".math", ".por"))

#looking it over
colnames(student_alc)
glimpse(student_alc)

#let's get rid of the doubled columns
# create a new data frame with only the joined columns
alc <- select(student_alc, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(student_mat)[!colnames(student_mat) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'student_alc' with the same original name
  two_columns <- select(student_alc, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse the combined data
glimpse(alc)

#make a new column that expresses overall alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

#and a column that expresses high alcohol use
alc <- mutate(alc, high_use = alc_use > 2)

#check your work
glimpse(alc)

#save your work
write.csv(alc, file = "alc.csv")
