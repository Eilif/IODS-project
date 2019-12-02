#Data wrangling for week 5
#Elizabeth Oakes
#25.11.2019

#loading the libraries
library(dplyr)
library(magrittr)
library(stringr)

#load the datasets
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#exploring the structure and creating a summary of hd
dim(hd)
str(hd)
summary(hd)

#exploring the structure and creating a summary of gii
dim(gii)
str(gii)
summary(gii)

#rename the variables in hd
colnames(hd)[colnames(hd)=="Human.Development.Index..HDI."] <- "HDI"
colnames(hd)[colnames(hd)=="Life.Expectancy.at.Birth"] <- "life_expect"
colnames(hd)[colnames(hd)=="Expected.Years.of.Education"] <- "ed_expect"
colnames(hd)[colnames(hd)=="Mean.Years.of.Education"] <- "av_ed"
colnames(hd)[colnames(hd)=="Gross.National.Income..GNI..per.Capita"] <- "GNI"
colnames(hd)[colnames(hd)=="GNI.per.Capita.Rank.Minus.HDI.Rank"] <- "GNI-HDIrank"

#rename the variables in gii
colnames(gii)[colnames(gii)=="Gender.Inequality.Index..GII."] <- "GII"
colnames(gii)[colnames(gii)=="Maternal.Mortality.Ratio"] <- "mat_mort"
colnames(gii)[colnames(gii)=="Adolescent.Birth.Rate"] <- "teen_preg"
colnames(gii)[colnames(gii)=="Percent.Representation.in.Parliament"] <- "parl_rep"
colnames(gii)[colnames(gii)=="Population.with.Secondary.Education..Female."] <- "F2ed"
colnames(gii)[colnames(gii)=="Population.with.Secondary.Education..Male."] <- "M2ed"
colnames(gii)[colnames(gii)=="Labour.Force.Participation.Rate..Female."] <- "Fwork"
colnames(gii)[colnames(gii)=="Labour.Force.Participation.Rate..Male."] <- "Mwork"

#add two new columns to describe gender inequality
gii <- mutate(gii, FvsM2ed = F2ed / M2ed)
gii <- mutate(gii, FvsMwork = Fwork / Mwork)

#join the datasets
human <- inner_join(hd, gii, by = "Country")
dim(human)
str(human)

#write to csv
write.csv(human, file = "human.csv")




#1 Mutate the data: transform the Gross National Income (GNI) variable to numeric 
#(Using string manipulation. Note that the mutation of 'human' was not done on DataCamp). (1 point)
# remove the commas from GNI and print out a numeric version of it
human <- mutate(human, GNI = (str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()))
glimpse(human)
#2 Exclude unneeded variables: keep only the columns matching the following variable names 
#(described in the meta file above):  "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", 
#"Mat.Mor", "Ado.Birth", "Parli.F" (1 point)
# columns to keep
keep <- c("Country", "FvsM2ed", "FvsMwork", "life_expect", "ed_expect", "GNI", "mat_mort", "teen_preg", "parl_rep")

# select the 'keep' columns
human <- select(human, one_of(keep))

#3 Remove all rows with missing values (1 point).
# print out a completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_ <- filter(human, complete.cases(human))

#4 Remove the observations which relate to regions instead of countries. (1 point)
# look at the last 10 observations of human
tail(human, n = 10L)

# define the last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human_ <- human[1:last, ]

#5 Define the row names of the data by the country names and remove the country name column from 
#the data. The data should now have 155 observations and 8 variables. Save the human data in your 
#data folder including the row names. You can overwrite your old ‘human’ data. (1 point)
# add countries as rownames
rownames(human_) <- human_$Country
human_ <- select(human, -Country)
dim(human_)
glimpse(human_)
#write to csv
write.csv(human_, file = "human.csv")
