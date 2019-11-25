#Data wrangling for week 5
#Elizabeth Oakes
#25.11.2019

#loading the libraries
library(dplyr)

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
