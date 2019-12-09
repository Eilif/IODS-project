#Elizabeth Oakes
#8.12.2019
#Exercise 6: Data Wrangling

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

#load data
rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep = "\t", header = T)
bprs <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep = " ", header = T)

#check data
names(rats)
dim(rats)
str(rats)
summary(rats)

names(bprs)
dim(bprs)
str(bprs)
summary(bprs)

#convert categorical variables to factors
rats$ID <- factor(rats$ID)
rats$Group <- factor(rats$Group)
bprs$treatment <- factor(bprs$treatment)
bprs$subject <- factor(bprs$subject)

#convert bprs to long form and add week variable
bprsL <-  bprs %>% gather(key = weeks, value = bprs, -treatment, -subject)
bprsL <-  bprsL %>% mutate(week = as.integer(substr(weeks, 5,5)))
glimpse(bprsL)

#convert rats to long form and add Time variable
ratsL <- rats %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD, 3,4))) 
glimpse(ratsL)

#write to csv
write.csv(ratsL, file = "ratsL.csv")
write.csv(bprsL, file = "bprsL.csv")
