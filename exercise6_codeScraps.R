#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

#load datasets
ratsL <- read.csv("ratsL.csv", header = T, sep = ",")
bprsL <- read.csv("bprsL.csv", header = T, sep = ",")

#check datasets
glimpse(ratsL)
glimpse(bprsL)

#refactor if necessary
ratsL$ID <- factor(ratsL$ID)
ratsL$Group <- factor(ratsL$Group)
bprsL$treatment <- factor(bprsL$treatment)
bprsL$subject <- factor(bprsL$subject)

# Analysis for exercise 6, cut once you have knit up a course diary

######multiple plots one graph
#Comparison with graphs 
ggplot(bprsL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(bprsL$bprs), max(bprsL$bprs)))

#and again with rats
ggplot(ratsL, aes(x = Time, y = Weight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(ratsL$Weight), max(ratsL$Weight)))

# Plot the RATSL data
ggplot(ratsL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) +
  scale_y_continuous(name = "Weight (grams)") +
  theme(legend.position = "top")

######
# Standardise the variable bprs
BPRSL <- bprsL %>%
  group_by(week) %>%
  mutate(stdbprs = (bprs - mean(bprs))/sd(bprs) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL)

# Plot again with the standardised bprs
ggplot(BPRSL, aes(x = week, y = stdbprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(name = "standardized bprs")
######
#same with rats instead
#Standardise the variable Weight
ratsL <- ratsL %>%
  group_by(WD) %>%
  mutate(stdWeight = (Weight - mean(Weight))/sd(Weight) ) %>%
  ungroup()

# Glimpse the data
glimpse(ratsL)

# Plot again with the standardised Weight
ggplot(ratsL, aes(x = Time, y = stdWeight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) +
  scale_y_continuous(name = "standardized Weight") +
  theme(legend.position = "top")
#########


#########single line per treatment/group summary plot

# Number of weeks, baseline (week 0) included
n <- bprsL$week %>% unique() %>% length()
n
# Summary data with mean and standard error of bprs by treatment and week 
bprsS <- bprsL %>%
  group_by(treatment, week) %>%
  summarise( mean = mean(bprs), se = (sd(bprs)/sqrt(n)) ) %>%
  ungroup()

# Glimpse the data
glimpse(bprsS)

# Plot the mean profiles
ggplot(bprsS, aes(x = week, y = mean, linetype = treatment, shape = treatment)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)")


#same thing with rats
# Number of Times
n <- ratsL$Time %>% unique() %>% length()
n
# Summary data with mean and standard error of Weight by Group and Time 
ratsS <- ratsL %>%
  group_by(Group, Time) %>%
  summarise( mean = mean(Weight), se = (sd(Weight)/sqrt(n)) ) %>%
  ungroup()

# Glimpse the data
glimpse(ratsS)

# Plot the mean profiles
ggplot(ratsS, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2,3)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,3)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.9,0.5)) +
  scale_y_continuous(name = "mean(Weight) +/- se(Weight)")

#########
# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
bprsL8S <- bprsL %>%
  filter(week > 0) %>%
  group_by(treatment, subject) %>%
  summarise( mean=mean(bprs) ) %>%
  ungroup()

# Glimpse the data
glimpse(bprsL8S)

# Draw a boxplot of the mean versus treatment
ggplot(bprsL8S, aes(x = treatment, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 1-8")

#now for rats
# Create summary data by group and ID with mean as the summary variable (ignoring baseline Time 1).
ratsL8S <- ratsL %>%
  filter(Time > 1) %>%
  group_by(Group, ID) %>%
  summarise( mean=mean(Weight) ) %>%
  ungroup()

# Glimpse the data
glimpse(ratsL8S)

# Draw a boxplot of the mean versus treatment
ggplot(ratsL8S, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "blue") +
  scale_y_continuous(name = "mean(Weight) without day 1")

####################T-test
# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
BPRSL8S1 <- BPRSL8S %>%
  filter(mean < 60)


# Perform a two-sample t-test
t.test(mean ~ treatment, data = BPRSL8S1, var.equal = TRUE)


##########linear model and Anova

# Add the baseline from the original data as a new variable to the summary data
BPRSL8S2 <- BPRSL8S %>%
  mutate(baseline = BPRS$week0)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ baseline + treatment, data = BPRSL8S2)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit)

####################


#aggressively applying inappropriate linear model (may not be included)
str(ratsL)
# create a regression model RATS_reg
RATS_reg <- lm(Weight ~ Time + Group, data = ratsL)

# print out a summary of the model
summary(RATS_reg)

#creating a regression model for bprsL
BPRS_reg <- lm(bprs ~ week + treatment, data = bprsL)
summary(BPRS_reg)



# access library lme4
library(lme4)

# Create a random intercept model
RATS_ref <- lmer(Weight ~ Time + Group + (1 | ID), data = ratsL, REML = FALSE)

# Print the summary of the model
summary(RATS_ref)

#random intercept model for bprsL
BPRS_ref <- lmer(bprs ~ week + treatment + (1 | subject), data = bprsL, REML = FALSE)
summary(BPRS_ref)


# create a random intercept and random slope model
RATS_ref1 <- lmer(Weight ~ Time + Group + (Time | ID), data = ratsL, REML = FALSE)

# print a summary of the model
summary(RATS_ref1)

#random intercept and random slope model for bprsL
BPRS_ref1 <- lmer(bprs ~ week + treatment + (week | subject), data = bprsL, REML = FALSE)
summary(BPRS_ref1)

# perform an ANOVA test on the two models
anova(RATS_ref1, RATS_ref)


# create a random intercept and random slope model
RATS_ref2 <- lmer(Weight ~ Time * Group + (Time | ID), data = ratsL, REML = FALSE)

BPRS_ref2 <- lmer(bprs ~ week * treatment + (week | subject), data = bprsL, REML = FALSE)
summary(BPRS_ref2)
# print a summary of the model
summary(RATS_ref2)

# perform an ANOVA test on the two models
anova(RATS_ref2, RATS_ref1)

# draw the plot of RATSL
ggplot(ratsL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Observed weight (grams)") +
  theme(legend.position = "top")

# Create a vector of the fitted values
Fitted <- fitted(RATS_ref2)

FittedB <- fitted(BPRS_ref2)

# Create a new column fitted to RATSL
RATSL <- RATSL %>%
  mutate(Fitted)

bprsL <- bprsL %>%
  mutate(FittedB)

# draw the plot of RATSL
ggplot(RATSL, aes(x = Time, y = Fitted, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Fitted weight (grams)") +
  theme(legend.position = "top")



ggplot(bprsL, aes(x = week, y = FittedB, group = subject)) +
  geom_line(aes(linetype = treatment)) +
  scale_x_continuous(name = "Week", breaks = seq(0, 8, 1)) +
  scale_y_continuous(name = "Fitted bprs") +
  theme(legend.position = "top")




