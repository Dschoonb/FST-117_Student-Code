# import data
library(readxl)
potato_chips1 <- read_excel("potato_chips.xlsx", 
                            sheet = "test1") # need to clarify which sheet b/c we have 2 sheets in Excel
View(potato_chips1)
potato_chips2 <- read_excel("potato_chips.xlsx", 
                            sheet = "test2")
View(potato_chips2)





# t-test decision tree

## 1 mean or 2 means?
### 1 mean -> one-sample t-test
### 2 means -> two-sample t-test -> are these 2 means evaluated by the same group of panelists or different groups of panelists?
#### same panel -> paired t-test
#### different panel -> independent t-test

## do you know which direction the test will go?
### yes -> one-tailed -> less or more?
#### less -> alternative="l"
#### more/greater -> alternative="g"
### no -> two-tailed -> alternative="t"(default in t.test())
?t.test




# scenario 1: You want to test the liking scores of potato chip formula a vs. potato chip formula b. Are they significantly different?
## 2 means -> two-sample t-test
## evaluated by same group of panelists -> paired t-test
## did not clarify direction -> two-tailed
t.test(potato_chips1$a,potato_chips1$b, # call 2 means and order does not matter here, order only change +/- t-score, does not affect absolute value
       paired = TRUE, # paired t-test
       alternative = "t", # two-tailed
       var.equal = TRUE) # assume equal variance





# scenario 2: Is the saltiness rating of the new formulation significantly less than 8 (saltiness rating of previous formulation)?
## 1 mean -> one-sample t-test
## knew the direction (less than 8) -> alternative="l"
t.test(potato_chips2$liking,mu=8, # one-sample t-test
       alternative = "l", # one-tailed less
       var.equal = TRUE)

## play around with l/g/t, observe (a)t-score, (b)df, (c)p-value
t.test(potato_chips2$liking,mu=8,
       alternative = "l", # one-tailed less
       var.equal = TRUE)
t.test(potato_chips2$liking,mu=8,
       alternative = "g", # one-tailed greater/more
       var.equal = TRUE)
t.test(potato_chips2$liking,mu=8,
       alternative = "t", # two-tailed
       var.equal = TRUE)
### t-score: same for all
### df: same for all
### p-value: different for all
#### one-tailed less vs. two-tailed: two-tailed p-value doubled b/c it can be either less or greater
#### one-tailed more: p-value=1, fail to reject and accept null
##### null: liking is not significantly greater than 8 -> less or equal to 8
##### alternative: liking is significantly greater than 8
##### mean=7.45 <8, liking mean is less than 8 and impossible to be greater than 8, actually no need to run t-test to see significance





# scenario 3a: Do females have a different liking score than males? (assume only female and male in dataset)
## 2 means -> two-sample t-test
## evaluated by different groups of panelists -> independent t-test
## did not know direction -> two-tailed
t.test(data=potato_chips2,
       liking~gender, # call gender as a factor of liking
       paired=FALSE,
       alternative="t", # can omit this line since default in t.test() is alternative="t"
       var.equal = TRUE)





# scenario 3b: Do females have a different liking score than males? (assume responses are female, male, and prefer not to answer in dataset)
library(dplyr)
potato_new <- filter (potato_chips2,
                      gender == "female" | gender == "male") # "|" stands for "or", you are filtering for the desired variables





# TESTING ASSUMPTIONS: all tests above are used for parametric data with equal variance and t-distribution

# tests to prove normality of distribution: 
## (1) plotting histogram 
## (2) shapiro wilk test

## plotting a histogram: visualize distribution
library(ggplot2)
ggplot(data=potato_chips2) + geom_histogram(mapping=aes(x=liking))

## shapiro wilk test: null hypothesis is that the data is distributed normally. 
### if the p-value is greater than the alpha (e.g., 0.05), then the data is normally distributed
### note that t-distribution is more conservative than normal distribution
with(potato_chips2, shapiro.test(liking))



# test to prove equal variance:
## F test: compare two variances
### null hypothesis is that variances are equal
results <- var.test(liking~gender, data=potato_chips2)
results
