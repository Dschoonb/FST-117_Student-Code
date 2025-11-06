## Discussion 5
#This weeks dataset contains data from a sensory descriptive analysis panel which assessed liquids containing different thickening agents.


# Load Data -----------------------------------------------------------
library(readxl)
liquids <- read_excel("Week 5/liquids.xlsx")


# Clean Data --------------------------------------------------------------
library(tidyverse)
liquids <- liquids[,c(1:7)]
liquids <- rename_at(liquids,3,~"Sample")


# Making Factors, Factors -------------------------------------------------
#Since we are looking at categorical predictors we need to have that reflected in the code
str(liquids) #Check the structure of the data
liquids[,c(1:3,7)] <- lapply(liquids[,c(1:3,7)], as.factor)
str(liquids) #Check the structure again


#Visualize Data----------------------------------------------------------
library(ggplot2)
#plot this in histrogram form
ggplot(data=liquids) + geom_histogram(aes(x=thickness), position = "dodge", bins = 15)
#add sample explanatory variable
ggplot(data=liquids) + geom_histogram(aes(x=thickness, fill=Sample), bins = 15)
#scatter plot
ggplot(data=liquids) + geom_point(aes(x=Sample, y=thickness, color=Sample))


# ANOVA ------------------------------------------------
liquids.anova <- lm(thickness~Sample, data = liquids)
summary(liquids.anova)
anova(liquids.anova)


# Multiple Mean Comparison Tests -----------------------------------------
library(agricolae)
#LSD
LSD.test(liquids.anova, trt = "Sample", console = TRUE, alpha = 0.05)
#Bonferroni
LSD.test(liquids.anova, trt = "Sample", console = TRUE, alpha = 0.05, p.adj = "bon")
#HSD
HSD.test(liquids.anova, trt = "Sample", console = TRUE, alpha = 0.05)



#Visualization before after the test -----------------------------------------------------------
# Labeling means with letters to indicate significant difference
#Fist you need to make a new table with the means of each sample
#You can use the aggregate function or the means_methods function
means_methods <- aggregate(thickness ~ Sample, liquids, FUN = mean)
means_methods <- means_methods[order(means_methods$thickness,decreasing=TRUE),]

#Run the Multiple means again and place that data within to an object
LSDresult <- LSD.test(liquids.anova, trt = "Sample", console = TRUE, alpha = 0.05)

#Visualize the data with bar charts
ggplot(means_methods, aes(x=Sample, y=thickness)) + geom_bar(stat = "identity") + 
  geom_text(label=c(LSDresult$groups$groups), mapping = aes(x=Sample, y=thickness+0.5))
#violin plot
ggplot(liquids, aes(x=Sample, y=thickness)) + geom_violin() + 
  stat_summary(fun=mean, geom="point", size=2, color="red") +
  geom_text(data=means_methods, label=c(LSDresult$groups$groups), mapping = aes(x=Sample, y=thickness+0.5))






