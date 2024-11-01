## Discussion 5

# Cleaning Data -----------------------------------------------------------
#This weeks dataset contains data from a sensory descriptive analysis panel which assessed liquids containing different thickening agents.
#Load up the dataset.
library(readxl)
liquids <- read_excel("Week 5/liquids.xlsx")
View(liquids)

#The dataset contains some junk in the last few rows.
#We need to get rid of this junk and give the sample column a better name.
library(tidyverse)
liquids <- liquids[,c(1:7)]
liquids <- rename_at(liquids,3,~"Sample")


# Making Factors, Factors -------------------------------------------------
#Since we are looking at categorical explanatory variables we need to have that reflected in the code
#First check the structure of the data
str(liquids)
#then use the as.factor function to make the column a factor
liquids$Session <- as.factor(liquids$Session)
liquids$Panelist <- as.factor(liquids$Panelist)
liquids$Sample <- as.factor(liquids$Sample)
liquids$gender <- as.factor(liquids$gender)
str(liquids)
#Check the structure again to make sure it worked
#use can use as.numeric to revert any changes to a column


#Visualization before the test -----------------------------------------------------------
#Lets take a peak at the spread of out data first
library(ggplot2)
anova_plot <- ggplot(data=liquids) + geom_boxplot(aes(x=Sample, y=thickness))
lm_plot <- ggplot(data=liquids) + geom_point(aes(x=Sample, y=thickness))



# Performing a 1-Way ANOVA ------------------------------------------------
# 1-way ANOVA
liquids.anova <- aov(thickness~Sample, data = liquids)
summary(liquids.anova)



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








