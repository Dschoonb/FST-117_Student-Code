#Discussion 6 (2-way ANOVA with interaction) -----------------------------
library(readxl)
liquids <- read_excel("Week 6/liquids.xlsx")

# Clean Data --------------------------------------------------------------
library(tidyverse)
liquids <- liquids[,c(1:7)]
liquids <- rename_at(liquids,3,~"Sample")


# Making Factors, Factors -------------------------------------------------
#Since we are looking at categorical predictors we need to have that reflected in the code
str(liquids) #Check the structure of the data
liquids[,c(1:3,7)] <- lapply(liquids[,c(1:3,7)], as.factor)
str(liquids) #Check the structure again


# ANOVA -------------------------------------------------------------------
# Refresher - 1-way ANOVA
model1 <- lm(thickness~Sample, data = liquids)
anova(model1)

# 2-way ANOVA
model2 <- lm(thickness~Sample+Panelist, liquids)
anova(model2)

# 2-way ANOVA with interaction
model2int <- lm(thickness~Sample*Panelist, liquids)
anova(model2int)




# Visualizing interactions ------------------------------------------------
library(ggplot2)
ggplot(data=liquids, mapping = aes(x=Sample, y=stickiness, group=Panelist, color=Panelist)) + geom_point()+stat_summary(aes(y = stickiness, group=Panelist, color=Panelist), fun=mean, geom="line")

#Separating Data by panelist using facet_wrap function
ggplot(data=liquids, mapping = aes(x=Sample, y=stickiness, group=Panelist, color=Panelist)) + geom_point()+stat_summary(aes(y = stickiness, group=Panelist, color=Panelist), fun=mean, geom="line") + facet_wrap(vars(Panelist))
