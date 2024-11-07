#Discussion 6 (2-way ANOVA with interaction) -----------------------------
library(readxl)
liquids <- read_excel("Week 6/liquids.xlsx")

#Setting our factors
str(liquids)
liquids$Session <- as.factor(liquids$Session)
liquids$Panelist <- as.factor(liquids$Panelist)
liquids$Sample <- as.factor(liquids$Sample)
liquids$gender <- as.factor(liquids$gender)
str(liquids)

#Changing a factor back to a numeric
liquids$stickiness <- as.factor(liquids$stickiness)
str(liquids)
liquids$stickiness <- as.numeric(liquids$stickiness)
str(liquids)




# Refresher - 1-way ANOVA
stickiness_1way <- aov(stickiness~Sample, liquids)
summary(stickiness_1way)




# 2-way ANOVA
stickiness_2way <- aov(stickiness~Sample+Panelist, liquids)
summary(stickiness_2way)




# 2-way ANOVA with 1-way interaction
stickiness_2way1way1 <- aov(stickiness~Sample*Panelist, liquids)
summary(stickiness_2way1way1)

stickiness_2way1way1 <- aov(stickiness~(Sample+Panelist)^2, liquids)
summary(stickiness_2way1way1)


#above shows two ways of running the same model

stickiness_2way1way2 <- aov(stickiness~Sample*Session,liquids)
summary(stickiness_2way1way2)

stickiness_2way1way3 <- aov(stickiness~Sample*gender,liquids)
summary(stickiness_2way1way3)





#Plotting our data to visualize interaction effects
library(ggplot2)
liquids$Sample <- as.numeric(liquids$Sample)

ggplot(data=liquids, mapping = aes(x=Sample, y=stickiness, group=Panelist, color=Panelist)) + geom_point()+stat_summary(aes(y = stickiness, group=Panelist, color=Panelist), fun=mean, geom="line")

#Separating Data by panelist using facet_wrap function
ggplot(data=liquids, mapping = aes(x=Sample, y=stickiness, group=Panelist, color=Panelist)) + geom_point()+stat_summary(aes(y = stickiness, group=Panelist, color=Panelist), fun=mean, geom="line") + facet_wrap(vars(Panelist))
