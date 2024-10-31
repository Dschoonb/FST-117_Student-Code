## Discussion 5

library(readxl)
liquids <- read_excel("~/PhD/TA/FST 117/Discussion Week 4/liquids.xlsx")
View(liquids)

library(tidyverse)

liquids <- liquids[,c(1:7)]
liquids <- rename_at(liquids,3,~"Sample")

str(liquids)

liquids1 <- liquids
str(liquids1)

liquids$Session <- as.factor(liquids$Session)
liquids$Panelist <- as.factor(liquids$Panelist)
liquids$Sample <- as.factor(liquids$Sample)
liquids$gender <- as.factor(liquids$gender)

library(ggplot2)
anova_plot <- ggplot(data=liquids) + geom_boxplot(aes(x=Sample, y=thickness))
lm_plot2 <- ggplot(data=liquids1) + geom_point(aes(x=Sample, y=thickness))
lm_plot <- ggplot(data=liquids) + geom_point(aes(x=slipperiness, y=thickness)) 

# 1-way ANOVA

head(liquids)

liquids.anova <- aov(thickness~Sample, data = liquids)
summary(liquids.anova)

liquids1.anova <- aov(thickness~Sample, data = liquids1) #this is what happens if we don't set our qualitative variables as factors
summary(liquids1.anova)

# Multiple Mean Comparison Tests (Post-hoc Tests)

library(agricolae)

LSD.test(liquids.anova, trt = "Sample", console = TRUE, alpha = 0.05)

LSD.test(liquids.anova, trt = "Sample", console = TRUE, alpha = 0.05, p.adj = "bon")

HSD.test(liquids.anova, trt = "Sample", console = TRUE, alpha = 0.05)



## Data Visualization

# Plotting with Asterisks

library(ggplot2)
library(ggsignif)
ggplot(liquids, aes(x=Sample, y=thickness)) + geom_boxplot() + geom_signif(comparisons = list(c("3","2"),c("1","2")), test=t.test,map_signif_level=TRUE)




# Labeling means with letters to indicate significant difference

means_methods <- aggregate(thickness ~ Sample, liquids, FUN = mean)

means_methods <- means_methods[order(means_methods$thickness,decreasing=TRUE),]

LSDresult <- LSD.test(liquids.anova, trt = "Sample", console = TRUE, alpha = 0.05)

ggplot(means_methods, aes(x=Sample, y=thickness)) + geom_bar(stat = "identity") + 
  geom_text(label=c(LSDresult$groups$groups), mapping = aes(x=Sample, y=thickness+0.5))








