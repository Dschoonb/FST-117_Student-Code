#Discussion 6 (2-way ANOVA with interaction) -----------------------------
library(readxl)
liquids <- read_excel("Week 6/liquids.xlsx")

# Making Factors, Factors -------------------------------------------------
#Since we are looking at categorical predictors we need to have that reflected in the code
library(tidyverse)
str(liquids) #Check the structure of the data
liquids[,c(1:4)] <- lapply(liquids[,c(1:4)], as.factor)
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
a <- 
  liquids %>% 
  ggplot(aes(x=Sample, y=thickness, group=Panelist, color=Panelist)) + 
  geom_point() +
  stat_summary(aes(y = thickness, group=Panelist, color=Panelist), fun=mean, geom="line")
a
#Separating Data by panelist using facet_wrap function
a1 <- a + facet_wrap(~Panelist)
a1

#Using interaction function to create interaction plot
interaction.plot(x.factor = liquids$Sample,
                 trace.factor = liquids$Panelist,
                 response = liquids$thickness,
                 fun = mean,
                 type = "b",
                 col = c("red","blue","green","purple"),
                 pch = c(16,17,15,18),
                 xlab = "Sample",
                 ylab = "Mean thickness",
                 legend = TRUE)
