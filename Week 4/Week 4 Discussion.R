##Formatting the Dataset
library(readxl)
Data <- read_excel("Week 4/Study Score and More.xlsx")
colnames(Data)[3:6] <- c("Attendance","Hours","Mid-Term","Final")
Data <- as.data.frame(Data)



## Simple Linear Regression: 1 Outcome / 1 Predictor ---------------------------------
library(ggplot2)
library(tidyverse)

# Creating a linear model to predict final score based on hours studied
model1 <- lm(Final~Hours, Data)
summary(model1)
confint(model1)

# remember that R squared is the proportion of variance explained by the model
# and is equal to the square of the correlation between predictor and outcome
a <- cor(Data$Hours, Data$Final)
a^2  # R squared value


# Plot Hours Studies vs Final Score
Data %>% 
  ggplot(aes(x=Hours, y=Final)) + 
  geom_point()

# Adding a linear regression line
Data %>% 
  ggplot(aes(x=Hours, y=Final)) + 
  geom_point() + 
  stat_smooth(formula = y~x, method = lm, se = FALSE)

# Adding a linear regression line with confidence interval
# Q- What does a confidence interval tell us about our model?
Data %>% 
  ggplot(aes(x=Hours, y=Final)) + 
  geom_point() + 
  stat_smooth(formula = y~x, method = lm, se = TRUE) #TRUE = confidence interval

# checking if our model violates homoscedasticity assumption
par(mfrow = c(1, 1))
plot(model1)

# Making Predictions based on this model (e.g., a student who studies 40 hours a week)
y = 38.8023 + 1.2997*40
y


## Multiple Linear Regression: 1 Outcome / >1 Predictors ------------------------------
# Creating a linear model to predict final score based on hours studied, attendance, and mid-term score
model2a <- lm(Final~Hours+Attendance+`Mid-Term`, Data)
summary(model2a)
confint(model2a)

# Refining the model by removing non-significant predictors
model2b <- lm(Final~Hours+`Mid-Term`, Data)
summary(model2b)
confint(model2b)

# Extract confidence interval from the model
#
anova(model2a,model2b)

## Making predictions using the model
# e.g., A student who studied 40 hours a week and a 90 Mid Term Score?
# The "Manual" way
y = 17.50503 + 0.82324*40 + 0.48221*90
y  
  
# The "Automatic" way 
new <- as.data.frame(cbind(40,90))
colnames(new) <- c("Hours","Mid-Term")
predict.lm(model2b, new, level = 0.95)



# Data Visualization ------------------------------------------------------
#Plotting the model using in 3 dimensions (interactive)
library(plotly)

p <- Data %>% 
  plot_ly(x = ~Hours, 
          y = ~Attendance, 
          z = ~Final, 
          type = 'scatter3d', 
          mode = 'markers', 
          marker = list(size = 5, color = ~Final, colorscale = 'Viridis', opacity = 0.8)) %>%
  layout(scene = list( xaxis = list(title = 'Study Time per Month (Hours)'), 
                       yaxis = list(title = 'Attendance Rate (%)'), 
                       zaxis = list(title = 'Final Exam Score')))

print(p)




#P lotting the model in 3 dimensions (non interactive)
s3d <- scatterplot3d::scatterplot3d(x=Data$Hours, y=Data$Attendance, z=Data$Final, pch = 16, color="steelblue",
                     xlab = "Study Time per month (hour)",
                     ylab = "Attendance Rate (%)",
                     zlab = "Final Exam Score",
                     angle=70)


# Changing the angle for better visualization
s3d <- scatterplot3d::scatterplot3d(x=Data$Hours, y=Data$Attendance, z=Data$Final, pch = 16, color="steelblue",
                     xlab = "Study Time per month (hour)",
                     ylab = "Attendance Rate (%)",
                     zlab = "Final Exam Score",
                     angle=50)

# Add regression plane
s3d$plane3d(model2b)


# Adding grade as a categorical variable
library(wesanderson)

Data$Grade <- as.factor(Data$Grade)

colors <- wes_palette("GrandBudapest1",3) # select 3 colors from a color palette

colors <- colors[as.numeric(Data$Grade)]

s3d <- scatterplot3d::scatterplot3d(x=Data$Hours, y=Data$Attendance, z=Data$Final, pch = 16, color=colors,
                     xlab = "Study Time per month (hour)",
                     ylab = "Attendance Rate (%)",
                     zlab = "Final Exam Score",
                     angle=50)
# Add regression plane
s3d$plane3d(model2b)

#add legend
legend("top", legend = levels(Data$Grade), col=wes_palette("GrandBudapest1",3), pch = 16, inset = -0.25, xpd = TRUE, horiz = TRUE)

