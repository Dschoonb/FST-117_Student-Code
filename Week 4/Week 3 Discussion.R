##Formatting the Dataset
library(readxl)
Data <- read_excel("Week 4/Study Score and More.xlsx")
colnames(Data)[3:6] <- c("Attendance","Hours","Mid-Term","Final")
Data <- as.data.frame(Data)



# SLR ---------------------------------------------------------------------
##Using SLR to predict score from hours studied
library(tidyverse)
Data %>% 
  ggplot(aes(x=Hours, y=Final)) + 
  geom_point() + 
  stat_smooth(formula = y~x, method = lm, se = FALSE)
#FALSE = no confidence interval

Data %>% 
  ggplot(aes(x=Hours, y=Final)) + 
  geom_point() + 
  stat_smooth(formula = y~x, method = lm, se = TRUE)
#TRUE = confidence interval


##Getting slope and intercept by creating a linear model
whystudy <- lm(Final~Hours, Data)
summary(whystudy)

#checking out how "good" our model is
par(mfrow = c(1, 1))
plot(whystudy)

#Would this be considered a good model based on the residuals vs fitted?





# MLR ---------------------------------------------------------------------
##Using MLR to predict score using hours studied attendance and liking
whatscore <- lm(Final~Hours+Attendance+`Mid-Term`, Data)
summary(whatscore)

#Attendance is not significant so we can remove from the model and rerun
whatscore <- lm(Final~Hours+`Mid-Term`, Data)
summary(whatscore)

#extract confidence interval from the model
confint(whatscore)

summary.aov(whatscore)


## Prediction using the model
# What will be the predicted score of a student who studied 40 hours a week and attended 90% of their classes?
new <- as.data.frame(cbind(40,90))
colnames(new) <- c("Hours","Mid-Term")
predict.lm(whatscore, new, level = 0.95)



# Data Visualization ------------------------------------------------------
#Plotting the MLR model using 3 dimensions (interactive)
library(plotly)
Study$Grade <- as.factor(Study$Grade)
p <- plot_ly(Study, x = ~Hours, y = ~Attendance, z = ~Final, color = ~Grade)
p %>% layout(scene = list(xaxis = list(title = 'Study Time per month (hour)'),
                          yaxis = list(title = 'Attendance Rate (%)'),
                          zaxis = list(title = 'Final Exam Score')))


#Plotting the MLR model using 3 dimensions (non interactive)
library(scatterplot3d)
s3d <- scatterplot3d(x=Study$Hours, y=Study$Attendance, z=Study$Final, pch = 16, color="steelblue",
                     xlab = "Study Time per month (hour)",
                     ylab = "Attendance Rate (%)",
                     zlab = "Final Exam Score",
                     angle=50)



s3d <- scatterplot3d(x=Study$Hours, y=Study$Attendance, z=Study$Final, pch = 16, color="steelblue",
                     xlab = "Study Time per month (hour)",
                     ylab = "Attendance Rate (%)",
                     zlab = "Final Exam Score",
                     angle=50)
#add best fit plane
s3d$plane3d(whatscore)

#color the points by the grade that they received
library(wesanderson) # color package
Study$Grade <- as.factor(Study$Grade)
colors <- wes_palette("GrandBudapest1",3) # select 3 colors from a color palette
colors <- colors[as.numeric(Study$Grade)]
s3d <- scatterplot3d(x=Study$Hours, y=Study$Attendance, z=Study$Final, pch = 16, color=colors,
                     xlab = "Study Time per month (hour)",
                     ylab = "Attendance Rate (%)",
                     zlab = "Final Exam Score",
                     angle=50)
#add best fit plane
s3d$plane3d(whatscore)

#add legend
legend("top", legend = levels(Study$Grade), col=wes_palette("GrandBudapest1",3), pch = 16,
       inset = -0.25, xpd = TRUE, horiz = TRUE)

