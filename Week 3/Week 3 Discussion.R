##Creation of the Dataset
library(readxl)
Study <- read_excel("Study Score and More.xlsx")
colnames(Study)[3:6] <- c("Attendance","Hour","Midterm","Final")









# SLR ---------------------------------------------------------------------

##Using SLR to predict final score from hours studied
library(tidyverse)
ggplot(data=Study, aes(x=Hour, y=Final)) + geom_point() + stat_smooth(formula = y~x, method = lm, se = FALSE)
#FALSE = no confidence interval
ggplot(data=Study, aes(x=Hour, y=Final)) + geom_point() + stat_smooth(formula = y~x, method = lm, se = TRUE)
#TRUE = confidence interval


##Getting slope and intercept by creating a linear model
whystudy <- lm(Final~Hour, data = Study)
summary(whystudy)

y = 38.8 + 1.30*20
y

#to get and anova table
summary.aov(whystudy)

#checking out how "good" our model is
par(mfrow = c(1, 1))
plot(whystudy)


#Would this be considered a good model based on the residuals vs fitted?














# MLR ---------------------------------------------------------------------

##Using MLR to predict final score using hours studied attendance and liking
whatscore <- lm(Final~Hour+Attendance, data = Study)
summary(whatscore)

#Remember you can remove independent varaibles that are not significant from the model

#extract confidence interval from the model
confint(whatscore)

summary.aov(whatscore)


##Prediction using the model
#What will be the predicted final core of a student who studied 40 hours a month and attended 90% of their classes?
new <- as.data.frame(cbind(40,90))
colnames(new) <- c("Hour","Attendance")
predict.lm(whatscore, new, level = 0.95)


#Plotting the MLR model using 3 dimensions (interactive)
install.packages("plotly")
library(plotly)

Study$Grade <- as.factor(Study$Grade)
p <- plot_ly(Study, x = ~Hour, y = ~Attendance, z = ~Final, color = ~Grade)
p %>% layout(scene = list(xaxis = list(title = 'Study Time per month (hour)'),
                          yaxis = list(title = 'Attendance Rate (%)'),
                          zaxis = list(title = 'Final Exam Score')))


#Plotting the MLR model using 3 dimensions (non interactive)
install.packages("scatterplot3d")
library(scatterplot3d)
s3d <- scatterplot3d(x=Study$Hour, y=Study$Attendance, z=Study$Final, pch = 16, color="steelblue",
                     xlab = "Study Time per month (hour)",
                     ylab = "Attendance Rate (%)",
                     zlab = "Final Exam Score",
                     angle=50)

#add best fit plane
s3d$plane3d(whatscore)

#color the points by the grade that they are in
install.packages("wesanderson")
library(wesanderson) # color package
Study$Grade <- as.factor(Study$Grade)
colors <- wes_palette("GrandBudapest1",3) # select 3 colors from a color palette
colors <- colors[as.numeric(Study$Grade)]
s3d <- scatterplot3d(x=Study$Hour, y=Study$Attendance, z=Study$Score, pch = 16, color=colors,
                     xlab = "Study Time per month (hour)",
                     ylab = "Attendance Rate (%)",
                     zlab = "Final Exam Score",
                     angle=50)
#add best fit plane
s3d$plane3d(whatscore)

#add legend
legend("top", legend = levels(Study$Grade), col=wes_palette("GrandBudapest1",3), pch = 16,
       inset = -0.25, xpd = TRUE, horiz = TRUE)
