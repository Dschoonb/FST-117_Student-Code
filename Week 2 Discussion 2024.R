## export plots in png format for following homework





## FAQs
# change column name
# option 1
colnames(yogurt) <- c("consumer","product","liking")
View(yogurt)
# option 2
install.packages("tidyverse")
library(tidyverse)
yogurt1 <-rename_at(yogurt,1,~"consumer")
View(yogurt1)
yogurt1 <-rename_at(yogurt,5,~"product")
# for aforementioned codes, you should only select one option
# if you run option 1, 4th column to last columns will result in empty column names (indicated as NA by RStudio) because you only renamed 3 columns here, and you won't be able to run option 2
# to resolve this problem, you have 2 ways: (1) import your dataset again; (2) rename every column, in this case 9 of them, and then run option 2
# do not make a column name blank, if the column name is blank originally, you can change it with colnames()





## import dataset
library(readxl)
Study <- read_excel("Study Score.xlsx", 
                          sheet = "Sheet 1")





## library packages needed
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)





## Scatter Plots


# basic scatter plot
scatter <- ggplot(data = Study,aes(x = Hour, y = Score)) + 
  geom_point() 
scatter # view your plot on bottom right corner


# add axis label and title
scatter+labs(x="Study Time per Month (hour)", 
             y="Midterm Exam Score", 
             title = "Score vs. Study Time")


# change color of points
scatter1 <- ggplot(data = Study,aes(x = Hour, y = Score)) + 
  geom_point(color="orange") + # where we change the color
  labs(x="Study Time per Month (hour)", 
       y="Midterm Exam Score", 
       title = "Score vs. Study Time")
scatter1


# add another factor and change color base on this factor
Study$Grade<-as.factor(Study$Grade) # set Grade as a factor
scatter2 <- ggplot(data = Study,aes(x = Hour, y = Score)) + 
  geom_point(mapping = aes(color = Grade)) + # where we add the new factor
  labs(x="Study Time per Month (hour)", 
       y="Midterm Exam Score", 
       title = "Score vs. Study Time") #same as above
scatter2


# make sub-plots base on a factor
scatter2+facet_wrap(~Grade)





## Correlation
cor.test (Study$Score, Study$Hour, method = "pearson") # we can change the method according to need, pearson/ spearman/ kendall
?cor.test





## measures of dispersion

# mean
mean(Study$Hour)


# median
median(Study$Hour)


# mode
# R does not have a built-in function for mode, we need to create our own:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(Study$Hour)



# range
range(Study$Score)


# variance
var(Study$Score)


# standard deviation
sd(Study$Score)


# IQR
IQR(Study$Score) # 3rd quartile - 1st quartile





## Histograms
# basic histogram
ggplot(data = Study)+
  geom_histogram(mapping = aes(x=Hour))


# prettier histogram
ggplot(data = Study) +
  geom_histogram(mapping = aes(x = Hour), 
                 bins = 53, # adjust number of bins
                 fill = "skyblue", # change fill color
                 color = "black") + # add border color
  labs(title = "Distribution of Study Hours",
       x = "Study Hour",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),# center and resize title
        axis.title = element_text(size = 12))# resize axis labels

Study %>% 
  count(Hour) %>%
  View() # see count of study hour, and adjust bins accordingly
