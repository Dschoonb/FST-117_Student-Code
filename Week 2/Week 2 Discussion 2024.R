# Load Packages -----------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggplot2)


# Load Data ---------------------------------------------------------------
Data_Long <- read.csv("week 2/FST 117_2025_Jelly Bean_Dataset_Long.csv")


# Basic Data Exploration --------------------------------------------------
## measures of dispersion
# mean
mean(Data_Long$Liking_Score)

# median
median(Data_Long$Liking_Score)

# mode
# R does not have a built-in function for mode, we need to create our own:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(Data_Long$Liking_Score)

# range
range(Data_Long$Liking_Score)
  
# variance
var(Data_Long$Liking_Score)

# standard deviation
sd(Data_Long$Liking_Score)

# IQR
IQR(Data_Long$Liking_Score) # 3rd quartile - 1st quartile

# What if I want the means for each product?
Data_Long %>%
  group_by(product) %>%
  summarise(mean(Liking_Score))

# Summary of the whole dataset
summary(Data_Long)



# Simple Visualizations of Data ---------------------------------------------------
## Histograms
# basic histogram
Data_Long %>% 
  ggplot() +
  geom_histogram(mapping = aes(x=Liking_Score))


# prettier histogram
Data_Long %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = Liking_Score), 
                 fill = "skyblue") +
  facet_wrap(~product)


# change number of bins (matters when looking at true continuous data)
Data_Long %>%
  ggplot() +
  geom_histogram(mapping = aes(x = Liking_Score), 
                 fill = "skyblue", 
                 bins = 5) +
  facet_wrap(~product)


#2D Visualizations of Data ---------------------------------------------------------------
## Basic Scatter Plot
a1 <- Data_Long %>% 
  ggplot(aes(x = Liking_Score, y = Flavor_Novelty))
print(a1) #view plot

# add points
a2 <- a1 + geom_point()
print(a2)

# Jitter points
a3 <- a1 + geom_jitter()
print(a3)

#A dd Labels
a4 <- a3 + labs(x="Liking", y="Flavor Novelty", title = "Jelly Bean: Liking vs. Flavor Novelty")
print(a4)


## Scatter Plot with colors and a different theme
b1 <- Data_Long %>% 
  ggplot(aes(x = Liking_Score, y = Flavor_Novelty)) +
  geom_jitter(color= "orange") + # change point color
  labs(x="Liking", y="Flavor Novelty", title = "Jelly Bean: Liking vs. Flavor Novelty") +
  theme_minimal() # change overall theme
print(b1)

## Adding additional variables
Data_Long$Candy_Frequency <- as.factor(Data_Long$Candy_Frequency) # set Grade as a factor

c1 <- Data_Long %>% 
  ggplot(aes(x = Liking_Score, y = Flavor_Novelty)) + 
  geom_jitter(mapping = aes(color = Candy_Frequency)) + # where we add the new factor
  labs(x="Liking", y="Flavor Novelty", title = "Jelly Bean: Liking vs. Flavor Novelty", color = "Candy Frequency") +
  theme_minimal()
print(c1)

# make sub-plots base on a factor
c1 + facet_wrap(~Candy_Frequency)

# Statistical Analysis of Data ---------------------------------------------------------------
## Correlation
cor.test(Data_Long$Liking_Score, Data_Long$Flavor_Novelty, method = "pearson") 
# we can change the method according to need, pearson/ spearman/ kendall
?cor.test

Data_Long %>%
  group_by(Candy_Frequency) %>%
  summarise(cor(Liking_Score, Flavor_Novelty, method = "pearson")) # need to use cor instead of cor.test

Data_Long %>%
  summarise(cor(Liking_Score, Flavor_Novelty, method = "pearson")) # need to use cor instead of cor.test

table(Data_Long$Candy_Frequency)



# how many observations in each group
# if one group has very few observations, correlation may not be reliable
