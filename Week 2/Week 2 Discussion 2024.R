# Load Packages -----------------------------------------------------------
library(readr)
library(tidyverse)
library(ggplot2)


# Load Data ---------------------------------------------------------------
Data_Long <- read.csv("week 2/FST 117_2025_Jelly Bean_Dataset_Long.csv")

# Modify Data Types -------------------------------------------------------
Data_Long <- 
  Data_Long %>% 
  mutate( 
    Subject = as.factor(Subject),
    product = as.factor(product),
    Order = as.factor(Order),
    Gender = as.factor(Gender),
    Region = as.factor(Region),
    Candy_Frequency = as.factor(Candy_Frequency),
    Jelly_Bean_Frequency = as.factor(Jelly_Bean_Frequency),
    Liking_Score = as.numeric(Liking_Score),
    Sweetness_Intensity = as.numeric(Sweetness_Intensity),
    Flavor_Novelty = as.numeric(Flavor_Novelty)
  )

# Basic Data Exploration --------------------------------------------------
## measures of central tendency
# mean
a <- mean(Data_Long$Liking_Score)

# What if I want the means for each product?
Data_Long %>%
  group_by(product) %>%
  summarise(mean(Liking_Score))

# median
median(Data_Long$Liking_Score)

# mode
# R does not have a built-in function for mode, we need to create our own:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(Data_Long$Liking_Score)

## measures of dispersion
# range
range(Data_Long$Liking_Score)
  
# variance
var(Data_Long$Liking_Score)

# standard deviation
sd(Data_Long$Liking_Score)

# IQR
IQR(Data_Long$Liking_Score) # 3rd quartile - 1st quartile


# Summary of the whole dataset
summary(Data_Long)



# Simple Visualizations of Data ---------------------------------------------------
## Histograms
# Basic histogram
Data_Long %>% 
  ggplot() +
  geom_histogram(aes(x=Liking_Score))


# More information with color and facets
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
                 bins = 9)

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
  geom_jitter(color= "firebrick") + # change point color
  labs(x="Liking", y="Flavor Novelty", title = "Jelly Bean: Liking vs. Flavor Novelty") +
  theme_minimal() # change overall theme
print(b1)


## Adding additional variables
c1 <- Data_Long %>% 
  ggplot(aes(x = Liking_Score, y = Flavor_Novelty)) + 
  geom_jitter(mapping = aes(color = Candy_Frequency)) + # where we add the new factor
  labs(x="Liking", y="Flavor Novelty", title = "Jelly Bean: Liking vs. Flavor Novelty", color = "Candy Frequency") +
  theme_minimal()
print(c1)

c1 + facet_wrap(~Candy_Frequency, drop=FALSE)


#Making sure the levels are in order
Data_Long$Candy_Frequency <- factor(
  Data_Long$Candy_Frequency,
  levels = c(
    "Everyday",
    "Several times a week",
    "Once a week",
    "Several times a month",
    "Once a month",
    "Less than once a month"
  )
)
#re-run the plot



# Correlation ---------------------------------------------------------------
cor.test(Data_Long$Liking_Score, Data_Long$Flavor_Novelty, method = "pearson") 
# we can change the method according to need, pearson/ spearman/ kendall

Data_Long %>%
  group_by(Candy_Frequency) %>%
  summarise(cor(Liking_Score, Flavor_Novelty, method = "pearson")) # need to use cor instead of cor.test for this coding structure


# how many observations in each group
# if one group has very few observations, correlation may not be reliable
table(Data_Long$Candy_Frequency)



