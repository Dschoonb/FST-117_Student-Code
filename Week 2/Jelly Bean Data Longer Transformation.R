# Load Packages -----------------------------------------------------------
library(readxl)
library(tidyverse)

# Load Data ---------------------------------------------------------------
Data_Wide <- read_excel("week 2/FST 117_2025_Jelly Bean_Dataset.xlsx")


# Modify Structure of Dataset ---------------------------------------------
Data_Long <- Data_Wide %>%
  pivot_longer(
    cols = matches("_(green_apple|juicy_pear|sunkist_lime|mango|matcha_milk_tea)$"), 
    # select columns that end with these flavor names
    names_to = c("attribute", "product"), 
    # split column names into two new columns: attribute and product
    names_pattern = "(.*)_(green_apple|juicy_pear|sunkist_lime|mango|matcha_milk_tea)$", 
    # regex pattern to capture attribute and product
    values_to = "value",
    # name of the new column to hold values (temporary)
    values_transform = list(value = as.character)  
    # convert everything to character temporarily
  ) %>%
  pivot_wider( # Column too long now we pivot wider to get attributes as columns
    names_from = attribute, # names of new columns from attribute values
    values_from = value
  ) %>%
  # Convert numeric columns back to numeric
  mutate(
    Subject = as.factor(Subject),
    Gender = as.factor(Gender),
    Region = as.factor(Region),
    Candy_Frequency = as.factor(Candy_Frequency),
    Jelly_Bean_Frequency = as.factor(Gender),
    product = as.factor(product),
    product = as.factor(product),
    product = as.factor(product),
    Guess_the_Flavor = as.character(Guess_the_Flavor),
    Guess_the_Flavor_Corret = as.character(Guess_the_Flavor_Corret),
    Liking_Score = as.numeric(Liking_Score),
    Sweetness_Intensity = as.numeric(Sweetness_Intensity),
    Flavor_Novelty = as.numeric(Flavor_Novelty),
    Order = as.factor(Order),
    Triangle_Test = as.character(Triangle_Test),
    Triangle_Test_Correct = as.character(Triangle_Test_Correct)
  )

str(Data_Long)

#Modify flavor novelty variable to be flipped 1 is 7 and 7 is 1
Data_Long <- Data_Long %>%
  mutate(Flavor_Novelty = (7 + 1) - Flavor_Novelty)

write.csv(Data_Long, "week 2/FST 117_2025_Jelly Bean_Dataset_Long.csv", row.names = FALSE)
