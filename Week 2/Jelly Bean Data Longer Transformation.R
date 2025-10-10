# Load Packages -----------------------------------------------------------
library(readxl)
library(tidyverse)

# Load Wide Data ---------------------------------------------------------------
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
  mutate(Flavor_Novelty = (7 + 1) - as.numeric(Flavor_Novelty)) %>%
  #flip novelty scale so 1 = very novel, 7 = not at all novel
  select(-starts_with("Guess_the_Flavor"), -starts_with("Triangle_Test")) %>%
  # remove Guess the Flavor and Triangle Test columns
  select(Subject, product, Order, everything())
  # reorder columns to have Subject, product, Order first)

str(Data_Long)

write.csv(Data_Long, "week 2/FST 117_2025_Jelly Bean_Dataset_Long.csv", row.names = FALSE)
