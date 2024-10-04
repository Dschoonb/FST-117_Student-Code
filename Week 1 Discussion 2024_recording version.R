# option 1

# option 2

# option 3
install.packages("readxl")

library(readxl)

yogurt <- read_excel("week 1/Yogurt_liking.xlsx",
                     sheet = "Liking_8yogurts")
View(yogurt)

soda <- read_excel("week 1/Soda demo.xlsx")
View(soda)


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


# delete column from dataset
yogurt11 <-select(yogurt1,-3,-4)
View(yogurt11)



# format data
View(yogurt)

library(tidyverse)
yogurt1 <-rename_at(yogurt,1,~"consumer")
View(yogurt1)

library(tidyverse)
yogurt2 <- pivot_longer(yogurt1,!consumer,
                        names_to = "product",
                        values_to = "liking")
View(yogurt2)

yogurt22 <- pivot_longer(yogurt1,
                         cols = starts_with("P"),
                         names_to = "product",
                         names_prefix = "P", # removing P from product column
                         values_to = "liking",
                         values_drop_na = TRUE)


# basic computations

### always work on cleaned/formatted dataset!!!

# cleaned: yogurt2, soda

# count
yogurt2 %>% count(consumer,product)


# average
yogurt2 %>% 
  group_by(product)%>%
  dplyr::summarize(Mean=mean(liking,na.rm = TRUE))


#chi-sq test
soda_table <- table(soda$`Soda Preference`)
soda_table
chisq.test(soda_table)

soda %>% count(`Soda Preference`)
soda_table2 <- c(34,52,76,38)
chisq.test(soda_table2)


# 2-way chi-sq test
soda_table3 <- table(soda$`Soda Preference`,soda$Gender)
soda_table3
chisq.test(soda_table3)



# histograms
ggplot(data = yogurt2)+
  geom_histogram(mapping = aes(x=liking))



























