## practice install.packages() and library()
install.packages("tidyverse") # with quotation marks
library(tidyverse) # without quotation marks



## import data with coding
library(readxl)
yogurt <- read_excel("week 1/Yogurt_liking test.xlsx", # notice I have a "week 1/" before file name b/c this file locates under a sub-folder on my laptop
                     sheet = "Liking_8yogurts")

yogurt <- read_excel("week 1/Yogurt_liking test.xlsx",
                     sheet = 1) # page number in excel, ie. first sheet -> 1, second sheet -> 2

View(yogurt)



## change column title
yogurt_clean <-read_excel("week 1/Yogurt_liking test.xlsx", 
                          sheet = "option3") # does note clean/format the table, so we use cleaned table as example
library(tidyverse)

# option1

colnames(yogurt_clean) <- c("consumer","product","liking")

# option2
yogurt1 <- rename_at(yogurt_clean,1,~"consumer")
View(yogurt1)




## format table
yogurt2 <- pivot_longer(yogurt,
                        cols = starts_with("P"), # col name starts with P, ie. P1, P2,...,P8
                        names_to = "product", # transpose to a new column call "product"
                        values_to = "liking",
                        values_drop_na = TRUE)
#names_prefix = "P", removes the P from column names

View(yogurt2)
yogurt2 <- rename_at(yogurt2,1,~"consumer")




## delete columns
yogurt2 <- select(yogurt,-3,-4)
View(yogurt2) # previous dataset overwritten



## count
yogurt2 %>% count(consumer) # for each consumer, how many data points we collected
yogurt2 %>% count(consumer, product) # for each consumer per product, how many data points we collected -> once 



## average
yogurt2 %>%
  group_by(product)%>%
  summarise(Mean=mean(liking,na.rm=TRUE))



## 1-way chi-sq
soda <- read_excel("week 1/Soda test.xlsx")
soda

# option1
soda_table <- table(soda$'Soda Preference')
soda_table
chisq.test(soda_table)

# option2
soda %>% count(`Soda Preference`) #``tilde instead of '
soda_table2 <- c(34,52,76,38)
soda_table2
chisq.test(soda_table2)

# find critical chi-sq
qchisq()



## 2-way chi-sq
soda_table3 <-table(soda$`Soda Preference`, soda$Gender)
soda_table3
chisq.test(soda_table3)



## histogram
ggplot(data = yogurt2)+
  geom_histogram(mapping = aes(x=liking))



## boxplot
ggplot(data = yogurt2)+
  geom_boxplot(mapping = aes(x=product, y=liking))
ggplot(data = yogurt2)+
  geom_boxplot(mapping = aes(x=product, 
                             y=liking,
                             color=product))



## binomial test
dbinom()
