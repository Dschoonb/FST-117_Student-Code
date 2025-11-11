# import data ------------------------------------------------------------------
library(readxl)
liquids <- read_excel("week 7/liquids2.xlsx")


## take a look at our data
str(liquids)
## we need to change the independent variables into factors


## option 1 - lapply() from base R
liquids[,c(1:4)] <- lapply(liquids[,c(1:4)],factor)
str(liquids)


## option 2 - mutate() from tidyverse
library(tidyverse)
liquids <- 
  liquids %>% 
  mutate(across(c(Session,Panelist,Gum,Level),
                as.factor))
str(liquids)








# Multi-factor ANOVA with interactions -----------------------------------------

## keep in mind:
## 3 Gums * 3 Levels = 9 samples
## 4 Sessions * 10 Panelists = 40 observarions per sample


## df(model)
3*3*4*10-1 # 359



## 4-way ANOVA with all interactions (we don't have enough degrees of freedom to do this)
library(lmerTest)
model1 <- lm(Thickness ~ (Session*Panelist*Gum*Level), data=liquids)
summary(model1)
##  we don't see F-values and p-values here


## 4-way ANOVA with 3 way interactions
model2 <- lm(Thickness ~ (Session+Panelist+Gum+Level)^3, data=liquids)
summary(model2)





# Refining the model -----------------------------------------------------------
# 4-way ANOVA with 3-way interactions but terms are written out individually
model2.1 <- lm(Thickness ~ 
                (Session+Panelist+Gum+Level+
                   Session:Panelist + Session:Gum + Session:Level + 
                   Panelist:Gum + Panelist:Level + Gum:Level + 
                   Session:Panelist:Gum + Session:Panelist:Level + 
                   Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model2.1)


# took out interactions that were not significant 
# (Session:Level, Session:Panelist:Gum, and Session:Panelist:Level)
model2.2 <- lm(Thickness ~ 
                (Session+Panelist+Gum+Level+
                   Session:Panelist + Session:Gum + 
                   Panelist:Gum + Panelist:Level + Gum:Level + 
                   Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(mode2.2)


# continue refining model 
# took out Session:Gum and Session:Gum:Level
model2.3 <- lm(Thickness ~ 
                (Session+Panelist+Gum+Level+
                   Session:Panelist +  
                   Panelist:Gum + Panelist:Level + Gum:Level + 
                   Panelist:Gum:Level), data=liquids)
summary(model2.3) 

#mean sq error increased!



# let's backtrack 
# only remove Session:Gum this time
# keep Session:Gum:Level
model2.4 <- lm(Thickness ~ 
                (Session+Panelist+Gum+Level+
                   Session:Panelist +  
                   Panelist:Gum + Panelist:Level + Gum:Level + 
                   Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model2.4)




# Pseudo mixed model -----------------------------------------------------------

## re-run refined full model
model2.4 <- lm(Thickness ~ 
                  (Session+Panelist+Gum+Level+
                     Session:Panelist +  
                     Panelist:Gum + Panelist:Level + Gum:Level + 
                     Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model2.4)


## testing the effect of gum
model2.4.mixed <- lm(Thickness ~ Gum + Error(Panelist/Gum), data=liquids)
summary(model2.4.mixed)

# testing the effect of level
model2.4.mixed2 <- lm(Thickness ~ Level + Error(Panelist/Level), data=liquids)
summary(model2.4.mixed2)


# what if our dependent variable is slipperiness?

model3 <- lm(Slipperiness ~ 
                (Session+Panelist+Gum+Level+
                   Session:Panelist + Session:Gum + Session:Level + 
                   Panelist:Gum + Panelist:Level + Gum:Level + 
                   Session:Panelist:Gum + Session:Panelist:Level + 
                   Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model3)

model3.1 <- lm(Slipperiness ~ 
                  (Session+Panelist+Gum+Level+
                     Session:Panelist +  
                     Panelist:Gum + Panelist:Level + Gum:Level + 
                     Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model3.1)


model3.1.mixed <- lm(Slipperiness ~ Gum + Error(Panelist/Gum), data=liquids)
summary(model3.1.mixed) # with the pseudo mixed model, gum effect is no longer significant

model3.1.mixed2 <- lm(Slipperiness ~ Level + Error(Panelist/Level), data=liquids)
summary(model3.1.mixed2)
