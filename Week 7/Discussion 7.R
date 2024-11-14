#### loading datasets ####

library(readxl)
liquids <- read_excel("Week 7/liquids2.xlsx")
liquids[,c(1:4)] <- lapply(liquids[,c(1:4)],factor)
str(liquids)


#### Multi-factor ANOVA with interactions ####

# 4-way ANOVA with all interactions (we don't have enough degrees of freedom to do this)
model1 <- aov(Thickness ~ (Session*Panelist*Gum*Level), data=liquids)
summary(model1)

# 4-way ANOVA with 3 way interactions
model2 <- aov(Thickness ~ (Session+Panelist+Gum+Level)^3, data=liquids)
summary(model2)


#### Refining the model ####

# 4-way ANOVA with 3-way interactions but terms are written out individually
model3 <- aov(Thickness ~ 
                  (Session+Panelist+Gum+Level+
                     Session:Panelist + Session:Gum + Session:Level + 
                     Panelist:Gum + Panelist:Level + Gum:Level + 
                     Session:Panelist:Gum + Session:Panelist:Level + 
                     Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model3)

# took out interactions that were not significant (Session:Level, Session:Panelist:Gum, and Session:Panelist:Level)
model4 <- aov(Thickness ~ 
                  (Session+Panelist+Gum+Level+
                     Session:Panelist + Session:Gum + 
                     Panelist:Gum + Panelist:Level + Gum:Level + 
                     Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model4)

# continue refining model - took out Session:Gum and Session:Gum:Level
model5 <- aov(Thickness ~ 
                  (Session+Panelist+Gum+Level+
                     Session:Panelist +  
                     Panelist:Gum + Panelist:Level + Gum:Level + 
                     Panelist:Gum:Level), data=liquids)
summary(model5) 

#mean sq error increased!



# let's backtrack - only remove Session:Gum this time
model6 <- aov(Thickness ~ 
                  (Session+Panelist+Gum+Level+
                     Session:Panelist +  
                     Panelist:Gum + Panelist:Level + Gum:Level + 
                     Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model6)


#### Pseudo mixed model ####

# testing the effect of gum
model3.mixed <- aov(Thickness ~ Gum + Error(Panelist/Gum), data=liquids)
summary(model3.mixed)

# testing the effect of level
model4.mixed <- aov(Thickness ~ Level + Error(Panelist/Level), data=liquids)
summary(model4.mixed)


# what if our dependent variable is slipperiness?

model5 <- aov(Slipperiness ~ 
                  (Session+Panelist+Gum+Level+
                     Session:Panelist + Session:Gum + Session:Level + 
                     Panelist:Gum + Panelist:Level + Gum:Level + 
                     Session:Panelist:Gum + Session:Panelist:Level + 
                     Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model5)

model5.1 <- aov(Slipperiness ~ 
                (Session+Panelist+Gum+Level+
                   Session:Panelist +  
                   Panelist:Gum + Panelist:Level + Gum:Level + 
                   Session:Gum:Level + Panelist:Gum:Level), data=liquids)
summary(model5.1)


model5.mixed <- aov(Slipperiness ~ Gum + Error(Panelist/Gum), data=liquids)
summary(model5.mixed) # with the pseudo mixed model, gum effect is no longer significant

model5.mixed2 <- aov(Slipperiness ~ Level + Error(Panelist/Level), data=liquids)
summary(model5.mixed2)
