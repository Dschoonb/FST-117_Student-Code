# import -----------------------------------------------------------------------
library(readxl)
biscuit <- read_excel("week 8/biscuits_sensory_profile.xlsx")
str(biscuit)



# format -----------------------------------------------------------------------
library(tidyverse)
biscuit <- 
  biscuit %>% 
  mutate(across(c(Judge,Product),
                as.factor))

str(biscuit)
## cols: judge, product, 32 attributes
## 9 judges, 11 products





# prepare for PCA --------------------------------------------------------------

## we are looking for a wide format data 
## with products as rows and attributes as columns
## so we are gonna average across judges

biscuit_mean <- 
  biscuit %>% 
  group_by(Product) %>% 
  summarise(across(where(is.numeric),
                   mean))
## now we have a df with products as rows and atts as columns

## some sensory scientists suggest that non-sig attributes should be removed
## while others believe including the full data would be more meaningful for a 
## multivariate analysis like PCA

## assume if we want to remove non-sig what analysis should be done here?
## yep, ANOVA
## for this discussion, I've removed non-sig attributes
## but this is not a necessary step as it has both pros and cons

## by screening, Roasted odor, cereal flavor, astringent, sticky are non-sig
## let's remove it for this example
biscuit_mean <- 
  biscuit_mean %>% 
  select(-c(`Roasted odor`,
            `Cereal flavor`,
            Astringent,
            Sticky))
# THIS IS NOT MANDATORY AND YOU CAN CHOOSE BASE ON YOUR PREFERENCE
# FOR YOUR HOMEWORK, YOU DO NOT NEED TO REMOVE ANYTHING



# run PCA ----------------------------------------------------------------------

## let's run a PCA and store the result
library(FactoMineR)
biscuit_pca <- 
  biscuit_mean %>% 
  arrange(Product) %>% 
  as.data.frame() %>% 
  column_to_rownames(var = "Product")

res_pca_covariance <- 
  PCA(biscuit_pca, 
      scale.unit = FALSE,
      graph = FALSE) # this is the function doing PCA

res_pca_correlation <- 
  PCA(biscuit_pca, 
      scale.unit = TRUE,
      graph = FALSE) # this is the function doing PCA







# visualize PCA (covariance)----------------------------------------------------


## loading plot ----------------------------------------------------------------

## we can also just use the base R to plot, but it looks ugly...
PCA(biscuit_pca, 
    scale.unit = FALSE, # scale to unit variance
    graph = TRUE)
## this is loading plot
## another thing we look at is score plot
## or combination of both -> biplot



## factoextra allow us to customize more!!
library(factoextra)



## basic pca loading plot
fviz_pca_var(res_pca_covariance,
             repel = TRUE) # avoid tags overlapping

fviz_pca_var(res_pca_covariance,
             repel = TRUE) +
  labs(title = "Covariance PCA")


## score plot ------------------------------------------------------------------

## basic pca score plot
fviz_pca_ind(res_pca_covariance,
             repel = TRUE)


## let add color by product and legend
fviz_pca_ind(res_pca_covariance,
             col.ind = biscuit_mean$Product,
             geom = "point",
             legend.title = "Product",
             repel = TRUE)


## lets make all points into solid dots
fviz_pca_ind(res_pca_covariance,
             col.ind = biscuit_mean$Product,
             geom = "point",
             pointshape = 16, # you can also choose other shapes
             legend.title = "Product",
             repel = TRUE) +
  labs(title = "Covariance PCA")


## and you can save the plot you prefer for export
p_score_covariance <- 
  fviz_pca_ind(res_pca_covariance,
               col.ind = biscuit_mean$Product,
               geom = "point",
               pointshape = 16, # you can also choose other shapes
               legend.title = "Product",
               repel = TRUE)+
  labs(title = "Covariance PCA")

p_score_covariance


## biplot plot ----------------------------------------------------------------
fviz_pca_biplot(res_pca_covariance,
                repel = TRUE)+
  labs(title = "Covariance PCA")



## Scree plot -----------------------------------------------------------------

#Extracting Eigen Values - numeric
get_eigenvalue(res_pca_covariance)

#Generating a Scree Plot
fviz_screeplot(res_pca_covariance)+
  labs(title = "Covariance PCA")



# visualize PCA (correlation)---------------------------------------------------

## loading
fviz_pca_var(res_pca_correlation) +
  labs(title = "Correlation PCA")
# you can see a circle outside
# the circle represent length of 1 unit in all directions

## score
fviz_pca_ind(res_pca_correlation) +
  labs(title = "Correlation PCA")


## biplot
fviz_pca_biplot(res_pca_correlation) +
  labs(title = "Correlation PCA")


## scree
fviz_screeplot(res_pca_correlation) +
  labs(title = "Correlation PCA")

