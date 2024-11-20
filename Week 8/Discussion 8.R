#Load Full Data set
library(readxl)
EBar.OG <- read_excel("Week 8/E Bar Week 8.xlsx")



# Formating our Data set to run PCA ---------------------------------------
#Removing Factors and creating a matrix for our variables
EBar.mat <- as.matrix(EBar.OG[,c(4:16)])

#Calculating Mean Sensory Intensities for Each Product
EBar.means <- aggregate(EBar.mat ~ Product, data=EBar.OG, FUN=mean) 
View(EBar.means)

#Changing defult row names to sample names and removing sample names from matrix
row.names(EBar.means) <- EBar.means$Product
EBar.means$Product <- NULL
View(EBar.means)

# Running the PCA ---------------------------------------------------------
library(FactoMineR)
EBar.pca <- PCA(EBar.means)

#Extracting Eigen Values
library(factoextra)
get_eigenvalue(EBar.pca)

#Generating a Scree Plot
fviz_screeplot(EBar.pca)

#Generating a Bi-plot
fviz_pca_biplot(EBar.pca)

##Bonus Information!
#Generating a correlation Matrix -> extracting matrix from R -> Analyzing in excel
cor_matrix <- cor(EBar.means)

write.csv(cor_matrix, "FST 117 Discussion 8 Correlation Matrix.csv")