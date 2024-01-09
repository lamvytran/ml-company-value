pca.df <- subset(final_df, select=-c(objective))

library("ggplot2")
library("ggfortify")
library("gridExtra")
library("carData")
library("car")
library("factoextra")
library("corrplot")
library('RPostgres')
library("tidyr")

summary(pca.df)
#First step: computation of PCA. Centering data around 0 by shifting the varibles, rescaling the variance to 1 unit; data standarization needed due to the fact that variables are measured in different scales. Additionally, the eigenvalues are extracted by get_eigenvalue() function. Eigenvalues measure the amount of variation held by each principal component (PC). They are evaluated to determine the number of principal components to be considered.
scale.pca <- prcomp(pca.df,scale=TRUE)
print(scale.pca)
summary(scale.pca)

#Calculate the eigen value
eig.val<-get_eigenvalue(scale.pca)
eig.val

#Draft the scree plot
fviz_eig(scale.pca,col.var="blue")

#20 variables could represent 80% of the dataset.
n_components <- 20
pca_data<- as.data.frame(predict(scale.pca, newdata = pca.df)[, 1:n_components])
pca_data['objective'] = final_df$objective
pca_data['date'] = final_df$date

train_start <- 20110131
train_end <- 20181231
valid_start <- 20190101
valid_end <- 20191231
test_start <- 20200101
test_end <- 20201231

pca_train <- pca_data[pca_data$date >= train_start & pca_data$date <= train_end, ]
pca_valid <- pca_data[pca_data$date >= valid_start & pca_data$date <= valid_end, ]
pca_test <- pca_data[pca_data$date >= test_start & pca_data$date <= test_end, ]







