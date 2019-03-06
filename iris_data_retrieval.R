#Packages to be installed

install.packages('dplyr')
install.packages('ggplot2')
install.packages("devtools")
devtools::install_github("phil8192/lazy-iris")
install.packages('caTools')
install.packages('rpart')
install.packages('rpart.plot')

#Libraries to be installed

library(dplyr)
library(ggplot2)
require(lazyIris)
library(readr)
library(devtools)
library(rpart)
library(rpart.plot)
library(caTools)
#To clear Environment
rm(list =ls())

#Read the dataset from onmline
iris_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",
              header = FALSE)

#Assigning column names to the dataset
colnames(iris_data)=c('sepal.length','sepal.width','petal.length','petal.width','Class')

#Data checking

checkData <- function(iris_data) {
  
  # clean missing values (could also interpolate).
  if(any(is.na(iris_data))) {
    iris_data <- iris_data[!apply(iris_data, 1, function(v) any(is.na(v))), ]
    warning("removed rows with missing values.")
  }
  
  # remove duplicates (could also check for conflicting species.)
  if(anyDuplicated(iris_data)) {
    iris_data <- unique(iris_data)
    warning("removed duplicated rows.")   
  }
  
  # remove strange measurements.
  if(any(iris_data[, 1:4] <= 0)) {
    iris_data <- iris_data[!apply(iris_data, 1, function(v) any(v <= 0)), ]
    warning("removed instances with width/length <= 0.") 
  }
  
  # check for anything odd. (could also check for outliers etc.)
  if(any(iris_data[, 1:4] > 100)) {
    warning("dataset contains gigantic iris plants.") 
  }
}

checkData(iris_data)

#Prompt the user for 4 inputs needed

query_from_user <- list(
  sepal.length = as.numeric(readline('Please input Sepal length')),
  sepal.width = as.numeric(readline('Please input Sepal width')),
  petal.length = as.numeric(readline('Please input petal length')),
  petal.width = as.numeric(readline('Please input petal width')))

#obtain the nearest-neighbours using euclidean distance
top.10 <- knn(query_from_user, iris_data, 10)
print(top.10, row.names=FALSE)

#Assigning column name
colnames(top.10)[5]<-'Species'
#Visualization 
visualise(iris_data, class.name="Class", query=query_from_user, neighbours=top.10,
          main="Iris data neighbours", plot.hist=FALSE, plot.cor=FALSE)

#To make the results reproducible
set.seed(6)  

#Splitting Training and test data
total_data <- sample.split(seq_len(nrow(iris_data)), 0.7)
train_data <- iris_data[total_data, ]
test_data <-  iris_data[!total_data, ]

#Findings using decision trees
set.seed(2387)
#Building decision tree model
dt_model <- rpart(Class ~ ., train_data) # training
#Visualizing Decision Tree
prp(dt_model)
