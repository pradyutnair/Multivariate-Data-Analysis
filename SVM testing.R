dirName <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirName)
############################################################
library(pacman)
pacman::p_load(caTools,tidyverse,caret,e1071,splitTools,ranger,mdatools)
############################################################
# Import data
df <- read.csv('./NIR_Preprocessed.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
df <- df[,-1]
############################################################
# Split data into train and test sets
base_sample <- partition(df$Freshness, p = c(train = 0.7, valid=0.2, test = 0.1))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]

y_train <- base_train$Freshness
y_valid <- base_valid$Freshness
y_test <- base_test$Freshness

base_train <- base_train[,5:length(base_train)]
base_valid <- base_valid[,5:length(base_valid)]
base_test <- base_test[,5:length(base_test)]
############################################################
m <- tune.svm(x = base_train,y=y_train,
              tunecontrol=tune.control(cross=10),cost=1:3,gamma=seq(0,0.5,by=0.1),
              kernel="polynomial",degree=1:4)

m$best.parameters
m$best.model
