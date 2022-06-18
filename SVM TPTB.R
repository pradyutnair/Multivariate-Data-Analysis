dirName <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirName)
############################################################
library(pacman)
pacman::p_load(caTools,tidyverse,caret,e1071,splitTools,ranger,mdatools)

############################################################
# Import data
df <- read.csv('./NIR_Preprocessed.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
df <- df[,-1]
# Use only TB-TP Data
tbtp.df <- df[(df$Scan_type == "TB") | (df$Scan_type == "TP"),]
df <- tbtp.df

############################################################
# Split data into train and test sets
base_sample <- partition(df$Freshness, p = c(train = 0.6, valid=0.2, test = 0.2))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]

############################################################
# Train set
X_tb_train <- base_train[base_train$Scan_type == "TB",][,5:length(base_train)]
X_tp_train <- base_train[base_train$Scan_type == "TP",][,5:length(base_train)]

y_tb_train <- base_train[base_train$Scan_type == "TB",]$Freshness
y_tp_train <- base_train[base_train$Scan_type == "TP",]$Freshness

# Validation set
X_tb_valid <- base_valid[base_valid$Scan_type == "TB",][,5:length(base_valid)]
X_tp_valid <- base_valid[base_valid$Scan_type == "TP",][,5:length(base_valid)]

y_tb_valid <- base_valid[base_valid$Scan_type == "TB",]$Freshness
y_tp_valid <- base_valid[base_valid$Scan_type == "TP",]$Freshness

# Test set
X_tb_test <- base_test[base_test$Scan_type == "TB",][,5:length(base_test)]
X_tp_test <- base_test[base_test$Scan_type == "TP",][,5:length(base_test)]

y_tb_test <- base_test[base_test$Scan_type == "TB",]$Freshness
y_tp_test <- base_test[base_test$Scan_type == "TP",]$Freshness

############################################################
# Baseline performances
svm.tb.base <- svm(X_tb_train, y_tb_train, type = "C-classification",
                   kernel = "polynomial", degree = 2)

tb_train_preds <- predict(svm.tb.base,newdata=data.frame(X_tb_train))
tb_test_preds <- predict(svm.tb.base,newdata=data.frame(X_tb_test))

# Accuracies
print(paste0("SVM BASE accuracy on TB train set: ",
             round(mean(tb_train_preds==y_tb_train),3)))
print(paste0("SVM BASE accuracy on TB test set: ",
             round(mean(tb_test_preds==y_tb_test),3)))

svm.tp.base <- svm(X_tp_train, y_tp_train, type = "C-classification",
                   kernel = "polynomial", degree = 2)

tp_train_preds <- predict(svm.tp.base,newdata=data.frame(X_tp_train))
tp_test_preds <- predict(svm.tp.base,newdata=data.frame(X_tp_test))

# Accuracies
print(paste0("SVM BASE accuracy on TP train set: ",
             round(mean(tp_train_preds==y_tp_train),3)))
print(paste0("SVM BASE accuracy on TP test set: ",
             round(mean(tp_test_preds==y_tp_test),3)))

############################################################
# SVM on TB
m <- tune.svm(x = X_tb_valid,y=y_tb_valid,
              tunecontrol=tune.control(cross=10),cost=1:3,gamma=seq(0,1,by=0.1),kernel="polynomial",degree=2)


m$best.model
m$best.parameters

svm.tb <- svm(formula = y_tb_train ~ .,
             data = data.frame(X_tb_train),
              gamma=m$best.parameters$gamma,
              cost=m$best.parameters$cost,
             type = 'C-classification',
             kernel = 'polynomial',degree=2)

tb_train_preds <- predict(svm.tb,newdata=data.frame(X_tb_train))
tb_test_preds <- predict(svm.tb,newdata=data.frame(X_tb_test))

# Accuracies
print(paste0("SVM accuracy on TB train set: ",
             round(mean(tb_train_preds==y_tb_train),3)))
print(paste0("SVM accuracy on TB test set: ",
             round(mean(tb_test_preds==y_tb_test),3)))

# Sensitivity and specificity
print(paste0("SVM Train TB Sensitivity: ",
             round(sensitivity(tb_train_preds, y_tb_train),3)))
print(paste0("SVM Train TB Specificity: ",
             round(specificity(tb_train_preds, y_tb_train),3)))

print(paste0("SVM Test TB Sensitivity: ",
             round(sensitivity(tb_test_preds, y_tb_test),3)))
print(paste0("SVM Test TB Specificity: ",
             round(specificity(tb_test_preds, y_tb_test),3)))

############################################################
# SVM on TP
m <- tune.svm(x = X_tp_valid,y=y_tp_valid,
              tunecontrol=tune.control(cross=10),cost=1:3,gamma=seq(0,1,by=0.1))

m$best.model
m$best.parameters

svm.tp <- svm(formula = y_tp_train ~ .,
             data = data.frame(X_tp_train),
             gamma=m$best.parameters$gamma,
             cost=m$best.parameters$cost,
             type = 'C-classification',
             kernel = 'radial')

tp_train_preds <- predict(svm.tp,newdata=data.frame(X_tp_train))
tp_test_preds <- predict(svm.tp,newdata=data.frame(X_tp_test))

# Accuracies
print(paste0("SVM accuracy on TP train set: ",
             round(mean(tp_train_preds==y_tp_train),3)))
print(paste0("SVM accuracy on TP test set: ",
                round(mean(tp_test_preds==y_tp_test),3)))

# Sensitivity and specificity
print(paste0("SVM Train TP Sensitivity: ",
             round(sensitivity(tp_train_preds, y_tp_train),3)))
print(paste0("SVM Train TP Specificity: ",
             round(specificity(tp_train_preds, y_tp_train),3)))

print(paste0("SVM Test TP Sensitivity: ",
             round(sensitivity(tp_test_preds, y_tp_test),3)))
print(paste0("SVM Test TP Specificity: ",
             round(specificity(tp_test_preds, y_tp_test),3)))

############################################################

