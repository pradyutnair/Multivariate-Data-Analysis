dirName <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirName)
############################################################
library(pacman)
pacman::p_load(caTools,tidyverse,caret,e1071,splitTools,ranger,mdatools)
set.seed(123)
############################################################
# Import data
df <- read.csv('./NIR_Preprocessed.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
df <- df[,-1]

############################################################
# Split data into train and test sets
base_sample <- partition(df$Freshness, p = c(train = 0.7, valid=0.1, test = 0.2))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]

############################################################
# Train set
X_om_train <- base_train[base_train$Scan_type == "OM",][,5:length(base_train)]
X_tb_train <- base_train[base_train$Scan_type == "TB",][,5:length(base_train)]
X_tp_train <- base_train[base_train$Scan_type == "TP",][,5:length(base_train)]

y_om_train <- base_train[base_train$Scan_type == "OM",]$Freshness
y_tb_train <- base_train[base_train$Scan_type == "TB",]$Freshness
y_tp_train <- base_train[base_train$Scan_type == "TP",]$Freshness

# Validation set
X_om_valid <- base_valid[base_valid$Scan_type == "OM",][,5:length(base_valid)]
X_tb_valid <- base_valid[base_valid$Scan_type == "TB",][,5:length(base_valid)]
X_tp_valid <- base_valid[base_valid$Scan_type == "TP",][,5:length(base_valid)]

y_om_valid <- base_valid[base_valid$Scan_type == "OM",]$Freshness
y_tb_valid <- base_valid[base_valid$Scan_type == "TB",]$Freshness
y_tp_valid <- base_valid[base_valid$Scan_type == "TP",]$Freshness

# Test set
X_om_test <- base_test[base_test$Scan_type == "OM",][,5:length(base_test)]
X_tb_test <- base_test[base_test$Scan_type == "TB",][,5:length(base_test)]
X_tp_test <- base_test[base_test$Scan_type == "TP",][,5:length(base_test)]

y_om_test <- base_test[base_test$Scan_type == "OM",]$Freshness
y_tb_test <- base_test[base_test$Scan_type == "TB",]$Freshness
y_tp_test <- base_test[base_test$Scan_type == "TP",]$Freshness

############################################################
plsda.base.train <- plsda(base_train[,5:length(base_train)],base_train$Freshness,cv=10)
summary(plsda.base)

plsda.base.valid <- plsda(base_valid[,5:length(base_valid)],base_valid$Freshness,cv=10)
summary(plsda.base)

plsda.base.test <- plsda(base_test[,5:length(base_test)],base_test$Freshness,cv=10)
summary(plsda.base)
############################################################
plsda.om.train <- plsda(X_om_train, y_om_train,cv=10)
summary(plsda.om.train)

plsda.om.valid <- plsda(X_om_valid, y_om_valid,cv=10)
summary(plsda.om.valid )

plsda.om.test <- plsda(X_om_test, y_om_test,cv=10)
predict(plsda.om.train, X_om_test)
summary(plsda.om.test)
############################################################
plsda.tb.train <- plsda(X_tb_train, y_tb_train,cv=10)
summary(plsda.tb.train)

plsda.tb.valid  <- plsda(X_tb_valid, y_tb_valid,cv=10)
summary(plsda.tb.valid )

plsda.tb.test <- plsda(X_tb_test, y_tb_test,cv=10)
summary(plsda.tb)
############################################################

plsda.tp.train <- plsda(X_tp_train, y_tp_train,cv=10)
summary(plsda.tp.train)

plsda.tp.valid  <- plsda(X_tp_valid, y_tp_valid,cv=10)
summary(plsda.tp.valid )

plsda.tp.test <- plsda(X_tp_test, y_tp_test,cv=10)
summary(plsda.tp.test)

############################################################


