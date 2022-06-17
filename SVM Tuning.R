library(pacman)
pacman::p_load(caTools,tidyverse,caret,e1071,splitTools,ranger)

############################################################
# Import data
df <- read.csv('./NIR_SNV_MSC.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
df <- df[,-1]

############################################################
# Split data into train and test sets
base_sample <- partition(df$Freshness, p = c(train = 0.7, test = 0.3))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]

############################################################
# Train set
X_om_train <- base_train[base_train$Scan_type == "OM",][,5:length(base_train)]
y_om_train <- base_train[base_train$Scan_type == "OM",]$Freshness

X_tb_train <- base_train[base_train$Scan_type == "TB",][,5:length(base_train)]
y_tb_train <- base_train[base_train$Scan_type == "TB",]$Freshness

X_tp_train <- base_train[base_train$Scan_type == "TP",][,5:length(base_train)]
y_tp_train <- base_train[base_train$Scan_type == "TP",]$Freshness

# Test set
X_om_test <- base_test[base_test$Scan_type == "OM",][,5:length(base_test)]
X_tb_test <- base_test[base_test$Scan_type == "TB",][,5:length(base_test)]
X_tp_test <- base_test[base_test$Scan_type == "TP",][,5:length(base_test)]

y_om_test <- base_test[base_test$Scan_type == "OM",]$Freshness
y_tb_test <- base_test[base_test$Scan_type == "TB",]$Freshness
y_tp_test <- base_test[base_test$Scan_type == "TP",]$Freshness
############################################################
# SVM on OM
m <- tune.svm(x = X_om_train,y=y_om_train,
              tunecontrol=tune.control(cross=10),cost=1:3,gamma=seq(0,1,by=0.1))

attributes(m)
m$performances
m$best.model
m$best.parameters

svm.om<- svm(formula = y_om_train ~ .,
             data = data.frame(X_om_train),
             gamma=m$best.parameters$gamma,
             cost=m$best.parameters$cost,
             type = 'C-classification',
             kernel = 'radial')

om_train_preds <- predict(svm.om,newdata=data.frame(X_om_train))
om_test_preds <- predict(svm.om,newdata=data.frame(X_om_test))
print(paste0("SVM accuracy on OM train set: ",
             round(mean(om_train_preds==y_om_train),3)))
print(paste0("SVM accuracy on OM test set: ",
             round(mean(om_test_preds==y_om_test),3)))
############################################################
# SVM on TB
m <- tune.svm(x = X_tb_train,y=y_tb_train,
              tunecontrol=tune.control(cross=10),cost=1:3,gamma=seq(0,1,by=0.1))

m$best.model
m$best.parameters

svm.tb <- svm(formula = y_tb_train ~ .,
             data = data.frame(X_tb_train),
              gamma=m$best.parameters$gamma,
              cost=m$best.parameters$cost,
             type = 'C-classification',
             kernel = 'radial')

tb_train_preds <- predict(svm.tb,newdata=data.frame(X_tb_train))
tb_test_preds <- predict(svm.tb,newdata=data.frame(X_tb_test))
print(paste0("SVM accuracy on TB train set: ",
             round(mean(tb_train_preds==y_tb_train),3)))
print(paste0("SVM accuracy on TB test set: ",
             round(mean(tb_test_preds==y_tb_test),3)))
############################################################
# SVM on TP
m <- tune.svm(x = X_tp_train,y=y_tp_train,
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
print(paste0("SVM accuracy on TP train set: ",
             round(mean(tp_train_preds==y_tp_train),3)))
print(paste0("SVM accuracy on TP test set: ",
                round(mean(tp_test_preds==y_tp_test),3)))
############################################################
