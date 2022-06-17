library(pacman)
pacman::p_load(randomForest,mlbench,tidyverse,caret,e1071,splitTools,ranger,mdatools,superml)

############################################################
# Import data
df <- read.csv('./NIR_Preprocessed.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
df <- df[,-1]

############################################################
# Split data into train and test sets
base_sample <- partition(df$Freshness, p = c(train = 0.7, test = 0.3))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]

############################################################
# Train set
X_om_train <- base_train[base_train$Scan_type == "OM",]
X_tb_train <- base_train[base_train$Scan_type == "TB",]
X_tp_train <- base_train[base_train$Scan_type == "TP",]

y_om_train <- base_train[base_train$Scan_type == "OM",]$Freshness
y_tb_train <- base_train[base_train$Scan_type == "TB",]$Freshness
y_tp_train <- base_train[base_train$Scan_type == "TP",]$Freshness

# Test set
X_om_test <- base_test[base_test$Scan_type == "OM",]
X_tb_test <- base_test[base_test$Scan_type == "TB",]
X_tp_test <- base_test[base_test$Scan_type == "TP",]

y_om_test <- base_test[base_test$Scan_type == "OM",]$Freshness
y_tb_test <- base_test[base_test$Scan_type == "TB",]$Freshness
y_tp_test <- base_test[base_test$Scan_type == "TP",]$Freshness

############################################################
# RF on OM
rf <- RFTrainer$new()
gst <-GridSearchCV$new(trainer = rf,
                       parameters = list(n_estimators = c(10,15,20,50),
                                         max_depth = c(2,3,4)),
                       n_folds = 10,
                       scoring = c('accuracy','auc'))
gst$fit(X_om_train,"Freshness")
gst$best_iteration()

rf.om <- randomForest(y_om_train~.,
                      X_om_train,
                      n_estimators =gst$best_iteration()$n_estimators,
                      max_depth = gst$best_iteration()$max_depth,
                      n_trees = 1)

om_train_preds <- predict(rf.om,newdata=data.frame(X_om_train))
om_test_preds <- predict(rf.om,newdata=data.frame(X_om_test))

# Accuracies
print(paste0("RF accuracy on OM train set: ",
             round(mean(om_train_preds==y_om_train),3)))
print(paste0("RF accuracy on OM test set: ",
             round(mean(om_test_preds==y_om_test),3)))
# Sensitivity and specificity
print(paste0("RF Train OM Sensitivity: ",
             round(sensitivity(om_train_preds, y_om_train),3)))
print(paste0("RF Train OM Specificity: ",
             round(specificity(om_train_preds, y_om_train),3)))

print(paste0("RF Test OM Sensitivity: ",
             round(sensitivity(om_test_preds, y_om_test),3)))
print(paste0("RF Test OM Specificity: ",
             round(specificity(om_test_preds, y_om_test),3)))
############################################################
gst$fit(X_tb_train,"Freshness")
gst$best_iteration()

rf.tb <- randomForest(y_tb_train~.,
                      X_tb_train,
                      n_estimators =gst$best_iteration()$n_estimators,
                      max_depth = gst$best_iteration()$max_depth,
                      n_trees = 1)

tb_train_preds <- predict(rf.tb,newdata=data.frame(X_tb_train))
tb_test_preds <- predict(rf.tb,newdata=data.frame(X_tb_test))

print(paste0("RF accuracy on TB train set: ",
             round(mean(tb_train_preds==y_tb_train),3)))
print(paste0("RF accuracy on TB test set: ",
                round(mean(tb_test_preds==y_tb_test),3)))

# Sensitivity and specificity
print(paste0("RF Train TB Sensitivity: ",
             round(sensitivity(tb_train_preds, y_tb_train),3)))
print(paste0("RF Train TB Specificity: ",
             round(specificity(tb_train_preds, y_tb_train),3)))

print(paste0("RF Test TB Sensitivity: ",
             round(sensitivity(tb_test_preds, y_tb_test),3)))
print(paste0("RF Test TB Specificity: ",
             round(specificity(tb_test_preds, y_tb_test),3)))
############################################################
gst$fit(X_tp_train,"Freshness")
gst$best_iteration()

rf.tp <- randomForest(y_tp_train~.,
                      X_tp_train,
                      n_estimators =gst$best_iteration()$n_estimators,
                      max_depth = gst$best_iteration()$max_depth,
                      n_trees = 1)

tp_train_preds <- predict(rf.tp,newdata=data.frame(X_tp_train))
tp_test_preds <- predict(rf.tp,newdata=data.frame(X_tp_test))

print(paste0("RF accuracy on TP train set: ",
             round(mean(tp_train_preds==y_tp_train),3)))
print(paste0("RF accuracy on TP test set: ",
                round(mean(tp_test_preds==y_tp_test),3)))
# Sensitivity and specificity
print(paste0("RF Train TP Sensitivity: ",
             round(sensitivity(tp_train_preds, y_tp_train),3)))
print(paste0("RF Train TP Specificity: ",
             round(specificity(tp_train_preds, y_tp_train),3)))

print(paste0("RF Test TP Sensitivity: ",
             round(sensitivity(tp_test_preds, y_tp_test),3)))
print(paste0("RF Test TP Specificity: ",
             round(specificity(tp_test_preds, y_tp_test),3)))