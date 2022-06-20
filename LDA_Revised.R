dirName <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirName)
############################################################
library(pacman)
pacman::p_load(tidyverse, PredPsych,splitTools, ranger)

############################################################
# Import data
df <- read.csv('./NIR_Preprocessed.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
df <- df[,-1]

############################################################
# Split data by scan type
om_data <- df[df$Scan_type == "OM",]
tb_data <- df[df$Scan_type == "TB",]
tp_data <- df[df$Scan_type == "TP",]
tptp_data <- df[(df$Scan_type == "TB") | (df$Scan_type == "TP"),]

lda.om <- LinearDA(Data = om_data , classCol = "Freshness", selectedCols = 4:length(om_data),
                   cvFraction = 0.7,cvType="folds",nTrainFolds=10,
                   extendedResults=TRUE,CV=TRUE)

lda.om$ConfusionMatrixResults

############################################################
lda.tb <- LinearDA(Data = tb_data , classCol = "Freshness", selectedCols = 4:length(tb_data),
                   cvFraction = 0.7,cvType="folds",nTrainFolds=10,
                   extendedResults=TRUE,CV=TRUE)

lda.tb$ConfusionMatrixResults

############################################################

lda.tp <- LinearDA(Data = tp_data , classCol = "Freshness", selectedCols = 4:length(tp_data),
                   cvFraction = 0.7,cvType="folds",nTrainFolds=10,
                   extendedResults=TRUE,CV=TRUE)

lda.tp$ConfusionMatrixResults
############################################################
lda.tptb <- LinearDA(Data = tptp_data , classCol = "Freshness", selectedCols = 4:length(tptp_data),
                   cvFraction = 0.7,cvType="folds",nTrainFolds=10,
                   extendedResults=TRUE,CV=TRUE)

lda.tptb$ConfusionMatrixResults
############################################################
data.om <- data.frame(cbind(seq(1:10),lda.om$accTestRun))
ggplot(data=data.om,aes(x=X1, y=X2)) +
  geom_line(color="red")+ geom_point() + ggtitle("Accuracies across test folds for OM") +
  xlab("Fold") + ylab("Accuracy")

data.tb <- data.frame(cbind(seq(1:10),lda.tb$accTestRun))
ggplot(data=data.tb,aes(x=X1, y=X2)) +
  geom_line(color="green")+ geom_point() + ggtitle("Accuracies across test folds for TB") +
  xlab("Fold") + ylab("Accuracy")
data.tp <- data.frame(cbind(seq(1:10),lda.tp$accTestRun))
ggplot(data=data.tp,aes(x=X1, y=X2)) +
    geom_line(color="blue")+ geom_point() + ggtitle("Accuracies across test folds for TP") +
    xlab("Fold") + ylab("Accuracy")
data.tptb <- data.frame(cbind(seq(1:10),lda.tptb$accTestRun))
ggplot(data=data.tptb,aes(x=X1, y=X2)) +
    geom_line(color="orange")+ geom_point() + ggtitle("Accuracies across test folds for TP/TB") +
    xlab("Fold") + ylab("Accuracy")

#############################################################
print(paste0("LDA train accuracy on OM: ",round(lda.om$accTest,3)))
print(paste0("LDA train accuracy on TB: ",round(lda.tb$accTest,3)))
print(paste0("LDA train accuracy on TP: ",round(lda.tp$accTest,3)))
print(paste0("LDA train accuracy on TP/TB: ",round(lda.tptb$accTest,3)))
#############################################################
############################################################
# Split data into train and test sets
base_sample <- partition(df$Freshness, p = c(train = 0.6, valid=0.2, test = 0.2))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]

############################################################
# Train set
X_om_train <- base_train[base_train$Scan_type == "OM",][,5:length(base_train)]
X_tb_train <- base_train[base_train$Scan_type == "TB",][,5:length(base_train)]
X_tp_train <- base_train[base_train$Scan_type == "TP",][,5:length(base_train)]
X_tptb_train <- base_train[base_train$Scan_type == "TP" | base_train$Scan_type == "TB",][,5:length(base_train)]

y_om_train <- base_train[base_train$Scan_type == "OM",]$Freshness
y_tb_train <- base_train[base_train$Scan_type == "TB",]$Freshness
y_tp_train <- base_train[base_train$Scan_type == "TP",]$Freshness
y_tptb_train <- base_train[base_train$Scan_type == "TP" | base_train$Scan_type == "TB",]$Freshness


# Validation set
X_om_valid <- base_valid[base_valid$Scan_type == "OM",][,5:length(base_valid)]
X_tb_valid <- base_valid[base_valid$Scan_type == "TB",][,5:length(base_valid)]
X_tp_valid <- base_valid[base_valid$Scan_type == "TP",][,5:length(base_valid)]
X_tptb_valid <- base_valid[base_valid$Scan_type == "TP" | base_valid$Scan_type == "TB",][,5:length(base_valid)]


y_om_valid <- base_valid[base_valid$Scan_type == "OM",]$Freshness
y_tb_valid <- base_valid[base_valid$Scan_type == "TB",]$Freshness
y_tp_valid <- base_valid[base_valid$Scan_type == "TP",]$Freshness
y_tptb_valid <- base_valid[base_valid$Scan_type == "TP" | base_valid$Scan_type == "TB",]$Freshness


# Test set
X_om_test <- base_test[base_test$Scan_type == "OM",][,5:length(base_test)]
X_tb_test <- base_test[base_test$Scan_type == "TB",][,5:length(base_test)]
X_tp_test <- base_test[base_test$Scan_type == "TP",][,5:length(base_test)]
X_tptb_test <- base_test[base_test$Scan_type == "TP" | base_test$Scan_type == "TB",][,5:length(base_test)]


y_om_test <- base_test[base_test$Scan_type == "OM",]$Freshness
y_tb_test <- base_test[base_test$Scan_type == "TB",]$Freshness
y_tp_test <- base_test[base_test$Scan_type == "TP",]$Freshness
y_tptb_test <- base_test[base_test$Scan_type == "TP" | base_test$Scan_type == "TB",]$Freshness

############################################################
library(MASS)
lda.om <- lda(y_om_train~as.matrix(X_om_train))
attributes(lda.om)

print(paste0("LDA test accuracy on OM: ",
      round(mean(y_om_test==predict(lda.om, X_om_test)$class),3)))
print(paste0("LDA valid accuracy on OM: ",
      round(mean(y_om_valid==predict(lda.om, X_om_valid)$class),3)))

############################################################

lda.tb <- lda(y_tb_train~as.matrix(X_tb_train))
print(paste0("LDA test accuracy on TB: ",
        round(mean(y_tb_test==predict(lda.tb, X_tb_test)$class),3)))
print(paste0("LDA valid accuracy on TB: ",
        round(mean(y_tb_valid==predict(lda.tb, X_tb_valid)$class),3)))

############################################################

lda.tp <- lda(y_tp_train~as.matrix(X_tp_train))
print(paste0("LDA test accuracy on TP: ",
        round(mean(y_tp_test==predict(lda.tp, X_tp_test)$class),3)))
print(paste0("LDA valid accuracy on TP: ",
        round(mean(y_tp_valid==predict(lda.tp, X_tp_valid)$class),3)))

############################################################
lda.tptb <- lda(y_tptb_train~as.matrix(X_tptb_train))
print(paste0("LDA test accuracy on TP/TB: ",
        round(mean(y_tptb_test==predict(lda.tptb, X_tptb_test)$class),3)))
print(paste0("LDA valid accuracy on TP/TB: ",
        round(mean(y_tptb_valid==predict(lda.tptb, X_tptb_valid)$class),3)))
############################################################