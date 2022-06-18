library(adegenet)
############################################################################
df <- read.csv('../Assignment 1/NIR_SNV_MSC.csv', sep=',', stringsAsFactors=TRUE, strip.white = TRUE)
df <- df[,-1]
############################################################################
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
############################################################################
# tune PCA-LDA / DAPC hyperparameter (n.pca)
d.dapc <- xvalDapc(x = X_om_train, grp =y_om_train,
                   result = "groupMean",
                   training.set = 0.7, n.pca.max = 3, n.rep = 100)
(n.pca <- as.numeric(d.dapc$`Number of PCs Achieving Lowest MSE`))