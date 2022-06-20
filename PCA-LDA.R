library(adegenet)
library(splitTools)
library(ranger)
############################################################################
df <- read.csv('../Assignment 1/NIR_SNV_MSC.csv', sep=',', stringsAsFactors=TRUE, strip.white = TRUE)
df <- df[,-1]
############################################################################
############################################################
# Split data into train and test sets
base_sample <- partition(df$Freshness, p = c(train = 0.6, valid=0.2, test = 0.2))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]

y_train <- as.factor(base_train$Freshness)
y_valid <- as.factor(base_valid$Freshness)
y_test <- as.factor(base_test$Freshness)
##############################################################
# Train set
X_om_train <- base_train[base_train$Scan_type == "OM",][,5:length(base_train)]
X_tb_train <- base_train[base_train$Scan_type == "TB",][,5:length(base_train)]
X_tp_train <- base_train[base_train$Scan_type == "TP",][,5:length(base_train)]
X_tbtp_train <- base_train[(base_train$Scan_type == "TB")| (base_train$Scan_type=="TP"),][,5:length(base_train)]

y_om_train <- base_train[base_train$Scan_type == "OM",]$Freshness
y_tb_train <- base_train[base_train$Scan_type == "TB",]$Freshness
y_tp_train <- base_train[base_train$Scan_type == "TP",]$Freshness
y_tbtp_train <- base_train[(base_train$Scan_type == "TB")| (base_train$Scan_type=="TP"),]$Freshness

# Validation set
X_om_valid <- base_valid[base_valid$Scan_type == "OM",][,5:length(base_valid)]
X_tb_valid <- base_valid[base_valid$Scan_type == "TB",][,5:length(base_valid)]
X_tp_valid <- base_valid[base_valid$Scan_type == "TP",][,5:length(base_valid)]
X_tbtp_valid <- base_valid[(base_valid$Scan_type == "TB")| (base_valid$Scan_type=="TP"),][,5:length(base_valid)]

y_om_valid <- base_valid[base_valid$Scan_type == "OM",]$Freshness
y_tb_valid <- base_valid[base_valid$Scan_type == "TB",]$Freshness
y_tp_valid <- base_valid[base_valid$Scan_type == "TP",]$Freshness
y_tbtp_valid <- base_valid[(base_valid$Scan_type == "TB")| (base_valid$Scan_type=="TP"),]$Freshness

# Test set
X_om_test <- base_test[base_test$Scan_type == "OM",][,5:length(base_test)]
X_tb_test <- base_test[base_test$Scan_type == "TB",][,5:length(base_test)]
X_tp_test <- base_test[base_test$Scan_type == "TP",][,5:length(base_test)]
X_tbtp_test <- base_test[(base_test$Scan_type == "TB")| (base_test$Scan_type=="TP"),][,5:length(base_test)]

y_om_test <- base_test[base_test$Scan_type == "OM",]$Freshness
y_tb_test <- base_test[base_test$Scan_type == "TB",]$Freshness
y_tp_test <- base_test[base_test$Scan_type == "TP",]$Freshness
y_tbtp_test <- base_test[(base_test$Scan_type == "TB")| (base_test$Scan_type=="TP"),]$Freshness

############################################################################
base_train  <- base_train[,5:length(base_train)]
base_valid <- base_valid[,5:length(base_valid)]
base_test <- base_test[,5:length(base_test)]
############################################################################
# tune PCA-LDA / DAPC hyperparameter (n.pca)
d.dapc <- xvalDapc(x = base_valid, grp =y_valid,
                   result = "groupMean",
                   training.set = 0.7, n.pca=1:4, n.pca.max = 4, n.rep = 100)
(n.pca <- as.numeric(d.dapc$`Number of PCs Achieving Lowest MSE`))

dd.dapc <- dapc(x =  base_train, grp =y_train, n.pca = n.pca, n.da = 1,
                var.loadings = TRUE)
par(mfrow=c(2,1))
scatter(dd.dapc, legend = TRUE); title(main="Density of Dependent Variable")
loadingplot(dd.dapc$var.contr, axis = 1, main = "Loadings on LD1")
par(mfrow=c(1,1))
print(paste0("Accuracy of DAPC on base train:",
             mean(predict(dd.dapc, base_train)$assign==y_train)))
print(paste0("Accuracy of DAPC on base valid:",
             mean(predict(dd.dapc, base_valid)$assign==y_valid)))
print(paste0("Accuracy of DAPC on base test:",
             mean(predict(dd.dapc, base_test)$assign==y_test)))


############################################################################

dd.dapc <- dapc(x =  X_om_train, grp =y_om_train, n.pca = n.pca, n.da = 1,
                 var.loadings = TRUE)
par(mfrow=c(2,1))
scatter(dd.dapc, legend = TRUE); title(main="Density of Dependent Variable")
loadingplot(dd.dapc$var.contr, axis = 1, main = "Loadings on LD1")
par(mfrow=c(1,1))
print(paste0("Accuracy of DAPC on OM train:",
             mean(predict(dd.dapc, X_om_train)$assign==y_om_train)))
print(paste0("Accuracy of DAPC on OM valid:",
             mean(predict(dd.dapc, X_om_valid)$assign==y_om_valid)))
print(paste0("Accuracy of DAPC on OM test:",
             mean(predict(dd.dapc, X_om_test)$assign==y_om_test)))


############################################################################
dapc.tb <- dapc(x =  X_tb_train, grp =y_tb_train, n.pca = n.pca, n.da = 1,
                var.loadings = TRUE)

print(paste0("Accuracy of DAPC on TB train: ",
             round(mean(predict(dapc.tb, X_tb_train)$assign==y_tb_train),3)))
print(paste0("Accuracy of DAPC on TB valid: ",
             round(mean(predict(dapc.tb, X_tb_valid)$assign==y_tb_valid),3)))
print(paste0("Accuracy of DAPC on TB test: ",
             round(mean(predict(dapc.tb, X_tb_test)$assign==y_tb_test),3)))
############################################################################
dapc.tp <- dapc(x =  X_tp_train, grp =y_tp_train, n.pca = n.pca, n.da = 1,
                var.loadings = TRUE)

print(paste0("Accuracy of DAPC on TP train: ",
             round(mean(predict(dapc.tp, X_tp_train)$assign==y_tp_train),3)))
print(paste0("Accuracy of DAPC on TP valid: ",
             round(mean(predict(dapc.tp, X_tp_valid)$assign==y_tp_valid),3)))
print(paste0("Accuracy of DAPC on TP test: ",
             round( mean(predict(dapc.tp, X_tp_test)$assign==y_tp_test),3)))
############################################################################
dapc.tptb <- dapc(x =  X_tbtp_train, grp =y_tbtp_train, n.pca = n.pca, n.da = 1,
                var.loadings = TRUE)

print(paste0("Accuracy of DAPC on TBTP train: ",
             round(mean(predict(dapc.tptb, X_tbtp_train)$assign==y_tbtp_train),3)))
print(paste0("Accuracy of DAPC on TBTP valid: ",
             round(mean(predict(dapc.tptb, X_tbtp_valid)$assign==y_tbtp_valid),3)))
print(paste0("Accuracy of DAPC on TBTP test: ",
             round( mean(predict(dapc.tptb, X_tbtp_test)$assign==y_tbtp_test),3)))
############################################################################