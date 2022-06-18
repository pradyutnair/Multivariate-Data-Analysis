library(adegenet)
library(splitTools)
library(ranger)
############################################################################
df <- read.csv('./NIR_SNV_MSC.csv', sep=',', stringsAsFactors=TRUE, strip.white = TRUE)
df <- df[,-1]
############################################################################

# Split data into train and test sets
base_sample <- partition(df$Freshness, p = c(train = 0.6, valid=0.2, test = 0.2))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]

X_train <- base_train[,5:length(base_train)]
y_train <- base_train$Freshness
X_valid <- base_valid[,5:length(base_valid)]
y_valid <- base_valid$Freshness
X_test <- base_test[,5:length(base_test)]
y_test <- base_test$Freshness
############################################################################
# tune PCA-LDA / DAPC hyperparameter (n.pca)
d.dapc <- xvalDapc(x = X_train, grp =y_train,
                   result = "groupMean",
                   training.set = 0.7, n.pca=1:4, n.pca.max = 4, n.rep = 100)
(n.pca <- as.numeric(d.dapc$`Number of PCs Achieving Lowest MSE`))

dd.dapc <- dapc(x =  X_train, grp =y_train, n.pca = n.pca, n.da = 4,
                var.loadings = TRUE)

scatter(dd.dapc, legend = TRUE)
loadingplot(dd.dapc$var.contr, axis = 1, main = "Loadings on LD1")
