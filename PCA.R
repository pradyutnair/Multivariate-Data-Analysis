library(pacman)
pacman::p_load(dplyr,MASS,ggplot2,openxlsx, factoextra, tidyverse, plotly,ggpubr,reshape2,Hmisc,
               cowplot, PerformanceAnalytics,signal, caTools, randomForest,e1071)

# Import the data and create dataframe
setwd('./')
df <- data.frame(read.csv('../GitHub/Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';',
                          stringsAsFactors = TRUE))

df <- read.csv('../Assignment 1/NIR_SNV_MSC.csv', sep=',', stringsAsFactors=TRUE, strip.white = TRUE)
df <- df[,-1]

# Remove row 455 as it is most likely a measurement error
#df <- df[-455,]
fresh <- df[df$Freshness == "FR",]
thawed <- df[df$Freshness == "TH",]


# PCA of entire df
df.pca <- prcomp(df[,5:length(df)], center=TRUE, scale = TRUE)
summary(df.pca)
fviz_eig(df.pca, addlabels = TRUE, ylim = c(0, 80))
ind <- get_pca_ind(df.pca)

groups <- as.factor(df$Freshness)

# Plot individual variables in the principal component axes
ind.p <- fviz_pca_ind(df.pca, geom = "point", col.ind = df$Scan_type)
ggpubr::ggpar(ind.p,
              title = "PCA-Individuals",
              subtitle = "Chicken NIR",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Scan_type", legend.position = "top",
              ggtheme = theme_minimal(), palette="npg"
)

# Plot the cos2 of each PCA variable (wavelengths)
fviz_pca_var(df.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Contributions of wavelengths to PC1
fviz_contrib(df.pca, choice = "var", axes = 1, top = 50,fill="blue",col="black")
# Contributions of wavelengths to PC2
fviz_contrib(df.pca, choice = "var", axes = 2, top = 50,fill="orange",col="black")



########################################################################
# Create train and test sets
sample <- sample.split(df$Scan_type, SplitRatio = 0.7)
train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)

X_train_base <- as.matrix(train[,5:length(train)])
X_test_base <- as.matrix(test[,5:length(test)])
y_train <- as.factor(train$Freshness)
y_test <- as.factor(test$Freshness)

# Remove first 4 columns from train and test sets
train <- train[,5:length(train)]
test <- test[,5:length(test)]

# Convert to pca data
train.pca <- prcomp(train, center = TRUE, scale. = TRUE)
test.pca <- prcomp(test, center = TRUE, scale. = TRUE)

# Use the first two components as training data
train.pca <- train.pca$x[,1:3]
test.pca <- test.pca$x[,1:3]

########################################################################
# First test SVM with baseline and PCA data
svm.base <- svm(formula = y_train ~ .,
                data = data.frame(X_train_base),
                type = 'C-classification',
                kernel = 'linear')

svm.pca <- svm(formula = y_train ~ .,
               data = data.frame(train.pca),
               type = 'C-classification',
               kernel = 'linear')

# Create prediction variables for train and test data
pred_base_train <- predict(svm.base,newdata=data.frame(X_train_base))
pred_base_test <- predict(svm.base,newdata=data.frame(X_test_base))


pred_pca_train <- predict(svm.pca)
pred_pca_test <- predict(svm.pca,newdata=data.frame(test.pca))


# Return predictions
print(paste0("SVM accuracy on base train set: ",
             round(mean(pred_base_train==y_train),3)))
print(paste0("SVM Accuracy on base test set: ",
             round(mean(pred_base_test==y_test),3)))

print(paste0("SVM accuracy on PCA train set: ",
             round(mean(pred_pca_train==y_train),3)))
print(paste0("SVM Accuracy on PCA test set: ",
             round(mean(pred_pca_test==y_test),3)))

########################################################################
# Instatiate the RF model on pca transformed data. Set number of trees = 2
rf_base <- randomForest(y_train~.,data=X_train_base, ntree = 2,importance = TRUE)
rf_pca <- randomForest(y_train~., data=train.pca, nTree=2,importance = TRUE)

print(rf_base)
print(rf_pca)

# Plot feature importances
varImpPlot(rf_base,main="Feature Importances of RF Classifier on base data")
varImpPlot(rf_pca,main="Feature Importances of RF Classifier on PCA transformed data")

# Create prediction variables for train and test data
pred_base_train <- predict(rf_base,newdata=X_train_base,type='class')
pred_base_test <- predict(rf_base,newdata=X_test_base,type='class')

pred_pca_train <- predict(rf_pca,type='class')
pred_pca_test <- predict(rf_pca,newdata=test.pca,type='class')

# Return predictions
print(paste0("RF accuracy on base train set: ",
             round(mean(pred_base_train==y_train),3)))
print(paste0("RF Accuracy on base test set: ",
             round(mean(pred_base_test==y_test),3)))

print(paste0("RF accuracy on PCA train set: ",
             round(mean(pred_pca_train==y_train),3)))
print(paste0("RF Accuracy on PCA test set: ",
             round(mean(pred_pca_test==y_test),3)))

########################################################################
