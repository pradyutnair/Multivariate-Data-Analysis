library(pacman)
pacman::p_load(dplyr,MASS,ggplot2,openxlsx, tidyverse, plotly,ggpubr,reshape2,Hmisc,
               cowplot, PerformanceAnalytics,signal)

# Import the data and create dataframe
setwd('./')
df <- data.frame(read.csv('../GitHub/Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';',
                          stringsAsFactors = TRUE))
# Remove row 455 as it is most likely a measurement error
df <- df[-455,]
fresh <- df[df$Freshness == "FR",]
thawed <- df[df$Freshness == "TH",]

# PCA of entire df
df.pca <- prcomp(df[,5:length(df)], center=TRUE, scale = TRUE)
summary(df.pca)
fviz_eig(df.pca, addlabels = TRUE, ylim = c(0, 80))
ind <- get_pca_ind(df.pca)

groups <- as.factor(df$Scan_type)

# Plot individual variables in the principal component axes
ind.p <- fviz_pca_ind(df.pca, geom = "point", col.ind = df$Scan_type)
ggpubr::ggpar(ind.p,
              title = "PCA-Individuals",
              subtitle = "Chicken NIR",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Scan_Type", legend.position = "top",
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

# Use PCA transformed data on a Random Forest Classifier

# Use a 70-30 split and create train and test sets. Set  target vars as factors
sample <- sample.split(df$Scan_type, SplitRatio = 0.7)
train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)


X_train_base <- as.matrix(train[,5:length(train)])
X_test_base <- as.matrix(test[,5:length(test)])
y_train <- as.factor(train$Scan_type)
y_test <- as.factor(test$Scan_type)

# Remove first 4 columns from train and test sets
train <- train[,5:length(train)]
test <- test[,5:length(test)]

# Convert to pca data
train.pca <- prcomp(train, center = TRUE, scale. = TRUE)
test.pca <- prcomp(test, center = TRUE, scale. = TRUE)

# Use the first two components as training data
train.pca <- train.pca$x[,1:2]
test.pca <- test.pca$x[,1:2]

# Instatiate the model on pca transformed data. Set number of trees = 2
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

