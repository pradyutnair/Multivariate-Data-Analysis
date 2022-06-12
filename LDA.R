library(pacman)
pacman::p_load(dplyr,MASS,ggplot2,openxlsx, tidyverse, plotly,ggpubr,reshape2,Hmisc,
               cowplot, PerformanceAnalytics,signal)
set.seed(123)
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

# Create list of wavelengths from column names
wavelengths <- substring(colnames(df[5:length(df)]),2,8)
wavelengths <- as.numeric(wavelengths)

###############################################
# Load preprocessed data
# This dataset contains NIR that has baseline removed and Savitsky-Golay applied
preprocessed.df <- read.csv('./NIR_Preprocessed.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
preprocessed.df <- preprocessed.df[,-1]

###############################################
# Create train and test sets for baseline data and smoothed data
base_sample <- sample.split(df$Scan_type, SplitRatio = 0.7)
base_train  <- subset(df, base_sample == TRUE)
base_test   <- subset(df, base_sample == FALSE)

X_train_base <- as.matrix(base_train[,5:length(base_train)])
y_train_base <- base_train$Scan_type
X_test_base  <- as.matrix(base_test[,5:length(base_test)])
y_test_base  <- base_test$Scan_type

smooth_sample <- sample.split(mydataBSL$Scan_type, SplitRatio = 0.7)
smooth_train  <- subset(mydataBSL, smooth_sample == TRUE)
smooth_test   <- subset(mydataBSL, smooth_sample == FALSE)

X_train_smooth <- as.matrix(smooth_train[,5:length(smooth_train)])
y_train_smooth <- smooth_train$Scan_type
X_test_smooth  <- as.matrix(smooth_test[,5:length(smooth_test)])
y_test_smooth  <- smooth_test$Scan_type

lda.base <- lda(y_train_base~X_train_base)
lda.smooth <- lda(y_train_smooth~X_train_smooth)

lda.base$prior
lda.smooth$prior
attributes(lda.base)

train_preds_base <- predict(lda.base, data.frame(X_train_base))
test_preds_base  <- predict(lda.base, data.frame(X_test_base))
train_preds_smooth <- predict(lda.smooth, data.frame(X_train_smooth))
test_preds_smooth  <- predict(lda.smooth, data.frame(X_test_smooth))

print(paste0("Accuracy on train set for raw data: ",
             round(mean(train_preds_base$class==y_train_base),3)))
print(paste0("Accuracy on test set for raw data: ",
             round(mean(test_preds_base$class==y_test_base),3)))
print(paste0("Accuracy on train set for Savitsky-Golay data: ",
             round(mean(train_preds_smooth$class==y_train_smooth),3)))
print(paste0("Accuracy on test set for Savitsky-Golay data: ",
             round(mean(test_preds_smooth$class==y_test_smooth),3)))
###############################################
# Create baseline model
linear.da <- function(df,title){
  # use 70% of dataset as training set and 30% as test set
  sample <- sample.split(df$Scan_type, SplitRatio = 0.7)
  train  <- subset(df, sample == TRUE)
  test   <- subset(df, sample == FALSE)


  X_train <- as.matrix(train[,5:length(train)])
  y_train <- train$Scan_type

  X_test  <- as.matrix(test[,5:length(test)])
  y_test  <- test$Scan_type

  lda <- lda(y_train~X_train)
  predictions <- predict(lda,data.frame(X_train))
  pred_test <- predict(lda,newdata=data.frame(X_test))

  print(paste0("Accuracy on train set for ",title, " fillets: ",
               round(mean(predictions$class==y_train),3)))
  print(paste0("Accuracy on test set for ",title, " fillets: ",
               round(mean(pred_test$class==y_test),3)))

  lda1 <- data.frame(predictions$x[,1])
  lda2 <- data.frame(predictions$x[,2])

  ldahist(data = lda1, g = train$Scan_type,col="blue",type="both"); title(paste0("Group Distributions of LD1-",title))

  ldahist(data = lda2, g = train$Scan_type, col="orange",type="both");title(paste0("Group Distributions of LD2-",title))


  ll <- data.frame(lda1,lda2)
  plot(ll[,1],ll[,2],col=train$Scan_type,pch=19,cex=0.6, main=paste0("Linear Discriminant Analysis of ",title),
       xlab="LD1",ylab="LD2")
  legend(x="topleft", legend=c("OM", "TB","TP"),
         col=unique(train$Scan_type), lty=1, cex=0.9)
}

linear.da(df,"Raw Data")
linear.da(preprocessed.df,"Preprocessed Data")

###############################################
print.posteriors <- function(train.set, y.train){
  posteriors <- train.set$posterior # N x K matrix
  # MAP classification for sample 1:
  pred.class <- names(which.max(posteriors[1,])) # <=> lda.prediction.train$class[1]
  print(paste0("Posterior of predicted class '", pred.class,
               "' is: ", round(posteriors[1,pred.class], 3)))
  ## [1] "Posterior of predicted class 'sh' is: 1"
  # what are the mean posteriors for individual groups?
  res <- do.call(rbind, (lapply(levels(y.train), function(x) apply(posteriors[y.train == x, ], 2, mean))))
  rownames(res) <- levels(y.train)
  print(round(res, 3))
}
print.posteriors(train_preds_base, y_train_base)
print.posteriors(train_preds_smooth, y_train_smooth)



