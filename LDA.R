library(pacman)
pacman::p_load(dplyr,MASS,ggplot2,openxlsx, tidyverse, plotly,ggpubr,reshape2,Hmisc,
               cowplot, PerformanceAnalytics)

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

# Linear Discriminant Analysis

# Function that creates train and test sets, conducts lda, returns train and test accuracies
# and lda plots
linear.da <- function(df,title){
  # use 70% of dataset as training set and 30% as test set
  sample <- sample.split(df$Scan_type, SplitRatio = 0.7)
  train  <- subset(df, sample == TRUE)
  test   <- subset(df, sample == FALSE)

  preproc.parameter <- train[,5:length(df)] %>%
    preProcess(method = c("center", "scale"))

  train.transform <- preproc.parameter %>% predict(train[,5:length(train)])
  X_train <- as.matrix(train.transform)
  y_train <- train$Scan_type

  test.transform <- preproc.parameter %>% predict(test[,5:length(test)])
  X_test  <- as.matrix(test.transform)
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

  ldahist(data = lda1, g = train$Scan_type,col="blue",type="both"); title(paste0("Group Distributions of LDA1-",title))

  ldahist(data = lda2, g = train$Scan_type, col="orange",type="both");title(paste0("Group Distributions of LDA2-",title))


  ll <- data.frame(lda1,lda2)
  plot(ll[,1],ll[,2],col=train$Scan_type,pch=19,cex=0.6, main=paste0("Linear Discriminant Analysis of ",title," Fillets"),
       xlab="LDA1",ylab="LDA2")
  legend(x="topleft", legend=c("OM", "TB","TP"),
         col=unique(train$Scan_type), lty=1, cex=0.9)
}

linear.da(df,"All")
#linear.da(fresh,"Fresh")
#linear.da(thawed,"Thawed")


#check <- peakshape(om.spec,plot=FALSE)
#which(check$B3 > 2.5)
#check$B3[118]
#peakshape(om.spec,select=119)