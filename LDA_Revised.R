library(pacman)
pacman::p_load(tidyverse, PredPsych,splitTools, ranger)

#############################################################
# Import data
setwd('./')
df <- data.frame(read.csv('../GitHub/Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';',
                          stringsAsFactors = TRUE))
# Remove row 455 as it is most likely a measurement error
df <- df[-455,]

#############################################################
# Split data by scan type
om_data <- df[df$Scan_type == "OM",]
tb_data <- df[df$Scan_type == "TB",]
tp_data <- df[df$Scan_type == "TP",]
lda.om <- LinearDA(Data = om_data , classCol = "Freshness", selectedCols = c(4:length(df)),cvType="folds",nTrainFolds=10,extendedResults=TRUE,CV=TRUE)
print(lda.om)
lda.tb <- LinearDA(Data = tb_data , classCol = "Freshness", selectedCols = c(4:length(df)),cvType="folds",nTrainFolds=10,extendedResults=TRUE,CV=TRUE)
print(lda.tb)
lda.tp <- LinearDA(Data = tp_data , classCol = "Freshness", selectedCols = c(4:length(df)),cvType="folds",nTrainFolds=10,extendedResults=TRUE,CV=TRUE)
print(lda.tp)

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


#############################################################