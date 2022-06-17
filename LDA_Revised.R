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

lda.om <- LinearDA(Data = om_data , classCol = "Freshness", selectedCols = 4:length(df),
                   cvFraction = 0.7,cvType="folds",nTrainFolds=10,
                   extendedResults=TRUE,CV=TRUE)

lda.om$ConfusionMatrixResults

############################################################
lda.tb <- LinearDA(Data = tb_data , classCol = "Freshness", selectedCols = 4:length(df),
                   cvFraction = 0.7,cvType="folds",nTrainFolds=10,
                   extendedResults=TRUE,CV=TRUE)

lda.tb$ConfusionMatrixResults

############################################################

lda.tp <- LinearDA(Data = tp_data , classCol = "Freshness", selectedCols = 4:length(df),
                   cvFraction = 0.7,cvType="folds",nTrainFolds=10,
                   extendedResults=TRUE,CV=TRUE)

lda.tp$ConfusionMatrixResults

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


#############################################################
print(paste0("LDA test accuracy on OM: ",round(lda.om$accTest,3)))
print(paste0("LDA test accuracy on TB: ",round(lda.tb$accTest,3)))
print(paste0("LDA test accuracy on TP: ",round(lda.tp$accTest,3)))

#############################################################

