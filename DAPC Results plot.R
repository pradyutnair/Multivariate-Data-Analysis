dirName <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirName)
############################################################
library(pacman)
pacman::p_load(dplyr,MASS,ggplot2,openxlsx, factoextra, tidyverse, plotly,ggpubr,reshape2,Hmisc,
               cowplot, PerformanceAnalytics,signal, caTools, randomForest,e1071)

############################################################
df <- read.csv('./DAPC Results.csv', stringsAsFactors=TRUE, strip.white = TRUE)
colnames(df[1]) <- "Dataset"
colnames(df)

p <- ggplot(df, aes(df[,1], Accuracy, fill = as.factor(df[,2]))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") + guides(fill=guide_legend(title="Accuracy Type"))  +xlab("Scan Type") +
  ggtitle("DAPC Results")

ggsave(p, width=12,height = 9, filename = "DAPC Results.png",dpi=120)

