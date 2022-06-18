library(adegenet)

df <- read.csv('../Assignment 1/NIR_SNV_MSC.csv', sep=',', stringsAsFactors=TRUE, strip.white = TRUE)
df <- df[,-1]

# tune PCA-LDA / DAPC hyperparameter (n.pca)
d.dapc <- xvalDapc(x = iris.s, grp = iris[,5],
                   result = "groupMean",
                   training.set = 0.7, n.pca.max = 3, n.rep = 100)
(n.pca <- as.numeric(d.dapc$`Number of PCs Achieving Lowest MSE`))