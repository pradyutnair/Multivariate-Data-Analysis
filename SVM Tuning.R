library(pacman)
pacman::p_load(caTools,tidyverse,caret,e1071,splitTools,ranger)

############################################################
# Import data
df <- read.csv('./NIR_SNV_MSC.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
df <- df[,-1]

############################################################
# Split data into train test and validation sets
base_sample <- partition(df$Freshness, p = c(train = 0.6, valid = 0.2, test = 0.2))
base_train <- df[base_sample$train, ]
base_valid <- df[base_sample$valid, ]
base_test <- df[base_sample$test, ]
?tune.svm
############################################################
# Create baseline model
m <- tune.svm(x = train.pca,y=y_train,,
              tunecontrol=tune.control(cross=5),cost=1:3,gamma=seq(0,1,by=0.1))

attributes(m)
m$best.parameters
m$best.model
m$performances
############################################################
# Conduct PCA on data
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
              ggtheme = theme_minimal(), palette="npg")

############################################################
# Conduct PCA on train set
df.pca.train <- prcomp(base_train[,5:length(base_train)], center=TRUE, scale = TRUE)
df.pca.valid <- prcomp(base_valid[,5:length(base_valid)], center=TRUE, scale = TRUE)
fviz_eig(df.pca.valid, addlabels = TRUE, ylim = c(0, 80))