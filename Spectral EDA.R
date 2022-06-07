library(pacman)
pacman::p_load(ggplot2, MASS, Hmisc, dplyr, squash, pavo,plyr,
               caTools,caret, klaR, factoextra,FactoMineR)
set.seed(123)

################################################
# Load data
df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';', dec=".",
                          stringsAsFactors = TRUE))

# Remove row 455 as it is most likely a measurement error
df <- df[-455,]
fresh <- df[df$Freshness == "FR",]
thawed <- df[df$Freshness == "TH",]

# Create list of wavelengths from column names
wavelengths <- substring(colnames(df[5:length(df)]),2,8)
wavelengths <- as.numeric(wavelengths)

###############################################
# Function for creating spectral data
create.spectra <- function(df,type){
  # input is a word indicating whether we want fresh or thawed data,
  # divide data by Scan_Type
  if (type=="TH"){
    om <- t(df[(df$Freshness=="TH" & df$Scan_type=="OM"),][5:length(df)])
    tb <- t(df[(df$Freshness=="TH" & df$Scan_type=="TB"),][5:length(df)])
    tp <- t(df[(df$Freshness=="TH" & df$Scan_type=="TP"),][5:length(df)])
  }else{
    om <- t(df[(df$Freshness=="FR" & df$Scan_type=="OM"),][5:length(df)])
    tb <- t(df[(df$Freshness=="FR" & df$Scan_type=="TB"),][5:length(df)])
    tp <- t(df[(df$Freshness=="FR" & df$Scan_type=="TP"),][5:length(df)])
  }

  # rename the first column to wl
  colnames(om)[0] <- "wl"
  colnames(tb)[0] <- "wl"
  colnames(tp)[0] <- "wl"

  # convert to spectral data using pavo package
  om.spec <- as.rspec(om)
  tb.spec <- as.rspec(tb)
  tp.spec <- as.rspec(tp)

  # change index column to wavelengths
  om.spec$wl <-wavelengths
  tb.spec$wl <-wavelengths
  tp.spec$wl <-wavelengths

  # return list of all spectral scan type data
  spectra_data <- list("om"=om.spec,"tb"=tb.spec,"tp"=tp.spec,check.rows = TRUE)
  return (spectra_data)

}

# Function for plotting spectral data
plot.spectra <- function(om.spec,tb.spec,tp.spec,title,colors){
  # Plot spectral data sequentially
  plot(om.spec, xlab="",ylab="",col =colors[1], xlim=c(900,1680),
       ylim=c(0.5,3.5))
  par(new=TRUE)
  plot(tp.spec, xlab="",ylab="",col =colors[3],xlim=c(900,1680),
       ylim=c(0.5,3.5))
  par(new=TRUE)
  plot(tb.spec, xlab="Wavelength (nm)",ylab="Absorbance", col =colors[2],main=title,
       xlim=c(900,1680),ylim=c(0.5,3.5))
  legend(x="topleft", legend=c("OM", "TB","TP"),
         col=c(colors[1],colors[2],colors[3]), lty=1, cex=0.8)
}

################################################
# Create spectral data for fresh and thawed data
om.f <- create.spectra(df,"FR")$om
tb.f <- create.spectra(df,"FR")$tb
tp.f <- create.spectra(df,"FR")$tp

om.t <- create.spectra(df,"TH")$om
tb.t <- create.spectra(df,"TH")$tb
tp.t <- create.spectra(df,"TH")$tp

# Plot spectral data
plot.spectra(om.f,tb.f,tp.f,title="Raw NIR data of fresh chicken fillets",
             colors=c("blue","red","green"))
plot.spectra(om.t,tb.t,tp.t,title="Raw NIR data of thawed chicken fillets",
             colors=c("cyan","black","magenta"))

###############################################
# Perform spectral smoothing

om.f.smooth <- procspec(om.f,opt="smooth",fixneg="none")
tb.f.smooth <- procspec(tb.f,opt="smooth",fixneg="none")
tp.f.smooth <- procspec(tp.f,opt="smooth",fixneg="none")

om.t.smooth <- procspec(om.t,opt="smooth",fixneg="none")
tb.t.smooth <- procspec(tb.t,opt="smooth",fixneg="none")
tp.t.smooth <- procspec(tp.t,opt="smooth",fixneg="none")

plot.spectra(om.f.smooth,tb.f.smooth,tp.f.smooth,title="Smoothed NIR data of fresh chicken fillets",
             colors=c("blue","red","green"))
plot.spectra(om.t.smooth,tb.t.smooth,tp.t.smooth,title="Smoothed NIR data of thawed chicken fillets",
                colors=c("cyan","black","magenta"))


###############################################
# Linear Discriminant Analysis
# use 70% of dataset as training set and 30% as test set
linear.da <- function(df,title){
  sample <- sample.split(df$Scan_type, SplitRatio = 0.7)
  train  <- subset(df, sample == TRUE)
  #test   <- subset(df, sample == FALSE)

  preproc.parameter <- train[,5:length(df)] %>%
    preProcess(method = c("center", "scale"))

  train.transform <- preproc.parameter %>% predict(train[,5:length(train)])
  X_train <- as.matrix(train.transform)
  y_train <- train$Scan_type

  #test.transform <- preproc.parameter %>% predict(test[,5:length(test)])
  #X_test  <- as.matrix(test.transform)
  #y_test  <- test$Scan_type


  lda <- lda(y_train~X_train)
  predictions <- predict(lda,data.frame(X_train))

  print(paste0("Accuracy on train set for ",title, " fillets: ",
               round(mean(predictions$class==y_train),3)))

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

###############################################
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




