library(pacman)
library(matrixStats)

pacman::p_load(matrixStats, comprehenr,ggplot2, MASS, Hmisc, dplyr, squash, pavo,plyr,signal,
               caTools,caret, klaR, factoextra,FactoMineR, randomForest,hyperSpec)
set.seed(123)

################################################
# Load data
df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';', dec=".",
                          stringsAsFactors = TRUE))


#Outlier detection: Minimum Covariance Determinant
# TLDR about the techinique, it calculates the mean and covariance Matrix for a central portion of the data,
#  making it thus robust to outliers, then it calculates the Mahalanobis distance of the points, and selects 
# values based on the predetermined cuttof constant

alpha = 0.01 #cuttoff constant
cutoff <- qchisq(p = 1 - alpha, df = length(df)-5) 
df_out = cov.mcd(df[5:length(df)]) #calculates the minimum covariance determinant (beware it takes some running time)
mhmcd = mahalanobis(df[5:length(df)], df_out$center, df_out$cov) #mahalanobis distance (also beware of running time)
names_outlier_MCD75 = which(mhmcd > cutoff) 
excluded_mcd75 = names_outlier_MCD75
df_outlier = df[-excluded_mcd75, ] # takes the outliers out of the dataframe
#df_outlier = df[excluded_mcd75, ]


# Remove row 455 as it is most likely a measurement error
#df <- df[-455,]
fresh <- df_outlier[df_outlier$Freshness == "FR",]
thawed <- df_outlier[df_outlier$Freshness == "TH",]

# Create list of wavelengths from column names
wavelengths <- substring(colnames(df[5:length(df)]),2,8)
wavelengths <- as.numeric(wavelengths)

###############################################
# Function for creating spectral data
create.spectra <- function(df,type){
  # input is a word indicating whether we want fresh or thawed data,
  # divide data by Scan_Type
  if (type=="TH"){
    om <- t(df[(df_outlier$Freshness=="TH" & df_outlier$Scan_type=="OM"),][5:length(df)])
    tb <- t(df[(df_outlier$Freshness=="TH" & df_outlier$Scan_type=="TB"),][5:length(df)])
    tp <- t(df[(df_outlier$Freshness=="TH" & df_outlier$Scan_type=="TP"),][5:length(df)])
  }else{
    om <- t(df[(df_outlier$Freshness=="FR" & df_outlier$Scan_type=="OM"),][5:length(df)])
    tb <- t(df[(df_outlier$Freshness=="FR" & df_outlier$Scan_type=="TB"),][5:length(df)])
    tp <- t(df[(df_outlier$Freshness=="FR" & df_out$Scan_type=="TP"),][5:length(df)])
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
om.fresh <- create.spectra(df_outlier,"FR")$om
tb.fresh <- create.spectra(df_outlier,"FR")$tb
tp.fresh <- create.spectra(df_outlier,"FR")$tp


om.thawed <- create.spectra(df_outlier,"TH")$om
tb.thawed <- create.spectra(df_outlier,"TH")$tb
tp.thawed <- create.spectra(df_outlier,"TH")$tp


# Plot spectral data
plot.spectra(om.fresh,tb.fresh,tp.fresh,title="Raw NIR data of fresh chicken fillets",
             colors=c("blue","red","green"))
plot.spectra(om.thawed,tb.thawed,tp.thawed,title="Raw NIR data of thawed chicken fillets",
             colors=c("cyan","black","magenta"))