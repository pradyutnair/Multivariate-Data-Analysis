library(pacman)
pacman::p_load(tidyverse,mdatools)
set.seed(123)
dirName = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirName)
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
  plot(om.spec, xlab="",ylab="",col =colors[1], axes=F)
  par(new=TRUE)
  plot(tp.spec, xlab="",ylab="",col =colors[3],axes=F)
  par(new=TRUE)
  plot(tb.spec, xlab="Wavelength (nm)",ylab="Absorbance", col =colors[2],main=title,
       xlim=c(900,1680),ylim=c(-10,10))
  legend(x="topleft", legend=c("OM", "TB","TP"),
         col=c(colors[1],colors[2],colors[3]), lty=1, cex=0.8)
}

################################################
# Perform SNV and MSC
NIR <- df[,5:length(df)]
NIR.svg<-apply(NIR,1, FUN=sgolayfilt, p = 2, n = 3, m = 0, ts = 1)
sg.df <- cbind(df[,1:4],t(NIR.svg))
snv.spectra <- prep.snv(t(NIR.svg))
msc.spectra <- prep.snv(as.matrix(snv.spectra))
snv.df <- cbind(df[,1:4],snv.spectra)
msc.df <- cbind(df[,1:4],msc.spectra)
################################################

om.f.snv <- create.spectra(msc.df,"FR")$om
tb.f.snv <- create.spectra(msc.df,"FR")$tb
tp.f.snv <- create.spectra(msc.df,"FR")$tp

om.t.snv <- create.spectra(msc.df,"TH")$om
tb.t.snv <- create.spectra(msc.df,"TH")$tb
tp.t.snv <- create.spectra(msc.df,"TH")$tp

# Plot spectral data
plot.spectra(om.f.snv,tb.f.snv,tp.f.snv,title="MSC NIR data of fresh chicken fillets",
             colors=c("blue","red","green"))
plot.spectra(om.t.snv,tb.t.snv,tp.t.snv,title="MSC NIR data of thawed chicken fillets",
             colors=c("cyan","black","magenta"))

###############################################
write.csv(msc.df,"NIR_Preprocessed.csv")
