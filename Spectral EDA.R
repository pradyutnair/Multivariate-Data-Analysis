library(pacman)
pacman::p_load(ggplot2, MASS, Hmisc, dplyr, squash, pavo,plyr,signal,
               caTools,caret, klaR, factoextra,FactoMineR, randomForest,hyperSpec)
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
# Function to plot matrix spectral data
myBasicPlot<-function(mydata, wavelengths, xlim, ylim,title){

  #Create color map:
  par(font=2,las=1,mar = c(5,4,4,10) + 0.1)

  #Plot spectra:
  matplot(wavelengths,t(mydata$NIR),font.axis=2,main=title,
          col=mydata$Scan_type,lty=1, xlab="Wavelength (nm)",ylab="Absorbance (log[1/R])",type="l",lwd=3, xlim=xlim, ylim=ylim);
  minor.tick(nx=2, ny=2,tick.ratio=0.75);
  par(mar = c(5,4,4,6) + 0.1)
  legend(x="topleft", legend=c("OM", "TB","TP"),
         col=unique(mydata$Scan_type), lty=1, cex=0.8)
}
###############################################
# Define plot limits:
xlim<-c(900,1686)
ylim<-c(0.5,3.5)

###############################################
# Conduct baseline removal

# Apply savitsky-golay smoothing to the data
NIR<-apply(df[,5:length(df)],1, FUN=sgolayfilt, p = 2, n = 3, m = 0, ts = 1)

# Convert mydata to an hyperSpec S4 object:
mydataHS<-new("hyperSpec", spc = as.matrix(t(NIR)), wavelength = wavelengths)

# Compute baselines using order 2 polynomials and append to a dataframe
baseline<-spc.fit.poly.below(fit.to = mydataHS, poly.order = 2)
mybaseline<-data.frame(df[,1:4], NIR = I(baseline@data$spc))

# Plot baseline:
myBasicPlot(mybaseline, wavelengths, xlim, ylim,title="Baseline")

#Baseline removal:
newspectra<-mydataHS@data$spc-baseline@data$spc
mydataBSL<-data.frame(df[,1:4], NIR = I(newspectra))

# Plot baseline:
myBasicPlot(mybaseline, wavelengths, xlim, ylim,title="Baseline of Chicken Fillets NIR")

###############################################
#Baseline removal:
newspectra<-mydataHS@data$spc-baseline@data$spc
mydataBSL<-data.frame(df[,1:4], NIR = I(newspectra))

#Plot new spectra:
myBasicPlot(mydataBSL, wavelengths, xlim, ylim=c(0,1.5),title = "Baseline removed")

###############################################
# Save as csv
write.csv(mydataBSL, file = "NIR_Preprocessed.csv")




