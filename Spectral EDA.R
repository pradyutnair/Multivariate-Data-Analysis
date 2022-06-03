library(pacman)
pacman::p_load(ggplot2, Hmisc, dplyr, squash,superml,pavo)

df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';', dec=".",
                          stringsAsFactors = TRUE))

fresh <- df[df$Freshness == "FR",][5:length(df)]
thawed <- df[df$Freshness == "TH",][5:length(df)]

wavelengths <- substring(colnames(df[5:length(df)]),2,8)
wavelengths <- as.numeric(wavelengths)


##############################################
om <- t(df[(df$Freshness=="FR" & df$Scan_type=="OM"),][5:length(df)])
tb <- t(df[(df$Freshness=="FR" & df$Scan_type=="TB"),][5:length(df)])
tp <- t(df[(df$Freshness=="FR" & df$Scan_type=="TP"),][5:length(df)])


colnames(om)[0] <- "wl"
colnames(tb)[0] <- "wl"
colnames(tp)[0] <- "wl"

om.spec <- as.rspec(om)
tb.spec <- as.rspec(tb)
tp.spec <- as.rspec(tp)

om.spec$wl <-wavelengths
tb.spec$wl <-wavelengths
tp.spec$wl <-wavelengths

plot(om.spec, xlab="",ylab="",col ="Blue",
     ylim=c(0.5,3.5))
par(new=TRUE)
plot(tp.spec, xlab="",ylab="",col ="Green",
     ylim=c(0.5,3.5))
par(new=TRUE)
plot(tb.spec, xlab="Wavelength (nm)",ylab="Absorbance", col ="Red",main="Spectroscopic data of fresh chicken fillets"
  ,ylim=c(0.5,3.5))
legend(x="topleft", legend=c("OM", "TB","TP"),
       col=c("blue", "red","green"), lty=1, cex=0.8)
plot.spec <- function(word,title){
  if (word == "FR"){
    df <- t(df[df$Freshness=="FR",][5:length(df)])
  }else if (word == "TH"){
    df <- t(df[df$Freshness=="TH",][5:length(df)])}

  om <- t(df[df$Scan_type=="OM",])
  tb <- t(df[df$Scan_type=="TB",])
  tp <- t(df[df$Scan_type=="TP",])

  colnames(om)[0] <- "wl"
  colnames(tb)[0] <- "wl"
  colnames(tp)[0] <- "wl"

  om.spec <- as.rspec(om)
  tb.spec <- as.rspec(tb)
  tp.spec <- as.rspec(tp)

  om.spec$wl <-wavelengths
  tb.spec$wl <-wavelengths
  tp.spec$wl <-wavelengths

  om.spec$wl <-wavelengths
  tb.spec$wl <-wavelengths
  tp.spec$wl <-wavelengths

  plot(om.spec, xlab="",ylab="",col ="Blue",
       ylim=c(0.5,3.5))
  par(new=TRUE)
  plot(tp.spec, xlab="",ylab="",col ="Green",
       ylim=c(0.5,3.5))
  par(new=TRUE)
  plot(tb.spec, xlab="Wavelength (nm)",ylab="Absorbance", col ="Red",main=title
    ,ylim=c(0.5,3.5))
}

##############################################
om <- t(df[(df$Freshness=="TH" & df$Scan_type=="OM"),][5:length(df)])
tb <- t(df[(df$Freshness=="TH" & df$Scan_type=="TB"),][5:length(df)])
tp <- t(df[(df$Freshness=="TH" & df$Scan_type=="TP"),][5:length(df)])


colnames(om)[0] <- "wl"
colnames(tb)[0] <- "wl"
colnames(tp)[0] <- "wl"

om.spec <- as.rspec(om)
tb.spec <- as.rspec(tb)
tp.spec <- as.rspec(tp)

om.spec$wl <-wavelengths
tb.spec$wl <-wavelengths
tp.spec$wl <-wavelengths

plot(om.spec, xlab="",ylab="",col ="cyan",
     ylim=c(0.5,3.5))
par(new=TRUE)
plot(tp.spec, xlab="",ylab="",col ="magenta",
     ylim=c(0.5,3.5))
par(new=TRUE)
plot(tb.spec, xlab="Wavelength (nm)",ylab="Absorbance", col ="black",main="Spectroscopic data of Thawed chicken fillets"
  ,ylim=c(0.5,3.5))
legend(x="topleft", legend=c("OM", "TB","TP"),
       col=c("cyan", "black","magenta"), lty=1, cex=0.8)