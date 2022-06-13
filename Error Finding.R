library(pacman)
pacman::p_load(pavo)
set.seed(123)

################################################
# Load data
df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';', dec=".",
                          stringsAsFactors = TRUE))
################################################
# Error finding
df.rspec <- as.rspec(df[,5:length(df)])
# Plot row 455
plot(t(df.rspec[455,])[-1,], xlab="Wavelength (nm)",ylab="Absorbance (log[1/R])",
     main="Raw NIR data of chicken fillets",ylim=c(0.5,3.5),type="l")

#peakshape(om.f,select=130)