library(pacman)
pacman::p_load(ggplot2, Hmisc, dplyr, squash,superml)

setwd('./')
df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';', dec=".",
                          stringsAsFactors = TRUE))

# Take the column names of the dataframe and extract the numbers from 'X' features as wavelengths
wavelengths <- substring(colnames(df[5:length(df)]),2,8)
wavelengths <- as.numeric(wavelengths)

# One hot encode the scan_type to use it as colour map
lbl <- LabelEncoder$new()
encoded_st <- lbl$fit_transform(df$Scan_type)



# Subdivision of FR and TH chicken fillets
fresh <- df %>% filter(Freshness == "FR")
fresh <- fresh[,-c(1,2,3,4)]

thawed <- df %>% filter(Freshness == "TH")
thawed <- thawed[,-c(1,2,3,4)]

# Plot the spectra
plot_wavelengths <-function(data,wavelengths,xlim,ylim,title){
    # Variable containing all spectra values
    NIR <- t(data)
    # Create a colour map
    map <- makecmap(encoded_st)
    mycol <- cmap(encoded_st, map=map)
    par(font=2,las=1,mar = c(5,4,4,10) + 0.1)
    # Create a plot
    matplot(x=wavelengths,y=NIR,font.axis=2,main=title,
            col=mycol,lty=1, xlab="",ylab="",type="l",lwd=3, xlim=xlim, ylim=ylim)
    minor.tick(nx=2, ny=2,tick.ratio=0.75)
    par(mar = c(5,4,4,6) + 0.1)
    title(xlab="Wavelength (nm)",ylab="Absorbance (log[1/R])",font.lab=2)

    #Plot color map:
    legend(x='topleft', inset=0.01, legend=c('OP','TB','TP'), col=c('Blue','Red','Green'),pch=15,
           bg= ("white"), horiz=F,title = 'Scan Type')
}
#Define plot limits:
xlim<-c(min(wavelengths),max(wavelengths))
ylim<-c(0.5,4)

plot_wavelengths(fresh,wavelengths,xlim,ylim,"Spectroscopic data of fresh chicken fillets")
plot_wavelengths(thawed,wavelengths,xlim,c(0.5,3.5),"Spectroscopic data of thawed chicken fillets")

map <- makecmap(encoded_st)
mycol <- cmap(encoded_st, map=map)

f <- df[df$Freshness == "FR",][,5:length(df)]
t <- df[df$Freshness == "TH",][,5:length(df)]
plot_wavelengths(f,wavelengths,xlim,ylim,"Spectroscopic data of fresh chicken fillets")
plot_wavelengths(t,wavelengths,xlim,ylim,"Spectroscopic data of thawed chicken fillets")