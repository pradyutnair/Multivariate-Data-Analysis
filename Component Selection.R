library(mixOmics)
library(sgPLS)
#################################################################
# Import
df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';',
                          stringsAsFactors = TRUE))

df2 <- read.csv('./NIR_Preprocessed.csv',sep=',',stringsAsFactors=TRUE,strip.white = TRUE)
df2 <- df2[,-1]
# Remove row 455 as it is most likely a measurement error
#df <- df[-455,]
fresh <- df[df$Freshness == "FR",]
thawed <- df[df$Freshness == "TH",]

# Create list of wavelengths from column names
wavelengths <- substring(colnames(df[5:length(df)]),2,8)
wavelengths <- as.numeric(wavelengths)

#################################################################
# Create x and y calibration variables

x.cal <- df2[,5:length(df2)] # NIR data as the x variables
y.cal <- df2[,4]             # Scan_type

#################################################################
# Find optimal number of components for PLS
ncomp <- 10
MyResult.splsda <- splsda(x.cal, y.cal,ncomp=ncomp) # 1 Run the method
plotIndiv(MyResult.splsda)
plotVar(MyResult.splsda)
background <- background.predict(MyResult.splsda, comp.predicted=2,
                                 dist = "max.dist")

plotIndiv(MyResult.splsda, comp = 1:2, group = df2$Scan_type, keepX=c(50,50),
          ind.names = FALSE, title = "Maximum distance",
          legend = TRUE,  background = background)

auc.plsda <- auroc(MyResult.splsda)
MyResult.splsda2 <- splsda(x.cal,y.cal, ncomp=ncomp)
selectVar(MyResult.splsda2, comp=ncomp)$value
MyResult.plsda2 <- splsda(x.cal,y.cal, ncomp=ncomp)
set.seed(30) # for reproducbility in this vignette, otherwise increase nrepeat
MyPerf.plsda <- perf(MyResult.plsda2, validation = "Mfold", folds = 3,
                     progressBar = TRUE, nrepeat = 1) # we suggest nrepeat = 50
par(mfrow = c(1, 1))
plot(MyPerf.plsda, col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")


#################################################################

