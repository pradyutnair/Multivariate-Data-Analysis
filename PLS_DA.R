### Examples for PLS-DA model class

#code from: https://search.r-project.org/CRAN/refmans/mdatools/html/plsda.html and https://mdatools.com/docs/plsda--calibration.html
library(pacman)
pacman::p_load(ggplot2,tidyverse,mdatools,pls)

#################################################################
# Import
df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';',
                          stringsAsFactors = TRUE))
# Remove row 455 as it is most likely a measurement error
df <- df[-455,]
fresh <- df[df$Freshness == "FR",]
thawed <- df[df$Freshness == "TH",]

# Create list of wavelengths from column names
wavelengths <- substring(colnames(df[5:length(df)]),2,8)
wavelengths <- as.numeric(wavelengths)

#################################################################
# Make a PLS-DA model with full cross-validation and show model overview
x.cal <- df[,5:length(df)] # NIR data as the x variables
y.cal <- df[,3] # Scan_type

# Create a PLS-DA model with 2 components and 5 fold cv
plsda.model <- plsda(x.cal, y.cal,ncomp=9, cv = 5)
model <- selectCompNum(plsda.model, 9)

summary(model)                    # Model summary
getConfusionMatrix(model$calres)  # Confusion matrix
summary(model$calres)             # Summary of per class performance
plotPredictions(model)            # Plot of predictions

## 2. Show performance plots for a model
par(mfrow = c(2, 2))
plotSpecificity(model)
plotSensitivity(model)
plotMisclassified(model)
plotMisclassified(model, nc = 2)
par(mfrow = c(1, 1))

## 3. Show both class and y values predictions
par(mfrow = c(2, 2))
plotPredictions(model)
plotPredictions(model, res = "cal", ncomp = 9, nc = 2)
plotPredictions(structure(model, class = "regmodel"))
plotPredictions(structure(model, class = "regmodel"), ncomp = 9, ny = 2)
par(mfrow = c(1, 1))

## 4. All plots from ordinary PLS can be used, e.g.:
par(mfrow = c(2, 2))
plotXYScores(model)
plotYVariance(model)
plotXResiduals(model)
plotRegcoeffs(model, ny = 2)

#################################################################
library(sgPLS)
library(mixOmics)

MyResult.splsda <- splsda(x.cal, y.cal, keepX = c(50,50)) # 1 Run the method
plotIndiv(MyResult.splsda)
plotVar(MyResult.splsda)
background <- background.predict(MyResult.splsda, comp.predicted=2,
                                 dist = "max.dist")
#plotIndiv(MyResult.splsda, comp = 1:2, group = df$Scan_type,
          #ind.names = FALSE, title = "Maximum distance",
          #legend = TRUE,  background = background)

auc.plsda <- auroc(MyResult.splsda)
MyResult.splsda2 <- splsda(x.cal,y.cal, ncomp=9, keepX=c(15,10,5))
selectVar(MyResult.splsda2, comp=9)$value

MyResult.plsda2 <- splsda(x.cal,y.cal, ncomp=10)
set.seed(30) # for reproducbility in this vignette, otherwise increase nrepeat
MyPerf.plsda <- perf(MyResult.plsda2, validation = "Mfold", folds = 3,
                     progressBar = TRUE, nrepeat = 1) # we suggest nrepeat = 50

plot(MyPerf.plsda, col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")
??selectCompNum


#################################################################
