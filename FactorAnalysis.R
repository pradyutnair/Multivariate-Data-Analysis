NIR <- NIR_Preprocessed
NIR <- NIR_Preprocessed

library(psych)
library(corrplot)
library("psych")
library(ggplot2)
library(car)

datamatrix <- cor(NIR[6:130])
corrplot(datamatrix, method="number")

X <- NIR[6:130]
Y <- NIR[2:5]

KMO(r=cor(X))
cortest.bartlett(X)


fafitfree <- fa(NIR[6:130],nfactors = ncol(X), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")


parallel <- fa.parallel(X)


fa.none <- fa(r=X, 
              nfactors = 7, 
              # covar = FALSE, SMC = TRUE,
              fm="pa", # type of factor analysis we want to use ("pa" is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate="varimax") # none rotation
print(fa.none)

fa.diagram(fa.none)

head(fa.none$scores)


Y <- NIR[2:5]
#Y["Production_system"][Y["Production_system"]=="1 Star"] <- 10
#Y["Production_system"][Y["Production_system"]=="2 stars"] <- 20
#Y["Production_system"][Y["Production_system"]=="CONV"] <- 30
#Y["Production_system"][Y["Production_system"]=="ORG"] <- 40
#Y["Production_system"][Y["Production_system"]=="STD"] <- 50
#Y["Production_system"][Y["Production_system"]=="FR"] <- 60
#Y["Production_system"][Y["Production_system"]=="CF"] <- 70
#Y["Production_system"][Y["Production_system"]=="MAR"] <- 80

Y["Freshness"][Y["Freshness"]=="FR"] <- 0
Y["Freshness"][Y["Freshness"]=="TH"] <- 1

#Y
#rapply(num_Ps, function(x) func(x), how = "replace")

#Y["Production_system"]
#Y["Freshness"]
#fa.none$scores[,"PA3"]

regdata <- data.frame (first_column  = Y["Freshness"], 
                  PA3 = fa.none$scores[,"PA3"],
                  PA1 = fa.none$scores[,"PA1"],
                  PA2 = fa.none$scores[,"PA2"],
                  PA4 = fa.none$scores[,"PA4"],
                  PA5 = fa.none$scores[,"PA5"],
                  PA6 = fa.none$scores[,"PA6"],
                  PA7 = fa.none$scores[,"PA7"]
)
regdata

#regdata <- cbind(Y$Production_system, fa.none$scores)
#Labeling the data
#names(regdata) <- c("Production_system", "F1", "F2",
 #                    "F3", "F4", "F5", "F6")
head(regdata)

set.seed(100)
indices= sample(1:nrow(regdata), 0.7*nrow(regdata))
train=regdata[indices,]
test = regdata[-indices,]

model.fa.score = lm(Freshness~., data = train)
summary(model.fa.score)

vif(model.fa.score)

pred_test <- predict(model.fa.score, newdata = test, type = "response")
pred_test
test$Freshness_pred <- round(pred_test, digits = 0)#signif(pred_test, digits = 0)#round(pred_test, digits = 0)
head(test[c("Freshness","Freshness_pred")], 100)
table(test[,"Freshness"] == test[, "Freshness_pred"])
738/(478+738)
