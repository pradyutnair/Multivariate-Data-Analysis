NIR <- NIR_Preprocessed

library(klaR)
library(psych)
library(MASS)
library(ggplot2)
library(devtools)

NIR

NIR_data <- cbind(NIR[3:3], NIR[6:130])
NIR_data

#pairs.panels(NIR[6:130],
#             gap = 0,
#             bg = c("red", "green", "blue")[NIR$Production_system],
#             pch = 21)


set.seed(123)
ind <- sample(2, nrow(NIR_data),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- NIR_data[ind==1,]
testing <- NIR_data[ind==2,]

linear <- lda(Production_system~., training)
linear

p <- predict(linear, training)
p
ldahist(data = p$x[,1], g = training$Production_system)
ldahist(data = p$x[,2], g = training$Production_system)

ggord(linear, training$Production_system, ylim = c(-10, 10))

partimat(Production_system~., data = training, method = "lda")

p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$Production_system)
tab

sum(diag(tab))/sum(tab)

p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2, Actual = testing$Production_system)
tab1
sum(diag(tab1))/sum(tab1)
