set.seed(355)
library(caret); library(ggplot2); library(RANN); library(klaR)
setwd("D:/Coursera/MLFirst")
testing3<- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
training3<- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
training3<-training3[ , apply( training3, 2, function(x) !any(is.na(x)))]

prePt<- preProcess(training3, method=c("knnImpute"))
training1<- predict(prePt,training3)
testing1<- predict(prePt,testing3)
training1$classe <- factor(training1$classe)
validation<- testing1[,-c(1:7)]
training2<- training1[,-c(1:7)]
INtraining<- createDataPartition(training2$classe, p=0.7, list=FALSE)
training<- training2[INtraining,]
testing<- training2[-INtraining,]


train_control <- trainControl(method="cv", number=5)


fitnb<- train(classe~., data=training, trControl=train_control, method="nb", na.action=na.pass)
fittednb<- predict(fitnb, testing)
CMnb<- confusionMatrix(fittednb,testing$classe)


fitgbm<- train(classe~., data=training, trControl=train_control, method="gbm", na.action=na.pass, verbose=FALSE)
fittedgbm<- predict(fitgbm, testing)
CMgbm<- confusionMatrix(fittedgbm,testing$classe)


fitrf<- train(classe~., data=training, trControl=train_control, method="rf", na.action=na.pass)
fittedrf<- predict(fitrf, testing)
CMrf<- confusionMatrix(fittedrf,testing$classe)



prDF<-data.frame(fittednb,fittedrf, fittedgbm, classe=testing$classe)
fitcombined<- train(classe~., data=prDF, trControl=train_control, method="rf", na.action=na.pass)
fittedcombined<- predict(fitcombined, prDF)
CMcombined<- confusionMatrix(fittedcombined,testing$classe)



fittedrfV<- predict(fitrf, validation)
fittedgbmV<- predict(fitgbm, validation)
fittednbV<- predict(fitnb, validation)
fittedcombinedV<- predict(fitcombined, prDF)


