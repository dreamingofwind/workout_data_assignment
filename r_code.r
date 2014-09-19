
dat<-read.csv("C:/users/wjc/downloads/pml-training.csv")
pred=cbind(dat[8:11],dat[37:49],dat[60:68],dat[84:86],dat[102],dat[113:124],dat[140],dat[151:159])
class=dat$classe
pred<-cbind(pred,class)

## cross validation preparation

trainIndex<-createDataPartition(pred$class,p=0.8,list=FALSE)
training<-pred[trainIndex,]
testing<-pred[-trainIndex,]

## random forest model 1
rf1<-randomForest(class~.,data=training,ntree=100)
plot(rf1)

## out of sample error estimation
confusionMatrix(predict(rf1,testing),testing$class)

varImpPlot(rf1)

## second model SVM with regularization

mod_svm_train<-svm(class~.,data=training,cost=1)
predictions_train<-predict(mod_svm_train)
confusionMatrix(predictions_train,training$class)

## out of sample error calculation
predictions_test<-predict(mod_svm_train,testing)
confusionMatrix(predictions_test,testing$class)

## looking at test set for this assignment

dat<-read.csv("C:/users/wjc/downloads/pml-testing.csv")
predictors=cbind(dat[8:11],dat[37:49],dat[60:68],dat[84:86],dat[102],dat[113:124],dat[140],dat[151:159])
predictions<-predict(mod_svm_train,predictors)
predictions2<-predict(rf1,predictors)
predictions==predictions2
