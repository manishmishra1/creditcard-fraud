creditcard<-read.csv("creditcard.csv")
str(creditcard)
table(creditcard$Class)
#  0      1 
# 284315    492 
#284315*100/(284315+492)=99.82 is baseline accuracy
#imbalanced classification
creditcard$Class<-as.factor(creditcard$Class)
table(is.na(creditcard))
# FALSE 
# 8829017
creditcard$Time<-NULL(as time variable has no significance)
str(creditcard)
#treating the imbalanced data using synthetic data generating with SMOTE 
library(caret)
library(caTools)
split<-sample.split(creditcard$Class,SplitRatio = 0.7)
train<-subset(creditcard,split==T)
test<-subset(creditcard,split==F)
str(train)
dim(test)
train.rose <- ROSE(Class ~ ., data = train, seed = 1)$data
table(train.rose$Class)
test.rose <- ROSE(Class ~ ., data = test, seed = 1)$data
table(test.rose$Class)
#data_modelling
#1.logistic regression
lr<-glm(Class~.,data=train.rose,family = "binomial")
summary(lr)
pred_glm<-predict(lr,newdata = test.rose,type="response")
table(test.rose$Class, pred_glm > 0.5)
library(ROCR)
pr <- prediction(pred_glm, test.rose$Class)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
#auc value is 0.9463662
accuracy.meas(test.rose$Class, pred_glm>0.5)
lr_imbalanced<-glm(Class~.,data=train,family = "binomial")
pred_imbalancedglm<-predict(lr,newdata = test,type="response")
pr_imbal <- prediction(pred_imbalancedglm, test$Class)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
accuracy.meas(test.rose$Class, pred_glm>0.5)
accuracy.meas(test$Class, pred_imbalancedglm>0.5)
roc.curve(test.rose$Class, pred_glm>0.5,plotit = T)
roc.curve(test$Class, pred_imbalancedglm>0.5,plotit = T)

#improving eval_metric usin other models
#trees
library(e1071)
tree.model <- rpart(Class ~ ., data = train.rose, method = "class", minbucket = 5)
prp(tree.model)
tree.predict <- predict(tree.model, test.rose, type = "class")
confusionMatrix(test.rose$Class, tree.predict)
# Accuracy : 0.965 Sensitivity : 0.9677 Specificity : 0.9623 
#cart on unbalanced data
tree.model1 <- rpart(Class ~ ., data = train, method = "class", minbucket = 5)
prp(tree.model1)
tree.predict1 <- predict(tree.model1, test, type = "class")
confusionMatrix(test$Class, tree.predict1)
#Accuracy : 0.9994 Sensitivity : 0.9995 Specificity : 0.9060 

accuracy.meas(test.rose$Class, tree.predict)
accuracy.meas(test$Class, tree.predict1)


#using random forest approach

rf.model<-randomForest(Class~., data = train.rose ,ntree=10)
rf.model
rf.predict<-predict(rf.model,newdata = test.rose)
confusionMatrix(rf.predict,test.rose$Class)
#accuracy 0.997 sensitivity 0.9952 specificity 0.9989

#on unbalanced data
rf.model1<-randomForest(Class~., data = train ,ntree=10)
rf.predict1<-predict(rf.model1,newdata = test)
confusionMatrix(rf.predict1,test$Class)
#accuracy=0.9995 sensitivity 1.00000 specificity 0.7635

accuracy.meas(test.rose$Class, rf.predict)
accuracy.meas(test$Class, rf.predict1)

#using XgBoost
library(xgboost)
xgb.train.rose <- xgb.DMatrix(as.matrix(train.rose[, colnames(train.rose) != "Class"]), label = train.rose$Class)
xgb.test.rose <- xgb.DMatrix(as.matrix(test.rose[, colnames(test.rose) != "Class"]), label = test.rose$Class)




