setwd("C:/Users/Class2017/Desktop/University/FE582/Codes/HW3")
library(gdata)

Default <- read.csv("Default.csv")
Default2Predict <- read.csv("DefaultPredict.csv")


##Logistic
glm.fit=glm(default~balance+student+income,data=Default,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,newdata = Default, type="response")
glm.probs[1:10]
dim(Default)
glm.pred=rep("No",dim(Default)[1])
glm.pred[glm.probs>0.5]="Yes"
prediction.glm=cbind(Default,glm.pred)
head(prediction.glm)
colnames(prediction.glm)[5]="default prediction"
head(prediction.glm)
#View(prediction.glm)
contrasts(Default$default)
table(glm.pred,Default$default)
228/(228+105)
mean(glm.pred == Default$default)
#overall error rate
1-mean(glm.pred == Default$default)
#error among defaulted
228/(228+105)
#sensitivity (percentage of true defaulters identified)
105/(228+105)
#specificity (percentage of true non-defaulters that are correctly identified)
9627/(9627+40)

dim(Default)
training=Default
test=Default2Predict;
glm.fit=glm(default~balance+student+income,data=training,family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,newdata = test, type="response")
glm.probs[1:10]
dim(test)
glm.pred=rep("No",dim(test)[1])
glm.pred[glm.probs>0.5]="Yes"
prediction = cbind(test,glm.pred)
head(prediction.glm)
colnames(prediction)[5]="default prediction LOG"
head(prediction.glm)
#View(prediction.glm)


##LDA
library(MASS)
lda.fit=lda(default~student+balance+income,data=training)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
prediction =cbind(prediction,lda.class)
colnames(prediction)[6]="default prediction LDA"
#View(prediction.lda)

#QDA
qda.fit=qda(default~student+balance+income,data=training)
qda.fit
qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class
prediction = cbind(prediction,qda.class)
colnames(prediction)[7]="default prediction QDA"
#View(prediction.lda)



#KNN
test.x=cbind(test$index,test$student,test$balance,test$income)
training.x=cbind(training$index,training$student,training$balance,training$income)
library(class) #Library for KNN
knn.pred=knn(training.x,test.x,training$default,k=5)
prediction = cbind(prediction,knn.pred)
colnames(prediction)[8]="default prediction KNN"

write.csv(prediction,file = "output")
