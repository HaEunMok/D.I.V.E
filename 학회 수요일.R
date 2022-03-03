####train 전처리###############
train<-read.csv('train.csv')
test<-read.csv('test.csv')
colSums(is.na(train))#나이 NA 177/891 많음
train$Age<-cut(train$Age, breaks=c(-Inf ,10, 20, 30, 40,50, 60,70,80,90,100, Inf),
                                                 labels = c("under 10","10~19","20~29",
                                                            "30~39","40~49","50~59","60~69",
                                                            "70~79","80~89","90~99","over100"),
                                                 right = F)#나이대 나누기
train$Fare<-cut(train$Fare, breaks=c(-Inf ,10, 20, 30, 40,50, 60,70,80,90,100, Inf),
                labels = c("under 10","10~19","20~29","30~39","40~49","50~59","60~69",
                           "70~79","80~89","90~99","over100"),
                right = F)
train$Cabin<-substr(train$Cabin,1,1)

sum(is.na(train$Age))
###test 전처리###
colSums(is.na(test))#NA가 나이에 86개, Fare에 1
test$Age<-cut(test$Age, breaks=c(-Inf ,10, 20, 30, 40,50, 60,70,80,90,100, Inf),
                                              labels = c("under 10","10~19","20~29",
                                                         "30~39","40~49","50~59","60~69",
                                                         "70~79","80~89","90~99","over100"),
                                              right = F)#나이대 나누기
test$Fare<-cut(test$Fare, breaks=c(-Inf ,10, 20, 30, 40,50, 60,70,80,90,100, Inf),
               labels = c("under 10","10~19","20~29","30~39","40~49","50~59","60~69",
                          "70~79","80~89","90~99","over100"),
               right = F)
test$Cabin<-substr(test$Cabin,1,1)
colSums(is.na(test))
test<-test[complete.cases(test[,c(9)]),]
####model만들기####################
library(rpart)
library(rpart.plot)
model <- rpart(Survived ~Sex+Cabin+Fare+Pclass+Embarked+Age+SibSp, data = train, method = "class",
               control = rpart.control(cp = 0))
rpart.plot(model)

rpart.plot(model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

#train 모델 만들기
train$pred <- predict(model, train, type = 'class')
table(train$Survived, train$pred)
mean(train$Survived == train$pred)#accuracy
"""
Pclass 0.6790123
Sex 0.7867565
Age 0.6329966
SibSp 06329966
Parch 0.630752
Name 1 x
Ticket 0.9416386 x
Fare 0.6857464
Cabin 0.694725
Embarked 0.6386083

Sex+Cabin 0.7901235
Sex+Cabin+Fare 0.8024691
Sex+Cabin+Fare+Pclass 0.8226712
Sex+Cabin+Fare+Pclass+Embarked 0.8305275
Sex+Cabin+Fare+Pclass+Embarked+Age 0.8608305
Sex+Cabin+Fare+Pclass+Embarked+Age+SibSp 0.8675645 최선
Sex+Cabin+Fare+Pclass+Embarked+Age+SibSp+Parch 0.8653199
"""
#test 예측
test$prediction<-predict(model, test, typr="class")
#과적합 발생

####pre-pruning######
# Grow a tree with maxdepth of 5
model2 <- rpart(Survived ~Sex+Cabin+Fare+Pclass+Embarked+Age+SibSp, data = train, method = "class",
                    control = rpart.control(cp = 0, maxdepth = 5))
test$pred <- predict(model2, test, type = 'class')
rpart.plot(model2)
# Grow a tree with minsplit of 50
model3 <- rpart(Survived ~ Sex+Cabin+Fare+Pclass+Embarked+Age+SibSp, data = train, method =
                       "class", control = rpart.control(cp = 0, minsplit = 50))
# Compute the accuracy of the simpler tree
test$pred2 <- predict(model3, test, type = 'class')
rpart.plot(model3)

####post-pruning#####
model4 <- rpart(Survived ~ Sex+Cabin+Fare+Pclass+Embarked+Age+SibSp, data = train, method = "class", control =
                      rpart.control(cp = 0))
plotcp(model4)#
printcp(model4)
# Prune the tree
model_pruned <- prune(model4, cp = 0)
test$pred3 <- predict(model_pruned, test, type = 'class')
rpart.plot(model4)
#labs do not fit even at cex 0.15, there may be some overplotting




####train 전처리###############
train<-read.csv('train.csv')
test<-read.csv('test.csv')
colSums(is.na(train))#나이 NA 177/891 많음
ave_age1<-mean(train$Age, na.rm=T)
train$Age<-ifelse(is.na(train$Age),ave_age1, train$Age)#Na를 평균나이로 바꿔줌
train$Fare<-cut(train$Fare, breaks=c(-Inf ,10, 20, 30, 40,50, 60,70,80,90,100, Inf),
                labels = c("under 10","10~19","20~29","30~39","40~49","50~59","60~69"
                           ,"70~79","80~89","90~99","over100"),
                right = F)
train$Cabin<-substr(train$Cabin,1,1)

sum(is.na(train$Age))
###test 전처리###
colSums(is.na(test))#NA가 나이에 86개, Fare에 1
ave_age2<-mean(test$Age, na.rm=T)
test$Age<-ifelse(is.na(test$Age),ave_age2, test$Age)#Na를 평균나이로 바꿔줌
test$Fare<-cut(test$Fare, breaks=c(-Inf ,10, 20, 30, 40,50, 60,70,80,90,100, Inf),
               labels = c("under 10","10~19","20~29","30~39","40~49","50~59","60~69",
                          "70~79","80~89","90~99","over100"),
               right = F)
test$Cabin<-substr(test$Cabin,1,1)
colSums(is.na(test))
test<-test[complete.cases(test[,c(9)]),]#fare가 NA인 부분, 하나니까 삭제 


####Knn전처리####
library(class)

train<-as.data.frame(train)
test<-as.data.frame(test)

train_label <- train$Survived
str(train)#sex,Fare,  cabin, Emb리arked
train$Survived <- NULL
sapply(train, class)
sapply(test,class)

###dummy coding###
library(caret)
dum<-predict(dummyVars(~ Sex+Fare+Cabin+Embarked, data = train), train)
train$Sex <- NULL
train$Fare <- NULL
train$Cabin <- NULL
train$Embarked <- NULL
train$Name<-NULL
train$Ticket<-NULL
minmax_norm <- function(x) { (x-min(x))/(max(x)-min(x)) }
train<- sapply(train[,-1], minmax_norm)
train<-cbind(train, dum)

#test dummy coding
dum_test<-predict(dummyVars(~ Sex+Fare+Cabin+Embarked, data = test), test)
test$Sex <- NULL
test$Fare <- NULL
test$Cabin <- NULL
test$Embarked <- NULL
test$Name<-NULL
test$Ticket<-NULL
test<- sapply(test[,-1], minmax_norm)
test<-cbind(test, dum_test)
CabinT<-0
Embarked.<-0
test<-cbind(test,CabinT,Embarked.)

# split data 
# choosing proper k(split 기준) 
sqrt(nrow(train))#29.84962

library(class) 

dim(train)
dim(test)

train_pred <- knn(train = train, test = train, cl = train_label, k = 29)
test_pred <- knn(train = train, test = test, cl = train_label, k = 29)

#accuracy 

mean(train_label == train_pred)#0.7957351
cmat <- table(train_label, train_pred) 
#precision 
cmat[2,2] / sum(cmat[,2])
#recall 
cmat[2,2] / sum(cmat[2,])

train_pred <- knn(train = train, test = train, cl = train_label, k = 29, prob = TRUE)
test_pred <- knn(train = train, test = test, cl = train_label, k = 29, prob = TRUE)
View(test_pred)
# converting all Prob to P
train_pred_prob <- ifelse(train_pred == 0, attributes(train_pred)$prob, 1-attributes(train_pred)$prob)
head(train_pred_prob)

library(ROCR)
plot(performance(prediction(train_pred_prob, train_label == 0), 'tpr', 'fpr'))
calAUC <- function(predCol, targetCol){ 
  perf <- performance(prediction(predCol, targetCol), 'auc') 
  as.numeric(perf@y.values) 
}
# AUC for our kNN 
calAUC(train_pred_prob, train_label == 0)
# set lower threshold 
threshold <- 0.1 
test_pred_new <- ifelse(train_pred_prob > threshold, 0, 1)
#accuracy 
mean(train_label == train_pred)
#precision 
cmat[2,2] / sum(cmat[,2])
#recall 
cmat[2,2] / sum(cmat[2,])





####train 전처리###############
train<-read.csv('train.csv')
test<-read.csv('test.csv')
colSums(is.na(train))#나이 NA 177/891 많음
ave_age1<-mean(train$Age, na.rm=T)
train$Age<-ifelse(is.na(train$Age),ave_age1, train$Age)#Na를 평균나이로 바꿔줌

train$Fare<-cut(train$Fare, breaks=c(-Inf ,10, 20, 30, 40,50, 60,70,80,90,100, Inf),
                labels = c("under 10","10~19","20~29","30~39","40~49","50~59","60~69"
                           ,"70~79","80~89","90~99","over100"),
                right = F)
train$Cabin<-substr(train$Cabin,1,1)

sum(is.na(train$Age))
###test 전처리###
colSums(is.na(test))#NA가 나이에 86개, Fare에 1
ave_age2<-mean(test$Age, na.rm=T)
test$Age<-ifelse(is.na(test$Age),ave_age2, test$Age)#Na를 평균나이로 바꿔줌

test$Fare<-cut(test$Fare, breaks=c(-Inf ,10, 20, 30, 40,50, 60,70,80,90,100, Inf),
               labels = c("under 10","10~19","20~29","30~39","40~49","50~59","60~69",
                          "70~79","80~89","90~99","over100"),
               right = F)
test$Cabin<-substr(test$Cabin,1,1)
colSums(is.na(test))
test<-test[complete.cases(test[,c(9)]),]#fare가 NA인 부분, 하나니까 삭제 
colSums(is.na(train))
#####Naive bayes####
library(naivebayes)
#factor로 변환

train_factor<-sapply(train, as.factor)
test_factor<-sapply(test, as.factor)

train_factor<-data.frame(train_factor)
test_factor<-data.frame(test_factor)

domodel<-naive_bayes(Survived~Sex,data=train_factor)

#predict
test<-as.data.frame(test)
train<-as.data.frame(train)

pred<-predict(domodel, newdata=train)

pred_test<-predict(domodel,newdata=test)
str(train)
#accuracy
mean(train$Survived==pred)
cmat2<-table(train$Survived, pred)
#precision
cmat2[2,2]/sum(cmat2[,2])
#recall
cmat[2,2]/sum(cmat[2,])


