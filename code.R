
# 목표 : 퇴근시간 버스 승차인원을 예측해보자. 
# 그리고 퇴근시간 버스 승차인원과 다른 시간대의 승하차 인원 사이에 어떤 관계가 있는지 알아보도록 하자.


##데이터 가져오기
setwd("C:\\Users\\moonf\\Desktop\\tacademy")
library("data.table")
bus.original <- fread("train.csv", header=TRUE, nrow=277477,encoding="UTF-8") 

which.max(bus.original$date=="2019-09-07")
which(bus.original$date=="2019-09-15")[1]
which.max(bus.original$date=="2019-09-21")


bus.train<-bus.original[c(1:87781,186997:277477),]
str(bus.train)


bus.process<-bus.train[,-c('id', 'station_name','latitude','longitude','bus_route_id')]
bus.process<-as.data.frame(bus.process)

library(dplyr)
bus.process2<-bus.process %>% mutate(dawn_ride=bus.process$'6~7_ride'+bus.process$'7~8_ride') %>%
  mutate(work_ride=bus.process$'8~9_ride'+bus.process$'9~10_ride') %>%
  mutate(day_ride=bus.process$'10~11_ride'+bus.process$'11~12_ride') %>%
  mutate(dawn_takeoff=bus.process$'6~7_takeoff'+bus.process$'7~8_takeoff') %>%
  mutate(work_takeoff=bus.process$'8~9_takeoff'+bus.process$'9~10_takeoff') %>%
  mutate(day_takeoff=bus.process$'10~11_takeoff'+bus.process$'11~12_takeoff')
str(bus.process2)
bus.ex<-bus.process2[,c(1:3, 16:22)]

bus.ex<-bus.ex %>% mutate( bus_in=ifelse(in_out=="시내", 1, 0))
bus.ex<-bus.ex[,-2]
str(bus.ex)

names(bus.ex)[3]<-c('y')

bus.ex.try<-bus.ex %>% group_by(date, station_code, bus_in) %>% summarize(dawn_ride = sum(dawn_ride),
                                                      dawn_takeoff = sum(dawn_takeoff),
                                                      work_ride = sum(work_ride),
                                                      work_takeoff = sum(work_takeoff),
                                                      day_ride = sum(day_ride),
                                                      day_takeoff = sum(day_takeoff),
                                                      y=sum(y)) %>% data.frame()
                             
str(bus.ex.try)
head(bus.ex.try)


##데이터 이해하기
# 1. 데이터의 사이즈는? 
# 33471 개로 크기가 작지 않다.

str(bus.ex.try)


# 2. missing value는? ***

sum(is.na(bus.ex.try))


##3. 데이터 시각화(y 정규성, x와 y의 선형성, x 사이의 관계)

#y 정규성
#정규성을 따르지 않는 것을 볼 수 있다.

d<-density(bus.ex.try$y)
plot(d, main="Plot of 18~20 ride")

d2<-density(log1p(bus.ex.try$y)) #log를 해 주었을 때도 정규성을 따르지 않는 것을 볼 수 있음
plot(d2, main="Plot of 18~20 ride")

summary(bus.ex.try$y)

plot(factor(bus.ex.try$date), bus.ex.try$y)

#x와 y의 선형성, x 사이의 관계(시각화)

library(psych)
pairs.panels(bus.ex.try2)


#x와 y의 선형성, x 사이의 관계(수치)

cor(bus.ex.try)


# 4. 모델링을 위해 데이터 전처리 2번째


# 데이터를 일주일 단위로 두 주 뽑았는데, 그래프를 보면 x, y 모두
# bus.ex.try의 2019-09-07, 2019-09-20 (토요일)에 버스 승하차 인원이 적은 것을
# 확인 할 수 있다.
# 
# 이 특성을 date 변수 대신 넣어주려 한다.


bus.ex.try2<-transform(bus.ex.try, saturday=ifelse(date=="2019-09-07"|date=="2019-09-21", 1, 0))



##모델링을 위해 데이터 column 빼주기
## station_code, date는 모델링에서 의미를 가지지 않기 때문에 빼준다.
#앞으로도 "saturday"가 큰 영향을 주지 않는 변수이기 때문에
#노이즈 제거라 생각하고 지우도록 하겠습니다.
bus.ex.try2<-bus.ex.try2[, -c(1:2, 11)]

str(bus.ex.try2)
#5. 데이터의 특이한/주목해야 할 부분은?
# 해당 버스정류장에 대한 각각의 위도, 경도가 제공이 되어있는 상태로 같은 정류장 이름이지만 위도와 경도가 서로 다른 경우가 존재합니다. 해당 경우는, 같은 정류장 이름을 가지고 있는 길 건너편의 정류장에 해당이 됩니다.
# 
# 해당 데이터에는 버스카드를 통해 결제를 한 경우에 대한 정류소 승, 하차 데이터로 모든 승차정보의 경우는 기록이 되어있지만, 버스에서 하차를 할 때, 버스카드를 찍지 않는 경우, 해당 기록이 비어 있는 상태입니다. 따라서, 승차 인원수와 하차 인원수가 동일하지 않고 다소 차이가 있음을 미리 알려드립니다.


######### train, validation set으로 나누기
#개수는 반반으로 나누었고
#createDataPartition을 이용해 sample 함수로 야기되는 데이터의 치우침을 완화시켰다.

library(caret)
set.seed(1)
idx<-createDataPartition(bus.ex.try2$y, p=0.7, list=FALSE)
bus.ex.train<-bus.ex.try2[idx,]
bus.ex.test<-bus.ex.try2[-idx,]
summary(bus.ex.train$y)
summary(bus.ex.test$y)

##############################################################


##회귀분석-선형(다중선형회귀)

set.seed(1)
lm.fit<-lm(y~., data=bus.ex.train)
summary(lm.fit)
anova(lm.fit)

lm.pred<-predict(lm.fit, newdata=bus.ex.test )
rmse(bus.ex.test$y, lm.pred)

##예측 정확성 판단하기
par(mfrow=c(1,1))
plot(bus.ex.test$y, lm.pred, col="purple")
abline(0, 1, col="red", lwd=2)

library(Metrics)
library(car)
vif(lm.fit) #VIF (Variance Inflation Factors, 분산팽창요인)

##residual plot
par(mfrow=c(2, 2))
plot(lm.fit)


## 성능 개선 모델
lm.fit2<-lm(y~dawn_ride+dawn_takeoff+work_ride+work_takeoff+day_ride, data=bus.ex.train)
summary(lm.fit2)
anova(lm.fit2)

lm.pred2<-predict(lm.fit2, newdata=bus.ex.test)
rmse(bus.ex.test$y, lm.pred2)
vif(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(1,1))


###scaling 

library(ggplot2)
bus.ex.scale<-data.frame(bus_in=bus.ex.try$bus_in,scale(bus.ex.try2[,2:7]),y=bus.ex.try2$y)
bus.scale.train<-bus.ex.scale[idx,]
bus.scale.test<-bus.ex.scale[-idx,]

train.x=model.matrix(y~.,bus.ex.train )[,-1]
train.y=bus.scale.train$y

test.x=model.matrix(y~.,bus.ex.test )[,-1]
test.y=bus.scale.test$y


#Ridge Regression(능형회귀)

library(glmnet)
set.seed(1)
cv.out=cv.glmnet(train.x, train.y, alpha=0, nfolds=5)
bestlam=cv.out$lambda.min
bestlam 

ridge.fit=glmnet(train.x, train.y, alpha=0, lambda=bestlam)
summary(ridge.fit)
ridge.pred<-predict(ridge.fit, s=bestlam, newx=test.x)

rmse(test.y, ridge.pred)
plot(test.y, ridge.pred, col=c(3))
abline(0, 1, col="red", lwd=2)
ridge.fit$beta

#Lasso

set.seed(1)
cv.out=cv.glmnet(train.x, train.y, alpha=1, nfolds=3)
bestlam=cv.out$lambda.min
bestlam

lasso.fit=glmnet(train.x, train.y, alpha=1, lambda=bestlam)
lasso.pred<-predict(ridge.fit, s=bestlam, newx=test.x)
rmse(test.y, lasso.pred)
summary(lasso.fit)
lasso.fit$beta

plot(bus.scale.test$y, lasso.pred, col="blue")
abline(0, 1, col="red", lwd=2)
points(ridge.pred, col="blue")



#pcr
#pls는 pcr과 유사하므로 pls는 생략하겠다.
library(pls)

set.seed(1)
pcr.fit=pcr(y~., data=bus.ex.train, validation="CV")
validationplot(pcr.fit,val.type="MSEP") #MSEP가 안정화가 되는 4 지정

pcr.pred=predict(pcr.fit,bus.ex.test,ncomp=4)

rmse(bus.ex.test$y,pcr.pred)

plot(bus.scale.test$y, pcr.pred, col=c(6))
abline(0, 1, col="red", lwd=2)

##pcr 데이터 해석

names(pcr.fit)
cor(pcr.fit$scores) #주성분들의 상관계수가 모두 0임을 확인, 다중공선성 해결
pcr.fit$loadings #주성분들의 계수
pcr.fit$coefficients
biplot(pcr.fit, scale=F, cex=0.7) #그래프 그리기

##pcr 주성분 lm에 적합해보기

new.data<-as.data.frame(cbind(pcr.fit$scores[,1:4], y=bus.ex.train$y))
pc_lm<-lm(y~., data=new.data)
summary(pc.lm)

##polynomial

# dawn_takeoff, poly

cv.error.poly<-matrix(NA, nrow=3, ncol=5)
set.seed(1)
cv_list<-createFolds(bus.ex.try2$y, k=3, list=TRUE, returnTrain=FALSE)
for(i in 1:length(cv_list)){
  valid_ind<-cv_list[[i]]
  
  cv.test.set<-bus.ex.try2[valid_ind,]
  cv.train.set<-bus.ex.try2[-valid_ind,]
  
  for(j in 1:5){
    poly.fit<-glm(y~poly(dawn_takeoff,j), data=cv.train.set)
    ylims=range(cv.train.set$dawn_takeoff)
    y.grid=seq(from=ylims[1], to=ylims[2])
    
    poly.pred<-predict(poly.fit, newdata=list(dawn_takeoff=y.grid), se=TRUE)
    cv.error.poly[i,j]<-mean((cv.test.set$dawn_takeoff-poly.pred$fit)^2)
  }
  
}
cv.error.poly<-as.data.frame(cv.error.poly)
cv.error.poly.dawn_takeoff=apply(cv.error.poly, 2, mean)
which.min(cv.error.poly.dawn_takeoff) ##2차가 가장 적합한 모델

#work_takeoff

cv.error.poly<-matrix(NA, nrow=3, ncol=5)
set.seed(1)
cv_list<-createFolds(bus.ex.try2$y, k=3, list=TRUE, returnTrain=FALSE)
for(i in 1:length(cv_list)){
  valid_ind<-cv_list[[i]]
  
  cv.test.set<-bus.ex.try2[valid_ind,]
  cv.train.set<-bus.ex.try2[-valid_ind,]
  
  for(j in 1:5){
    poly.fit<-glm(y~poly(work_takeoff,j), data=cv.train.set)
    ylims=range(cv.train.set$work_takeoff)
    y.grid=seq(from=ylims[1], to=ylims[2])
    
    poly.pred<-predict(poly.fit, newdata=list(work_takeoff=y.grid), se=TRUE)
    cv.error.poly[i,j]<-mean((cv.test.set$work_takeoff-poly.pred$fit)^2)
  }
  
}
cv.error.poly<-as.data.frame(cv.error.poly)
cv.error.poly.work_takeoff=apply(cv.error.poly, 2, mean)
which.min(cv.error.poly.work_takeoff) ##4차가 가장 적합한 모델

## day_takeoff

cv.error.poly<-matrix(NA, nrow=3, ncol=5)
set.seed(1)
cv_list<-createFolds(bus.ex.try2$y, k=3, list=TRUE, returnTrain=FALSE)
for(i in 1:length(cv_list)){
  valid_ind<-cv_list[[i]]
  
  cv.test.set<-bus.ex.try2[valid_ind,]
  cv.train.set<-bus.ex.try2[-valid_ind,]
  
  for(j in 1:5){
    poly.fit<-glm(y~poly(day_takeoff,j), data=cv.train.set)
    ylims=range(cv.train.set$day_takeoff)
    y.grid=seq(from=ylims[1], to=ylims[2])
    
    poly.pred<-predict(poly.fit, newdata=list(day_takeoff=y.grid), se=TRUE)
    cv.error.poly[i,j]<-mean((cv.test.set$day_takeoff-poly.pred$fit)^2)
  }
  
}
cv.error.poly<-as.data.frame(cv.error.poly)
cv.error.poly.day_takeoff=apply(cv.error.poly, 2, mean)
which.min(cv.error.poly.day_takeoff) ##2차가 가장 적합한 모델


##smooth spline
smooth.fit.dawn_takeoff<-smooth.spline(bus.ex.try2$dawn_takeoff, bus.ex.try2$y, cv=TRUE) #leave-one-out cv
smooth.fit.dawn_takeoff$df

smooth.fit.work_takeoff<-smooth.spline(bus.ex.try2$work_takeoff, bus.ex.try2$y, cv=TRUE) #leave-one-out cv
smooth.fit.work_takeoff$df

smooth.fit.day_takeoff<-smooth.spline(bus.ex.try2$day_takeoff, bus.ex.try2$y, cv=TRUE) #leave-one-out cv
smooth.fit.day_takeoff$df

par(mfrow=c(3,1))


##회귀분석-비선형(일반화가법모델-선형+자연스플라인)
library(gam)
bus.ex.train<-as.data.frame(bus.ex.train)
bus.ex.test<-as.data.frame(bus.ex.test)

gam.fit.poly1<-gam(y~poly(dawn_takeoff^2)+poly(work_takeoff^4)+
                     poly(day_takeoff^2)+dawn_ride+work_ride+day_ride, data=bus.ex.train)
summary(gam.fit.poly1)


gam.fit.spline<-gam(y~s(dawn_takeoff, 6.879156)+s(work_takeoff, 10.99203)+
                      s(day_takeoff, 13.18741)+dawn_ride+day_ride+work_ride, data=bus.ex.train)
summary(gam.fit.spline)

poly.pred<-predict(gam.fit.poly1, newdata=bus.ex.test)
rmse(bus.ex.test$y, poly.pred)

spline.pred<-predict(gam.fit.spline, newdata=bus.ex.test)
rmse(bus.ex.test$y, spline.pred)


#plot 그리기+해석
par(mfrow=c(1,3))
plot(gam.fit.spline, se=TRUE)


library(mgcv)
##잔차분석
par(mfrow=c(2,2))
gam.check(gam.fit.poly1)
ggGam(gam.fit.spline)


## Regression tree
library(tree)
tree.rfit<-tree(y~., bus.ex.train)
summary(tree.rfit)

plot(tree.rfit) ; text(tree.rfit, pretty=0)


#pruning-cv_ hyper parameter 조정
tree.cv<-cv.tree(tree.rfit)
plot(tree.cv$size,tree.cv$dev,type='b')

#가지치기 해줌
tree.prun.r=prune.tree(tree.rfit,best=6)
plot(tree.prun.r) ;text(tree.prun.r,pretty=0)

#예측 비교
tree.pred.r<-predict(tree.rfit,newdata= bus.ex.test) #pruning 전
rmse(bus.ex.test$y, tree.pred.r)

tree.prun.pred.r<-predict(tree.prun.r,newdata= bus.ex.test) #pruning 후
rmse(bus.ex.test$y, tree.prun.pred.r)

#Bagging

library(randomForest)
set.seed(1)
bag.fit=randomForest(y~.,data=bus.ex.train,mtry=7,ntree=300,importance=TRUE)


#모델 성능평가
bag.pred = predict(bag.fit,newdata=bus.ex.test)
rmse(bag.pred, bus.ex.test$y)

plot(bag.pred, bus.ex.test$y, col=c(1,2))
abline(0,1, col="purple")


#Randomforest
set.seed(1)
features<-setdiff(names(bus.ex.train), "y")
tuneRF(x=bus.ex.train[features], y=bus.ex.train$y,
       ntreeTry=300, stepFactor=1.5, improve=0.01)

rf.fit=randomForest(y~.,data=bus.ex.train,mtry=3,ntree=300,importance=TRUE) 
rf.fit
rf.pred<-predict(rf.fit, bus.ex.test)
rmse(rf.pred, bus.ex.test$y)
#importance 볼 수 있음
importance(rf.fit)
varImpPlot(rf.fit) #plot




##cv function
mean.rmse<-matrix(NA, nrow=3, ncol=5)

set.seed(1)
cv_list<-createFolds(bus.ex.class$class, k=5, list=TRUE, returnTrain=FALSE)

for(i in 1:length(cv_list)){
  valid_ind<-cv_list[[i]]
  
  cv_test_set<-bus.ex.class[valid_ind,]

  lm.cv.pred<-predict(rf.fit, newdata=cv_test_set)
  mean.rmse[1, i]<-confusionMatrix(lm.cv.pred, cv_test_set$class)$overall[1]
  mean.rmse[2, i]<-confusionMatrix(lm.cv.pred, cv_test_set$class)$byClass[2]
  mean.rmse[3, i]<-confusionMatrix(lm.cv.pred, cv_test_set$class)$byClass[3]
  
  
}
mean.rmse<-as.data.frame(mean.rmse)
apply(mean.rmse, 1, mean)


############################################################

##분류 모형 살펴보기

#뎅이터 전처리
summary(bus.ex.try2$y) 
sum(bus.ex.try2$y>30) #1696
str(bus.ex.try2) #33471

bus.ex.class<-bus.ex.try2 %>% mutate(class=ifelse(bus.ex.try2$y>60, "yes", "no")) #yes : 2, no : 1
bus.ex.class<-bus.ex.class[,-8]
bus.ex.class$class<-as.factor(bus.ex.class$class)
str(bus.ex.class)


##시각화

library(GGally)
bus.ex.class %>% ggpairs(., 
               mapping = ggplot2::aes(colour=class), 
               )



#train/test set 나누기
idx<-createDataPartition(bus.ex.class$class, p=0.7, list=FALSE)
bus.ex.ctrain<-bus.ex.class[idx,]
bus.ex.ctest<-bus.ex.class[-(idx),]
table(bus.ex.ctrain$class); table( bus.ex.ctest$class)

## 로지스틱 회귀

logistic.fit<-glm(class~., data=bus.ex.ctrain, family="binomial")
summary(logistic.fit) #bus_in, dawn_takeoff 가 유의하지 않게 나옴

logistic.pred.prob<-predict(logistic.fit, newdata=bus.ex.ctest, type="response")
logistic.pred.prob[1:10]
contrasts(bus.ex.class$class) # yes를 1로 지정한 것을 볼 수 있음

#ROCR 곡선을 통해 cutoff 조정
library(ROCR)
pr<-prediction(logistic.pred.prob, bus.ex.ctest$class)
prf<-performance(pr, measure="tpr",x.measure="fpr")
str(prf)
plot(prf, main="ROC of logistic")

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(prf, pr))

logistic.pred.class=rep("yes", length(logistic.pred.prob))
logistic.pred.class[logistic.pred.prob<.3]="no"


confusionMatrix(as.factor(logistic.pred.class),bus.ex.ctest$class)

## 선형판별분석(Linear discriminant Analysis)

library(MASS)
lda.fit<-lda(class~work_ride+day_ride, data=bus.ex.ctrain)
lda.fit
plot(lda.fit)

lda.pred<-predict(lda.fit, newdata = bus.ex.ctest) #임계치 : 0.5
confusionMatrix(bus.ex.ctest$class, as.factor(lda.pred$class))


## 이차판별분석(Quadratic Discriminant Analysis)

qda.fit=qda(class~work_ride+day_ride, data=bus.ex.ctrain)
qda.fit #lda와 별 차이 없음

qda.pred<-predict(qda.fit, newdata=bus.ex.ctest)
confusionMatrix(bus.ex.ctest$class, as.factor(qda.pred$class))



##서포트벡터분류기

library(ggplot2)
bus.ex.scale<-data.frame(bus_in=bus.ex.class$bus_in,scale(bus.ex.class[,2:7]),class=bus.ex.class$class)
str(bus.ex.scale)

bus.scale.train<-bus.ex.scale[idx,] 
bus.scale.test<-bus.ex.scale[-(idx),] 

library(e1071)
set.seed(1)
cost=expand.grid(cost=c(0.01, 0.1, 1, 5))
svm.fit<-svm(y~., data=bus.scale.train, kernel="linear",cost=0.01, scale=FALSE)

tune.out=tune(svm,class~.,data=bus.ex.class,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5)))
summary(tune.out)
bestmod=tune.out$best.model


##tree 기반 모델-tree, randomforest

# classification tree
# 강의와 다른 방식으로 함, tree가 오류남

library(rpart)

tree.fit<-rpart(class~., data=bus.ex.ctrain)
#그래프 그리기
plot(tree.fit) ; text(tree.fit, cex=0.8)
tree.fit


#pruning 하기
printcp(tree.fit)
tree.prune<-prune(tree.fit, cp=tree.fit$cptable[which.min(tree.fit$cptable[,"xerror"]),"CP"])
plot(tree.prune) ; text(tree.prune)

#
#예측 비교
tree.pred<-predict(tree.fit,newdata= bus.ex.ctest, type="class") #pruning 전
confusionMatrix(factor(bus.ex.ctest$class), tree.pred)

tree.prun.pred<-predict(tree.prune,newdata= bus.ex.ctest, type="class") #pruning 후
confusionMatrix(tree.prun.pred, factor(bus.ex.ctest$class))


##random forest
library(randomForest)
set.seed(1)
features<-setdiff(names(bus.ex.ctrain), "class")
tuneRF(x=bus.ex.ctrain[features], y=bus.ex.ctrain$class,
       ntreeTry=500, stepFactor=1.5, improve=0.01)

rf.fit=randomForest(class~.,data=bus.ex.ctrain,mtry=3,ntree=300,importance=TRUE) 
rf.fit
rf.pred<-predict(rf.fit, bus.ex.ctest)
names(confusionMatrix(rf.pred, bus.ex.ctest$class))
a<-confusionMatrix(rf.pred, bus.ex.ctest$class)
a$byClass
#importance 볼 수 있음
importance(rf.fit)
varImpPlot(rf.fit) #plot




#################전체적으로 비교하기


##대표적으로 roc커브 곡선 비교해보기, logistic이랑 lda랑

pr<-prediction(logistic.pred.prob, bus.ex.ctest$class)
prf<-performance(pr, measure="tpr",x.measure="fpr")

pr.lda<-prediction(lda.pred$posterior[,2], bus.ex.ctest$class)
prf.lda<-performance(pr.lda, measure="tpr",x.measure="fpr")

library(arules)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
svc.fit=svm(class~., data=bus.ex.ctrain, kernel="linear",cost=0.1,decision.values=T)
fitted=attributes(predict(svc.fit,bus.ex.ctest, decision.values=TRUE))$decision.values
bus.ex.ctest2<-bus.ex.ctest
bus.ex.ctest2$class<-ifelse(bus.ex.ctest$class=="yes", 0, 1)


plot(prf, main="ROC", col="red")
plot(prf.lda,add=T, col="green") ## logistic이 좀 더 좋은 것을 확인 할 수 있음
rocplot(fitted, bus.ex.ctest2$class, add=T, col="blue")


##ggmap 그리기


library(devtools)
library(ggmap)

register_google(key='AIzaSyCWwYdWgp1oEooq-V1q5kwjWIV0W_LL2dE')
ggmap(get_map(location='south korea', zoom=7))

map<-get_map(location='Jeju-do', zoom=10)


bus.ex.class<-bus.ex.try %>% mutate(class=ifelse(bus.ex.try$y>60, "yes", "no")) #yes : 2, no : 1
bus.ex.class$class<-as.factor(bus.ex.class$class)


ggmap(map)+geom_point(data=bus.ex.class, aes(x=longitude, y=latitude, size=y,color=class),alpha=0.5)


ggmap(get_map(location='Jeju-do', zoom=14))+geom_point(data=bus.ex.class, aes(x=longitude, y=latitude, size=y,color=class),alpha=0.5)



























