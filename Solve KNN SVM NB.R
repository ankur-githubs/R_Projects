getwd()
setwd("E:/R_class/linear regresion/Boot Camp - 27th August 2018 - Linear Regression/final case study/FS CS by KNN,ANN,SVM,NB")
library(readxl)
mydata <- readxl::read_excel(path = "Linear Regression Case.xlsx",sheet =1) 
str(mydata)
#check the miss value 
sapply(mydata, FUN=function(x)sum(is.na(x)))

mydata$lntollmon <- NULL
mydata$lntollten <- NULL
mydata$lnequipmon <- NULL
mydata$lnequipten <- NULL
mydata$lncardmon <- NULL
mydata$lncardten <- NULL
mydata$lnwiremon <- NULL
mydata$lnwireten <- NULL
mydata$age <- NULL
mydata$ed <- NULL
mydata$employ <- NULL
mydata$income <- NULL
mydata$birthmonth <- NULL
mydata$lninc <- NULL
mydata$creddebt <- NULL 
mydata$othdebt <- NULL
mydata$spoused <- NULL
# mydata$pets <- NULL
mydata$address <- NULL
mydata$carvalue <- NULL
mydata$commute <- NULL
mydata$cardtenure <- NULL
mydata$card2tenure <- NULL
str(mydata)
ncol(mydata)

#make col var in Factor
col<- c("region","townsize","gender","agecat","edcat","jobcat","union","empcat","retire","inccat","default","jobsat","marital",
        "spousedcat","homeown","hometype","addresscat","cars","carown","cartype","carcatvalue","carbought","carbuy","commutecat",
        "commutecar","commutemotorcycle","commutecarpool","commutebus","commuterail","commutepublic","commutebike","commutewalk",
        "commutenonmotor","telecommute","reason","polview","polparty","polcontrib","vote","card","cardtype",
        "cardbenefit","cardfee","cardtenurecat","card2","card2type","card2benefit","card2fee","card2tenurecat",
        "active","bfast","churn","tollfree","equip","callcard","wireless","multline","voice","pager","internet",
        "callid","callwait","forward","confer","ebill","owntv","ownvcr","owndvd","owncd","ownpda","ownpc",
        "ownipod","owngame","ownfax","news","response_01","response_02","response_03")


mydata[col] <- lapply(mydata[col], factor)
str(mydata)

library(psych)
psych::describe(mydata)

num_var <- names(mydata)[sapply(mydata,is.numeric)]
cat_var <- names(mydata)[!sapply(mydata,is.numeric)]
ncol(mydata[num_var])
ncol(mydata[cat_var])

# summary(mydata)

mystats_num = function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  cv = sd(x, na.rm=T)/mean(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, cv=cv, var=var, range=range, pctl=pctl))
}

mystats_cat = function(x){
  Var_Type=class(x)
  n<-length(x)
  nmiss<-sum(is.na(x))
  fre<-list(table(x))
  prop<-list(prop.table(table(x)))
  #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
  return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
}

apply(mydata[num_var],2,mystats_num)
options(scipen = 9999)
write.csv(apply(mydata[num_var],2,mystats_num),"stat.csv")
sapply(mydata[num_var], FUN=function(x)sum(is.na(x)))

apply(mydata[cat_var],2,mystats_cat)
sapply(mydata[cat_var],function(x)sum(is.na(x)))

sum(is.na(mydata[num_var]))
sum(is.na(mydata[cat_var]))

psych::describe(mydata[num_var])

#check outlier by box

boxplot(mydata[num_var])
outlier2 <- function(x){
  UC2=quantile(x,0.99,na.rm=T)
  LC2=quantile(x,0.01,na.rm=T)
  x=ifelse(x>UC2,UC2,x)
  x=ifelse(x<LC2,LC2,x)
  return(x)
}

mydata[num_var] <- sapply(mydata[num_var],outlier2)
boxplot(mydata[num_var])

psych::describe(mydata[num_var])

#miss treat for numeric variable
miss_treat <- function(x){
  x[is.na(x)] <- median(x,na.rm = T)
  return(x)
}

mydata[num_var] <- sapply(mydata[num_var],miss_treat)
sum(is.na(mydata[num_var]))

#miss treat for catagorical variable
miss_treat_cat <- function(x){
  x[is.na(x)] <- which.max(table(x))
  return(x)
}

mydata$townsize <- sapply(mydata$townsize,miss_treat_cat)
sum(is.na(mydata[cat_var]))

sum(is.na(mydata))

mydata$total_spent <- mydata$cardspent+mydata$card2spent
mydata$cardspent <- NULL
mydata$card2spent <- NULL

num_var <- names(mydata)[sapply(mydata,is.numeric)]
cat_var <- names(mydata)[!sapply(mydata,is.numeric)]

ncol(mydata)
ncol(mydata[num_var])
ncol(mydata[cat_var])

# check significance of continuos variable
require(corrplot)
psych::describe(mydata)
#factor Analysis :-
corrm <- cor(mydata[num_var])

eigen(corrm)
ls(eigen(corrm))
eigen(corrm)$values
require(dplyr)
eigen_val <- mutate(data.frame(eigen(corrm)$values),
                    cum_sum_eigen=cumsum(eigen(corrm)$values),
                    pct_var = eigen(corrm)$values/sum(eigen(corrm)$values),
                    cum_pct_var=cum_sum_eigen/sum(eigen(corrm)$values))


FA <- fa(r = corrm,11,rotate = "varimax",fm = "ml")
summary(FA)
fa_sort <- fa.sort(FA)

fa_sort$e.values
fa_sort$loadings
loading <- data.frame(fa_sort$loadings[1:ncol(mydata[num_var]),])
write.csv(loading,"load12.csv")

#factor anlysis give 7 variable which is lnlongten
# lnlongmon
# tenure
# lnothdebt
# total_spent
# debtinc
# tollten
# equipmon
# wiremon
# pets_freshfish
# pets_cats
# pets_dogs
# pets_small
# pets_birds
# pets_saltfish
# pets_reptiles


L<- c("lnlongmon","tenure","lnothdebt","total_spent","debtinc","pets_freshfish","tollten","equipmon","wiremon","pets_cats","pets_dogs","pets_small","pets_birds","pets_saltfish","pets_reptiles")

ncol(mydata[cat_var])
ncol(mydata[L])

mydata1 <- cbind(mydata[cat_var],mydata[L])

#feature sampling by boruta algorithm
library(Boruta)
boruta= Boruta(total_spent~.,data = mydata1)
print(boruta)
plot(boruta)
plot(boruta,las = 2,cex.axis = 0.7)
plotImpHistory(boruta)
getNonRejectedFormula(boruta)
getConfirmedFormula(boruta)

#tentative fix
bor = TentativeRoughFix(boruta)
print(bor)
attStats(boruta)
getNonRejectedFormula(bor)
getConfirmedFormula(bor)


#Assumption
#names(mydata[num_var])
#1.Y should be normal distributed
dim(mydata1)
class(mydata1$total_spent)
#graphics.off()
hist(mydata1$total_spent)
plot(density(mydata1$total_spent))
#nearly normal distribution
#2 .corelation
library(corrplot)
num_var1 <- names(mydata1)[sapply(mydata1,is.numeric)]
cat_var1 <- names(mydata1)[!sapply(mydata1,is.numeric)]
corrplot(cor(mydata1[num_var1]),method = "circle",number.font = 1)
#####################################################################################################
#need to divide data
sample <- sample(1:nrow(mydata1),size = floor(nrow(mydata1)*0.7))
train <- mydata1[sample,]
test <- mydata1[-sample,]
####################################################################################################################3333
#1. Model  by Nural Network 
library(caret)
ctrl_lm <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
t.grid=expand.grid(size=5,decay=0.2)
fit.nn <- train(total_spent ~ agecat + empcat + retire + inccat + default + jobsat + 
                  addresscat + carown + carcatvalue + reason + card + cardtenurecat + 
                  card2 + card2tenurecat + tollfree + callcard + wireless + 
                  voice + pager + internet + callid + callwait + forward + 
                  confer + ownvcr + owndvd + owncd + ownpda + owngame + ownfax + 
                  news + lnlongmon + tenure + lnothdebt + debtinc + tollten + 
                  equipmon + wiremon,train,method="nnet", 
                  trControl=ctrl_lm, metric="RMSE",  maxit = 100, 
                  linout = FALSE,tuneGrid=t.grid)
varImp(fit.nn)
#Model Check & Accuracy Check
library(ROCR)
library(pROC)
predict.nn <- predict(fit.nn)
#Check Mape on train
(Mape <- mean(abs(train$total_spent- predict.nn)/train$total_spent))
#predict for test data
predict.nn_test <- predict(fit.nn,newdata = test)
#Check Mape on test
(Mape <- mean(abs(test$total_spent- predict.nn_test)/test$total_spent))
################################################################################################################################
# 2. Model build by KNN
grid <- expand.grid(k=c(3,4,5,6,7,8,9,10,11,12,13,14,15))
fit.knn <- train(total_spent ~ agecat + empcat + retire + inccat + default + jobsat + 
                   addresscat + carown + carcatvalue + reason + card + cardtenurecat + 
                   card2 + card2tenurecat + tollfree + callcard + wireless + 
                   voice + pager + internet + callid + callwait + forward + 
                   confer + ownvcr + owndvd + owncd + ownpda + owngame + ownfax + 
                   news + lnlongmon + tenure + lnothdebt + debtinc + tollten + 
                   equipmon + wiremon, 
                   data = train, 
                   method = "knn",                 
                   trControl = ctrl_lm,
                   metric="RMSE",
                   preProcess = c("center","scale"), 
                   tuneLength = 15,tuneGrid=grid)

varImp(fit.knn)
#Model Check & Accuracy Check
predict.knn <- predict(fit.knn)
#Check Mape on train
(Mape <- mean(abs(train$total_spent- predict.knn)/train$total_spent))
#predict for test data
predict.knn_test <- predict(fit.knn,newdata = test)
#Check Mape on test
(Mape <- mean(abs(test$total_spent- predict.knn_test)/test$total_spent))
####################################################################################################################################
#3.Model with Svm
grid <- expand.grid(C = c(0.25, 0.5, 0.75, 1.0, 1.25))

#Train and Tune the SVM
fit.svm <- train(total_spent ~ agecat + empcat + retire + inccat + default + jobsat + 
                   addresscat + carown + carcatvalue + reason + card + cardtenurecat + 
                   card2 + card2tenurecat + tollfree + callcard + wireless + 
                   voice + pager + internet + callid + callwait + forward + 
                   confer + ownvcr + owndvd + owncd + ownpda + owngame + ownfax + 
                   news + lnlongmon + tenure + lnothdebt + debtinc + tollten + 
                   equipmon + wiremon, 
                       data = train, 
                       method = "svmLinear",
                       preProc = c("center","scale"),
                       tuneGrid = grid,
                       metric="RMSE",                      
                       trControl=ctrl_lm)
varImp(fit.svm)
#Model Check & Accuracy Check
predict.svm <- predict(fit.svm)
#Check Mape on train
(Mape <- mean(abs(train$total_spent- predict.svm)/train$total_spent))
#predict for test data
predict.svm_test <- predict(fit.svm,newdata = test)
#Check Mape on test
(Mape <- mean(abs(test$total_spent- predict.svm_test)/test$total_spent))
#########################################################################################################################
#4.Model by lm
regressControl  <- trainControl(method="repeatedcv",
                                number = 4,
                                repeats = 5) 
fit.lm <- train(total_spent ~ agecat + empcat + retire + inccat + default + jobsat + 
                  addresscat + carown + carcatvalue + reason + card + cardtenurecat + 
                  card2 + card2tenurecat + tollfree + callcard + wireless + 
                  voice + pager + internet + callid + callwait + forward + 
                  confer + ownvcr + owndvd + owncd + ownpda + owngame + ownfax + 
                  news + lnlongmon + tenure + lnothdebt + debtinc + tollten + 
                  equipmon + wiremon,
                data = train,
                method  = "lm",
                trControl = regressControl, 
                tuneGrid  = expand.grid(intercept = FALSE))    
varImp(fit.lm)
#Model Check & Accuracy Check
predict.lm <- predict(fit.lm)
#Check Mape on train
(Mape <- mean(abs(train$total_spent- predict.lm)/train$total_spent))
#predict for test data
predict.lm_test <- predict(fit.lm,newdata = test)
#Check Mape on test
(Mape <- mean(abs(test$total_spent- predict.lm_test)/test$total_spent))
##############################################################################################################################

#here the best model come is by nural network
