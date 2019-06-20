getwd()
setwd("E:/R_class/linear regresion/Boot Camp - 27th August 2018 - Linear Regression/final case study/1.FS CS by TL/")

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


#variable reduction of catag var
summary(aov(mydata$total_spent~mydata[cat_var]$region))#significant
summary(aov(mydata$total_spent~mydata[cat_var]$townsize))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$gender))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$agecat))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$edcat))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$jobcat))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$union))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$empcat))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$retire))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$inccat))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$default))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$jobsat))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$marital))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$spousedcat))#medium signficant
summary(aov(mydata$total_spent~mydata[cat_var]$homeown))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$hometype))#high sgnificant
summary(aov(mydata$total_spent~mydata[cat_var]$addresscat))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$cars))# no significant
summary(aov(mydata$total_spent~mydata[cat_var]$carown))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$cartype))#  no significant
summary(aov(mydata$total_spent~mydata[cat_var]$carcatvalue))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$carbought))#no significant    
summary(aov(mydata$total_spent~mydata[cat_var]$carbuy))#no significant             
summary(aov(mydata$total_spent~mydata[cat_var]$commutecat))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$commutecar))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$commutemotorcycle))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$commutecarpool))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$commutebus))#no sgnificant
summary(aov(mydata$total_spent~mydata[cat_var]$commuterail))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$commutepublic))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$commutebike))# signficant
summary(aov(mydata$total_spent~mydata[cat_var]$commutewalk))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$commutenonmotor))#no significant
summary(aov(mydata$total_spent~mydata[cat_var]$telecommute))# no significant
summary(aov(mydata$total_spent~mydata[cat_var]$reason))# high significant
summary(aov(mydata$total_spent~mydata[cat_var]$polview))#NO significance
summary(aov(mydata$total_spent~mydata[cat_var]$polparty))#no significance
summary(aov(mydata$total_spent~mydata[cat_var]$polcontrib))#medium significance
summary(aov(mydata$total_spent~mydata[cat_var]$vote))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$card))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$cardfee))#no signifance
summary(aov(mydata$total_spent~mydata[cat_var]$cardtype))#no significance
summary(aov(mydata$total_spent~mydata[cat_var]$cardbenefit))# no significance
summary(aov(mydata$total_spent~mydata[cat_var]$cardtenurecat))#High significance
summary(aov(mydata$total_spent~mydata[cat_var]$card2))#High significance
summary(aov(mydata$total_spent~mydata[cat_var]$card2type))#no significance
summary(aov(mydata$total_spent~mydata[cat_var]$card2benefit))#no significance
summary(aov(mydata$total_spent~mydata[cat_var]$card2fee))# significance
summary(aov(mydata$total_spent~mydata[cat_var]$card2tenurecat))#High signicance
summary(aov(mydata$total_spent~mydata[cat_var]$active))# no significance
summary(aov(mydata$total_spent~mydata[cat_var]$bfast))#no signicance
summary(aov(mydata$total_spent~mydata[cat_var]$churn))# no significance
summary(aov(mydata$total_spent~mydata[cat_var]$tollfree))#High significance
summary(aov(mydata$total_spent~mydata[cat_var]$equip))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$callcard))#medium signigicant
summary(aov(mydata$total_spent~mydata[cat_var]$wireless))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$multline))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$voice))#medium significance
summary(aov(mydata$total_spent~mydata[cat_var]$pager))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$internet))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$callid))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$callwait))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$forward))#high signifcance
summary(aov(mydata$total_spent~mydata[cat_var]$confer))#high signifance
summary(aov(mydata$total_spent~mydata[cat_var]$ebill))#low significanct
summary(aov(mydata$total_spent~mydata[cat_var]$owntv))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$ownvcr))#high significant
summary(aov(mydata$total_spent~mydata[cat_var]$owndvd))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$owncd))#High significane
summary(aov(mydata$total_spent~mydata[cat_var]$ownpda))#high signicance
summary(aov(mydata$total_spent~mydata[cat_var]$ownpc))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$ownipod))#medium significance
summary(aov(mydata$total_spent~mydata[cat_var]$owngame))#No significance
summary(aov(mydata$total_spent~mydata[cat_var]$ownfax))#high significance
summary(aov(mydata$total_spent~mydata[cat_var]$news))#High significance
summary(aov(mydata$total_spent~mydata[cat_var]$response_01))# no significant
summary(aov(mydata$total_spent~mydata[cat_var]$response_02))#medium significance
summary(aov(mydata$total_spent~mydata[cat_var]$response_03))#high significant
#remove not significance from table
ncol(mydata[cat_var])

mydata$townsize <- NULL
mydata$union <- NULL
mydata$default <- NULL
mydata$martial <- NULL
mydata$cars <- NULL
mydata$cartype <- NULL
mydata$carbought <- NULL
mydata$carbuy <- NULL
mydata$commutecat <- NULL
mydata$commutecar <- NULL
mydata$commutemotorcycle <- NULL
mydata$commutecarpool <- NULL
mydata$commutebus <- NULL
mydata$commutebus <- NULL
mydata$commuterail <- NULL
mydata$commutepublic <- NULL
mydata$commutebike <- NULL
mydata$commutewalk <- NULL
mydata$commutenonmotor <- NULL
mydata$telecommute<- NULL
mydata$polview <- NULL
mydata$polparty <- NULL
mydata$cardfee<- NULL
mydata$cartype<- NULL
mydata$cardbenefit<- NULL
mydata$card2type<- NULL
mydata$card2benefit<- NULL
mydata$active<- NULL
mydata$bfast <- NULL
mydata$churn<- NULL
mydata$owngame <- NULL
mydata$response_01<- NULL



cat_var <- names(mydata)[!sapply(mydata,is.numeric)]

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
names(mydata1[cat_var1])
mydata1 <- mydata1[c(-1)]
names(mydata1)

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



numdata<- mydata[num_var]
names(mydata1[cat_var1])

str(mydata1)

# use neural network on model
library("keras")
library("mlbench")
library("dplyr")
library("magrittr")
library("neuralnet")

library(keras)
library(reticulate)
use_python('/opt/anaconda/anaconda3/envs/r-tensorflow/bin/python')
py_config()



data = mydata1 %>% mutate_if(is.factor,as.numeric)

str(data)

# neural network vizualisation :- 
n <- neuralnet(total_spent~region+gender+agecat+edcat+jobcat+empcat+retire+inccat+jobsat+marital+
                 spousedcat+homeown+hometype+addresscat+carown+carcatvalue+reason+polcontrib+vote+card+cardtype+cardtenurecat+
                 card2+card2fee+card2tenurecat+tollfree+equip+callcard+wireless+multline+voice+pager+internet+callid+callwait+
                 forward+confer+ebill+owntv+ownvcr+owndvd+owncd+ownpda+ownpc+ownipod+ownfax+news+response_02+response_03+
                 lnlongmon+tenure+lnothdebt+debtinc+pets_freshfish+tollten+equipmon+wiremon+pets_cats+pets_dogs+pets_small+pets_birds+pets_saltfish+
                 pets_reptiles,data = data,hidden = c(10,5),
               linear.output = F,
               lifesign = "full",
               rep = 1)

plot(n,col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')



#write.csv(data,"data_new.csv")

data = read.csv("data_new.csv")
str(data)
dim(data)
data = as.matrix(data)
dimnames(data) <- NULL

set.seed(1234)

ind = sample(2,nrow(data),replace = T,prob = c(.7,.3))

training = data[ind == 1, 1:64]
testing = data[ind == 1, 1:64]

trainingtarget = data[ind==1,65]
testingtarget  = data[ind == 1, 65]

#normalixze the data

m = colMeans(training)
s = apply(training, 2, sd)

training = scale(x = training, center = m, scale = s)
testing  = scale(x = testing , center = m, scale = s)


model = keras_model_sequential()


# model %>% layer_dense(units = 10,activation = 'relu',input_shape = dim(training)[2]) %>% 
#   layer_dense(units = 5,activation = 'relu') %>% layer_dense(units = 1)
# 
# summary(model)





model %>% layer_dense(units = 100,activation = 'relu',input_shape = dim(training)[2])%>% 
                     layer_dropout(rate = 0.4) %>% 
                     layer_dense(units = 50, activation = 'relu') %>% 
                     layer_dropout(rate = 0.3) %>% 
                     layer_dense(units = 20,activation = 'relu') %>% 
                     layer_dropout(rate = 0.2)%>%
                     layer_dense(units = 10,activation = 'relu') %>% 
                     layer_dropout(rate = 0.1) %>% 
                     layer_dense(units = 1)
                  
                   
  
       
summary(model)

model %>% compile(loss = 'mse',
               optimizer = 'rmsprop',
               metrics = 'mae')

# Fit Model
 mymodel <- model %>% fit(training,
                      trainingtarget,
                      epochs = 100,
                      batch_size = 32,
                      validation_split = 0.2)

model %>% evaluate(testing,testingtarget)

pred <- model %>% predict(testing)

mean((testingtarget - pred)^2)
plot(testingtarget,pred)

