getwd()
setwd("E:/R_class/linear regresion/Boot Camp - 27th August 2018 - Linear Regression/final case study/FS CS by rpart,ctree")
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
# use Boruta algorithm for feature scaling
library(Boruta)
boruta= Boruta(total_spent~.,data = mydata)
print(boruta)
plot(boruta)
plot(boruta,las = 2,cex.axis = 0.7)
plotImpHistory(boruta)
getNonRejectedFormula(boruta)
getConfirmedFormula(boruta)

bor = TentativeRoughFix(boruta)
print(bor)
attStats(boruta)
getNonRejectedFormula(bor)
getConfirmedFormula(bor)

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


#################################################################################################################################
sum(colSums(is.na(new_finaldata)))
#Build rpart model 
require(C50)
library(rpart)      	  # Popular decision tree algorithm
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(rattle)					# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)		# Color selection for fancy tree plot

#need to divide data
sample <- sample(1:nrow(mydata),size = floor(nrow(mydata)*0.7))
train <- mydata[sample,]
test <- mydata[-sample,]

fit <- rpart(total_spent ~ agecat + empcat + retire + inccat + debtinc + lncreddebt + 
               lnothdebt + jobsat + addresscat + carown + carcatvalue + 
               card + cardtenurecat + card2 + card2tenurecat + carditems + 
               card2items + tenure + longmon + lnlongmon + longten + lnlongten + 
               tollfree + tollmon + tollten + equipmon + equipten + callcard + 
               cardmon + cardten + wiremon + wireten + multline + voice + 
               pager + internet + callid + ownvcr + owndvd + owncd + ownpda + 
               news,data = train,
             method="anova", control = rpart.control(minsplit = 25))


summary(fit)
#plot to understand what base we model build
plot(fit)
text(fit,digits = 3)
rpart.plot(fit)
fancyRpartPlot(fit)
pred_prob = predict(fit,newdata = train)
#check Mape train 
(Mape <- mean(abs(train$total_spent- pred_prob)/train$total_spent))
#check Mape test
pred_prob = predict(fit,newdata = test)
(Mape <- mean(abs(test$total_spent- pred_prob)/test$total_spent))

post(fit,file = "E:/R_class/logestic/logestic class/final case study ECOMMERCE/Eccom by Rpart and ctree/tree.jpg",title. = "regression tree for total spent")

#prune tree
summary(fit)
opt <- which.min(fit$cptable[,'xerror'])
#which row give low error
cp <- fit$cptable[opt,"cp"]
#prune tree
pfit <- prune(fit,cp = 0.01000000)
fancyRpartPlot(pfit,cex = 0.7)
pred_prob = predict(pfit,newdata = train)
#check Mape train 
(Mape <- mean(abs(train$total_spent- pred_prob)/train$total_spent))
#check Mape test
pred_prob = predict(pfit,newdata = test)
(Mape <- mean(abs(test$total_spent- pred_prob)/test$total_spent))

###############################################################################################################


###################################################################################################################
###### Extract Prediction rules & Add to the Trng Data ###### 

require(Amelia)
require(graphics)
library(stringr)

############### Function Gen Rule ##################
genRule = function (fit,i) {
  rule.v<-path.rpart(fit, nodes=as.numeric(i),print.it=FALSE)      
  rule.v <- unlist(rule.v, use.names=FALSE)[-1]
  #Remove all the dots 
  rule.v <- str_replace_all(rule.v, pattern="([a-zA-Z])\\.([a-zA-Z])", replacement="\\1_\\2")
  #Remove all the equal signs to 'in ('
  rule.v <- str_replace_all(rule.v, pattern="([a-zA-Z0-9])=", replacement="\\1 in ('")
  #Embrace all elements with " ' " 
  rule.v <- str_replace_all(rule.v, pattern=",", replacement="','")
  #Close the last element with apostrophe and a ")" 
  for (i in which(!is.na(str_extract(pattern="in", string=rule.v)))) {
    rule.v[i] <- paste(append(rule.v[i], "')"), collapse="")
  }
  
  #Collapse all the list in one string joined by " AND "
  output <- paste(rule.v, collapse = " AND ")
  return (output)
}

############### Function Gen Rule Ends ##################
summary(fit)
#Find Nodes 
tree2 = fit
tree2$frame$yval = as.numeric(rownames(tree2$frame))
Node<-predict(tree2,mydata,type="vector")
#Generate Rules per Node
allRule<-NULL
localRule<-NULL

for (j in 1:length(Node))
{
  localRule<-genRule(fit, Node[j])
  allRule<-rbind(allRule,localRule)
  
}
test_all<-mydata
test_all['node'] <-Node
#test_all['Pred']<-x
test_all["Rules"]<-allRule
write.table(test_all, "RPART_All_Output.csv", sep=",", row.names=FALSE)

View(test_all)

getwd()

summary(fit)
