setwd('C:/Users/YSHAN/Downloads/FFChallenge')
library(readr)
bg=read_csv('background.csv')
train=read_csv('train.csv')
tar=read_csv('prediction.csv')
unique(tar$gpa)
sum(is.na(train$gpa))
sum(is.na(train$grit))
sum(is.na(train$materialHardship))
sum(is.na(train$eviction))
sum(is.na(train$layoff))
sum(is.na(train$eviction))
sum(is.na(train$jobTraining))
naid=which(is.na(train$gpa))
var(train$gpa,na.rm=T)
bg[is.na(bg)]=-999
bgtrain=bg[setdiff(train$challengeID,train$challengeID[naid]),]
unique(bg$m1intyr)

namelist=names(bgtrain)
i=0
for (f in namelist)
{
if(unique(bgtrain[[f]])==1)
{
bgtrain[[f]]=NULL
  
  
}
if(is.character(bgtrain[[f]]))
{
bgtrain[[f]]=as.numeric(as.factor(bgtrain[[f]]))  
  
  
}
  
  
cat(i)  
cat(' ')
i=i+1
}

library(xgboost)
id=bgtrain$challengeID
bgtrain$challengeID=NULL
dtrain=xgb.DMatrix(data.matrix(bgtrain),label=train$gpa[which(!is.na(train$gpa))])
params=list(
objective='reg:linear',
subsample=0.9,
colsample_bytree=0.05,
eta=0.1,
max_depth=6
)

model=xgb.train(data=dtrain,params = params,nround=350)
imp=xgb.importance(model=model)
0.62^2
var(train$gpa,na.rm=T)
