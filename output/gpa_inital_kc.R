setwd('C:/Users/YSHAN/Downloads/FFChallenge')
library(readr)
bg=read_csv('background.csv')
train=read_csv('train.csv')
#tar=read_csv('prediction.csv')
#unique(tar$gpa)
sum(is.na(train$gpa))
sum(is.na(train$grit))
sum(is.na(train$materialHardship))
sum(is.na(train$eviction))
sum(is.na(train$layoff))
sum(is.na(train$eviction))
sum(is.na(train$jobTraining))
# Delete the records that corresponding to NA in the train
naid=which(is.na(train$gpa))
var(train$gpa,na.rm=T)
bg[is.na(bg)]=-999
bgtrain=bg[setdiff(train$challengeID,train$challengeID[naid]),] 
unique(bg$m1intyr)


# names of all the features
namelist=names(bgtrain)
i=0
for (f in namelist){
  if(length(unique(bgtrain[[f]]))==1){
    bgtrain[[f]]=NULL
  }
  if(is.character(bgtrain[[f]])){
    bgtrain[[f]]=as.numeric(factor(bgtrain[[f]]))
  }
  cat(i)  
  cat(' ')
  i=i+1
}

library(xgboost)
id=bgtrain$challengeID
bgtrain$challengeID=NULL
gpa1 <- train$gpa[which(!is.na(train$gpa))]

xgboost_kc <- function(bgtrain1 = bgtrain, obj = gpa1){
  dtrain=xgb.DMatrix(data.matrix(bgtrain1),label= obj)
  params=list(
    objective='reg:linear',
    subsample=0.9,
    colsample_bytree=0.05,
    eta=0.1,
    max_depth=6
  )
  
  model=xgb.train(data=dtrain,params = params,nround=350)
  imp=xgb.importance(model=model)
  #0.62^2
  # var(train$gpa,na.rm=T)
  return(model)
}

sel <- sample(1:nrow(bgtrain), nrow(bgtrain)*0.8)
model1 <-xgboost_kc(bgtrain1 = bgtrain[sel, ], obj = gpa1[sel])


dtest1=xgb.DMatrix(data.matrix(bgtrain[-sel, ]))

pred_kc <- predict(model1, newdata = dtest1)
mean((pred_kc - gpa1[-sel])^2)
