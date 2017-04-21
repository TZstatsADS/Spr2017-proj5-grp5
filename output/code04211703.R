setwd('C:/Users/YSHAN/Downloads/FFChallenge')
library(readr)
bg=read_csv('../data/FFChallenge/background.csv')
train=read_csv('../data/FFChallenge/train.csv')
tar=read_csv('../data/FFChallenge/prediction.csv')
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

unique(bg$m1intyr)

namelist=names(bg)
i=0
for (f in namelist)
{
if(unique(bg[[f]])==1)
{
bg[[f]]=NULL
  
  
}
if(is.character(bg[[f]]))
{
bg[[f]]=as.numeric(as.factor(bg[[f]]))  
  
  
}
  

cat(i)  
cat(' ')
i=i+1
}
bgtrain=bg[setdiff(train$challengeID,train$challengeID[naid]),]
library(xgboost)
id=bgtrain$challengeID
bgtrain$challengeID=NULL
dtrain=xgb.DMatrix(data.matrix(bgtrain[,setdiff(names(bgtrain),dou_index)]),label=train$gpa[which(!is.na(train$gpa))])
params=list(
objective='reg:linear',
subsample=0.9,
colsample_bytree=0.1,
eta=0.1,
max_depth=6
)
var(train$gpa,na.rm = T)
model=xgb.train(data=dtrain,params = params,nround=40)
xgb.cv(data=dtrain,nfold=5,params = params,nround=350)
dtest=xgb.DMatrix(data=data.matrix(bg[,setdiff(names(bgtrain),dou_index)]))
testGPA=predict(model,dtest)
imp=xgb.importance(model=model)

var(train$gpa,na.rm=T)
tar$gpa=testGPA
write_csv(tar,'prediction244.csv')



fill_each_column <- function(each_col){
  na_label <- is.na(each_col)
  if(sum(na_label) > 0){
    cate_col <- ifelse(na_label == T, 1, 0) ## T = 1, is NA
    fill <- median(each_col, na.rm = T)
    each_col[na_label] <- fill
    return(list(NEW_COLUMN = each_col, NEW_CATE = cate_col))
  }else{
    ## no NA in a column
    return(list(NEW_COLUMN = rep(0,length(each_col)), NEW_CATE = rep(0,length(each_col))))
  }
  
  
}
fill_each_column(gp5data_dou[,839])

## INPUT data= continuous dataframe 
shabidata <- matrix(NA,5,5)
shabidata[1,] <- 5

gp5data_dou <- bgtrain_dou
gp5data_dou <- matrix(unlist(gp5data_dou), nrow(gp5data_dou))
## 1 for new column, 2 for new categorical features
gp5data_dou_RMNA <- apply(gp5data_dou, 2, function(col){fill_each_column(col)[[1]]})
cate_dou_na <- apply(gp5data_dou, 2, function(col){fill_each_column(col)[[2]]})

## remove NON-NA columns
non_na_dou <- colSums(cate_dou_na) == 0
cate_dou_na <- cate_dou_na[,!non_na_dou]

