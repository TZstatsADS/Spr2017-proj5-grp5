setwd('../data')
library(readr)
bg=read_csv('background.csv')
train=read_csv('train.csv')

# Delete the records that corresponding to NA in the train
naid<- which(is.na(train$gpa))
bgtrain<- bg[setdiff(train$challengeID,train$challengeID[naid]),]

# Delete the variables that are 80% NAs
n<- ncol(bgtrain)
na_count<- rep(NA, n)
for(i in 1:n){
  na_count[i]<- sum(is.na(bgtrain[,i]))
}
na_index<- c(1:n)[na_count>=0.8*nrow(bgtrain)]
bgtrain<- bgtrain[,-na_index]

# Delete the variables that have the same value
namelist=names(bgtrain)
for (f in namelist) {
  if(nrow(unique(bgtrain[f]))==1) {
    bgtrain[f]=NULL
  }
}

n<- ncol(bgtrain)
t<- rep(NA, n)
for(i in 1:n){
  t[i]<- typeof(bgtrain[[1,i]])
}
char_index<- c(1:n)[t=="character"]
bgtrain_num<- bgtrain[, -char_index]
bgtrain_char<- bgtrain[, char_index]

