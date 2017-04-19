#setwd('C:/Users/YSHAN/Downloads/FFChallenge')

# Start loading
library(readr)
bg=read_csv('background.csv')
train=read_csv('train.csv')
#tar=read_csv('prediction.csv')
#unique(tar$gpa)

### End loading 

### Start cleaning
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
gpa1 <- train$gpa[-naid]
unique(bg$m1intyr)


# names of all the features
namelist=names(bgtrain)
i=0

for (f in namelist){
  if(length(unique(bgtrain[[f]]))==1){
    bgtrain[[f]]=NULL
  }
  if(!is.numeric(bgtrain[[f]])){
  #  bgtrain[[f]]=factor(bgtrain[[f]])
    bgtrain[[f]] = NULL
  }
#  cat(i)
#  cat(' ')

}
# End cleaning
id=bgtrain$challengeID
bgtrain$challengeID=NULL

sel <- sample(1:nrow(bgtrain), nrow(bgtrain)*0.8)

####### K-MEANS func
library(plyr)
new_features_kmeans <- function(data, K){
  ## Return cluster.id
  kmeans_results <- kmeans(t(data), centers = K, iter.max = 100)
  return(kmeans_results$cluster)
}

generate_new_f_kmeans <- function(data, cluster.id){
  new_data_lm <- data.frame(CLUS = cluster.id, 
                            Records = t(data))
  new_data_lm_done <- ddply((new_data_lm), .(CLUS), colMeans)
  return(DATA = t(new_data_lm_done[,-1]))
}

cs.id <- new_features_kmeans(data = bgtrain[sel, ], K =120)
new_features_train <- generate_new_f_kmeans(bgtrain[sel, ], cs.id)
new_features_test <- generate_new_f_kmeans(bgtrain[-sel, ], cs.id)



# new_features_kms <- new_features_kmeans(bgtrain, 120, sel)$DATA
# testing_dataframe <- data.frame()
# testing <- ddply((new_data_lm), .(CLUS), colMeans)

################# LM ################# Please

data_df_train <- data.frame(gpa = gpa1[sel], new_features_train)
lm_model1 <- lm(gpa ~ ., data = data_df_train)

pred_lm_kc <- predict(lm_model1, newdata = data.frame(new_features_test))
mean((pred_lm_kc - gpa1[-sel])^2)




library(xgboost)



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


################# xgboost #################

model1 <-xgboost_kc(bgtrain1 = bgtrain[sel, ], obj = gpa1[sel])


dtest1=xgb.DMatrix(data.matrix(bgtrain[-sel, ]))

pred_kc <- predict(model1, newdata = dtest1)
mean((pred_kc - gpa1[-sel])^2)


################# GLM #################


