
library(readr)
bg=read_csv('background.csv')
train=read_csv('train.csv')

# Delete the records that corresponding to NA in the train
naid<- which(is.na(train$gpa))
bgtrain<- bg[setdiff(train$challengeID,train$challengeID[naid]),]

# Delete the variables that are 80% NAs
n <- ncol(bgtrain)
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

# Delete the variables that can neither be used as charater 
n<- ncol(bgtrain)
t<- rep(NA, n)
for(i in 1:n){
        t[i]<- typeof(bgtrain[[1,i]])
}
# unique(t)
char_index<- c(1:n)[t=="character"]
inter_index<- c(1:n)[t=="integer"]
dou_index<- c(1:n)[t=="double"]

# seperate the variable by different type, charater are those that cannot
# be convert to any type of data, doubles are continuous data. And intergers may
# contain part of continuous data. Thus, we should also handle these variables.
bgtrain_char<- bgtrain[, char_index]
bgtrain_inter<- bgtrain[, inter_index]
bgtrain_dou<- bgtrain[, dou_index]

# table(bgtrain_inter[,17])
# For interger-type variables, check the factor number it contains. And we consider those have
# more than 20 factors as continuous number.
fac_num<- as.numeric(apply(bgtrain_inter, 2, function(vec) 
{return(length(table(vec)))} ))
fac_index<- which(fac_num<=20)
bgtrain_dou<- cbind(bgtrain_inter[,-fac_index], bgtrain_dou)
bgtrain_inter<- bgtrain_inter[,fac_index]

# Fianlly, we get two seperate dataset to deal with in next steps
bgtrain_dou<- data.frame(bgtrain_dou)
bgtrain_inter<- data.frame(bgtrain_inter)



### fill NA and create new data
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
#fill_each_column(gp5data_dou[,839])


my.text <- "^([a-z]{1,3}5)"
indices <- grepl(my.text,colnames(bgtrain_dou))
mat <- bgtrain_dou[,indices]

## INPUT data= continuous dataframe 
gp5data_dou <- mat
gp5data_dou <- matrix(unlist(gp5data_dou), nrow(gp5data_dou))
## 1 for new column, 2 for new categorical features
gp5data_dou_RMNA <- apply(gp5data_dou, 2, function(col){fill_each_column(col)[[1]]})
cate_dou_na <- apply(gp5data_dou, 2, function(col){fill_each_column(col)[[2]]})

## remove NON-NA columns
# non_na_dou <- colSums(cate_dou_na) == 0
# cate_dou_na <- cate_dou_na[,!non_na_dou]


dim(bg)
bg_dou <- bg[,colnames(mat)]
dim(bg_dou)

gp5pred_dou <- bg_dou
gp5pred_dou <- matrix(unlist(gp5pred_dou), nrow(gp5pred_dou))
## 1 for new column, 2 for new categorical features
gp5pred_dou_RMNA <- apply(gp5pred_dou, 2, function(col){fill_each_column(col)[[1]]})
cate_pred_dou_na <- apply(gp5pred_dou, 2, function(col){fill_each_column(col)[[2]]})

dtrain=xgb.DMatrix(cbind(data.matrix(mat),cate_dou_na),label=train$gpa[which(!is.na(train$gpa))])
dtest=xgb.DMatrix(cbind(data.matrix(bg_dou),cate_pred_dou_na))
#dtest=xgb.DMatrix(new_features_test)
params=list(
        objective='reg:linear',
        subsample=0.9,
        colsample_bytree=0.8,
        eta=0.05,
        max_depth=1
)

#xgb.cv(nfold=10,data=dtrain,params = params,nround=3000)
model=xgb.train(data=dtrain,params=params,nrounds=100)
# imp=xgb.importance(model=model)
# feid=imp$Feature[1:500]
# dtrain=xgb.DMatrix(cbind(data.matrix(bgtrain_dou),cate_dou_na)[,as.numeric(feid)],label=train$gpa[which(!is.na(train$gpa))])
# params=list(
#         objective='reg:linear',
#         subsample=0.9,
#         colsample_bytree=0.15,
#         eta=0.05,
#         max_depth=6
# )
# xgb.cv(nfold=10,data=dtrain,params = params,nround=3000)
sub=predict(model,dtest)
tar=read_csv('prediction.csv')
tar$gpa=sub
write_csv(tar,'submission.csv')

