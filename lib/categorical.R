#setwd('C:/Users/YSHAN/Downloads/FFChallenge')

source("cleandata.R")
bgtrain_inter[is.na(bgtrain_inter)] = -9

mat<- bgtrain_inter
mat[is.na(mat)]<- -9
mm<- t(matrix(unlist(mat), nrow(mat)))
sum(is.na(mm))


####### K-MEANS func

new_features_kmeans <- function(data, K){
        ## Return cluster.id
        kmodes_results <- kmodes(data, modes = K, iter.max = 10, weighted = F)
        return(kmodes_results$cluster)
}

generate_new_f_kmeans <- function(data, cluster.id){
        new_data_lm <- data.frame(CLUS = cluster.id, 
                                  Records = data)
        new_data_lm_done <- ddply((new_data_lm), .(CLUS), colMeans)
        return(DATA = t(new_data_lm_done[,-1]))
}


# Do cross validation, if you don't wish, then change 0.8 --> 1
sel <- sample(1:nrow(bgtrain_inter), nrow(bgtrain_inter)*0.8)
cs.id <- new_features_kmeans(data = mm[,sel], K =100)
new_features_train <- generate_new_f_kmeans(mm[,sel], cs.id)
# This is just for CV
new_features_test <- generate_new_f_kmeans(mm[,-sel], cs.id)

################# LM ################# 

data_df_train <- data.frame(gpa = gpa1[sel], new_features_train)
lm_model1 <- lm(as.character(gpa) ~ ., data = data_df_train)

pred_lm_kc <- predict(lm_model1, newdata = data.frame(new_features_test))
pred_lm_kc[pred_lm_kc>4] <- 4
#pred_lm_kc_14 <- ceiling((pred_lm_kc -0.125) / .25) * 0.25
#hist(pred_lm_kc_14)
mean((pred_lm_kc - gpa1[-sel])^2)

### Formal submission
cs.id2 <- new_features_kmeans(data = mm, K =100)
new_features_train2 <- generate_new_f_kmeans(mm, cs.id2)
data_df_train2 <- data.frame(gpa = gpa1, new_features_train2)
lm_model2 <- lm(as.character(gpa) ~ ., data = data_df_train2)

new_features_test2 <- generate_new_f_kmeans(bgpremm, cs.id2)
pred_lm_kc <- predict(lm_model2, newdata = data.frame(new_features_test2))

pred_lm_kc[pred_lm_kc>4] <- 4
length(pred_lm_kc)
hist(pred_lm_kc)
## end

write.csv(file = "prediction5.csv", pred_lm_kc)

