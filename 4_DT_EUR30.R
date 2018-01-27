######################################################################################################################## 
################################################### MODEL BUILDING ##################################################### 
################################################### MODEL: DECISION TREE ###############################################
######################################################################################################################## 

#     Note:We do not have to use feature selection for decision tree 
#          because the decision tree will be able to decide what are the variables that can best do the prediction
#          In addition, decision tree performs better when there are more variables. Hence removing variables from the
#          start will affect the decision tree's performance



################################################### BUILDING DECISION TREE #############################################
#Remove those columns that are not independent variables
dt_vars_remove<- c("reactivated","donorID","gender","language", "zipcode", "total_amount")
dt_vars= names(base_train_unique)[!names(base_train_unique) %in% dt_vars_remove]

# Construct a formula with all the variables
dt_formula<-formula(paste("reactivated","~",paste(dt_vars,collapse="+")))

# Plot the decision tree
# Train
dt_tree_train <- rpart(dt_formula, data = base_train_unique, method="class", control=rpart.control(minsplit=1, minbucket=3, cp=0.001))
plot(dt_tree_train)
text(dt_tree_train)
prp(dt_tree_train)
rpart.plot(dt_tree_train, type=1, extra = 102) 

# Test
dt_tree_test <- rpart(dt_formula, data = base_test_unique, method="class", control=rpart.control(minsplit=1, minbucket=3, cp=0.001))
plot(dt_tree_test)
text(dt_tree_test)
prp(dt_tree_test)
rpart.plot(dt_tree_test, type=1, extra = 102) 



######################################################################################################################## 
################################################### MODEL VALIDATION ################################################### 
######################################################################################################################## 

################################################### AUC ################################################################
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

dt_predict_train = predict(dt_tree_train, base_train_unique) #train dataset
dt_predict_test = predict(dt_tree_test, base_test_unique) #test dataset

auc_dt_train = auc(base_train_unique$reactivated,dt_predict_train) #calculate auc for train
auc_dt_test = auc(base_test_unique$reactivated,dt_predict_test) #calculate auc for test

#Check auc
print(paste0("AUC for Decision Tree in train: ", round(auc_dt_train,4)))
print(paste0("AUC for Decision Tree in test: ", round(auc_dt_test,4)))

#   Note: AUC for decision tree in test dataset is very low likely because the target variable is too little to be splitted into decision tree
#   Note: Low AUC could also be due to overplotting

################################################### CUMULATIVE GAINS CURVE #############################################

dt_pred_train_cg <- prediction(dt_predict_train[,2],base_train_unique$reactivated)
dt_perf_train_cg <- performance(dt_pred_train_cg,"tpr","fpr")
dt_pred_test_cg <- prediction(dt_predict_test[,2],base_test_unique$reactivated)
dt_perf_test_cg <- performance(dt_pred_test_cg ,"tpr","fpr")# Calculate the necessary data (data, y axis, x axis)

plot(dt_perf_train_cg, main="dt_cumulative gains", col="blue")
par(new=TRUE)
plot(dt_perf_test_cg, main="dt_cumulative gains", col="red")

################################################### LIFT ###############################################################

dt_pred_train_li <- prediction(dt_predict_train[,2],base_train_unique$reactivated)
dt_perf_train_li <- performance(dt_pred_train_li,"lift","rpp")
dt_pred_test_li <- prediction(dt_predict_test[,2],base_test_unique$reactivated)
dt_perf_test_li <- performance(dt_pred_test_li ,"lift","rpp")# Calculate the necessary data (data, y axis, x axis)

plot(dt_perf_train_li, main="dt_lift", col="blue")
par(new=TRUE)
plot(dt_perf_test_li, main="dt_lift", col="red")

################################################### CUMULATIVE RESPONSE CURVE ##########################################

dt_avgtar_train_li = sum(base_train_unique$reactivated)/nrow(base_train_unique) # calculate the average for train data
dt_avgtar_test_li = sum(base_test_unique$reactivated)/nrow(base_test_unique) # calculate the average for test data
dt_perf_train_li@y.values[[1]] = dt_perf_train_li@y.values[[1]] * dt_avgtar_train_li # calculate how many times above average for train
dt_perf_test_li@y.values[[1]] = dt_perf_test_li@y.values[[1]] * dt_avgtar_test_li # calculate how many times above average for test

plot(dt_perf_train_li,main="dt_Cumulative response",col="blue")
par(new=TRUE)
plot(dt_perf_test_li, main="dt_Cumulative response", col="red")