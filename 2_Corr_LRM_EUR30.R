######################################################################################################################## 
################################################### MODEL BUILDING #####################################################
################################################### MODEL: LOGISTIC REGRESSION #########################################
################################################### FEATURE SELECTION: PEARSON CORRELATION #############################
######################################################################################################################## 


#Cast variables as numeric for train and test datasets
base_train_unique$d_gender= as.numeric(base_train_unique$d_gender)
base_train_unique$d_language= as.numeric(base_train_unique$d_language)
base_train_unique$c_month= as.numeric(base_train_unique$c_month)
base_train_unique$c_inactive_days= as.numeric(base_train_unique$c_inactive_days)
base_train_unique$c_days_since_largest_donation= as.numeric(base_train_unique$c_days_since_largest_donation)

base_test_unique$d_gender= as.numeric(base_test_unique$d_gender)
base_test_unique$d_language= as.numeric(base_test_unique$d_language)
base_test_unique$c_month= as.numeric(base_test_unique$c_month)
base_test_unique$c_inactive_days= as.numeric(base_test_unique$c_inactive_days)
base_test_unique$c_days_since_largest_donation= as.numeric(base_test_unique$c_days_since_largest_donation)

################################################### FEATURE SELECTION ##################################################
#Remove those columns that are not used for modelling
corr_lrm_vars_remove<- c("reactivated","donorID","gender","language", "zipcode", "total_amount")
corr_lrm_vars= names(base_train_unique)[!names(base_train_unique) %in% corr_lrm_vars_remove]
corr_lrm_selected = c()

#Loops through each independent variable to compute the correlation and its pvalue
for(v in corr_lrm_vars){
  print(v)
  cortest = cor.test(base_train_unique[,c(v)],base_train_unique[,c("reactivated")],method="pearson") #calculates correlation
  pvalue = cortest$p.value #calculates pvalue
  print(pvalue)
  if(pvalue<0.01){ 
    corr_lrm_selected = c(corr_lrm_selected,v) 
    #save variables that have pvalue < 0.01 into selected vector - cannot accept null hypothesis, accept alternative hypothesis
    #variables saved in selected vector have association with target variable - reactivated
  }
}

################################################### BUILD LOGISTIC REGRESSION ##########################################

corr_lrm_f = paste("reactivated~", paste(corr_lrm_selected,collapse="+")) #construct the strings consisting target variables and independent variables
corr_lrmodel = glm(as.formula(corr_lrm_f),data=base_train_unique,family="binomial") #build logistic regression model
corr_lrm_predict_train = predict(corr_lrmodel,newdata=base_train_unique,type="response") #train dataset
corr_lrm_predict_test = predict(corr_lrmodel,newdata=base_test_unique,type="response") #test dataset


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

auc_corr_lrm_train = auc(base_train_unique$reactivated,corr_lrm_predict_train) #calculate auc for train
auc_corr_lrm_test = auc(base_test_unique$reactivated,corr_lrm_predict_test) #calculate auc for test

#Check auc
print(paste0("AUC for Logistic Regression using Pearson Correlation in train: ", round(auc_corr_lrm_train,4)))
print(paste0("AUC for Logistic Regression using Pearson Correlation in test: ", round(auc_corr_lrm_test,4)))

################################################### CUMULATIVE GAINS CURVE #############################################

corr_lrm_pred_train_cg = prediction(corr_lrm_predict_train,base_train_unique$reactivated)
corr_lrm_perf_train_cg = performance(corr_lrm_pred_train_cg,"tpr","fpr")
corr_lrm_pred_test_cg = prediction(corr_lrm_predict_test,base_test_unique$reactivated)
corr_lrm_perf_test_cg = performance(corr_lrm_pred_test_cg ,"tpr","fpr")# Calculate the necessary data (data, y axis, x axis)

plot(corr_lrm_perf_train_cg, main="corr_lm_cumulative gains", col="blue")
par(new=TRUE)
plot(corr_lrm_perf_test_cg, main="corr_lm_cumulative gains", col="red")

################################################### LIFT ###############################################################

corr_lrm_pred_train_li = prediction(corr_lrm_predict_train,base_train_unique$reactivated)
corr_lrm_perf_train_li = performance(corr_lrm_pred_train_li,"lift","rpp")
corr_lrm_pred_test_li = prediction(corr_lrm_predict_test,base_test_unique$reactivated)
corr_lrm_perf_test_li = performance(corr_lrm_pred_test_li ,"lift","rpp")# Calculate the necessary data (data, y axis, x axis)

plot(corr_lrm_perf_train_li, main="corr_lm_lift", col="blue")
par(new=TRUE)
plot(corr_lrm_perf_test_li, main="corr_lm_lift", col="red")

################################################### CUMULATIVE RESPONSE CURVE ##########################################

corr_lrm_avgtar_train_li = sum(base_train_unique$reactivated)/nrow(base_train_unique) # calculate the average for train data
corr_lrm_avgtar_test_li = sum(base_test_unique$reactivated)/nrow(base_test_unique) # calculate the average for test data
corr_lrm_perf_train_li@y.values[[1]] = corr_lrm_perf_train_li@y.values[[1]] * corr_lrm_avgtar_train_li # calculate how many times above average for train
corr_lrm_perf_test_li@y.values[[1]] = corr_lrm_perf_test_li@y.values[[1]] * corr_lrm_avgtar_test_li # calculate how many times above average for test

plot(corr_lrm_perf_train_li,main="corr_lm_Cumulative response",col="blue")
par(new=TRUE)
plot(corr_lrm_perf_test_li, main="corr_lm_Cumulative response", col="red")