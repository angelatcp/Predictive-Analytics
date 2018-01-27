######################################################################################################################## 
################################################### MODEL BUILDING ##################################################### 
################################################### MODEL: LOGISTIC REGRESSION #########################################
################################################### FEATURE SELECTION: STEPWISE ########################################
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
#Remove those columns that are not independent variables
step_lrm_vars_remove<- c("reactivated","donorID","gender","language", "zipcode", "total_amount")
step_lrm_vars= names(base_train_unique)[!names(base_train_unique) %in% step_lrm_vars_remove]
step_lrm_selected = c()

#Construct a logistic regression model with no variables
step_lrm_model = glm(reactivated ~ 1,data=base_train_unique,family=binomial)

#Construct a formula with all the variables
formula<-formula(paste("reactivated","~",paste(step_lrm_vars,collapse="+")))

#Loop over the steps
for(i in c(1:length(step_lrm_vars))){
  #calculate AIC of each model
  info = add1(step_lrm_model,scope=formula,data=base_train_unique)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  step_lrm_selected = append(step_lrm_selected,v)
  formulanew = formula(paste("reactivated","~",paste(step_lrm_selected,collapse = "+")))
  step_lrm_model = glm(formulanew,data=base_train_unique,family=binomial)
  print(v)
}

auc_step_lrm_train = rep(0,length(step_lrm_selected)-1)
auc_step_lrm_test = rep(0,length(step_lrm_selected)-1)

for(i in c(1:(length(step_lrm_selected)-1))){
  vars = step_lrm_selected[0:i+1]
  print(vars)
  formula<-paste("reactivated","~",paste(vars,collapse="+"))
  step_lrm_model<-glm(formula,data=base_train_unique,family="binomial")	
  step_lrm_predict_train <-predict(step_lrm_model,newdata=base_train_unique,type="response")
  step_lrm_predict_test<-predict(step_lrm_model,newdata=base_test_unique,type="response")
  auc_step_lrm_train[i] = auc(base_train_unique$reactivated,step_lrm_predict_train)
  auc_step_lrm_test[i] = auc(base_test_unique$reactivated,step_lrm_predict_test)
} 

#Plotting the auc curve to determine the optimal number of variables:
plot(auc_step_lrm_train, main="AUC", col="blue")
par(new=TRUE)
plot(auc_step_lrm_test, main="AUC",col="red")

#Select the model with optimal number of variables:
#   From the graph, we will choose 5 variables to be the optimal
step_lrm_finalvariables = step_lrm_selected[c(0:5)]
formula<-paste("reactivated","~",paste(step_lrm_finalvariables,collapse="+"))
step_lrm_model<-glm(formula,data=base_train_unique,family="binomial")	
step_lrm_predict_train_opt<-predict(step_lrm_model,newdata=base_train_unique,type="response")
step_lrm_predict_test_opt<-predict(step_lrm_model,newdata=base_test_unique,type="response")


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

auc_step_lrm_train_opt = auc(base_train_unique$reactivated,step_lrm_predict_train_opt)
auc_step_lrm_test_opt = auc(base_test_unique$reactivated,step_lrm_predict_test_opt)

#Check auc
print(paste0("AUC for Logistic Regression using Stepwise in train: ", round(auc_step_lrm_train_opt,4)))
print(paste0("AUC for Logistic Regression using Stepwise in test: ", round(auc_step_lrm_test_opt,4)))

################################################### CUMULATIVE GAINS CURVE #############################################

step_lrm_pred_train_cg <- prediction(step_lrm_predict_train_opt,base_train_unique$reactivated)
step_lrm_perf_train_cg <- performance(step_lrm_pred_train_cg,"tpr","fpr")
step_lrm_pred_test_cg <- prediction(step_lrm_predict_test_opt,base_test_unique$reactivated)
step_lrm_perf_test_cg <- performance(step_lrm_pred_test_cg ,"tpr","fpr")# Calculate the necessary data (data, y axis, x axis)

plot(step_lrm_perf_train_cg, main="step_lm_cumulative gains", col="blue")
par(new=TRUE)
plot(step_lrm_perf_test_cg, main="step_lm_cumulative gains", col="red")

################################################### LIFT ###############################################################

step_lrm_pred_train_li <- prediction(step_lrm_predict_train_opt,base_train_unique$reactivated)
step_lrm_perf_train_li <- performance(step_lrm_pred_train_li,"lift","rpp")
step_lrm_pred_test_li <- prediction(step_lrm_predict_test_opt,base_test_unique$reactivated)
step_lrm_perf_test_li <- performance(step_lrm_pred_test_li ,"lift","rpp")# Calculate the necessary data (data, y axis, x axis)

plot(step_lrm_perf_train_li, main="step_lm_lift", col="blue") 
par(new=TRUE)
plot(step_lrm_perf_test_li, main="step_lm_lift", col="red")

################################################### CUMULATIVE RESPONSE CURVE ##########################################

step_lrm_avgtar_train_li = sum(base_train_unique$reactivated)/nrow(base_train_unique) # calculate the average for train data
step_lrm_avgtar_test_li = sum(base_test_unique$reactivated)/nrow(base_test_unique) # calculate the average for test data
step_lrm_perf_train_li@y.values[[1]] = step_lrm_perf_train_li@y.values[[1]] * step_lrm_avgtar_train_li # calculate how many times above average for train
step_lrm_perf_test_li@y.values[[1]] = step_lrm_perf_test_li@y.values[[1]] * step_lrm_avgtar_test_li # calculate how many times above average for test

plot(step_lrm_perf_train_li,main="step_lm_Cumulative response",col="blue")
par(new=TRUE)
plot(step_lrm_perf_test_li, main="step_lm_Cumulative response", col="red")

