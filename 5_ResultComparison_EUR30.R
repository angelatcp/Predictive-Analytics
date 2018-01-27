
######################################################################################################################## 
################################################### MODEL COMPARISON BY AUC ############################################ 
################################################### MODEL: LOGISTIC REGRESSION & DEcISION TREE #########################
################################################### FEATURE SELECTION USED FOR LOGISTIC REGRESSION: ####################
###################################################                  PEARSON CORRELATION & STEPWISE ####################

######################################################################################################################## 

################################################### AUC ################################################################

# Model: Logistic Regression - Feature Selection: Pearson Correlation
auc_corr_lrm_train = auc(base_train_unique$reactivated,corr_lrm_predict_train) #calculate auc for train
auc_corr_lrm_test = auc(base_test_unique$reactivated,corr_lrm_predict_test) #calculate auc for test

# Model: Logistic Regression - Feature Selection: Stepwise
auc_step_lrm_train_opt = auc(base_train_unique$reactivated,step_lrm_predict_train_opt) #calculate auc for train
auc_step_lrm_test_opt = auc(base_test_unique$reactivated,step_lrm_predict_test_opt) #calculate auc for test

# Model: Decision Tree
auc_dt_train = auc(base_train_unique$reactivated,dt_predict_train) #calculate auc for train
auc_dt_test = auc(base_test_unique$reactivated,dt_predict_test) #calculate auc for test


#Print out all AUC for easy comparison

print(paste0("AUC for Logistic Regression using Pearson Correlation in train: ", round(auc_corr_lrm_train,4)))
print(paste0("AUC for Logistic Regression using Pearson Correlation in test: ", round(auc_corr_lrm_test,4)))

print(paste0("AUC for Logistic Regression using Stepwise in train: ", round(auc_step_lrm_train_opt,4)))
print(paste0("AUC for Logistic Regression using Stepwise in test: ", round(auc_step_lrm_test_opt,4)))

print(paste0("AUC for Decision Tree in train: ", round(auc_dt_train,4)))
print(paste0("AUC for Decision Tree in test: ", round(auc_dt_test,4)))


