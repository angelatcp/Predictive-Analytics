######################################################################################################################## 
################################################### BUSINESS CASE ###################################################### 
################################################### MODEL SELECTED: LOGISTIC REGRESSION ################################
################################################### FEATURE SELECTION: STEPWISE ########################################
######################################################################################################################## 


################## Find the optimal percentage of population to target to obtain maximum profit ########################

# First we bind the test predictions with the test data
data_test_bind = cbind(base_test_unique,step_lrm_predict_test_opt)
# Then we sort this data according to the prediction value from highest to lowest
data_test_bind = data_test_bind[order(-data_test_bind$step_lrm_predict_test_opt),]

# Label the number of observations in the sorted data that has the highest probability to be reactivated to the lowest probability
data_test_bind$obs = c(1:length(data_test_bind$step_lrm_predict_test_opt))

# Calculate the cumulative proportion of population
data_test_bind$popn_pct = data_test_bind$obs/length(data_test_bind$step_lrm_predict_test_opt)
for(i in length(data_test_bind$obs)-1) {
  data_test_bind$popn_pct[i+1] = data_test_bind$obs[i+1]/length(data_test_bind$step_lrm_predict_test_opt)
}

# Find the cumulative profit
data_test_bind$maxprofit = cumsum(data_test_bind$step_lrm_predict_test_opt*(Expected_Donation - Cost_Per_Letter) - (1 - data_test_bind$step_lrm_predict_test_opt)*Cost_Per_Letter)

# Find the percentage of population we should target to obtain the maximum profit
target_proportion = data_test_bind$popn_pct[which.max(data_test_bind$maxprofit)]

# Find the maximum profit 
maxprofit = max(data_test_bind$maxprofit)

# Plot the profit curve to find the percentage of population we should target to obtain the maximum profit
plot(data_test_bind$popn_pct,data_test_bind$maxprofit, 
     main = "Profit Curve", xlab = "% of population (by decreasing probabilty)", 
     ylab = "Cumulative Profit", cex=0.1, col = "gray") 
par(new=TRUE)
points(x = target_proportion, y = maxprofit, type = "p", pch = 19, col = "red3", bg = "red", cex = 1)
par(new=TRUE)
abline(a = NULL, b = NULL, h = NULL, v = target_proportion, reg = NULL,coef = NULL, untf = FALSE, col = "red3")
text(target_proportion,maxprofit,label=paste("EUR",as.character(round(maxprofit,0)),"with",as.character(round(target_proportion,2)),"of population",collapse=""),col="red3", pos=4)
axis(side=2, at=seq(-1500, 800, by=250))

# Now we separate the customers into top (targeting) and bottom (not targeting)
split = floor(target_proportion*nrow(data_test_bind))
top_test = data_test_bind[c(1:(split)),]
bottom_test = data_test_bind[c((split+1):nrow(data_test_bind)),]


################################################### PROFILING ##################################################

# Profile for d_language
#   Purpose: To identify if the donors are dutch or french speakers.
#            It could be useful for Business to send letters in that specific language.
#            Since Belgium has a clear distinction between dutch and french speakers geographically,
#            it could be useful for Business to know where they should send the letters to.
par(mfrow = c(1,1)) 
table_bottom_language= table(bottom_test$d_language)  
table_bottom_language= table_bottom_language[1:2]  
table_toplanguage= table(top_test$d_language)  
toplanguage_freq = table_toplanguage / nrow(top_test)  
bottomlanguage_freq = table_bottom_language / nrow(bottom_test)  
matrix_language = rbind(toplanguage_freq, bottomlanguage_freq)  
colnames(matrix_language) = c("Dutch", "French")  

bar_language = barplot(matrix_language,main = "Language breakdown", beside=T,   
                       col=c("thistle","lightsteelblue1"),xlab= "Language", ylab = "Percentage", ylim=c(0,1))  
legend("topright", c("Top Test","Bottom Test"), pch=15,   
       col=c("thistle","lightsteelblue1"),   
       bty="n") 
text(bar_language,0,round(matrix_language,2),matrix_language, cex=1,pos=3)  

# Profile for d_gender 
#   Purpose: To identify the gender distribution
#           It could be useful for business to know so that they may custom wording and tonality of letter
par(mfrow = c(1,1)) 
table_bottom_gender= table(bottom_test$d_gender)  
table_topgender= table(top_test$d_gender)  

topgender_freq = table_topgender / nrow(top_test)  
bottomgender_freq = table_bottom_gender / nrow(bottom_test)  
matrix_gender = rbind(topgender_freq,bottomgender_freq)  
colnames(matrix_gender) = c("M","F","C","S","U")  

bp <- barplot(matrix_gender,main = "Gender breakdown", beside=T,   
              col=c("thistle","lightsteelblue1"),xlab= "Gender", ylab = "Percentage", ylim= c(0,0.6))  

text(bp,0,round(matrix_gender,2),matrix_gender,cex=1,pos=3)   

legend("top", c("Top Test","Bottom Test"), pch=15,   
       col=c("thistle","lightsteelblue1"),   
       bty="n")  

# Profile for c_NumYr_15yr 
#   Displays the difference between the top and bottom for c_NumYr_15yr  
#   Purpose: To identify if the top has donated more times as compared to bottom in the past 15 years
#            It could be useful for Business to know that there is higher probability of receiving donations from the top
boxplot(top_test$c_NumYr_15yr, bottom_test$c_NumYr_15yr,  
        ylab ="Years", xlab ="Customers", main = "Number of years donors have donated in the past 15 years",  
        names = c("Top Test", "Bottom Test")) 

# Profile for c_month
#   Purpose: Is there a month that has higher chance of receiving donation?
#            It could be useful for Business to launch campaign at those months instead.
par(mfrow = c(1,2)) 
top_c_month <- table(top_test$c_month)
barplot(top_c_month, main="Top Test", 
        xlab="Number of donations by month", col = "thistle", ylim=c(0,4000))
axis(side=2, at=seq(0, 4000, by=500))
bottom_c_month <- table(bottom_test$c_month)
barplot(bottom_c_month, main="Bottom Test", 
        xlab="Number of donations by month", col = "lightsteelblue1", ylim=c(0,4000))
axis(side=2, at=seq(0, 4000, by=500))

# Profile for c_days_since_largest_donation 
#   Purpose: Compare the recency between top and bottom to see if the largest donation amount from the top 
#            was given more recently than the bottom
#            It could be useful for business to know that there is higher chance of these top donating
par(mfrow = c(1,2)) 
top_test$c_days_since_largest_donation = as.numeric(top_test$c_days_since_largest_donation) 
bottom_test$c_days_since_largest_donation = as.numeric(bottom_test$c_days_since_largest_donation) 
hist(top_test$c_days_since_largest_donation, xlab = "Number of Days", main = "Top Test", col = "thistle") 
hist(bottom_test$c_days_since_largest_donation, xlab = "Number of Days", main = "Bottom Test", col = "lightsteelblue1") 
