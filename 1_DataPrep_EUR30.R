################################################### LIBRARIES #################################################################
install.packages("dplyr")  
install.packages("dummies")  
install.packages("stringr") 
install.packages("pROC")
install.packages("ROCR")
install.packages("rpart")
install.packages("RColorBrewer")
install.packages('rpart.plot') 

library(dplyr)  
library(dummies) 
library(stringr) 
library(pROC)
library(ROCR)
library(rpart)
library(RColorBrewer)
library(rpart.plot) 

#Reads the raw data  

donor = read.table("C:/Users/ctan/Documents/Descriptive & Predictive Model/Project/donors.csv",sep=";",header = TRUE)  
train = read.table("C:/Users/ctan/Documents/Descriptive & Predictive Model/Project/campaign20130411.csv",sep=";",header = TRUE)  
test = read.table("C:/Users/ctan/Documents/Descriptive & Predictive Model/Project/campaign20140115.csv",sep=";",header = TRUE)  
gifts = read.table("C:/Users/ctan/Documents/Descriptive & Predictive Model/Project/gifts.csv",sep=";",header = TRUE)  

################################################################################################################################ 
################################################### DATA PREPARATION ###########################################################  
################################################################################################################################  

################################################### CAMPAIGN DATASETs ########################################################## 
#Aggregate test and train data by donorID  
#Purpose: To obtain a list of unique donors with aggregated donated amount 

tbl_df(train)  
tbl_df(test)  
train_aggregate = train %>% group_by(donorID) %>% summarize(total_amount = sum(amount))   
test_aggregate = test %>% group_by(donorID) %>% summarize(total_amount = sum(amount))  

#Create target variable 
#   value 1 - if donors have donated more than EUR 30 
#   value 0 - if donors have donated less than or equal to EUR 30  

Expected_Donation = 30
Cost_Per_Letter = 0.5

reactivated <- ifelse(train_aggregate$total_amount > Expected_Donation, 1, 0) 
train_aggregate_tar = cbind(train_aggregate,reactivated ) #target variable for train 

reactivated <- ifelse(test_aggregate$total_amount > Expected_Donation, 1, 0) 
test_aggregate_tar = cbind(test_aggregate,reactivated ) #target variable for test 




################################################### GIFTS DATASET ############################################################## 
#Subset gifts data 
#Purpose: To identify only inactive donors 
#Reactivation Campaign Date is at point 0 years
#Active donors: donation made between the last 2 years before reactivation campaign date (-2 years to 0 years)
#Inactive donors: 
#   a.no donation made between the last 2 years before reactivation campaign date
#   b.donation made within 15 years (-15 years to -2 years)

tbl_df(gifts) 
gifts$date = as.Date(gifts$date, format = "%d/%m/%Y") #Read date value as date 

#Keep gifts data only for inactive donors in train dataset (-15 years to -2 years)
date_gift_train_start = as.Date("1996-04-11") 
date_gift_train_end = as.Date("2011-04-11") 
gifts_train_inactive =  filter(gifts, date < date_gift_train_end & date >= date_gift_train_start ) 

#Keep gifts data only for inactive donors in test dataset (-15 years to -2 years)
date_gift_test_start = as.Date("1997-01-15") 
date_gift_test_end = as.Date("2012-01-15") 
gifts_test_inactive =  filter(gifts, date < date_gift_test_end & date >= date_gift_test_start ) 

#Create a dataset of active users who have made donation in the last 2 years for train and test datasets
#Purpose: To remove them from our dataset that contains the target variable before joining aggregated gifts and revised donor datasets
date_gift_train_start_act = as.Date("2011-04-11") 
date_gift_train_end_act = as.Date("2013-04-11") 
gifts_train_active =  filter(gifts, date < date_gift_train_end_act & date >= date_gift_train_start_act )

date_gift_test_start_act = as.Date("2012-01-15") 
date_gift_test_end_act = as.Date("2014-01-15") 
gifts_test_active =  filter(gifts, date < date_gift_test_end_act & date >= date_gift_test_start_act ) 



##########Create New Variables Using Gifts Dataset

#As our gift_train_inactive and gift_test_inactive have already been subset to contain only 15 years of data
#We will only need to specify date range for 10 years and 5 years for train and test dataset before creating new variables
#Purpose: to be used for creating below variables that we are exploring with 15, 10 and 5 years
#   Variable: Number of donations made by each donor (Distinct CampID)
#   Variable: Average amount of donations made by each donor
#   Variable: Total amount of donations made by each donor
#       Date range - 10 years
date_gift_train_10yrstart = as.Date("2001-04-11") 
date_gift_train_10yrend = as.Date("2011-04-11")

date_gift_test_10yrstart = as.Date("2002-01-15") 
date_gift_test_10yrend = as.Date("2012-01-15")

#       Date range - 5 years
date_gift_train_5yrstart = as.Date("2006-04-11") 
date_gift_train_5yrend = as.Date("2011-04-11") 

date_gift_test_5yrstart = as.Date("2007-01-15") 
date_gift_test_5yrend = as.Date("2012-01-15")

#New Variable - Number of donations made by each donor 
#   If a donor has made multiple donations in a campaign, it will be counted as multiple donation
#   Within 15 years
Freq_donation_train_15yr = gifts_train_inactive %>% 
                           group_by(donorID) %>% 
                           summarise(c_Freq_donation_15yr = n())

Freq_donation_test_15yr = gifts_test_inactive %>% 
                          group_by(donorID) %>% 
                          summarise(c_Freq_donation_15yr = n())

#   Within 10 years
Freq_donation_train_10yr = gifts_train_inactive %>% 
                           filter(date >= date_gift_train_10yrstart & date < date_gift_train_10yrend) %>%
                           group_by(donorID) %>% 
                           summarise(c_Freq_donation_10yr = n())

Freq_donation_test_10yr = gifts_test_inactive %>% 
                          filter(date >= date_gift_test_10yrstart & date < date_gift_test_10yrend) %>%
                          group_by(donorID) %>% 
                          summarise(c_Freq_donation_10yr = n())

#   Within 5 years
Freq_donation_train_5yr = gifts_train_inactive %>% 
                          filter(date >= date_gift_train_5yrstart & date < date_gift_train_5yrend) %>%
                          group_by(donorID) %>% 
                          summarise(c_Freq_donation_5yr = n())

Freq_donation_test_5yr = gifts_test_inactive %>% 
                         filter(date >= date_gift_test_5yrstart & date < date_gift_test_5yrend) %>%
                         group_by(donorID) %>% 
                         summarise(c_Freq_donation_5yr = n())

#New Variable - Average amount of donations made by each donor
#   If a donor has made multiple donations in a campaign, we will sum the amount in each campID for each donorID
#   Each CampID for a donor should only contain one line
#   Then, we will calculate the average amount of donations made by each donor across campID

#   Average 15 years
AvgAmt_donation_train_15yr = gifts_train_inactive %>% 
                             group_by(donorID, campID) %>% 
                             summarise(AmtPerCamp_15yr = sum(amount)) %>% 
                             group_by(donorID) %>% 
                             summarise(c_AvgAmt_15yr = mean(AmtPerCamp_15yr))

AvgAmt_donation_test_15yr = gifts_test_inactive %>% 
                            group_by(donorID, campID) %>% 
                            summarise(AmtPerCamp_15yr = sum(amount)) %>% 
                            group_by(donorID) %>% 
                            summarise(c_AvgAmt_15yr = mean(AmtPerCamp_15yr))

#   Average 10 years
AvgAmt_donation_train_10yr = gifts_train_inactive %>% 
                            filter(date >= date_gift_train_10yrstart & date < date_gift_train_10yrend) %>%
                            group_by(donorID, campID) %>% 
                            summarise(AmtPerCamp_10yr = sum(amount)) %>% 
                            group_by(donorID) %>% 
                            summarise(c_AvgAmt_10yr = mean(AmtPerCamp_10yr))

AvgAmt_donation_test_10yr = gifts_test_inactive %>% 
                            filter(date >= date_gift_test_10yrstart & date < date_gift_test_10yrend) %>%
                            group_by(donorID, campID) %>% 
                            summarise(AmtPerCamp_10yr = sum(amount)) %>% 
                            group_by(donorID) %>% 
                            summarise(c_AvgAmt_10yr = mean(AmtPerCamp_10yr))

#   Average 5 years
AvgAmt_donation_train_5yr = gifts_train_inactive %>% 
                            filter(date >= date_gift_train_5yrstart & date < date_gift_train_5yrend) %>%
                            group_by(donorID, campID) %>% 
                            summarise(AmtPerCamp_5yr = sum(amount)) %>% 
                            group_by(donorID) %>% 
                            summarise(c_AvgAmt_5yr = mean(AmtPerCamp_5yr))

AvgAmt_donation_test_5yr = gifts_test_inactive %>% 
                           filter(date >= date_gift_test_5yrstart & date < date_gift_test_5yrend) %>%
                           group_by(donorID, campID) %>% 
                           summarise(AmtPerCamp_5yr = sum(amount)) %>% 
                           group_by(donorID) %>% 
                           summarise(c_AvgAmt_5yr = mean(AmtPerCamp_5yr))

#New Variable - Total amount of donations made by each donor

#   Within 15 years
TotAmt_donation_train_15yr = gifts_train_inactive %>% 
                             group_by(donorID) %>% 
                             summarise(c_TotAmt_15yr = sum(amount))

TotAmt_donation_test_15yr = gifts_test_inactive %>% 
                            group_by(donorID) %>% 
                            summarise(c_TotAmt_15yr = sum(amount))

#   Within 10 years
TotAmt_donation_train_10yr = gifts_train_inactive %>% 
                             filter(date >= date_gift_train_10yrstart & date < date_gift_train_10yrend) %>%
                             group_by(donorID) %>% 
                             summarise(c_TotAmt_10yr = sum(amount)) 

TotAmt_donation_test_10yr = gifts_test_inactive %>% 
                            filter(date >= date_gift_test_10yrstart & date < date_gift_test_10yrend) %>%
                            group_by(donorID) %>% 
                            summarise(c_TotAmt_10yr = sum(amount))

#   Within 5 years
TotAmt_donation_train_5yr = gifts_train_inactive %>% 
                            filter(date >= date_gift_train_5yrstart & date < date_gift_train_5yrend) %>%
                            group_by(donorID) %>% 
                            summarise(c_TotAmt_5yr = sum(amount)) 

TotAmt_donation_test_5yr = gifts_test_inactive %>% 
                           filter(date >= date_gift_test_5yrstart & date < date_gift_test_5yrend) %>%
                           group_by(donorID) %>% 
                           summarise(c_TotAmt_5yr = sum(amount)) 

#New Variable - Number of years a donor has given gifts in the past 15 years
NumYrs_donation_train_15yr = gifts_train_inactive %>% 
                             mutate(year = format(date, "%Y")) %>%
                             group_by(donorID) %>% 
                             summarise(c_NumYr_15yr = n_distinct(year)) 

NumYrs_donation_test_15yr = gifts_test_inactive %>% 
                            mutate(year = format(date, "%Y")) %>%
                            group_by(donorID) %>% 
                            summarise(c_NumYr_15yr = n_distinct(year)) 


#New Variable - Number of inactive days from last date of donation to the actual campaign dates
#New Variable - Amount of last donation per donor. In event where donor donated more than once on the last day of donation, we will take the total sum of donation for that donor on the last day
#New Variable - Month of last donation per donor

gifts_train_inactive_calc = gifts_train_inactive %>%  
                            group_by(donorID) %>%  
                            filter(date == max(date)) %>% 
                            mutate(c_inactive_days = as.Date("2013-04-11") - date, c_month = format(date,"%m")) %>%  
                            rename(last_donation = amount) %>% 
                            select(donorID, last_donation, c_inactive_days, c_month) %>%
                            group_by(donorID, c_inactive_days, c_month) %>%  
                            summarise(c_last_donation = sum(last_donation))

gifts_test_inactive_calc = gifts_test_inactive %>%  
                           group_by(donorID) %>%  
                           filter(date == max(date)) %>% 
                           mutate(c_inactive_days = as.Date("2014-01-15") - date, c_month = format(date,"%m")) %>%  
                           rename(last_donation = amount) %>% 
                           select(donorID, last_donation, c_inactive_days, c_month)  %>% 
                           group_by(donorID, c_inactive_days, c_month) %>%  
                           summarise(c_last_donation = sum(last_donation))

#New Variable - Days since the largest donation per donor 
#   If there are more than one donation per donor with the same amount, we will take the observation with the latest date
#New Variable - Amount of the largest donation per donor 

gifts_train_inactive_dsld = gifts_train_inactive %>% 
                            group_by(donorID) %>%
                            filter(amount == max(amount)) %>%
                            mutate(c_days_since_largest_donation = as.Date("2013-04-11") - date) %>%
                            filter(date == max(date)) %>%
                            rename(c_max_donation = amount) %>%  
                            select(donorID, c_max_donation, c_days_since_largest_donation) 
                 
gifts_test_inactive_dsld = gifts_test_inactive %>% 
                           group_by(donorID) %>%
                           filter(amount == max(amount)) %>%
                           mutate(c_days_since_largest_donation = as.Date("2014-01-15") - date) %>%
                           filter(date == max(date)) %>% 
                           rename(c_max_donation = amount) %>%                          
                           select(donorID, c_max_donation, c_days_since_largest_donation)

#New Variable - One time donor vs multiple times donor in past 15 years  

Multi_donation_train = gifts_train_inactive %>% 
                       group_by(donorID) %>%
                       summarise(Cnt_15yr = n()) %>%
                       mutate(Multi_15yr = ifelse(Cnt_15yr > 1, 1, 0)) %>%
                       select(donorID, Multi_15yr)

Multi_donation_test = gifts_test_inactive %>% 
                      group_by(donorID) %>%
                      summarise(Cnt_15yr = n()) %>%
                      mutate(Multi_15yr = ifelse(Cnt_15yr > 1, 1, 0)) %>%
                      select(donorID, Multi_15yr)


#Aggregate the gift data using unique donor for train and test set 
#Purpose: To obtain one unique observation in gift dataset per donor with newly created variables
#   To obtain gift Dataset with unique donorID before merging with variables
gifts_train_uniqueid = gifts_train_inactive %>% 
                       distinct(donorID) %>% 
                       select(donorID)

gifts_test_uniqueid = gifts_test_inactive %>% 
                      distinct(donorID) %>% 
                      select(donorID)

#   Merge in the newly created variables for gift datasets for both train and test
gifts_train_aggregate = left_join(gifts_train_uniqueid,Freq_donation_train_15yr, by = "donorID") %>%
                        left_join(Freq_donation_train_10yr, by = "donorID") %>%
                        left_join(Freq_donation_train_5yr, by = "donorID") %>%
                        left_join(AvgAmt_donation_train_15yr, by = "donorID") %>%
                        left_join(AvgAmt_donation_train_10yr, by = "donorID") %>%
                        left_join(AvgAmt_donation_train_5yr, by = "donorID") %>%
                        left_join(TotAmt_donation_train_15yr, by = "donorID") %>%
                        left_join(TotAmt_donation_train_10yr, by = "donorID") %>%
                        left_join(TotAmt_donation_train_5yr, by = "donorID") %>%
                        left_join(NumYrs_donation_train_15yr, by = "donorID") %>%
                        left_join(gifts_train_inactive_calc, by = "donorID") %>%
                        left_join(gifts_train_inactive_dsld, by = "donorID") %>%
                        left_join(Multi_donation_train, by = "donorID") 

gifts_test_aggregate = left_join(gifts_test_uniqueid,Freq_donation_test_15yr, by = "donorID") %>%
                       left_join(Freq_donation_test_10yr, by = "donorID") %>%
                       left_join(Freq_donation_test_5yr, by = "donorID") %>%
                       left_join(AvgAmt_donation_test_15yr, by = "donorID") %>%
                       left_join(AvgAmt_donation_test_10yr, by = "donorID") %>%
                       left_join(AvgAmt_donation_test_5yr, by = "donorID") %>%
                       left_join(TotAmt_donation_test_15yr, by = "donorID") %>%
                       left_join(TotAmt_donation_test_10yr, by = "donorID") %>%
                       left_join(TotAmt_donation_test_5yr, by = "donorID") %>%
                       left_join(NumYrs_donation_test_15yr, by = "donorID") %>%
                       left_join(gifts_test_inactive_calc, by = "donorID") %>%
                       left_join(gifts_test_inactive_dsld, by = "donorID") %>%
                       left_join(Multi_donation_test, by = "donorID") 


################################################### DONOR ############################################################## 

#Create dummy values for gender, language and group zipcode by 13 regions 
d_gender<- c("M", "F","C","S","U")  
d_gendermap<- c("1", "2", "3","4","5")  
donor$d_gender<- d_gendermap[match(donor$gender, d_gender)]  

d_language<- c("N", "F")  
d_languagemap<- c("1", "2")  
donor$d_language<- d_languagemap[match(donor$language, d_language)]  

donor$zipcode = as.character(donor$zipcode) 
donor$zipcode = as.numeric(donor$zipcode) 
d_lowerzip = c(1000,1300,1500,2000,3000,3500, 4000, 5000,6000,6600,7000,8000,9000)
d_upperzip = c(1299,1499,1999,2999,3499,3999,4999,5999,6599,6999,7999,8999,9999)
for (i in 1:length(d_upperzip)) {
  donor$d_zipcode = replace(donor$d_zipcode, (donor$zipcode >= d_lowerzip[i] & donor$zipcode <= d_upperzip[i]), i)
}

#Deleted region column because there are too many missing values and hence, not useful for our analysis.
#Removed the 2 NA's in zipcode column and delete one line of zero zipcode. 

donor = subset(donor, select = -c(region))
donor = subset(donor, zipcode != 0) 


###################################### MERGE TARGET, AGGREGATED GIFTS, REVISED DONORS ##################################

#Exclude the donors who have been active (donated) in the last 2 years, keep only inactive donors
train_aggregate_tar_inactive = train_aggregate_tar[!(train_aggregate_tar$donorID %in% gifts_train_active$donorID),]
test_aggregate_tar_inactive = test_aggregate_tar[!(test_aggregate_tar$donorID %in% gifts_test_active$donorID),]

#Create base table with target variable, amount donated in reactivation campaign, variables from aggregated gifts and revised donor datasets

gifts_train_aggregate2 = left_join(gifts_train_aggregate,donor, by = "donorID")
base_train = inner_join(train_aggregate_tar_inactive,gifts_train_aggregate2, by = "donorID")
           
gifts_test_aggregate2 = left_join(gifts_test_aggregate,donor, by = "donorID")
base_test = inner_join(test_aggregate_tar_inactive,gifts_test_aggregate2, by = "donorID")
          

##################################################### DATA CLEANSING ###################################################

##########Deal with NA Values
#Omit the NA values in d_zipcode for train dataset
base_train <- subset(base_train, d_zipcode != 0) 

#Omit the NA values in d_zipcode for test dataset
base_test <- subset (base_test, d_zipcode != 0) 

#Replace NA values with median
#   For variable - Multi_15yr
#   As it is having either a 0 (one time donor) or 1 (multiple times donor), we will replace with median of 1
#   As there are only around 500 NA values, replacing with median of 1 will not skew the data

#   Train   
Train_data_median = median(base_train$Multi_15yr, na.rm = TRUE) 
base_train$Multi_15yr[is.na(base_train$Multi_15yr)] = Train_data_median 
summary(base_train$Multi_15yr) #check that there is no more NA values

#   Test 
Test_data_median <- median(base_test$Multi_15yr, na.rm = TRUE)  
base_test$Multi_15yr[is.na(base_test$Multi_15yr)] = Test_data_median  
summary(base_test$Multi_15yr) #check that there is no more NA values


#Replace NA values with median for base Train 

# Median for c_Freq_donation_15yr 
c_Freq_donation_15yr.trainmedian <- median(base_train$c_Freq_donation_15yr, na.rm = TRUE) 
base_train$c_Freq_donation_15yr[is.na(base_train$c_Freq_donation_15yr)] = c_Freq_donation_15yr.trainmedian 
c_Freq_donation_15yr.trainmedian 
# Median for c_Freq_donation_10yr 
c_Freq_donation_10yr.trainmedian <- median(base_train$c_Freq_donation_10yr, na.rm = TRUE) 
base_train$c_Freq_donation_10yr[is.na(base_train$c_Freq_donation_10yr)] = c_Freq_donation_10yr.trainmedian 
c_Freq_donation_10yr.trainmedian 
# Median for c_Freq_donation_5yr 
c_Freq_donation_5yr.trainmedian <- median(base_train$c_Freq_donation_5yr, na.rm = TRUE) 
base_train$c_Freq_donation_5yr[is.na(base_train$c_Freq_donation_5yr)] = c_Freq_donation_5yr.trainmedian 
c_Freq_donation_5yr.trainmedian 
# Median for c_AvgAmt_15yr 
c_AvgAmt_15yr.trainmedian <- median(base_train$c_AvgAmt_15yr, na.rm = TRUE) 
base_train$c_AvgAmt_15yr[is.na(base_train$c_AvgAmt_15yr)] = c_AvgAmt_15yr.trainmedian 
c_AvgAmt_15yr.trainmedian  
# Median for c_AvgAmt_10yr 
c_AvgAmt_10yr.trainmedian <- median(base_train$c_AvgAmt_10yr, na.rm = TRUE) 
base_train$c_AvgAmt_10yr[is.na(base_train$c_AvgAmt_10yr)] = c_AvgAmt_10yr.trainmedian 
c_AvgAmt_10yr.trainmedian 
# Median for c_AvgAmt_5yr 
c_AvgAmt_5yr.trainmedian <- median(base_train$c_AvgAmt_5yr, na.rm = TRUE) 
base_train$c_AvgAmt_5yr[is.na(base_train$c_AvgAmt_5yr)] = c_AvgAmt_5yr.trainmedian 
c_AvgAmt_5yr.trainmedian 
# Median for c_TotAmt_15yr 
c_TotAmt_15yr.trainmedian <- median(base_train$c_TotAmt_15yr, na.rm = TRUE) 
base_train$c_TotAmt_15yr[is.na(base_train$c_TotAmt_15yr)] = c_TotAmt_15yr.trainmedian 
c_TotAmt_15yr.trainmedian 
# Median for c_TotAmt_10yr 
c_TotAmt_10yr.trainmedian <- median(base_train$c_TotAmt_10yr, na.rm = TRUE) 
base_train$c_TotAmt_10yr[is.na(base_train$c_TotAmt_10yr)] = c_TotAmt_10yr.trainmedian 
c_TotAmt_10yr.trainmedian 
# Median for c_TotAmt_5yr 
c_TotAmt_5yr.trainmedian <- median(base_train$c_TotAmt_5yr, na.rm = TRUE) 
base_train$c_TotAmt_5yr[is.na(base_train$c_TotAmt_5yr)] = c_TotAmt_5yr.trainmedian 
c_TotAmt_5yr.trainmedian 
# Median for c_NumYr_15yr 
c_NumYr_15yr.trainmedian <- median(base_train$c_NumYr_15yr, na.rm = TRUE) 
base_train$c_NumYr_15yr[is.na(base_train$c_NumYr_15yr)] = c_NumYr_15yr.trainmedian 
c_NumYr_15yr.trainmedian 
# Median for c_last_donation 
c_last_donation.trainmedian <- median(base_train$c_last_donation, na.rm = TRUE) 
base_train$c_last_donation[is.na(base_train$c_last_donation)] = c_last_donation.trainmedian 
c_last_donation.trainmedian 
# Median for c_max_donation 
c_max_donation.trainmedian <- median(base_train$c_max_donation, na.rm = TRUE) 
base_train$c_max_donation[is.na(base_train$c_max_donation)] = c_max_donation.trainmedian 
c_max_donation.trainmedian 




#Replace NA values with median for base test 



# Median for c_Freq_donation_15yr 
c_Freq_donation_15yr.testmedian <- median(base_test$c_Freq_donation_15yr, na.rm = TRUE) 
base_test$c_Freq_donation_15yr[is.na(base_test$c_Freq_donation_15yr)] = c_Freq_donation_15yr.testmedian 
c_Freq_donation_15yr.testmedian 
# Median for c_Freq_donation_10yr 
c_Freq_donation_10yr.testmedian <- median(base_test$c_Freq_donation_10yr, na.rm = TRUE) 
base_test$c_Freq_donation_10yr[is.na(base_test$c_Freq_donation_10yr)] = c_Freq_donation_10yr.testmedian 
c_Freq_donation_10yr.testmedian 
# Median for c_Freq_donation_5yr 
c_Freq_donation_5yr.testmedian <- median(base_test$c_Freq_donation_5yr, na.rm = TRUE) 
base_test$c_Freq_donation_5yr[is.na(base_test$c_Freq_donation_5yr)] = c_Freq_donation_5yr.testmedian 
c_Freq_donation_5yr.testmedian 
# Median for c_AvgAmt_15yr 
c_AvgAmt_15yr.testmedian <- median(base_test$c_AvgAmt_15yr, na.rm = TRUE) 
base_test$c_AvgAmt_15yr[is.na(base_test$c_AvgAmt_15yr)] = c_AvgAmt_15yr.testmedian 
c_AvgAmt_15yr.testmedian 
# Median for c_AvgAmt_10yr 
c_AvgAmt_10yr.testmedian <- median(base_test$c_AvgAmt_10yr, na.rm = TRUE) 
base_test$c_AvgAmt_10yr[is.na(base_test$c_AvgAmt_10yr)] = c_AvgAmt_10yr.testmedian 
c_AvgAmt_10yr.testmedian 
# Median for c_AvgAmt_5yr 
c_AvgAmt_5yr.testmedian <- median(base_test$c_AvgAmt_5yr, na.rm = TRUE) 
base_test$c_AvgAmt_5yr[is.na(base_test$c_AvgAmt_5yr)] = c_AvgAmt_5yr.testmedian 
c_AvgAmt_5yr.testmedian 
# Median for c_TotAmt_15yr 
c_TotAmt_15yr.testmedian <- median(base_test$c_TotAmt_15yr, na.rm = TRUE) 
base_test$c_TotAmt_15yr[is.na(base_test$c_TotAmt_15yr)] = c_TotAmt_15yr.testmedian 
c_TotAmt_15yr.testmedian 
# Median for c_TotAmt_10yr 
c_TotAmt_10yr.testmedian <- median(base_test$c_TotAmt_10yr, na.rm = TRUE) 
base_test$c_TotAmt_10yr[is.na(base_test$c_TotAmt_10yr)] = c_TotAmt_10yr.testmedian 
c_TotAmt_10yr.testmedian 
# Median for c_TotAmt_5yr 
c_TotAmt_5yr.testmedian <- median(base_test$c_TotAmt_5yr, na.rm = TRUE) 
base_test$c_TotAmt_5yr[is.na(base_test$c_TotAmt_5yr)] = c_TotAmt_5yr.testmedian 
c_TotAmt_5yr.testmedian 
# Median for c_NumYr_15yr 
c_NumYr_15yr.testmedian <- median(base_test$c_NumYr_15yr, na.rm = TRUE) 
base_test$c_NumYr_15yr[is.na(base_test$c_NumYr_15yr)] = c_NumYr_15yr.testmedian 
c_NumYr_15yr.testmedian 
# Median for c_last_donation 
c_last_donation.testmedian <- median(base_test$c_last_donation, na.rm = TRUE) 
base_test$c_last_donation[is.na(base_test$c_last_donation)] = c_last_donation.testmedian 
c_last_donation.testmedian 
# Median for c_max_donation 
c_max_donation.testmedian <- median(base_test$c_max_donation, na.rm = TRUE) 
base_test$c_max_donation[is.na(base_test$c_max_donation)] = c_max_donation.testmedian 
c_max_donation.testmedian 

##############Find and remove duplicated values

#   Train 
#     Find the duplicate values 
base_train_dup <- data.frame(table(base_train$donorID)) ## gives you a data frame with a list of DonorId and the number of times they occur 
base_train_dup[base_train_dup$Freq > 1,] ## tells you which DonorID occurred more than once 

#     Returns the records with more than one occurrence 
base_train[base_train$donorID %in% base_train_dup$Var1[base_train_dup$Freq > 1],] 

#     Remove duplicates
base_train_unique = base_train[!duplicated(base_train), ]

#     Check and ensure that there are no duplicates
base_train_unique[duplicated(base_train_unique), ]

#   Test 
#     Find the duplicate values
base_test_dup <- data.frame(table(base_test$donorID))  ## gives you a data frame with a list of DonorId and the number of times they occur 
base_test_dup[base_test_dup$Freq > 1,]  ## tells you which DonorID occurred more than once 

#     Returns the records with more than one occurrence 
base_test[base_test$donorID %in% base_test_dup$Var1[base_test_dup$Freq > 1], ] 

#     Remove duplicates
base_test_unique = base_test[!duplicated(base_test), ]

#Check and ensure that there are no duplicates
base_test_unique[duplicated(base_test_unique), ]

##############Find and replace outliers using winsorization
###Note: Our group has explored replacing outliers using winsorization 
###      however due to the nature of the data where the upper limit is 23, 
###      it doesn't make sense to replace the data with upper limit of 23 as our target variable are those donors who will donate more than 30 EUR.
###      Lower limit is negative and since our gifts datasets contains only donors who have given more than 0 EUR, we will not have any negative value to replace
###      With this justification, we will not do the below winsorization for this project

#Identify the index of caculated columns based on column names that starts with c_ 
#Both base_train_unique and base_test_unique have the same column names
#     calc_fields = grep("^c_",names(base_train_unique)) 

#Cast the calculated fields as numeric 
#     base_train_unique[calc_fields] = as.numeric(calc_fields) 
#     base_test_unique[calc_fields] = as.numeric(calc_fields) 

#Create a vector of train and test to be referenced inside the for loop
#     type = c("train","test") 

#Loops through each calculated field for both train and test datasets
#Calculates the upper limit and lower limit for winsorization
#Replace outliers with lower limit or upper limit
#     for(var in calc_fields) { 
#       for (var2 in type) { 
#         table = paste("base",var2,"unique", sep = "_") #name of dataset to be looped 
#         table_exp = eval(parse(text = table)) #evaluating name of dataset as expression instead of strings
#         vars_required = eval(parse(text = paste(table, "[,",var,"]", sep=""))) #evaluating the names of the variables in the dataset as expression
#         LL = mean(vars_required)-3*sd(vars_required) #lower limit 
#         UL = mean(vars_required)+3*sd(vars_required) #upper limit
#         toreplaceLL = which(vars_required<LL) #finds observations where values are below lower limit 
#         table_exp[toreplaceLL ,var] = LL #replace these values to the lower limit 
#         toreplaceUL = which(vars_required>UL) #finds observations where values are above upper limit 
#         table_exp[toreplaceUL ,var] = UL #replace these values to the upper limit 
#       } 
#     } 
