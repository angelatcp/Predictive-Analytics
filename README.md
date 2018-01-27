# Predictive-Analytics

Goal of project: To identify inactive donors who are most likely to donate more than 30 Euros in a fundraising reactivation campaign

Summary: 
- Trained and evaluated Logistic Regression and Decision Tree models on 60,533 donors using R.
- Target variable are inactive donors who have donated more than 30 Euros in a fundraising reactivation campaign.
- Inactive donors are defined as donors who have not made any donation in the last 2 years before the campaign.
- Presented recommendation to business

Datasets: 
- Train: campaign20140115.csv
- Test: campaign20130411.csv
- Base table is built using the below historical data 
  - donors' demographics: donors.csv
  - donors' past donation: gifts.csv
  
Result: 
 - Model selected: Logistic Regression 
 - Feature selection: Stepwise gave a higher AUC than Pearson Correlation
 - Recommend to target 33% of the population
 - Potential profit: 763 Euros
 
Profit Optimization:
 - Using optimization function to identify the ideal donation amount to target other than 30 Euros
 - Optimized donation amount = 39 Euros
 - Recommend to target 67% of the population
 - Potential profit: 2948 Euros
 
