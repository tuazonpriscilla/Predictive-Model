# Project Description
In this project I used a K-Nearest Neighbors Algorithm to predict when a fault would occur in the next 6 hours based on prior data. 
The code that I have posted has been edited to protect the privacy of the company that I was partnered with during this project. 
It is unable to be run. 

# Data Preparation
To prepare the data I first read it in, cleaned it, reshaped it, imputed any missing values, lagged the data, then built predictive models with it. 
After reading in the data I aggregated it by 6 hour intervals by using a full merge on all the sensor data and grouped it using the 6 hour date time column and another variable in the data.
Within the fault statuses there were some faults that were not considered important by the company so I removed those from the data. 
I then replaced any missing data using kNN imputation, checking that the imputed data was reasonable using scatterplots. 
I then lagged the data by 1 row, the equivalent to a 6 hour lag in the data. 

# Predictive Modeling
I trained a random forest and kNN model that would predict whether an important fault would occur in the next six hours. After training both of these models I picked a final model that had a higher
AUC while keeping in mind the specificity and sensitivity of the predictions. I trained the model on 80% of our rows and evaluated their performance against the rest. After training I decided to use the kNN model
over the random forest since it had a higher AUC and balanced the trade of between sensitivity and specificity better. 

# Data Exploration
I explored the top three important predictor variables that my final random forest had identified and tried to see if there were any visible trends that explain their importance in 
predicting an important fault occurring.
