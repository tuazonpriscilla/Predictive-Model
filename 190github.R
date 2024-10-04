rm(list = ls())
# consolidating all sensor data ----------
# load packages for data cleaning 
library(tidyverse)
library(dplyr)
library(lubridate)

## Loading and Aggregating Variable 1 ----
# new data from 03/04
Var1Lst = list.files("OneDrive_2023-04-03/Variable 1/ Data Batch 2 - Variable 1/Variable 1")
Var1 = read.csv(paste0("OneDrive_2023-04-03/Variable 1/ Data Batch 2 - Variable 1/Variable 1/",Var1Lst[2]), header = FALSE)
for (i in 3:length(Var1Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 1/ Data Batch 2 - Variable 1/Variable 1/",Var1Lst[i]), header = FALSE)
  Var1 = rbind(Var1,new)
}

# change the format of the time recorded
Var1$V2 <- ymd_hms(Var1$V2)
# new column with that rounds the time record to every 6 hours
Var1$DateHour <- round_date(Var1$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var1 = subset(Var1, select=c(V1,V4, DateHour))
colnames(Var1) <-  c("location_id","Variable1","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar1 <- Var1 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVariable1 = mean(Variable1),MinVariable1 = min(Variable1),MaxVariable1 = max(Variable1),SDVariable1 = sd(Variable1))

## Loading and Aggregating Variable 2 ----------
# new data from 03/04
Var2Lst = list.files("OneDrive_2023-04-03/Variable 2/ Data Batch 2 - Variable 2/Variable 2")
Var2 = read.csv(paste0("OneDrive_2023-04-03/Variable 2/ Data Batch 2 - Variable 2/Variable 2/",Var2Lst[2]), header = F)
for (i in 3:length(Var2Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 2/ Data Batch 2 - Variable 2/Variable 2/",Var2Lst[i]), header = FALSE)
  Var2 = rbind(Var2,new)
}
# change the format of the time recorded
Var2$V2 <- ymd_hms(Var2$V2)
# new column with that rounds the time record to every 6 hours
Var2$DateHour <- round_date(Var2$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var2 = subset(Var2, select=c(V1,V4, DateHour))
colnames(Var2) <-  c("location_id","Variable2","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar2 <- Var2 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVar2 = mean(Variable2),MinVar2 = min(Variable2),MaxVar2 = max(Variable2),SDVar2 = sd(Variable2))

## Loading and Variable 3 -------
# new data from 03/04
Var3Lst = list.files("OneDrive_2023-04-03/Variable 3/ Data Batch 2 - Variable 3/Variable 3")
Var3 = read.csv(paste0("OneDrive_2023-04-03/Variable 3/ Data Batch 2 - Variable 3/Variable 3/",Var3Lst[2]), header = FALSE)
# start with file 2 because file 1 is empty 
for (i in 3:length(Var3Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 3/ Data Batch 2 - Variable 3/Variable 3/",Var3Lst[i]), header = FALSE)
  Var3 = rbind(Var3,new)
}

# change the format of the time recorded
Var3$V2 <- ymd_hms(Var3$V2)
# new column with that rounds the time record to every 6 hours
Var3$DateHour <- round_date(Var3$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var3 = subset(Var3, select=c(V1,V4, DateHour))
colnames(Var3) <-  c("location_id","Variable3","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar3 <- Var3 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVar3=mean(Variable3),MinVar3=min(Variable3),MaxVar3=max(Variable3),SDVar3=sd(Variable3))

## Loading and Aggregating Variable 4 ---------
# new data from 03/04
Var4Lst = list.files("OneDrive_2023-04-03/Variable 4/ Data Batch 2 - Variable 4/Variable 4")
Var5 = read.csv(paste0("OneDrive_2023-04-03/Variable 4/ Data Batch 2 - Variable 4/Variable 4/",Var4Lst[2]), header = FALSE)
for (i in 3:length(Var4Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 4/ Data Batch 2 - Variable 4/Variable 4/",Var4Lst[i]), header = FALSE)
  Var5 = rbind(Var5,new)
}
# change the format of the time recorded
Var5$V2 <- ymd_hms(Var5$V2)
# new column with that rounds the time record to every 6 hours
Var5$DateHour <- round_date(Var5$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var5 = subset(Var5, select=c(V1,V4, DateHour))
colnames(Var5) <-  c("location_id","Variable4","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar4 <- Var5 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVar4 = mean(Variable4),MinVar4 = min(Variable4),MaxVar4 = max(Variable4),SDVar4 = sd(Variable4))

## Loading and Aggregating Variable 5 -------
# new data from 03/04
Var5Lst = list.files("OneDrive_2023-04-03/Variable 5/ Data Batch 2 - Variable 5/Variable 5")
# start with file 5 because the first few are irrelevant 
Var5 = read.csv(paste0("OneDrive_2023-04-03/Variable 5/ Data Batch 2 - Variable 5/Variable 5/",Var5Lst[5]), header = FALSE)
for (i in 6:length(Var5Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 5/ Data Batch 2 - Variable 5/Variable 5/",Var5Lst[i]), header = FALSE)
  Var5 = rbind(Var5,new)
}
# change the format of the time recorded
Var5$V2 <- ymd_hms(Var5$V2)
# new column with that rounds the time record to every 6 hours
Var5$DateHour <- round_date(Var5$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var5 = subset(Var5, select=c(V1,V4, DateHour))
colnames(Var5) <-  c("location_id","Variable5","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar5 <- Var5 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVar5 = mean(Variable5),MinVar5 = min(Variable5),MaxVar5 = max(Variable5),SDVar5 = sd(Variable5))

## Loading and Variable 6 ---------
# new data from 03/04
Var6Lst = list.files("OneDrive_2023-04-03/Variable 6/ Data Batch 2 - Variable 6/Variable 6")
Var6 = read.csv(paste0("OneDrive_2023-04-03/Variable 6/ Data Batch 2 - Variable 6/Variable 6/",Var6Lst[2]), header = FALSE)
#start with the 2nd file because the first file is empty
for (i in 2:length(Var6Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 6/ Data Batch 2 - Variable 6/Variable 6/",Var6Lst[i]), header = FALSE)
  Var6 = rbind(Var6,new)
}
# change the format of the time recorded
Var6$V2 <- ymd_hms(Var6$V2)
# new column with that rounds the time record to every 6 hours
Var6$DateHour <- round_date(Var6$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var6 = subset(Var6, select=c(V1,V4, DateHour))
colnames(Var6) <-  c("location_id","Var6","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar6 <- Var6 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVar6 = mean(Var6),MinVar6 = min(Var6),MaxVar6 = max(Var6),SDVar6 = sd(Var6))

## Loading and Aggregating Variable 7 -----------
# new data from 03/04
Var7Lst = list.files("OneDrive_2023-04-03/Variable 7/ Data Batch 2 - Variable 7/Variable 7")
Var7 = read.csv(paste0("OneDrive_2023-04-03/Variable 7/ Data Batch 2 - Variable 7/Variable 7/",Var7Lst[2]), header = F)
#start with the 5th file because the first few are irrelevant (descriptions?) 
for (i in 3:length(Var7Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 7/ Data Batch 2 - Variable 7/Variable 7/",Var7Lst[i]), header = FALSE)
  Var7 = rbind(Var7,new)
}
# change the format of the time recorded
Var7$V2 <- ymd_hms(Var7$V2)
# new column with that rounds the time record to every 6 hours
Var7$DateHour <- round_date(Var7$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var7 = subset(Var7, select=c(V1,V4, DateHour))
colnames(Var7) <-  c("location_id","Var7","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar7 <- Var7 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVar7 = mean(Var7),MinVar7 = min(Var7),MaxVar7 = max(Var7),SDVar7 = sd(Var7))

## Loading and Aggregating Variable 8 ----------------
# new data from 03/04
Var8Lst = list.files("OneDrive_2023-04-03/Variable 8/ Data Batch 2 - Variable 8/Variable 8")
Var8 = read.csv(paste0("OneDrive_2023-04-03/Variable 8/ Data Batch 2 - Variable 8/Variable 8/",Var8Lst[5]), header = F)
#start with the 5th file because the first few are irrelevant (descriptions?) 
for (i in 6:length(Var8Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 8/ Data Batch 2 - Variable 8/Variable 8/",Var8Lst[i]), header = FALSE)
  Var8 = rbind(Var8,new)
}
# change the format of the time recorded
Var8$V2 <- ymd_hms(Var8$V2)
# new column with that rounds the time record to every 6 hours
Var8$DateHour <- round_date(Var8$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var8 = subset(Var8, select=c(V1,V4, DateHour))
colnames(Var8) <-  c("location_id","Variable8","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar8 <- Var8 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVar8 = mean(Variable8),MinVar8 = min(Variable8),MaxVar8 = max(Variable8),SDVar8 = sd(Variable8))

## Loading and Aggregating Variable 9 -------------
## windspeed data from 03/04
Var9Lst = list.files("OneDrive_2023-04-03/Variable 9/ Data Batch 2 - Variable 9/Variable 9")
Var9 = read.csv(paste0("OneDrive_2023-04-03/Variable 9/ Data Batch 2 - Variable 9/Variable 9/",Var9Lst[5]), header = FALSE)
for (i in 6:length(Var9Lst)){
  new = read.csv(paste0("OneDrive_2023-04-03/Variable 9/ Data Batch 2 - Variable 9/Variable 9/",Var9Lst[i]), header = FALSE)
  Var9 = rbind(Var9,new)
}

# change the format of the time recorded
Var9$V2 <- ymd_hms(Var9$V2)
# new column with that rounds the time record to every 6 hours
Var9$DateHour <- round_date(Var9$V2, "6 hours")

#Subset data to only have needed columns and name columns
Var9 = subset(Var9, select=c(V1,V4, DateHour))
colnames(Var9) <-  c("location_id","Var9","DateHour")

#Calculate mean, min, standard deviation, and max of each group of 6 hour intervals
AggVar9 <- Var9 %>% group_by(location_id, DateHour) %>% 
  summarise(AvgVar9 = mean(Var9),MinVar9 = min(Var9),MaxVar9 = max(Var9),SDVar9 = sd(Var9))


## clear up unnecessary data frames to make space in memory -----
rm(Var1,Var2,Var3,Var7,Var6,Var5,new,Var8,Var9)
rm(Var1Lst,Var2Lst,Var3Lst,Var7Lst,Var6Lst,i,Var4Lst,Var8Lst,Var9Lst)


## merge all the s data ----
s_data_joined <- AggVar1 %>% full_join(AggVar2, by=c('location_id', 'DateHour'))
s_data_joined <- s_data_joined %>% full_join(AggVar3, by=c('location_id', 'DateHour'))
s_data_joined <- s_data_joined %>% full_join(AggVar4, by=c('location_id', 'DateHour'))
s_data_joined <- s_data_joined %>% full_join(AggVar5, by=c('location_id', 'DateHour'))
s_data_joined <- s_data_joined %>% full_join(AggVar6, by=c('location_id', 'DateHour'))
s_data_joined <- s_data_joined %>% full_join(AggVar7, by=c('location_id', 'DateHour'))
s_data_joined <- s_data_joined %>% full_join(AggVar8, by=c('location_id', 'DateHour'))
s_data_joined <- s_data_joined %>% full_join(AggVar9, by=c('location_id', 'DateHour'))


## Write out S Data ----
str(s_data_joined) #  [5,238 × 38]
write.table(s_data_joined, file = "s_data_joined_6Hr.csv", sep = ",", row.names = FALSE)

# Load in and clean F code data -----------------
f_files = list.files("OneDrive_2023-04-03/F Status Codes/ Data Batch 2 - F Status Codes/F Status Codes", pattern = ".csv")
FCodes = read.csv(paste0("OneDrive_2023-04-03/F Status Codes/ Data Batch 2 - F Status Codes/F Status Codes/",f_files[2]), header = FALSE)
for (i in 3:length(f_files)){
  new = read.csv(paste0("OneDrive_2023-04-03/F Status Codes/ Data Batch 2 - F Status Codes/F Status Codes/",f_files[i]), header = FALSE)
  FCodes = rbind(FCodes,new)
}

str(FCodes) #718,122 obs. of  7 variables

## Clean F Codes
### remove duplicates 
FCodes <- unique(FCodes)
str(FCodes) # 237,507 obs. of  7 variables:

### Assign names to the columns 
colnames(FCodes) <- c("location_id","fc_time","fc_date","code","status_code","fc_desc","fc_type")

### change the format of the time recorded
FCodes$fc_time <- ymd_hms(FCodes$fc_time)

### new column with that rounds the time record to every 6 hours
FCodes$DateHour <- round_date(FCodes$fc_time, "6 hours")

### subset the data to only have columns we need 
FCodes = subset(FCodes, select=c(location_id,DateHour,fc_desc,fc_type))

### read through distinct fc_desc to identify fc_desc that are not actually relevant
unique(FCodes$fc_desc)

### remove rows that are not not actual faults 
not_actual_faults <- c("Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example",
                          "Not Actual Fault Example")

FCodes <- FCodes[!(FCodes$fc_desc %in% not_actual_faults),]

str(FCodes) # 236,268 obs. of  4 variables

### create fault Events 
FEvents <- FCodes %>% arrange(location_id,DateHour) %>% 
  group_by(location_id)%>%
  mutate(Prev_Fault = lag(fc_desc)) %>%
  filter(is.na(Prev_Fault) | fc_desc != Prev_Fault) %>%
  select(-Prev_Fault)

str(FEvents) # 9,686 × 4


# Merge FEvents with s data and create binary columns ------------
s_data = read.csv("s_data_joined_6Hr.csv")
s_data$DateHour <- ymd_hms(s_data$DateHour)
master <- FEvents %>% full_join(s_data, by=c('location_id', 'DateHour'))
str(master) # 14,103 × 40

### replace the NAs in fc_desc and fc_type with "No fault"
master$fc_desc = ifelse(is.na(master$fc_desc),"No Fault", master$fc_desc)
master$fc_type = ifelse(is.na(master$fc_type),"No Fault", master$fc_type)

### create a binary column to show if a fault occurred at the sensor reading
master$fault_flag = ifelse(master$fc_desc == "No Fault", 0 , 1)
table(master$fault_flag)
sum(master$fault_flag == 1)/nrow(master) # 0.69 of the rows were faults

### create a binary column to show if an expensive fault occurred at the sensor reading
exp_faults = c('Expensive Fault Example', 'Expensive Fault Example', 
                                 'Expensive Fault Example', 'Expensive Fault Example', 
                                 'Expensive Fault Example', 'Expensive Fault Example', 'Expensive Fault Example')


master$Exp_fault_flag <- ifelse(master$fc_desc %in% exp_faults, 1 , 0)
table(master$Exp_fault_flag)
sum(master$Exp_fault_flag == 1)/nrow(master) # 0.02715734 of the rows were expensive faults

### create a new column that extracts the month of the sensor reading 
master$month = month(master$DateHour)

# write out master - joins sensor with fault events and has binary columns for fault & expensive fault 
write.table(master, file = "master_6Hr.csv", sep = ",", row.names = FALSE) 

# Impute missing sensor data -----
library(VIM) # needed for imputing data 

master = read.csv("master_6Hr.csv")
str(master)
master$DateHour = ymd_hms(master$DateHour)
master_imputed = kNN(master)
summary(master_imputed) # No NAs anymore!

## plots that check imputes for means ----
ggplot(master_imputed, aes(x=DateHour, y=AvgVariable1, color= AvgVariable1_imp)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points

ggplot(master_imputed, aes(x=DateHour, y=AvgVar2, color=AvgVar2_imp)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points
#few imputed, reasonable placement around original data points

ggplot(master_imputed, aes(x=DateHour, y=AvgVar3, color= AvgVar3_imp)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points
#very few imputed, reasonable placement around original data points

ggplot(master_imputed, aes(x=DateHour, y=AvgVar4, color= AvgVar4_imp)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points
#very few imputed, reasonable placement around original data points

ggplot(master_imputed, aes(x=DateHour, y=AvgVar5, color=AvgVar5_imp)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points
#very few imputed, reasonable placement around original data points

ggplot(master_imputed, aes(x=DateHour, y=AvgVar6, color=AvgVar6)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points
#very few imputed, reasonable placement around original data points

ggplot(master_imputed, aes(x=DateHour, y=AvgVar7, color= AvgVar7)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points
#few imputed, reasonable placement around original data points 

ggplot(master_imputed, aes(x=DateHour, y=AvgVar8, color=AvgVar8)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points
#reasonable placement of imputed data points 

ggplot(master_imputed, aes(x=DateHour, y=AvgVar9, color= AvgVar9)) + geom_point()+ labs(color = "Imputed",x = "Date Hour")
#few imputed, reasonable placement around original data points
# Not really that reasonable - this is because variable 9 data is sparse 


# ADD PLOTS THAT CHECK IMPUTES FOR MAX, MIN AND SD

# remove columns that were added by the kNN to indicated if they were imputed or not
master_imputed<- select(master_imputed, -matches("_imp"))

# Add columns that lag the sensor data -----
# our sensor data was aggregated by every 6 hours
# we are lagging our sensor data by 1 unit so they should be lagged back by 6 hours
# *this also means if we are missing rows for certain hours then the data is lagged back by more than a 6 hours


# sorts the data by variable and time 
imputed_master_w_lag = master_imputed %>% arrange(location_id,DateHour)
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVar9 = lag(AvgVar9, n=1))
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVar9 = lag(MaxVar9, n=1))
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVar9 = lag(MinVar9, n=1))
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVar9 = lag(SDVar9, n=1))

imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVar6 = lag(AvgVar6, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVar6 = lag(MaxVar6, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVar6 = lag(MinVar6, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVar6 = lag(SDVar6, n=1)) 

imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVariable1 = lag(AvgVariable1, n=1))  
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVariable1 = lag(MaxVariable1, n=1))
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVariable1 = lag(MinVariable1, n=1))
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVariable1 = lag(SDVariable1, n=1))

imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVar3 = lag(AvgVar3, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVar3 = lag(MaxVar3, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVar3 = lag(MinVar3, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVar3 = lag(SDVar3, n=1)) 

imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVar7 = lag(AvgVar7, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVar7 = lag(MaxVar7, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVar7 = lag(MinVar7, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVar7 = lag(SDVar7, n=1)) 

imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVar8 = lag(AvgVar8, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVar8 = lag(MaxVar8, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVar8 = lag(MinVar8, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVar8 = lag(SDVar8, n=1)) 

imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVar4 = lag(AvgVar4, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVar4 = lag(MaxVar4, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVar4 = lag(MinVar4, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVar4 = lag(SDVar4, n=1)) 

imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVar5 = lag(AvgVar5, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVar5 = lag(MaxVar5, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVar5 = lag(MinVar5, n=1)) 
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVar5 = lag(SDVar5, n=1)) 

imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagAvgVar2 = lag(AvgVar2, n=1))
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMaxVar2 = lag(MaxVar2, n=1))
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagMinVar2 = lag(MinVar2, n=1))
imputed_master_w_lag = imputed_master_w_lag %>% group_by(location_id) %>% mutate(lagSDVar2 = lag(SDVar2, n=1))

write.table(imputed_master_w_lag, file = "imputed_master_w_lag_6Hr.csv", sep = ",", row.names = FALSE) 


# Random Forest Exploration and Tuning -------
library(randomForest)
library(caret)
library(yardstick)

master = read.csv("imputed_master_w_lag_6Hr.csv")
#14,103 observations

## cleaning for random forest
## Delete any rows with NAs - these NAs are present due the lagging we did with our data
master <- na.omit(master)
#14,086 observations

data <- transform(
  
  # transform all data to numeric and factors
  master,
  location_id=as.factor(location_id),
  lagAvgVar9=as.numeric(lagAvgVar9),
  lagMaxVar9=as.numeric(lagMaxVar9),
  lagMinVar9=as.numeric(lagMinVar9),
  lagSDVar9=as.numeric(lagSDVar9),
  
  lagAvgVar6=as.numeric(lagAvgVar6),
  lagMaxVar6=as.numeric(lagMaxVar6),
  lagMinVar6=as.numeric(lagMinVar6),
  lagSDVar6=as.numeric(lagSDVar6),
  
  lagAvgVariable1=as.numeric(lagAvgVariable1),
  lagMaxVariable1=as.numeric(lagMaxVariable1),
  lagMinVariable1=as.numeric(lagMinVariable1),
  lagSDVariable1=as.numeric(lagSDVariable1),
  
  lagAvgVar3=as.numeric(lagAvgVar3),
  lagMaxVar3=as.numeric(lagMaxVar3),
  lagMinVar3=as.numeric(lagMinVar3),
  lagSDVar3=as.numeric(lagSDVar3),
  
  lagAvgVar7=as.numeric(lagAvgVar7),
  lagMaxVar7=as.numeric(lagMaxVar7),
  lagMinVar7=as.numeric(lagMinVar7),
  lagSDVar7=as.numeric(lagSDVar7),
  
  lagAvgVar8=as.numeric(lagAvgVar8),
  lagMaxVar8=as.numeric(lagMaxVar8),
  lagMinVar8=as.numeric(lagMinVar8),
  lagSDVar8=as.numeric(lagSDVar8),
  
  lagAvgVar4=as.numeric(lagAvgVar4),
  lagMaxVar4=as.numeric(lagMaxVar4),
  lagMinVar4=as.numeric(lagMinVar4),
  lagSDVar4=as.numeric(lagSDVar4),
  
  lagAvgVar5=as.numeric(lagAvgVar5),
  lagMaxVar5=as.numeric(lagMaxVar5),
  lagMinVar5=as.numeric(lagMinVar5),
  lagSDVar5=as.numeric(lagSDVar5),
  
  lagAvgVar2=as.numeric(lagAvgVar2),
  lagMaxVar2=as.numeric(lagMaxVar2),
  lagMinVar2=as.numeric(lagMinVar2),
  lagSDVar2=as.numeric(lagSDVar2),
  
  fc_desc=as.factor(fc_desc),
  fc_type=as.factor(fc_type),
  fault_flag=as.factor(fault_flag),
  Exp_fault_flag=as.factor(Exp_fault_flag),
  month=as.factor(month)
)


str(data)
rm(master)

## Chose a seed for reproducible of results 
set.seed(132547)
## split into train and testing data
train.idx = sample(x = 1:nrow(data), size = floor(.8*nrow(data)))
# make training data
train.df = data[train.idx,]
##the rest will be for testing
test.df = data[-train.idx,]


## count of the number of X variables 
sum(grepl("^lag", names(data))) 
# 36 columns + 1 (month)
mtry<-c(3,12,19,24,30,36)

## make empty dataframes to store m and oob errors 
keeps <- data.frame(m = rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA,length(mtry)))

## create a loop will fill the keeps data frame
for(idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  set.seed(132547)
  tempforest <- randomForest(Exp_fault_flag~ lagAvgVar9 + lagMaxVar9 +lagMinVar9 +lagSDVar9 + lagAvgVar6 + lagMaxVar6 +lagMinVar6 +lagSDVar6 + lagAvgVariable1 + lagMaxVariable1 +lagMinVariable1 +lagSDVariable1 + lagAvgVar3 + lagMaxVar3 +lagMinVar3 +lagSDVar3 + lagAvgVar7 + lagMaxVar7 +lagMinVar7 +lagSDVar7 + lagAvgVar8 + lagMaxVar8 +lagMinVar8 +lagSDVar8 + lagAvgVar4 + lagMaxVar4 +lagMinVar4 +lagSDVar4 + lagAvgVar5 + lagMaxVar5 +lagMinVar5 +lagSDVar5 + lagAvgVar2  + lagMaxVar2  +lagMinVar2  +lagSDVar2  + month,
                             data = train.df,
                             ntree = 1000,
                             mtry = mtry[idx]) # mtry is varying
  # record OOB error for each forest fit
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train.df$Exp_fault_flag)
}

## plot m vs oob error rate
ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_error_rate)) +
  scale_x_continuous(breaks = c(1:37)) +
  labs(x = "m (mtry): # of x variables sampled",
       y = "OOB Error Rate")

# Final Random Forest -------
set.seed(132547)
## create random forests for each issue flag variable using the best mtry identified in the earlier random forests
expissueforest = randomForest(Exp_fault_flag ~ lagAvgVar9 + lagMaxVar9 +lagMinVar9 +lagSDVar9 + lagAvgVar6 + lagMaxVar6 +lagMinVar6 +lagSDVar6 + lagAvgVariable1 + lagMaxVariable1 +lagMinVariable1 +lagSDVariable1 + lagAvgVar3 + lagMaxVar3 +lagMinVar3 +lagSDVar3 + lagAvgVar7 + lagMaxVar7 +lagMinVar7 +lagSDVar7 + lagAvgVar8 + lagMaxVar8 +lagMinVar8 +lagSDVar8 + lagAvgVar4 + lagMaxVar4 +lagMinVar4 +lagSDVar4 + lagAvgVar5 + lagMaxVar5 +lagMinVar5 +lagSDVar5 + lagAvgVar2  + lagMaxVar2  +lagMinVar2  +lagSDVar2  + month,
                              data = train.df,
                              ntree = 1000, #fit B = 1000 separate classification trees
                              mtry = 24, # lowest OOB error rate 
                              importance = TRUE #importance can help us identify important predictors
)

expissueforest # OOB estimate of  error rate: 2.77%
pred <- predict(expissueforest, newdata = test.df)

# Create the confusion matrix
test.df$Exp_fault_flag <- as.factor(test.df$Exp_fault_flag)
confusionMatrix(pred, test.df$Exp_fault_flag)
# Accuracy : 0.9734
# Sensitivity : 0.9913 _ True Positives 
# Specificity : 0.1774 _ True Negatives 



## creates variable importance graphs
# graph for expensive issues
vi = as.data.frame(varImpPlot(expissueforest, type = 1))
vi$Variable <- rownames(vi)
ggplot(data = vi) +
  geom_bar(aes(x = reorder(Variable, MeanDecreaseAccuracy), weight = MeanDecreaseAccuracy),
           position = "identity") +
  coord_flip() +
  labs( x = "Variable Name", y = "Importance") +
  ggtitle(" Variable Importance Plot") +
  theme(plot.title = element_text(hjust = 0.5))


## ROC Curve graphs
library(pROC)

# expissueforest ROC graph
pi_hat <- predict(expissueforest, test.df, type = "prob")[, "1"]

#ROC Curve
rocCurve <- roc(response = test.df$Exp_fault_flag,#supply truth
                predictor = pi_hat,#supply predicted PROBABILITIES)
                levels = c("0","1") #(negative, positive)
)
#plot basic ROC curve
plot(rocCurve, print.thres = TRUE,print.auc = TRUE)
# pi*: 0.0025
# specificity at pi*: 0.908
# sensitivity at pi*: 0.694
# AUC: 0.807

# we would not want to use this pi* because it drops our models sensitivity to 0.694 from 0.9913


# kNN Exploration and Tuning ----

library(class)
# Train and tune a kNN model

set.seed(132547)
knn_model <- train(Exp_fault_flag ~ lagAvgVar9 + lagMaxVar9 + lagMinVar9 + lagSDVar9 + lagAvgVar6 + lagMaxVar6 + lagMinVar6 + lagSDVar6 + lagAvgVariable1 + lagMaxVariable1 + lagMinVariable1 + lagSDVariable1 + lagAvgVar3 + lagMaxVar3 + lagMinVar3 + lagSDVar3 + lagAvgVar7 + lagMaxVar7 + lagMinVar7 + lagSDVar7 + lagAvgVar8 + lagMaxVar8 + lagMinVar8 + lagSDVar8 + lagAvgVar4 + lagMaxVar4 + lagMinVar4 + lagSDVar4 + lagAvgVar5 + lagMaxVar5 + lagMinVar5 + lagSDVar5 + lagAvgVar2 + lagMaxVar2 + lagMinVar2 + lagSDVar2 + month,
                   data = train.df,
                   preProcess = c("center","scale"),
                   method = "knn",
                   trControl = trainControl(method = "cv", number = 10), #10-fold cross-validation to tune the model
                   tuneGrid = expand.grid(k = c(1,3,5, 11, 17, 23,37,53,79,100,200))) #list of k the model is trains on - it will pick the one that maximizes accuracy 


knn_model # k = 3
# Evaluate performance 
predictions <- predict(knn_model, newdata = test.df)
confusionMatrix(predictions, test.df$Exp_fault_flag)
# Accuracy : 0.973
# Sensitivity : 0.9902 
# Specificity :  0.2097 



pi_hat <- predict(knn_model, test.df, type = "prob")[, "1"]

#ROC Curve
rocCurve <- roc(response = test.df$Exp_fault_flag,#supply truth
                predictor = pi_hat,#supply predicted PROBABILITIES)
                levels = c("0","1") #(negative, positive)
)
plot(rocCurve, print.thres = TRUE,print.auc = TRUE)
# AUC: 0.951 
# spec: 0.905
# sens: 0.968





# Data Exploration ----
## load packages 
library(ggplot2)

master = read.csv("imputed_master_w_lag_6Hr.csv")

# correlation matrix for all the aggregated sensor data 
cor(master[,44:79], use="pairwise.complete.obs")

# Variable 9 vs Variable 1
min(master$lagAvgVar9,na.rm = TRUE) # 21.03
max(master$lagAvgVar9,na.rm = TRUE) # 23.47
min(master$lagAvgVariable1,na.rm = TRUE) # 31
max(master$lagAvgVariable1,na.rm = TRUE) # 2299.463

ggplot(data = master, aes(lagAvgVar9, lagAvgVariable1)) +
  geom_point(aes(color = as.factor(Exp_fault_flag)), alpha = 0.5) +
  scale_color_discrete(name = "Expensive Faults", labels = c("0", "1"))+
  scale_color_manual(values=c("grey", "red"))+
  facet_wrap(~location_id) +
  xlim(20, 25) +
  ylim(500, 2335) +
  theme_bw()+
  labs(color = "Expensive Fault",x = "Lagged Average Variable 9", y = "Lagged Average Variable 1")

# Exploring the top 3 important variables from the final random forest 
# lagSDVar5
ggplot(data = master, aes(lagSDVar5, lagAvgVariable1)) +
  geom_point(aes(color = as.factor(Exp_fault_flag)), alpha = 0.5) +
  scale_color_discrete(name = "Expensive Faults", labels = c("0", "1"))+
  scale_color_manual(values=c("grey", "red"))+
  facet_wrap(~Exp_fault_flag) +
  xlim(0,10)+
  ylim(0,2355)+
  theme_bw()+
  labs(color = "Expensive Fault",x = "Lagged SD of Var5", y = "Lagged Average Variable 1")

# lagMinVariable1
ggplot(master, aes(x = location_id, y=lagMinVariable1))+
  geom_boxplot(color = 'red',fill="orange", alpha=0.2,outlier.color = "NA")+labs(title = "Lagged Min. Variable 1 by ", x = "",y = "Lagged Min. Variable 1")+
  theme_bw() + 
  facet_wrap(~ Exp_fault_flag)+
  theme(axis.text.x = element_text(angle = 90))


ggplot(data = master, aes(lagMinVariable1, lagAvgVariable1)) +
  geom_point(aes(color = as.factor(Exp_fault_flag)), alpha = 0.5) +
  scale_color_discrete(name = "Expensive Faults", labels = c("0", "1"))+
  scale_color_manual(values=c("grey", "red"))+
  facet_wrap(~Exp_fault_flag) +
  xlim(0,2000)+
  ylim(450,2355)+
  theme_bw()+
  labs(color = "Expensive Fault",x = "Lagged Min. Variable 1", y = "Lagged Average Variable 1")


# lagMaxVar8
max(master$MaxVar8) # 258
min(master$MaxVar8) # 163
ggplot(data = master, aes(lagMaxVar8, lagAvgVariable1)) +
  geom_point(aes(color = as.factor(Exp_fault_flag)), alpha = 0.5) +
  scale_color_discrete(name = "Expensive Faults", labels = c("0", "1"))+
  scale_color_manual(values=c("grey", "red"))+
  facet_wrap(~Exp_fault_flag) +
  xlim(150,300)+
  ylim(0,2355)+
  theme_bw()+
  labs(color = "Expensive Fault",x = "Lagged Max. Variable 8", y = "Lagged Average Variable 1")
