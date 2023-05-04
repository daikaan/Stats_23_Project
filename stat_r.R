bank_data <- read.csv('~/GitHub/Stats_23_Project/BankChurners.csv')

# client_num should be added to qualitative as well as primary key
# dependent_count to qualitative? 

head(bank_data)
dim(bank_data) #10127 rows and 23 columns

col = c('CLIENTNUM', 'Attrition_Flag', 'Customer_Age',	'Gender',	'Dependent_count', 'Education_Level',	'Marital_Status',	'Income_Category', 'Card_Category',	'Months_on_book',	'Total_Relationship_Count', 'Months_Inactive_12_mon',	'Contacts_Count_12_mon', 'Credit_Limit', 'Total_Revolving_Bal',	'Avg_Open_To_Buy', 'Total_Amt_Chng_Q4_Q1', 'Total_Trans_Amt', 'Total_Trans_Ct', 'Total_Ct_Chng_Q4_Q1',	'Avg_Utilization_Ratio',	'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1',	'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2')

library(knitr)
library(forecast)
library(ggplot2)
library(PCAmixdata)
library(dplyr)



data.split <- splitmix(bank_data)
quantitative <- data.split$X.quanti
qualitative <- data.split$X.quali

length(quantitative)
length(qualitative)

#Checking if there is any null value in the data set
colSums(is.na(bank_data)) #no null value

# Descriptive Graphs

#Customer age
Cust.age.hist <- hist(quantitative$Customer_Age, xlab="age", ylab="freq",
                      main="Customer age distribution", col="orange")
Cust.age.hist
#using the histogram, dividing ages iinto 4 groups seems satisfying

#Creating age groups
quantitative[quantitative$Customer_Age <= 34, "age_group"] <- 1
quantitative[quantitative$Customer_Age > 34 & quantitative$Customer_Age <= 44, "age_group"] <- 2
quantitative[quantitative$Customer_Age > 44 & quantitative$Customer_Age <= 54, "age_group"] <- 3
quantitative[quantitative$Customer_Age > 54, "age_group"] <- 4

#grouped age histogram
Grouped.age.hist <- hist(as.numeric(quantitative$age_group), xlab="age_group", ylab="freq", breaks=4,
                      main="Customer age group distribution", col="green")

#Avg utilization ratios by age groups
avguti.agegrp <- quantitative %>% group_by(age_group) %>% summarise(avg_uti = mean(Avg_Utilization_Ratio))
plot(avguti.agegrp)

#combined figure
ggplot(data = quantitative, aes(x= as.numeric(age_group), color='red')) +
  geom_histogram(bins = 4, fill="white", show.legend = FALSE, size=1.1) +
  geom_line(data = avguti.agegrp, aes(x=age_group, y=avg_uti), color= 'blue', size=1.1) +
  labs(title= 'Avg uti by age group hist', x = 'age_group', y='Count')

#Months inactive



hist(bank_data$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, main = "Churn", xlab = "Churn Seg", ylab = "Count", col = "cornflowerblue")
