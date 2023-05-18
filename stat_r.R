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
#using the histogram, dividing ages into 4 groups seems satisfying

#Creating age groups
quantitative[quantitative$Customer_Age <= 34, "age_group"] <- 1
quantitative[quantitative$Customer_Age > 34 & quantitative$Customer_Age <= 44, "age_group"] <- 2
quantitative[quantitative$Customer_Age > 44 & quantitative$Customer_Age <= 54, "age_group"] <- 3
quantitative[quantitative$Customer_Age > 54, "age_group"] <- 4

unique(bank_data$Attrition_Flag) #to make sure there are only 2 strings
#change 'Existing Customer' to 1 and 'Attrited Customer' to 0 and add new column to quantitative
quantitative$attrition_flag_binary <- ifelse(bank_data$Attrition_Flag=='Existing Customer', 1, 0)

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
library(yarrr) #to make colors transparent
hist(bank_data$Months_Inactive_12_mon, col = yarrr::transparent('red',trans.val = 0.9))
hist(bank_data$Contacts_Count_12_mon, col = yarrr::transparent('blue', trans.val = 0.8), add = TRUE)

hist(bank_data$Total_Trans_Ct)

#investigate correlation
#we can add one column after we decide conditions as churn number
#and we can investigate the correlation this column with other columns
library(corrplot)
corr_quant <- subset(quantitative, select = -c(CLIENTNUM, Dependent_count, age_group, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))
corralted = cor(corr_quant)
corrplot(corralted, method = 'color', order = 'alphabet')

#create copy of qualitative data and make it quantitative
qual_to_quant <- qualitative

#change categories to the numeric values
qual_to_quant$Attrition_Flag <- ifelse(qual_to_quant$Attrition_Flag == 'Existing Customer', 1, 0)

qual_to_quant$Gender <- ifelse(qual_to_quant$Gender == 'M', 1, 0)

unique(bank_data$Education_Level)
qual_to_quant$Education_Level[qual_to_quant$Education_Level == 'Unknown'] <- 0
qual_to_quant$Education_Level[qual_to_quant$Education_Level == 'Uneducated'] <- 1
qual_to_quant$Education_Level[qual_to_quant$Education_Level == 'High School'] <- 2
qual_to_quant$Education_Level[qual_to_quant$Education_Level == 'College'] <- 3
qual_to_quant$Education_Level[qual_to_quant$Education_Level == 'Graduate'] <- 4
qual_to_quant$Education_Level[qual_to_quant$Education_Level == 'Post-Graduate'] <- 5
qual_to_quant$Education_Level[qual_to_quant$Education_Level == 'Doctorate'] <- 6
qual_to_quant$Education_Level = as.numeric(as.character(qual_to_quant$Education_Level))

unique(bank_data$Marital_Status)
qual_to_quant$Marital_Status[qual_to_quant$Marital_Status == 'Unknown'] <- 0
qual_to_quant$Marital_Status[qual_to_quant$Marital_Status == 'Single'] <- 1
qual_to_quant$Marital_Status[qual_to_quant$Marital_Status == 'Married'] <- 2
qual_to_quant$Marital_Status[qual_to_quant$Marital_Status == 'Divorced'] <- 3
qual_to_quant$Marital_Status = as.numeric(as.character(qual_to_quant$Marital_Status))

unique(bank_data$Income_Category)
qual_to_quant$Income_Category[qual_to_quant$Income_Category == 'Unknown'] <- 0
qual_to_quant$Income_Category[qual_to_quant$Income_Category == 'Less than $40K'] <- 1
qual_to_quant$Income_Category[qual_to_quant$Income_Category == '$40K - $60K'] <- 2
qual_to_quant$Income_Category[qual_to_quant$Income_Category == '$60K - $80K'] <- 3
qual_to_quant$Income_Category[qual_to_quant$Income_Category == '$80K - $120K'] <- 4
qual_to_quant$Income_Category[qual_to_quant$Income_Category == '$120K +'] <- 5
qual_to_quant$Income_Category = as.numeric(as.character(qual_to_quant$Income_Category))

unique(bank_data$Card_Category)
qual_to_quant$Card_Category[qual_to_quant$Card_Category == 'Silver'] <- 0
qual_to_quant$Card_Category[qual_to_quant$Card_Category == 'Gold'] <- 1
qual_to_quant$Card_Category[qual_to_quant$Card_Category == 'Platinum'] <- 2
qual_to_quant$Card_Category[qual_to_quant$Card_Category == 'Blue'] <- 3
qual_to_quant$Card_Category = as.numeric(as.character(qual_to_quant$Card_Category))

corr_qual_to_quant = cor(qual_to_quant)
corrplot(corr_qual_to_quant, method = 'number')

#histograms
hist(bank_data$Avg_Utilization_Ratio)
hist(log1p(bank_data$Avg_Utilization_Ratio))

hist(bank_data$Avg_Open_To_Buy)
hist(log1p(bank_data$Avg_Open_To_Buy))
hist(bank_data$Avg_Open_To_Buy)

mod.out <- glm()