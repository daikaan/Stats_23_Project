bank_data <- read.csv('~/GitHub/Stats_23_Project/BankChurners.csv')

# client_num should be added to qualitative as well as primary key
# dependent_count to qualitative? 

head(bank_data)
dim(bank_data) #10127 rows and 23 columns
summary(bank_data)

col = c('CLIENTNUM', 'Attrition_Flag', 'Customer_Age',	'Gender',	'Dependent_count', 'Education_Level',	'Marital_Status',	'Income_Category', 'Card_Category',	'Months_on_book',	'Total_Relationship_Count', 'Months_Inactive_12_mon',	'Contacts_Count_12_mon', 'Credit_Limit', 'Total_Revolving_Bal',	'Avg_Open_To_Buy', 'Total_Amt_Chng_Q4_Q1', 'Total_Trans_Amt', 'Total_Trans_Ct', 'Total_Ct_Chng_Q4_Q1',	'Avg_Utilization_Ratio',	'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1',	'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2')

library(knitr)
library(forecast)
library(ggplot2)
library(PCAmixdata)
library(dplyr)

#FEATURES: (I copied them from another project)
# No NA
# Clientum: represents the unique IDs of customers. It is formed by a unique sequence of 9 digits. There is a total of 10,127 unique customers in the datasets.
# Attrition_Flag: this target/output variable represents the current status of customers. It has two unique values: one is Existing Customer (current customer) and Attrited Customer (churned customer).
# Customer_Age: this variable consist of the age of customers. The age range of customers is between 27 and 73.
# Gender: this variable is coded as F for Female and M for Male.
# Dependent_count: this variable represents the number of dependents associated with a customer.
# Education_Level: this variable represents the educational qualification of a customer. It consist of 7 unique values which are High School, Graduate, Uneducated, College, Post-graduate, Doctorate and Unknown. The Unknown group has 1519 customers.
# Marital_Status: this variable represents the marital status of customers. It has 4 unique values which are Married, Single, Unknown, Divorced. The Unknown group has 749 customers.
# Income_Category: this variable represents the annual income category of card holder: Less than  40K,
# 40k- 60K,
# 60K- 80K,
# 80K-120K, $120+, Unknown. The Unknown group has 1112 customer in this category.
# Card_Category: this is a product variable that represents the credit card type. It has 4 unique values - Blue, Gold, Silver and Platinum.
# Months_on_book: represents the number of months (period) the account holder has been a customer in the bank.
# Total_Relationship_Count: represents the number of products held by the customer.
# Months_Inactive_12_mon: this is the number of months a customer has been inactive in the last 12 months (1 year).
# Contacts_Count_12_mon: this is the number of times a customer has made contact with the bank.
# Credit_Limit: this is the credit limit on the credit card owned by customer.
# Total_Revolving_Bal: represents total revolving balance on the credit card.
# Avg_Open_To_Buy: represents the average Open to Buy Credit Line for last 12 months.
# Total_Amt_Chng_Q4_Q1: represents the change in transaction amount from Q4 over Q1.
# Total_Trans_Amt: represents the total transaction amount in the last 12 months.
# Total_Trans_Ct: represents the total transaction count in the last 12 months.
# Total_Ct_Chng_Q4_Q1: represents the change in transaction count from Q4 over Q1.
# Avg_Utilization_Ratio: represents the average card utilization ratio.


data.split <- splitmix(bank_data)
quantitative <- data.split$X.quanti
qualitative <- data.split$X.quali

length(quantitative)
length(qualitative)

#Checking if there is any null value in the data set
colSums(is.na(bank_data)) #no null value

# Descriptive Graphs

#Customer age

#boxplot
cust.age.boxplot <- boxplot(quantitative$Customer_Age, ylab = "age")
cust.age.boxplot

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, 
#it is seen that customers over the age of 70 are outliers
boxplot.stats(quantitative$Customer_Age)$out


#histogram
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

# grouped age piechart
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 
cust.age.piechart <- pie(count(quantitative, age_group)$n, border="white", col=myPalette)
cust.age.piechart

# months on book (how long a customer is using the bank)

#boxplot
months.onbook.boxplot <- boxplot(quantitative$Months_on_book, ylab = "months")
months.onbook.boxplot

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, outliers
boxplot.stats(quantitative$Months_on_book)$out

# Credit Limit

#boxplot
credit.limit.boxplot <- boxplot(quantitative$Credit_Limit, ylab = "months")
credit.limit.boxplot

#outlier extraction using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule
boxplot.stats(quantitative$Credit_Limit)$out

#Avg utilization ratios by age groups
avguti.agegrp <- quantitative %>% group_by(age_group) %>% summarise(avg_uti = mean(Avg_Utilization_Ratio))
plot(avguti.agegrp, type = "o")

#combined figure
ggplot(data = quantitative, aes(x= as.numeric(age_group), color='red')) +
  geom_histogram(bins = 4, fill="white", show.legend = FALSE, size=1.1) +
  geom_line(data = avguti.agegrp, aes(x=age_group, y=avg_uti), color= 'blue', size=1.1) +
  labs(title= 'Avg uti by age group hist', x = 'age_group', y='Count') +   scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./10000, name="Second Axis")
  )

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

#function to normalize value
normalizer_fnc <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

quan_normalized <- as.data.frame(lapply(quantitative[2:18], normalizer_fnc))
qual_to_quant_normalized <- as.data.frame(lapply(qual_to_quant[2:6], normalizer_fnc))



#we need to calculate chi square for the categorical values 
#to see are they dependent or not
#assume conf interval 95%
table(quantitative$attrition_flag_binary, quantitative$age_group)
chisq.test(quantitative$attrition_flag_binary, quantitative$age_group, correct=FALSE)

table(quantitative$attrition_flag_binary, quantitative$Dependent_count)
chisq.test(quantitative$attrition_flag_binary, quantitative$Dependent_count, correct=FALSE)

table(quantitative$attrition_flag_binary, quantitative$Total_Relationship_Count)
chisq.test(quantitative$attrition_flag_binary, quantitative$Total_Relationship_Count, correct=FALSE)

table(quantitative$attrition_flag_binary, quantitative$Contacts_Count_12_mon)
chisq.test(quantitative$attrition_flag_binary, quantitative$Contacts_Count_12_mon, correct=FALSE)

table(qual_to_quant$Attrition_Flag, qual_to_quant$Gender)
chisq.test(qual_to_quant$Attrition_Flag, qual_to_quant$Gender, correct=FALSE)

table(qual_to_quant$Attrition_Flag, qual_to_quant$Education_Level)
chisq.test(qual_to_quant$Attrition_Flag, qual_to_quant$Education_Level, correct=FALSE)

table(qual_to_quant$Attrition_Flag, qual_to_quant$Marital_Status)
chisq.test(qual_to_quant$Attrition_Flag, qual_to_quant$Marital_Status, correct=FALSE)

table(qual_to_quant$Attrition_Flag, qual_to_quant$Income_Category)
chisq.test(qual_to_quant$Attrition_Flag, qual_to_quant$Income_Category, correct=FALSE)

#simulate.p.value
table(qual_to_quant$Attrition_Flag, qual_to_quant$Card_Category)
chisq.test(qual_to_quant$Attrition_Flag, qual_to_quant$Card_Category, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Months_Inactive_12_mon)
chisq.test(quantitative$attrition_flag_binary, quantitative$Months_Inactive_12_mon, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Months_on_book)
chisq.test(quantitative$attrition_flag_binary, quantitative$Months_on_book, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Credit_Limit)
chisq.test(quantitative$attrition_flag_binary, quantitative$Credit_Limit, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Total_Revolving_Bal)
chisq.test(quantitative$attrition_flag_binary, quantitative$Total_Revolving_Bal, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Avg_Open_To_Buy)
chisq.test(quantitative$attrition_flag_binary, quantitative$Avg_Open_To_Buy, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Total_Amt_Chng_Q4_Q1)
chisq.test(quantitative$attrition_flag_binary, quantitative$Total_Amt_Chng_Q4_Q1, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Total_Trans_Amt)
chisq.test(quantitative$attrition_flag_binary, quantitative$Total_Trans_Amt, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Total_Trans_Ct)
chisq.test(quantitative$attrition_flag_binary, quantitative$Total_Trans_Ct, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Total_Ct_Chng_Q4_Q1)
chisq.test(quantitative$attrition_flag_binary, quantitative$Total_Ct_Chng_Q4_Q1, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Avg_Utilization_Ratio)
chisq.test(quantitative$attrition_flag_binary, quantitative$Avg_Utilization_Ratio, correct=FALSE, simulate.p.value=TRUE)

table(quantitative$attrition_flag_binary, quantitative$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1)
chisq.test(quantitative$attrition_flag_binary, quantitative$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, correct=FALSE, simulate.p.value=TRUE)


#histograms
hist(bank_data$Avg_Utilization_Ratio)
hist(log1p(bank_data$Avg_Utilization_Ratio))

hist(bank_data$Avg_Open_To_Buy)
hist(log1p(bank_data$Avg_Open_To_Buy))
hist(bank_data$Avg_Open_To_Buy)
