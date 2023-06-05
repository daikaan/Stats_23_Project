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


#Checking if there is any null value in the data set
colSums(is.na(bank_data)) #no null value

#Customer age

#boxplot
cust.age.boxplot <- boxplot(quantitative$Customer_Age, ylab = "age")
cust.age.boxplot

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, 
#it is seen that customers over the age of 70 are outliers
age.exc.list <- boxplot.stats(quantitative$Customer_Age)$out

quantitative_less70 <- subset(quantitative,!(Customer_Age %in% age.exc.list))
boxplot(quantitative_less70$Customer_Age, ylab = "age")

#Card Category
ggplot(bank_data, aes(x=Months_on_book, y= Credit_Limit, shape = Card_Category, color= Card_Category))+
  geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

card.exc.list <- c("Silver", "Platinum", "Gold")

#bank_data_bluecard <- subset(bank_data,!(Card_Category %in% card.exc.list))


bank_data_cleaned <- subset(bank_data,!((Customer_Age %in% age.exc.list)| (Card_Category %in% card.exc.list)))

boxplot(bank_data_cleaned$Customer_Age, ylab = "age")

# Descriptive Graphs
#histogram
Cust.age.hist <- hist(bank_data_cleaned$Customer_Age, xlab="age", ylab="freq",
                      main="Customer age distribution", col="orange")
Cust.age.hist
#using the histogram, dividing ages into 4 groups seems satisfying

#Creating age groups
bank_data_cleaned[bank_data_cleaned$Customer_Age <= 34, "age_group"] <- 1
bank_data_cleaned[bank_data_cleaned$Customer_Age > 34 & bank_data_cleaned$Customer_Age <= 44, "age_group"] <- 2
bank_data_cleaned[bank_data_cleaned$Customer_Age > 44 & bank_data_cleaned$Customer_Age <= 54, "age_group"] <- 3
bank_data_cleaned[bank_data_cleaned$Customer_Age > 54, "age_group"] <- 4

#grouped age histogram
Grouped.age.hist <- hist(as.numeric(bank_data_cleaned$age_group), xlab="age_group", ylab="freq", breaks=4,
                         main="Customer age group distribution", col="green")


# grouped age piechart
library(RColorBrewer)#for the
myPalette <- brewer.pal(6, "Set2") 
cust.age.piechart <- pie(count(bank_data_cleaned, age_group)$n, border="white", col=myPalette)

#Dependent Count

ggplot(bank_data_cleaned, aes(x=Dependent_count)) +
  geom_bar(width=1)

depcount.labels <- c(0, 1, 2, 3, 4, 5)
dependent.count.piechart <- pie(count(bank_data_cleaned, Dependent_count)$n, border="white", col=myPalette, labels = depcount.labels)

# months on book (how long a customer is using the bank)
#histogram
hist(bank_data_cleaned$Months_on_book)

#boxplot
months.onbook.boxplot <- boxplot(quantitative$Months_on_book, ylab = "months")

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, outliers
boxplot.stats(quantitative$Months_on_book)$out

#Since the outliers in months on books can be identifying on whether the customer is going to churn we decided to keep them in the data set

#Total Relationships Count
ggplot(bank_data_cleaned, aes(x=Total_Relationship_Count)) +
  geom_bar(width=1)

#Months_Inactive_12months
ggplot(bank_data_cleaned, aes(x=Months_Inactive_12_mon)) +
  geom_bar(width=1)

#Contacts Count 12 months
ggplot(bank_data_cleaned, aes(x=Contacts_Count_12_mon)) +
  geom_bar(width=1)

# Credit Limit

#boxplot
credit.limit.boxplot <- boxplot(bank_data_cleaned$Credit_Limit, ylab = "Dollars")

#histogram
hist(bank_data_cleaned$Credit_Limit)


# Total Revolving Balance
#histogram
hist(bank_data_cleaned$Total_Revolving_Bal)

#Average Open to Buy
#histogram
hist(bank_data_cleaned$Avg_Open_To_Buy)

#Total Amount Change Between Q1 and Q4

#histogram
hist(bank_data_cleaned$Total_Amt_Chng_Q4_Q1)

#boxplot
credit.limit.boxplot <- boxplot(bank_data_cleaned$Total_Amt_Chng_Q4_Q1, ylab = "Dollars")

#Total Transaction Amount

#histogram
hist(bank_data_cleaned$Total_Trans_Amt)

#Total Count Change Between Q1 and Q4

#histogram
hist(bank_data_cleaned$Total_Ct_Chng_Q4_Q1)

#Average Utilization Rate

#histogram
hist(bank_data_cleaned$Avg_Utilization_Ratio)

unique(bank_data_cleaned$Attrition_Flag) #to make sure there are only 2 strings
#change 'Existing Customer' to 1 and 'Attrited Customer' to 0 and add new column to quantitative
data.split <- splitmix(bank_data_cleaned)
quantitative <- data.split$X.quanti
qualitative <- data.split$X.quali

length(quantitative)
length(qualitative)
quantitative$attrition_flag_binary <- ifelse(bank_data_cleaned$Attrition_Flag=='Existing Customer', 1, 0)

#grouped age histogram
Grouped.age.hist <- hist(as.numeric(quantitative$age_group), xlab="age_group", ylab="freq", breaks=4,
                      main="Customer age group distribution", col="green")

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

