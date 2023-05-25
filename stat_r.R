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
corrplot(corralted, method = 'color')

#took log and calculate corr again
log_quant <- log1p(quantitative)
corr_log_quant <- subset(log_quant, select = -c(CLIENTNUM, Dependent_count, age_group, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))
log_corralted = cor(corr_log_quant)
corrplot(log_corralted, method = 'color')

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

#########
library("dplyr")
library("corrplot")
library("caTools")
library("ggpubr")
library("ROSE")
library("correlation")


bank_data_origin <- read.csv('~/GitHub/Stats_23_Project/BankChurners.csv')
bank_data <- data.frame(bank_data_origin)

#Dimension of dataset
dim(bank_data)

#Names of the columns
col <- colnames(bank_data)

#names of quantitative columns
col_quant <- colnames(select_if(bank_data,is.numeric))

#names of qualitative columns
col_qualit <- colnames(select_if(bank_data,is.character))

#Shows structure of dataset
str(bank_data)


#View the categorical variables

table(bank_data$Attrition_Flag)

table(bank_data$Gender)

table(bank_data$Education_Level)

table(bank_data$Marital_Status)

table(bank_data$Income_Category)

table(bank_data$Card_Category)


#Correlation matrix
cor_mat <- cor(bank_data[col_quant[2:15]])
corrplot(cor_mat,method = "number",type = "upper", tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)


#We convert categorical variables into numerical
new_bank_data <- data.frame(bank_data)

new_bank_data$Attrition_Flag <- as.numeric(new_bank_data$Attrition_Flag == "Attrited Customer")

new_bank_data$Gender <- as.numeric(new_bank_data$Gender == "F")
new_bank_data <- new_bank_data %>% rename("Is_Female" = "Gender")

order_education_level <- list("Unknown" = 0,
                              "Uneducated" = 1,
                              "High School" = 2,
                              "College" = 3,
                              "Graduate" = 4,
                              "Post-Graduate" = 5,
                              "Doctorate" = 6)
new_bank_data$Education_Level <- unlist(order_education_level[as.character(new_bank_data$Education_Level)])

order_Marital_Status <- list("Unknown" = 0,
                             "Single" = 1,
                             "Married" = 2,
                             "Divorced" = 3)
new_bank_data$Marital_Status <- unlist(order_Marital_Status[as.character(new_bank_data$Marital_Status)])

order_Income_Category <- list("Unknown" = 0,
                              "Less than $40K" = 1,
                              "$40K - $60K" = 2,
                              "$60K - $80K" = 3,
                              "$80K - $120K" = 4,
                              "$120K +" = 5)
new_bank_data$Income_Category <- unlist(order_Income_Category[as.character(new_bank_data$Income_Category)])


order_Card_Category <- list("Blue" = 1,
                            "Silver" = 2,
                            "Gold" = 3,
                            "Platinum" = 4)
new_bank_data$Card_Category <- unlist(order_Card_Category[as.character(new_bank_data$Card_Category)])


#calculate skewness in quant to find which are normally dist
skewness(quantitative$Customer_Age)
skewness(quantitative$Dependent_count)
skewness(quantitative$Months_on_book)
skewness(quantitative$Total_Relationship_Count)
skewness(quantitative$Months_Inactive_12_mon)
skewness(quantitative$Contacts_Count_12_mon)
skewness(quantitative$Total_Revolving_Bal)
skewness(quantitative$Total_Trans_Ct)
skewness(quantitative$Avg_Utilization_Ratio)

#we should take log to normalize and calculate skewness again for these
skewness(quantitative$Total_Ct_Chng_Q4_Q1)
skewness(quantitative$Total_Trans_Amt)
skewness(quantitative$Total_Amt_Chng_Q4_Q1)
skewness(quantitative$Avg_Open_To_Buy)
skewness(quantitative$Credit_Limit)

#they are normally dist now
skewness(log1p(quantitative$Total_Ct_Chng_Q4_Q1))
skewness(log1p(quantitative$Total_Trans_Amt))
skewness(log1p(quantitative$Total_Amt_Chng_Q4_Q1))
skewness(log1p(quantitative$Avg_Open_To_Buy))
skewness(log1p(quantitative$Credit_Limit))

quantitative$Total_Ct_Chng_Q4_Q1 <- log1p(quantitative$Total_Ct_Chng_Q4_Q1)
colnames(quantitative)[14] <- "log_Total_Ct_Chng_Q4_Q1"

quantitative$Total_Trans_Amt <- log1p(quantitative$Total_Trans_Amt)
colnames(quantitative)[12] <- "log_Total_Trans_Amt"

quantitative$Total_Amt_Chng_Q4_Q1 <- log1p(quantitative$Total_Amt_Chng_Q4_Q1)
colnames(quantitative)[11] <- "log_Total_Amt_Chng_Q4_Q1"

quantitative$Avg_Open_To_Buy <- log1p(quantitative$Avg_Open_To_Buy)
colnames(quantitative)[10] <- "log_Avg_Open_To_Buy"

quantitative$Credit_Limit <- log1p(quantitative$Credit_Limit)
colnames(quantitative)[8] <- "log_Credit_Limit"



#calculate skewness in qual_to_quant to find which are normally dist
skewness(qual_to_quant$Gender)
skewness(qual_to_quant$Education_Level)
skewness(qual_to_quant$Marital_Status)
skewness(qual_to_quant$Income_Category)

skewness(exp(qual_to_quant$Card_Category))
hist(qual_to_quant$Card_Category)

# Train-test Split

set.seed(0987)

sample <- sample.split(new_bank_data$Attrition_Flag,SplitRatio = 0.75)
train <- subset(new_bank_data[2:21],sample == TRUE)
test <- subset(new_bank_data[2:21],sample == FALSE)

#Proportion of Attrited and Existing Customer
prop.table(table(train$Attrition_Flag))
prop.table(table(test$Attrition_Flag))


#Original proportion of Attrited and Existing Customer
prop.table(table(new_bank_data$Attrition_Flag))

#It's an unbalanced dataset.
#It might be better to consider a resampling of the dataset

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N =7595)$data



#Thresholds for classification:
threshold1 <- 0.4
threshold2 <- 0.5
threshold3 <- 0.6









#This part is to check how the rows containg at least one "unknown" are distributed (Probably useless)

#Change Unknown value to NA
bank_data_copy <- data.frame(bank_data)
bank_data_copy[bank_data_copy=='Unknown'] <- NA

#Build a dataset without missing values
bank_data_rev <- na.omit(bank_data_copy)

# Number of rows containing at least one "Unknown"
dim(bank_data_copy)[1] - dim(bank_data_rev)[1]

#Split the initial data based on attrition flag
bank_data_split <- split(bank_data,bank_data$Attrition_Flag)

dim(bank_data_split$`Attrited Customer`)
dim(bank_data_split$`Existing Customer`)

#We check how the rows containing "Unknown" are distributed in relation to the split dataset

bank_data_split$`Attrited Customer`[bank_data_split$`Attrited Customer`=='Unknown'] <- NA
(dim(bank_data_split$`Attrited Customer`)[1]-dim(na.omit(bank_data_split$`Attrited Customer`))[1])/dim(bank_data_split$`Attrited Customer`)[1]

bank_data_split$`Existing Customer`[bank_data_split$`Existing Customer`=='Unknown'] <- NA
(dim(bank_data_split$`Existing Customer`)[1]-dim(na.omit(bank_data_split$`Existing Customer`))[1])/dim(bank_data_split$`Existing Customer`)[1]


model <- lm(Attrition_Flag ~ ., data = train_mix)
summary(model)$adj.r.squared
summary(model)$coeff

library(olsrr)
model_back1 <- ols_step_backward_p(model, prem = 0.05, progress = TRUE, details = TRUE)

model_back1$adjr

model_back1$rmse










#we dont need to normalize function because we are calculating the skewness of all columns
#function to normalize value
normalizer_fnc <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

quan_normalized <- as.data.frame(lapply(quantitative[2:18], normalizer_fnc))
qual_to_quant_normalized <- as.data.frame(lapply(qual_to_quant[2:6], normalizer_fnc))


#we should calculate this in one table
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
