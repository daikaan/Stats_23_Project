bank_data <- read.csv('~/GitHub/Stats_23_Project/BankChurners.csv')

#########
library("dplyr")
library("corrplot")
library("caTools")
library("ggpubr")
library("ROSE")
library("correlation")
library(moments) #to calculate skewness
library(olsrr) #to use ols_step_backward_p
library(MASS)
library(knitr)
library(forecast)
library(ggplot2)
library(PCAmixdata)
library(purrr)
library(formattable) # for giving a variable dictionary a better look

summary(bank_data)

# BRIEF DESCRIPTION OF EACH VARIABLE
var.names <- c("Clientnum", "Attrition_Flag", "Customer_Age", "Gender", "Dependent_count", 
               "Education_Level", "Marital_Status", "Income_Category", "Card_Category", "Months_on_book",
               "Total_Relationship_Count", "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit",
               "Total_Revolving_Bal", "Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1", "Total_Trans_Amt",
               "Total_Trans_Ct", "Total_Ct_Chng_Q4_Q1", "Avg_Utilization_Ratio")
descriptions <- c("refers to the distinct identification numbers assigned to customers, consisting of a unique sequence of 9 digits. The datasets contain a total of 10,127 customers with unique IDs.",
                  
                  "refers to the current status of customers, indicating whether they are Existing Customers (current customers) or Attrited Customers (churned customers). There are two distinct values for this target/output variable.",
                  
                  "represents the age of customers, with a range between 27 and 73.",
                  
                  "is encoded as 'F' for Female and 'M' for Male.",
                  
                  "represents the number of dependents associated with a customer.",
                  
                  "represents the educational qualification of a customer. It encompasses seven distinct values: High School, Graduate, Uneducated, College, Post-graduate, Doctorate, and Unknown. The Unknown category includes 1519 customers.",
                  
                  "represents the marital status of customers, with four unique values: Married, Single, Unknown, and Divorced. The Unknown category includes 749 customers.",
                  
                  "represents the annual income category of cardholders: Less than 40K, 40K-60K, 60K-80K, 80K-120K, $120+, and Unknown. The Unknown category includes 1112 customers.",
                  
                  "refers to a product variable that indicates the type of credit card held by customers. It includes four unique values: Blue, Gold, Silver, and Platinum.",
                  
                  "represents the duration, in months, that an account holder has been a customer at the bank.",
                  
                  "represents the number of products held by a customer.",
                  
                  "represents the number of months during which a customer has been inactive in the last 12 months (1 year).",
                  
                  "represents the number of times a customer has contacted the bank.",
                  
                  "represents the credit limit on the customer's credit card.",
                  
                  "represents the total revolving balance on the customer's credit card.",
                  
                  "represents the average Open to Buy Credit Line for the last 12 months.",
                  
                  "represents the change in transaction amount from the fourth quarter (Q4) to the first quarter (Q1).",
                  
                  "represents the total transaction amount in the last 12 months.",
                  
                  "represents the total transaction count in the last 12 months.",
                  
                  "represents the change in transaction count from the fourth quarter (Q4) to the first quarter (Q1).",
                  
                  "represents the average card utilization ratio.")
var.dict <- as.data.frame(descriptions, row.names = var.names, )
formattable(var.dict)

# DATA PREPERATION

#display of the categorical variables
table(bank_data$CLIENTNUM)

table(bank_data$Attrition_Flag)

table(bank_data$Gender)

table(bank_data$Education_Level)

table(bank_data$Marital_Status)

table(bank_data$Income_Category)

table(bank_data$Card_Category)

#Change Unknown value to NA
bank_data_NA <- data.frame(bank_data)
bank_data_NA[bank_data_NA=='Unknown'] <- NA

#Build a dataset without missing values
bank_data_withoutNA <- na.omit(bank_data_NA)


#We convert categorical variables into numerical
bank_data_withoutNA_quan <- data.frame(bank_data_withoutNA)

bank_data_withoutNA_quan$Attrition_Flag <- as.numeric(bank_data_withoutNA_quan$Attrition_Flag == "Attrited Customer")

bank_data_withoutNA_quan$Gender <- as.numeric(bank_data_withoutNA_quan$Gender == "F")
bank_data_withoutNA_quan <- bank_data_withoutNA_quan %>% rename("Is_Female" = "Gender")

order_education_level <- list("Unknown" = 0,
                              "Uneducated" = 1,
                              "High School" = 2,
                              "College" = 3,
                              "Graduate" = 4,
                              "Post-Graduate" = 5,
                              "Doctorate" = 6)
bank_data_withoutNA_quan$Education_Level <- unlist(order_education_level[as.character(bank_data_withoutNA_quan$Education_Level)])

order_Marital_Status <- list("Unknown" = 0,
                             "Single" = 1,
                             "Married" = 2,
                             "Divorced" = 3)
bank_data_withoutNA_quan$Marital_Status <- unlist(order_Marital_Status[as.character(bank_data_withoutNA_quan$Marital_Status)])

order_Income_Category <- list("Unknown" = 0,
                              "Less than $40K" = 1,
                              "$40K - $60K" = 2,
                              "$60K - $80K" = 3,
                              "$80K - $120K" = 4,
                              "$120K +" = 5)
bank_data_withoutNA_quan$Income_Category <- unlist(order_Income_Category[as.character(bank_data_withoutNA_quan$Income_Category)])


# Extracting Outliers from age and Rescoping the study to only focus on Blue Cards

#Customer age

#boxplot
cust.age.boxplot <- boxplot(cleaned_bank_data_withoutNA_quan$Customer_Age, ylab = "age")
cust.age.boxplot

# months on book (how long a customer is using the bank)
months.onbook.boxplot <- boxplot(quantitative$Months_on_book, ylab = "months")

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, outliers
boxplot.stats(quantitative$Months_on_book)$out

#Since the outliers in months on books can be identifying on whether the customer is going to churn we decided to keep them in the data set


#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, 
#it is seen that customers over the age of 70 are outliers
age.exc.list <- boxplot.stats(cleaned_bank_data_withoutNA_quan$Customer_Age)$out

# Card Category

ggplot(cleaned_bank_data_withoutNA_quan, aes(x = as.factor(Card_Category), fill = factor(Attrition_Flag))) +
  geom_bar() +
  labs(fill = "Attrition Flag") +
  theme_minimal()

ggplot(cleaned_bank_data_withoutNA_quan, aes(x=Months_on_book, y= Credit_Limit, shape = as.factor(Card_Category), color= as.factor(Card_Category)))+
  geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Since most of the data is coming from the Blue cards and there is a visible difference on many parameters among categories, we decided to only focus on blue card category
card.exc.list <- c("Silver", "Gold", "Platinum")

cleaned_bank_data_withoutNA_quan <- subset(cleaned_bank_data_withoutNA_quan,!((Customer_Age %in% age.exc.list)| (Card_Category %in% card.exc.list)))
cleaned_bank_data_withoutNA_quan


# Descriptive Graphs
#histogram
Cust.age.hist <- hist(cleaned_bank_data_withoutNA_quan$Customer_Age, xlab="age", ylab="freq",
                      main="Customer age distribution", col="orange")
Cust.age.hist
#using the histogram, dividing ages into 4 groups seems satisfying

#Creating age groups
cleaned_bank_data_withoutNA_quan[cleaned_bank_data_withoutNA_quan$Customer_Age <= 34, "age_group"] <- 1
cleaned_bank_data_withoutNA_quan[cleaned_bank_data_withoutNA_quan$Customer_Age > 34 & cleaned_bank_data_withoutNA_quan$Customer_Age <= 44, "age_group"] <- 2
cleaned_bank_data_withoutNA_quan[cleaned_bank_data_withoutNA_quan$Customer_Age > 44 & cleaned_bank_data_withoutNA_quan$Customer_Age <= 54, "age_group"] <- 3
cleaned_bank_data_withoutNA_quan[cleaned_bank_data_withoutNA_quan$Customer_Age > 54, "age_group"] <- 4

#grouped age histogram
Grouped.age.hist <- hist(as.numeric(cleaned_bank_data_withoutNA_quan$age_group), xlab="age_group", ylab="freq", breaks=4,
                         main="Customer age group distribution", col="green")


# grouped age piechart
library(RColorBrewer)#for the
myPalette <- brewer.pal(6, "Set2") 
age.labels <- c("<=34", "35-44", "45-54", ">=55")
cust.age.piechart <- pie(count(cleaned_bank_data_withoutNA_quan, age_group)$n, border="white", col=myPalette, labels = age.labels)

#Dependent Count

depcount.labels <- c(0, 1, 2, 3, 4, 5)
dependent.count.piechart <- pie(count(cleaned_bank_data_withoutNA_quan, Dependent_count)$n, border="white", col=myPalette, labels = depcount.labels)

# Credit Limit
credit.limit.boxplot <- boxplot(cleaned_bank_data_withoutNA_quan$Credit_Limit, ylab = "Dollars")
total.amtchg.boxplot <- boxplot(cleaned_bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1, ylab = "Dollars")


#Months inactive
library(yarrr) #to make colors transparent
barplot(table(factor(Months_Inactive_12_mon,levels=min(Months_Inactive_12_mon):max(Months_Inactive_12_mon))), col = yarrr::transparent('red',trans.val = 0.9))
barplot(table(factor(Contacts_Count_12_mon,levels=min(Contacts_Count_12_mon):max(Contacts_Count_12_mon))), col = yarrr::transparent('blue', trans.val = 0.8), add = TRUE)

hist(cleaned_bank_data_withoutNA_quan$Total_Trans_Ct)

int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}


unique(cleaned_bank_data_withoutNA_quan$Attrition_Flag) #to make sure there are only 2 strings
#change 'Existing Customer' to 1 and 'Attrited Customer' to 0 and add new column to quantitative
data.split <- splitmix(cleaned_bank_data_withoutNA_quan)
quantitative <- data.split$X.quanti
qualitative <- data.split$X.quali

length(quantitative)
length(qualitative)
quantitative$attrition_flag_binary <- ifelse(cleaned_bank_data_withoutNA_quan$Attrition_Flag=='Existing Customer', 1, 0)


dev.off(dev.list()["RStudioGD"]) #to clear the previous plots on the screen

#Histograms
attach(cleaned_bank_data_withoutNA_quan)
par(mfrow=c(3,2))
hist(Avg_Open_To_Buy)
hist(Total_Trans_Amt)
hist(Avg_Utilization_Ratio)
hist(Months_on_book)
hist(Credit_Limit)
hist(Months_Inactive_12_mon)


#Categorical Value Visualizations

#Bar plots

par(mfrow=c(2,2))
barplot(height=tabulate(as.factor(Income_Category)), names=unique(Income_Category), col= myPalette)
barplot(height=tabulate(as.factor(Marital_Status)), names=unique(Marital_Status), col= myPalette)
barplot(height=tabulate(as.factor(Education_Level)), names=unique(Education_Level), col= myPalette)


#grouped age histogram
par(mfrow=c(1,1))
Grouped.age.hist <- hist(as.numeric(cleaned_bank_data_withoutNA_quan$age_group), xlab="age_group", ylab="freq",
                         main="Customer age group distribution", col="green")


#Correlation matrix
cor_mat_new <- cor(bank_data_withoutNA_quan[2:15])
corrplot(cor_mat_new,method = "number",type = "upper", tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)


#calculate skewness in quant to find which are normally dist
skewness(cleaned_bank_data_withoutNA_quan$Customer_Age)
skewness(cleaned_bank_data_withoutNA_quan$Dependent_count)
skewness(cleaned_bank_data_withoutNA_quan$Months_on_book)
skewness(cleaned_bank_data_withoutNA_quan$Total_Relationship_Count)
skewness(cleaned_bank_data_withoutNA_quan$Months_Inactive_12_mon)
skewness(cleaned_bank_data_withoutNA_quan$Contacts_Count_12_mon)
skewness(cleaned_bank_data_withoutNA_quan$Total_Revolving_Bal)
skewness(cleaned_bank_data_withoutNA_quan$Total_Trans_Ct)
skewness(cleaned_bank_data_withoutNA_quan$Avg_Utilization_Ratio)
skewness(cleaned_bank_data_withoutNA_quan$Is_Female)
skewness(cleaned_bank_data_withoutNA_quan$Education_Level)
skewness(cleaned_bank_data_withoutNA_quan$Marital_Status)
skewness(cleaned_bank_data_withoutNA_quan$Income_Category)


#we should take log to normalize and calculate skewness again for these
skewness(cleaned_bank_data_withoutNA_quan$Total_Ct_Chng_Q4_Q1)
skewness(cleaned_bank_data_withoutNA_quan$Total_Trans_Amt)
skewness(cleaned_bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1)
skewness(cleaned_bank_data_withoutNA_quan$Avg_Open_To_Buy)
skewness(cleaned_bank_data_withoutNA_quan$Credit_Limit)

#they are normally dist now
skewness(log1p(cleaned_bank_data_withoutNA_quan$Total_Ct_Chng_Q4_Q1))
skewness(log1p(cleaned_bank_data_withoutNA_quan$Total_Trans_Amt))
skewness(log1p(cleaned_bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1))
skewness(log1p(cleaned_bank_data_withoutNA_quan$Avg_Open_To_Buy))
skewness(log1p(cleaned_bank_data_withoutNA_quan$Credit_Limit))


log_cleaned_bank_data_withoutNA_quan <- cleaned_bank_data_withoutNA_quan

log_cleaned_bank_data_withoutNA_quan$Total_Ct_Chng_Q4_Q1 <- log1p(log_cleaned_bank_data_withoutNA_quan$Total_Ct_Chng_Q4_Q1)
colnames(log_cleaned_bank_data_withoutNA_quan)[20] <- "log_Total_Ct_Chng_Q4_Q1"

log_cleaned_bank_data_withoutNA_quan$Total_Trans_Amt <- log1p(log_cleaned_bank_data_withoutNA_quan$Total_Trans_Amt)
colnames(log_cleaned_bank_data_withoutNA_quan)[18] <- "log_Total_Trans_Amt"

log_cleaned_bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1 <- log1p(log_cleaned_bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1)
colnames(log_cleaned_bank_data_withoutNA_quan)[17] <- "log_Total_Amt_Chng_Q4_Q1"

log_cleaned_bank_data_withoutNA_quan$Avg_Open_To_Buy <- log1p(log_cleaned_bank_data_withoutNA_quan$Avg_Open_To_Buy)
colnames(log_cleaned_bank_data_withoutNA_quan)[16] <- "log_Avg_Open_To_Buy"

log_cleaned_bank_data_withoutNA_quan$Credit_Limit <- log1p(log_cleaned_bank_data_withoutNA_quan$Credit_Limit)
colnames(log_cleaned_bank_data_withoutNA_quan)[14] <- "log_Credit_Limit"

#since we have only one card category we can remove it

log_cleaned_bank_data_withoutNA_quan <- subset(log_cleaned_bank_data_withoutNA_quan, select = -c(Card_Category))

cleaned_bank_data_withoutNA_quan <- subset(cleaned_bank_data_withoutNA_quan, select = -c(Card_Category))

#Thresholds for classification:
threshold1 <- 0.4
threshold2 <- 0.5
threshold3 <- 0.6


set.seed(0987)

sample <- sample.split(log_cleaned_bank_data_withoutNA_quan$Attrition_Flag,SplitRatio = 0.75)
train <- subset(log_cleaned_bank_data_withoutNA_quan[2:20],sample == TRUE)
test <- subset(log_cleaned_bank_data_withoutNA_quan[2:20],sample == FALSE)

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N = nrow(log_cleaned_bank_data_withoutNA_quan))$data


model <- glm(Attrition_Flag ~ ., data = train_mix, family = 'binomial')
summary(model)$coeff

summary(model)

pred <- (predict(model, test) >= 0.5)*1

mean(test$Attrition_Flag == pred)


log_cleaned_bank_data_withoutNA_quan1 <- subset(log_cleaned_bank_data_withoutNA_quan, select = -c(Months_on_book))

set.seed(0987)

sample <- sample.split(log_cleaned_bank_data_withoutNA_quan1$Attrition_Flag,SplitRatio = 0.75)
train <- subset(log_cleaned_bank_data_withoutNA_quan1[2:19],sample == TRUE)
test <- subset(log_cleaned_bank_data_withoutNA_quan1[2:19],sample == FALSE)

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N = nrow(log_cleaned_bank_data_withoutNA_quan))$data


model <- glm(Attrition_Flag ~ ., data = train_mix, family = 'binomial')
summary(model)$coeff

summary(model)

pred <- (predict(model, test) >= 0.5)*1

mean(test$Attrition_Flag == pred)



log_cleaned_bank_data_withoutNA_quan2 <- subset(log_cleaned_bank_data_withoutNA_quan1, select = -c(Education_Level))

sample <- sample.split(log_cleaned_bank_data_withoutNA_quan2$Attrition_Flag,SplitRatio = 0.75)
train <- subset(log_cleaned_bank_data_withoutNA_quan2[2:18],sample == TRUE)
test <- subset(log_cleaned_bank_data_withoutNA_quan2[2:18],sample == FALSE)

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N =7595)$data


model <- glm(Attrition_Flag ~ ., data = train_mix, family = 'binomial')
summary(model)$coeff

summary(model)

pred <- (predict(model, test) >= 0.5)*1

mean(test$Attrition_Flag == pred)


log_cleaned_bank_data_withoutNA_quan3 <- subset(log_cleaned_bank_data_withoutNA_quan2, select = -c(Dependent_count))

sample <- sample.split(log_cleaned_bank_data_withoutNA_quan3$Attrition_Flag,SplitRatio = 0.75)
train <- subset(log_cleaned_bank_data_withoutNA_quan3[2:17],sample == TRUE)
test <- subset(log_cleaned_bank_data_withoutNA_quan3[2:17],sample == FALSE)

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N =7595)$data


model <- glm(Attrition_Flag ~ ., data = train_mix, family = 'binomial')
summary(model)$coeff

summary(model)

pred <- (predict(model, test) >= 0.5)*1

mean(test$Attrition_Flag == pred)



#accuracy for not remove anything case
bank_data_withoutNA_quan


bank_data_withoutNA_quan$Total_Ct_Chng_Q4_Q1 <- log1p(bank_data_withoutNA_quan$Total_Ct_Chng_Q4_Q1)
colnames(bank_data_withoutNA_quan)[20] <- "log_Total_Ct_Chng_Q4_Q1"

bank_data_withoutNA_quan$Total_Trans_Amt <- log1p(bank_data_withoutNA_quan$Total_Trans_Amt)
colnames(bank_data_withoutNA_quan)[18] <- "log_Total_Trans_Amt"

bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1 <- log1p(bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1)
colnames(bank_data_withoutNA_quan)[17] <- "log_Total_Amt_Chng_Q4_Q1"

bank_data_withoutNA_quan$Avg_Open_To_Buy <- log1p(bank_data_withoutNA_quan$Avg_Open_To_Buy)
colnames(bank_data_withoutNA_quan)[16] <- "log_Avg_Open_To_Buy"

bank_data_withoutNA_quan$Credit_Limit <- log1p(bank_data_withoutNA_quan$Credit_Limit)
colnames(bank_data_withoutNA_quan)[14] <- "log_Credit_Limit"


set.seed(0987)

sample <- sample.split(bank_data_withoutNA_quan$Attrition_Flag,SplitRatio = 0.75)
train <- subset(bank_data_withoutNA_quan[2:21],sample == TRUE)
test <- subset(bank_data_withoutNA_quan[2:21],sample == FALSE)

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N = nrow(bank_data_withoutNA_quan))$data


model <- glm(Attrition_Flag ~ ., data = train_mix, family = 'binomial')
summary(model)$coeff

summary(model)

pred <- (predict(model, train_mix) >= 0.5)*1

mean(train_mix$Attrition_Flag == pred)





#try to find accuracy with not normalized columns
set.seed(0987)

sample <- sample.split(cleaned_bank_data_withoutNA_quan$Attrition_Flag,SplitRatio = 0.75)
train <- subset(cleaned_bank_data_withoutNA_quan[2:20],sample == TRUE)
test <- subset(cleaned_bank_data_withoutNA_quan[2:20],sample == FALSE)

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N = nrow(cleaned_bank_data_withoutNA_quan))$data


model <- glm(Attrition_Flag ~ ., data = train_mix, family = 'binomial')
summary(model)$coeff

summary(model)

pred <- (predict(model, test) >= 0.5)*1
mean(test$Attrition_Flag == pred)

cdplot(factor(Attrition_Flag)~ Total_Ct_Chng_Q4_Q1, data=cleaned_bank_data_withoutNA_quan)
cdplot(factor(Attrition_Flag)~ Total_Trans_Ct, data=cleaned_bank_data_withoutNA_quan)

cdplot(factor(Attrition_Flag)~ Total_Revolving_Bal, data=cleaned_bank_data_withoutNA_quan)

log_cleaned_bank_data_withoutNA_quan

colnames(log_cleaned_bank_data_withoutNA_quan)[3:20]
x <- log_cleaned_bank_data_withoutNA_quan$Attrition_Flag
predictors <- data.matrix(log_cleaned_bank_data_withoutNA_quan[, c('Customer_Age', 'Is_Female', 'Dependent_count',
                                                                   'Education_Level', 'Marital_Status', 'Income_Category',
                                                                   'Months_on_book', 'Total_Relationship_Count', 'Months_Inactive_12_mon',
                                                                   'Contacts_Count_12_mon', 'log_Credit_Limit', 'Total_Revolving_Bal',
                                                                   'log_Avg_Open_To_Buy', 'log_Total_Amt_Chng_Q4_Q1', 'log_Total_Trans_Amt',
                                                                   'Total_Trans_Ct', 'log_Total_Ct_Chng_Q4_Q1', 'Avg_Utilization_Ratio')]) 

set.seed(222)

ind <- sample(2, nrow(log_cleaned_bank_data_withoutNA_quan), replace = TRUE, prob = c(0.7, 0.3))

train <- log_cleaned_bank_data_withoutNA_quan[ind==1,]

head(log_cleaned_bank_data_withoutNA_quan)

test <- log_cleaned_bank_data_withoutNA_quan[ind==2,]

custom <- trainControl(method = "repeatedcv",
                       
                       number = 10,
                       
                       repeats = 5,
                       
                       verboseIter = TRUE)

set.seed(1234)

log_cleaned_bank_data_withoutNA_quan = data.frame(log_cleaned_bank_data_withoutNA_quan)

model <- train(
  Attrition_Flag ~ .,
  data = log_cleaned_bank_data_withoutNA_quan,
  method = 'lasso'
)
model

log_cleaned_bank_data_withoutNA_quan$Attrition_Flag = as.factor(log_cleaned_bank_data_withoutNA_quan$Attrition_Flag)

plot(model)

plot(varImp(model))

set.seed(1)

inTraining <- createDataPartition(log_cleaned_bank_data_withoutNA_quan$Attrition_Flag, p = .80, list = FALSE)
training <- log_cleaned_bank_data_withoutNA_quan[inTraining,]
testing  <- log_cleaned_bank_data_withoutNA_quan[-inTraining,]

training = data.frame(training)

set.seed(1)
model <- train(
  Attrition_Flag ~ .,
  data = training,
  method = 'lasso',
  preProcess = c("center", "scale")
)
model1

test.features = subset(testing, select=-c(Attrition_Flag))
test.target = subset(testing, select=Attrition_Flag)[,1]

predictions = predict(model3, newdata = test.features)

# RMSE
sqrt(mean((test.target - predictions)^2))

#R2
cor(test.target, predictions) ^ 2

knitr::spin("stat_r.R", precious=TRUE)

