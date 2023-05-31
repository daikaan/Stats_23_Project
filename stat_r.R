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


bank_data_origin <- read.csv('~/GitHub/Stats_23_Project/BankChurners.csv')
bank_data <- data.frame(bank_data_origin)

colSums(is.na(bank_data)) #there is no null value

#Dimension of dataset
dim(bank_data)

#Shows structure of dataset
str(bank_data)

#View the categorical variables
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


order_Card_Category <- list("Blue" = 1,
                            "Silver" = 2,
                            "Gold" = 3,
                            "Platinum" = 4)
bank_data_withoutNA_quan$Card_Category <- unlist(order_Card_Category[as.character(bank_data_withoutNA_quan$Card_Category)])

#delete naive...1 and 2
bank_data_withoutNA_quan <- subset(bank_data_withoutNA_quan, select = -c(Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))

cleaned_bank_data_withoutNA_quan <- bank_data_withoutNA_quan

age.exc.list <- boxplot.stats(cleaned_bank_data_withoutNA_quan$Customer_Age)$out
card.exc.list <- c(2, 3, 4)

cleaned_bank_data_withoutNA_quan <- subset(cleaned_bank_data_withoutNA_quan,!((Customer_Age %in% age.exc.list)| (Card_Category %in% card.exc.list)))
cleaned_bank_data_withoutNA_quan


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


