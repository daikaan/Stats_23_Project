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

pred <- (predict(model, train_mix) >= 0.5)*1

mean(train_mix$Attrition_Flag == pred)


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

pred <- (predict(model, train_mix) >= 0.5)*1

mean(train_mix$Attrition_Flag == pred)



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

pred <- (predict(model, train_mix) >= 0.5)*1

mean(train_mix$Attrition_Flag == pred)


log_cleaned_bank_data_withoutNA_quan3 <- subset(log_cleaned_bank_data_withoutNA_quan2, select = -c(Customer_Age))

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

pred <- (predict(model, train_mix) >= 0.5)*1

mean(train_mix$Attrition_Flag == pred)












#delete monts_on_book and try again, if we get all columns with triple stars we are ok
#if we use linear regression
model_back1 <- ols_step_backward_p(model, prem = 0.05, progress = TRUE, details = TRUE)

model_back1$adjr

model_back1$rmse

model_back1$model$coefficients

plot(model_back1)


model_back <- ols_step_backward_p(model, prem = 0.1, progress = TRUE, details = FALSE)

model_back$adjr

model_back1$rmse

library(forecast)
pred <- predict(model_back$model, test, se.fit = TRUE)
rmse <- accuracy(pred$fit, test$Attrition_Flag)[2]
rmse

resid <- test$Attrition_Flag - pred$fit
plot(resid)
abline(h = 0, col = "red")





$Attrition_Flag,SplitRatio = 0.75)
train <- subset(bank_data_rev[2:21],sample == TRUE)
test <- subset(bank_data_rev[2:21],sample == FALSE)

#Proportion of Attrited and Existing Customer
prop.table(table(train$Attrition_Flag))
prop.table(table(test$Attrition_Flag))


#Original proportion of Attrited and Existing Customer
prop.table(table(copy_new_bank_data$Attrition_Flag))

#It's an unbalanced dataset.
#It might be better to consider a resampling of the dataset

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N =7595)$data


model <- glm(Attrition_Flag ~ ., data = train_mix, family = 'binomial')
summary(model)$coeff

summary(model)

pred <- (predict(model, train_mix) >= 0.5)*1

mean(train_mix$Attrition_Flag == pred)

#delete monts_on_book and try again, if we get all columns with triple stars we are ok
#if we use linear regression
model_back1 <- ols_step_backward_p(model, prem = 0.05, progress = TRUE, details = TRUE)

model_back1$adjr

model_back1$rmse

model_back1$model$coefficients

plot(model_back1)


model_back <- ols_step_backward_p(model, prem = 0.1, progress = TRUE, details = FALSE)

model_back$adjr

model_back1$rmse

library(forecast)
pred <- predict(model_back$model, test, se.fit = TRUE)
rmse <- accuracy(pred$fit, test$Attrition_Flag)[2]
rmse

resid <- test$Attrition_Flag - pred$fit
plot(resid)
abline(h = 0, col = "red")












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


set.seed(0987)

sample <- sample.split(bank_data_rev$Attrition_Flag,SplitRatio = 0.75)
train <- subset(bank_data_rev[2:21],sample == TRUE)
test <- subset(bank_data_rev[2:21],sample == FALSE)

#Proportion of Attrited and Existing Customer
prop.table(table(train$Attrition_Flag))
prop.table(table(test$Attrition_Flag))


#Original proportion of Attrited and Existing Customer
prop.table(table(copy_new_bank_data$Attrition_Flag))

#It's an unbalanced dataset.
#It might be better to consider a resampling of the dataset

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N =7595)$data


model <- glm(Attrition_Flag ~ ., data = train_mix, family = 'binomial')
summary(model)$coeff

summary(model)

pred <- (predict(model, train_mix) >= 0.5)*1

mean(train_mix$Attrition_Flag == pred)

#delete monts_on_book and try again, if we get all columns with triple stars we are ok
#if we use linear regression
model_back1 <- ols_step_backward_p(model, prem = 0.05, progress = TRUE, details = TRUE)

model_back1$adjr

model_back1$rmse

model_back1$model$coefficients

plot(model_back1)


model_back <- ols_step_backward_p(model, prem = 0.1, progress = TRUE, details = FALSE)

model_back$adjr

model_back1$rmse

library(forecast)
pred <- predict(model_back$model, test, se.fit = TRUE)
rmse <- accuracy(pred$fit, test$Attrition_Flag)[2]
rmse

resid <- test$Attrition_Flag - pred$fit
plot(resid)
abline(h = 0, col = "red")






helmert <- function(n) {
  m <- t((diag(seq(n-1, 0)) - upper.tri(matrix(1, n, n)))[-n,])
  t(apply(m, 1, rev))
}
encode_helmert <- function(df, var) {
  x <- df[[var]]
  x <- unique(x)
  n <- length(x)
  d <- as.data.frame(helmert(n))
  d[[var]] <- rev(x)
  names(d) <- c(paste0(var, 1:(n-1)), var)
  d
}

copy_new_bank_data$Card_Category1 <- ifelse(copy_new_bank_data$Card_Category == '1', 1, 0)
copy_new_bank_data$Card_Category2 <- ifelse(copy_new_bank_data$Card_Category == '2', 1, 0)
copy_new_bank_data$Card_Category3 <- ifelse(copy_new_bank_data$Card_Category == '3', 1, 0)

skewness(copy_new_bank_data$Card_Category)

hist(log1p(copy_new_bank_data$Card_Category))

encode_helmert(new_bank_data, 'Card_Category')

a <- boxcox(lm(copy_new_bank_data$Card_Category ~ 1), lambda = seq(-200,3))
hist(a)
skewness((copy_new_bank_data$Card_Category^-11))
hist(copy_new_bank_data$Card_Category^-19)

copy_new_bank_data <- new_bank_data

copy_new_bank_data$Total_Ct_Chng_Q4_Q1 <- log1p(copy_new_bank_data$Total_Ct_Chng_Q4_Q1)
colnames(copy_new_bank_data)[20] <- "log_Total_Ct_Chng_Q4_Q1"

copy_new_bank_data$Total_Trans_Amt <- log1p(copy_new_bank_data$Total_Trans_Amt)
colnames(copy_new_bank_data)[18] <- "log_Total_Trans_Amt"

copy_new_bank_data$Total_Amt_Chng_Q4_Q1 <- log1p(copy_new_bank_data$Total_Amt_Chng_Q4_Q1)
colnames(copy_new_bank_data)[17] <- "log_Total_Amt_Chng_Q4_Q1"

copy_new_bank_data$Avg_Open_To_Buy <- log1p(copy_new_bank_data$Avg_Open_To_Buy)
colnames(copy_new_bank_data)[16] <- "log_Avg_Open_To_Buy"

copy_new_bank_data$Credit_Limit <- log1p(copy_new_bank_data$Credit_Limit)
colnames(copy_new_bank_data)[14] <- "log_Credit_Limit"

#delete naive...1 and 2
copy_new_bank_data <- subset(copy_new_bank_data, select = -c(Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))


#Thresholds for classification:
threshold1 <- 0.4
threshold2 <- 0.5
threshold3 <- 0.6



#This part is to check how the rows containg at least one "unknown" are distributed (Probably useless)

#Change Unknown value to NA
bank_data_copy <- data.frame(copy_new_bank_data)
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


set.seed(0987)

sample <- sample.split(bank_data_rev$Attrition_Flag,SplitRatio = 0.75)
train <- subset(bank_data_rev[2:21],sample == TRUE)
test <- subset(bank_data_rev[2:21],sample == FALSE)

#Proportion of Attrited and Existing Customer
prop.table(table(train$Attrition_Flag))
prop.table(table(test$Attrition_Flag))


#Original proportion of Attrited and Existing Customer
prop.table(table(copy_new_bank_data$Attrition_Flag))

#It's an unbalanced dataset.
#It might be better to consider a resampling of the dataset

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.4, N =7595)$data


model <- glm(Attrition_Flag ~ ., data = train_mix)
summary(model)$adj.r.squared
summary(model)$coeff

summary(model)

pred <- (predict(model, train_mix) >= 0.5)*1

mean(train_mix$Attrition_Flag == pred)

#delete monts_on_book and try again, if we get all columns with triple stars we are ok
#if we use linear regression
model_back1 <- ols_step_backward_p(model, prem = 0.05, progress = TRUE, details = TRUE)

model_back1$adjr

model_back1$rmse

model_back1$model$coefficients

plot(model_back1)


model_back <- ols_step_backward_p(model, prem = 0.1, progress = TRUE, details = FALSE)

model_back$adjr

model_back1$rmse

library(forecast)
pred <- predict(model_back$model, test, se.fit = TRUE)
rmse <- accuracy(pred$fit, test$Attrition_Flag)[2]
rmse

resid <- test$Attrition_Flag - pred$fit
plot(resid)
abline(h = 0, col = "red")



