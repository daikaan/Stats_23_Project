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

#boxplot
cust.age.boxplot <- boxplot(new_bank_data$Customer_Age, ylab = "age")
cust.age.boxplot

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, 
#it is seen that customers over the age of 70 are outliers
boxplot.stats(new_bank_data$Customer_Age)$out

# grouped age piechart
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 
cust.age.piechart <- pie(count(new_bank_data, Customer_Age)$n, border="white", col=myPalette)
cust.age.piechart

#boxplot
card.category.boxplot <- boxplot(new_bank_data$Card_Category, ylab = "category")
card.category.boxplot

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, outliers
boxplot.stats(new_bank_data$Card_Category)$out

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

bank_data_origin <- read.csv('~/GitHub/Stats_23_Project/BankChurners.csv')
bank_data <- data.frame(bank_data_origin)

colSums(is.na(bank_data)) #there is no null value

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

#Correlation matrix
cor_mat_new <- cor(new_bank_data[2:15])
corrplot(cor_mat_new,method = "number",type = "upper", tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)

as.matrix(cor_mat_new)

#calculate skewness in quant to find which are normally dist
skewness(new_bank_data$Customer_Age)
skewness(new_bank_data$Dependent_count)
skewness(new_bank_data$Months_on_book)
skewness(new_bank_data$Total_Relationship_Count)
skewness(new_bank_data$Months_Inactive_12_mon)
skewness(new_bank_data$Contacts_Count_12_mon)
skewness(new_bank_data$Total_Revolving_Bal)
skewness(new_bank_data$Total_Trans_Ct)
skewness(new_bank_data$Avg_Utilization_Ratio)
skewness(new_bank_data$Is_Female)
skewness(new_bank_data$Education_Level)
skewness(new_bank_data$Marital_Status)
skewness(new_bank_data$Income_Category)


#we should take log to normalize and calculate skewness again for these
skewness(new_bank_data$Total_Ct_Chng_Q4_Q1)
skewness(new_bank_data$Total_Trans_Amt)
skewness(new_bank_data$Total_Amt_Chng_Q4_Q1)
skewness(new_bank_data$Avg_Open_To_Buy)
skewness(new_bank_data$Credit_Limit)
skewness(new_bank_data$Card_Category)


#they are normally dist now
skewness(log1p(new_bank_data$Total_Ct_Chng_Q4_Q1))
skewness(log1p(new_bank_data$Total_Trans_Amt))
skewness(log1p(new_bank_data$Total_Amt_Chng_Q4_Q1))
skewness(log1p(new_bank_data$Avg_Open_To_Buy))
skewness(log1p(new_bank_data$Credit_Limit))


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



