bank_data <- read.csv('~/GitHub/Stats_23_Project/BankChurners.csv')

#########
library("dplyr")
library("corrplot")
library("caTools")
library("ggpubr")
library("ROSE")
library("correlation")
library("moments") #to calculate skewness
library("olsrr") #to use ols_step_backward_p
library("MASS")
library("knitr")
library("forecast")
library("ggplot2")
library("PCAmixdata")
library("purrr")
library("corpcor")
library("car")
library("e1071")
library("ppcor")
library("pROC")

# DATA PREPERATION

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

# Extracting Outliers from age and Rescoping the study to only focus on Blue Cards

#Customer age

#boxplot
cust.age.boxplot <- boxplot(cleaned_bank_data_withoutNA_quan$Customer_Age, ylab = "age")
cust.age.boxplot

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, 
#it is seen that customers over the age of 70 are outliers
age.exc.list <- boxplot.stats(cleaned_bank_data_withoutNA_quan$Customer_Age)$out

# Card Category

ggplot(cleaned_bank_data_withoutNA_quan, aes(x=Months_on_book, y= Credit_Limit, shape = as.factor(Card_Category), color= as.factor(Card_Category)))+
  geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Since most of the data is coming from the Blue cards and there is a visible difference on many parameters among categories, we decided to only focus on blue card category
card.exc.list <- c(2, 3, 4)

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
cust.age.piechart <- pie(count(cleaned_bank_data_withoutNA_quan, age_group)$n, border="white", col=myPalette)

#Dependent Count

ggplot(cleaned_bank_data_withoutNA_quan, aes(x=Dependent_count)) +
  geom_bar(width=1)

depcount.labels <- c(0, 1, 2, 3, 4, 5)
dependent.count.piechart <- pie(count(cleaned_bank_data_withoutNA_quan, Dependent_count)$n, border="white", col=myPalette, labels = depcount.labels)

# months on book (how long a customer is using the bank)
#histogram
hist(cleaned_bank_data_withoutNA_quan$Months_on_book)

#boxplot
months.onbook.boxplot <- boxplot(quantitative$Months_on_book, ylab = "months")

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, outliers
boxplot.stats(quantitative$Months_on_book)$out

#Since the outliers in months on books can be identifying on whether the customer is going to churn we decided to keep them in the data set

#Total Relationships Count
ggplot(cleaned_bank_data_withoutNA_quan, aes(x=Total_Relationship_Count)) +
  geom_bar(width=1)

#Months_Inactive_12months
ggplot(cleaned_bank_data_withoutNA_quan, aes(x=Months_Inactive_12_mon)) +
  geom_bar(width=1)

#Contacts Count 12 months
ggplot(cleaned_bank_data_withoutNA_quan, aes(x=Contacts_Count_12_mon)) +
  geom_bar(width=1)

# Credit Limit

#boxplot
credit.limit.boxplot <- boxplot(cleaned_bank_data_withoutNA_quan$Credit_Limit, ylab = "Dollars")

#histogram
hist(cleaned_bank_data_withoutNA_quan$Credit_Limit)


# Total Revolving Balance
#histogram
hist(cleaned_bank_data_withoutNA_quan$Total_Revolving_Bal)

#Average Open to Buy
#histogram
hist(cleaned_bank_data_withoutNA_quan$Avg_Open_To_Buy)

#Total Amount Change Between Q1 and Q4

#histogram
hist(cleaned_bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1)

#boxplot
credit.limit.boxplot <- boxplot(cleaned_bank_data_withoutNA_quan$Total_Amt_Chng_Q4_Q1, ylab = "Dollars")

#Total Transaction Amount

#histogram
hist(cleaned_bank_data_withoutNA_quan$Total_Trans_Amt)

#Total Count Change Between Q1 and Q4

#histogram
hist(cleaned_bank_data_withoutNA_quan$Total_Ct_Chng_Q4_Q1)

#Average Utilization Rate

#histogram
hist(cleaned_bank_data_withoutNA_quan$Avg_Utilization_Ratio)

unique(cleaned_bank_data_withoutNA_quan$Attrition_Flag) #to make sure there are only 2 strings
#change 'Existing Customer' to 1 and 'Attrited Customer' to 0 and add new column to quantitative
data.split <- splitmix(cleaned_bank_data_withoutNA_quan)
quantitative <- data.split$X.quanti
qualitative <- data.split$X.quali

length(quantitative)
length(qualitative)
quantitative$attrition_flag_binary <- ifelse(cleaned_bank_data_withoutNA_quan$Attrition_Flag=='Existing Customer', 1, 0)

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



set.seed(0987)

sample <- sample.split(log_cleaned_bank_data_withoutNA_quan$Attrition_Flag,SplitRatio = 0.75)
train <- subset(log_cleaned_bank_data_withoutNA_quan,sample == TRUE)
test <- subset(log_cleaned_bank_data_withoutNA_quan,sample == FALSE)


#Proportion of Attrited and Existing Customer
prop.table(table(train$Attrition_Flag))
prop.table(table(test$Attrition_Flag))


#Original proportion of Attrited and Existing Customer
prop.table(table(log_cleaned_bank_data_withoutNA_quan$Attrition_Flag))

#It's an unbalanced dataset.
#It might be better to consider a resampling of the dataset

#Mixed Sampling with 50% of Attrited Customer
train_bal <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.5, N =5309)$data


train$Attrition_Flag <- as.numeric(train$Attrition_Flag)

#Correlation matrix
cor_mat <- cor(train)
corrplot(cor_mat,method = "number",type = "upper",number.cex = 0.6, tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)

#Correlation with Attrition_Flag
corrplot(cor_mat[1,,drop=FALSE],method = "number",number.cex = 0.6, cl.pos = "n",tl.col = "black" ,tl.cex=0.5,diag = FALSE)

#Since Credit_Limit and Avg_Open_To_Buy have correlation 1 we can remove Avg_Open_To_Buy
train <- subset(train, select = -c(Avg_Open_To_Buy))

#Partial correlations (Takes time)
correlation(train,partial = TRUE)

#Partial Correlation matrix 
part_cor_mat <- pcor(train)$estimate
corrplot(part_cor_mat, method = "number",type = "upper",number.cex = 0.6, tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)



#Model without Unknown

glm_1 <- glm(data = train,Attrition_Flag~ .,family = "binomial")
summary(glm_1)

pred_glm_i <- predict(glm_1,test,type="response")
pred_1_i <- ifelse(pred_glm_i >= Threshold1 , 1,0)
pred_2_i <- ifelse(pred_glm_i >= Threshold2 , 1,0)
pred_3_i <- ifelse(pred_glm_i >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_i <- table(test$Attrition_Flag,pred_1_i)
c_mat_2_i <- table(test$Attrition_Flag,pred_2_i)
c_mat_3_i <- table(test$Attrition_Flag,pred_3_i)
c_mat_1_i
c_mat_2_i
c_mat_3_i

#Accuracy

mean(pred_1_i==test$Attrition_Flag)*100
mean(pred_2_i==test$Attrition_Flag)*100
mean(pred_3_i==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1_i <- c_mat_1_i[1,1]/sum(c_mat_1_i[1,])
Spec_2_i <- c_mat_2_i[1,1]/sum(c_mat_2_i[1,])
Spec_3_i <- c_mat_3_i[1,1]/sum(c_mat_3_i[1,])
Spec_1_i
Spec_2_i
Spec_3_i


#Precision / Positive Predicted Value

Prec_1_i <- c_mat_1_i[2,2]/sum(c_mat_1_i[,2])
Prec_2_i <- c_mat_2_i[2,2]/sum(c_mat_2_i[,2])
Prec_3_i <- c_mat_3_i[2,2]/sum(c_mat_3_i[,2])
Prec_1_i
Prec_2_i
Prec_3_i

#Recall / True Positive Rate / Sensitivity

Rec_1_i <- c_mat_1_i[2,2]/sum(c_mat_1_i[2,])
Rec_2_i <- c_mat_2_i[2,2]/sum(c_mat_2_i[2,])
Rec_3_i <- c_mat_3_i[2,2]/sum(c_mat_3_i[2,])
Rec_1_i
Rec_2_i
Rec_3_i

#F1 Score

F1_1_i <- 2 * (Prec_1_i * Rec_1_i)/(Prec_1_i + Rec_1_i)
F1_2_i <- 2 * (Prec_2_i * Rec_2_i)/(Prec_2_i + Rec_2_i)
F1_3_i <- 2 * (Prec_3_i * Rec_3_i)/(Prec_3_i + Rec_3_i)
F1_1_i
F1_2_i
F1_3_i

#VIF

vif(glm_1)

#Update Checking p-values and AIC

glm_2 <- update(glm_1, . ~ . - Education_Level - Customer_Age - Months_on_book - Credit_Limit - Avg_Utilization_Ratio)
summary(glm_2)


glm_3 <- update(glm_2, . ~ . + Total_Trans_Ct*Total_Revolving_Bal + Total_Trans_Ct*Marital_Status)
summary(glm_3)


glm_4 <- update(glm_3, . ~ . + Total_Relationship_Count*Total_Trans_Amt )
summary(glm_4)


glm_5 <- update(glm_4, . ~ . + Total_Revolving_Bal*Avg_Utilization_Ratio + Total_Revolving_Bal*Credit_Limit )
summary(glm_5)


glm_6 <- update(glm_5, . ~ . + Is_Female*Avg_Utilization_Ratio)
summary(glm_6)


glm_7 <- update(glm_6, . ~ . + Dependent_count*Total_Trans_Ct)
summary(glm_7)


glm_8 <- update(glm_7, . ~ . + Credit_Limit*Total_Trans_Amt + Total_Trans_Ct*Total_Trans_Amt )
summary(glm_8)


pred_glm_f <- predict(glm_8,test,type="response")
pred_1_f <- ifelse(pred_glm_f >= Threshold1 , 1,0)
pred_2_f <- ifelse(pred_glm_f >= Threshold2 , 1,0)
pred_3_f <- ifelse(pred_glm_f >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_f <- table(test$Attrition_Flag,pred_1_f)
c_mat_2_f <- table(test$Attrition_Flag,pred_2_f)
c_mat_3_f <- table(test$Attrition_Flag,pred_3_f)
c_mat_1_f
c_mat_2_f
c_mat_3_f

#Accuracy

mean(pred_1_f==test$Attrition_Flag)*100
mean(pred_2_f==test$Attrition_Flag)*100
mean(pred_3_f==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1_f <- c_mat_1_f[1,1]/sum(c_mat_1_f[1,])
Spec_2_f <- c_mat_2_f[1,1]/sum(c_mat_2_f[1,])
Spec_3_f <- c_mat_3_f[1,1]/sum(c_mat_3_f[1,])
Spec_1_f
Spec_2_f
Spec_3_f


#Precision / Positive Predicted Value

Prec_1_f <- c_mat_1_f[2,2]/sum(c_mat_1_f[,2])
Prec_2_f <- c_mat_2_f[2,2]/sum(c_mat_2_f[,2])
Prec_3_f <- c_mat_3_f[2,2]/sum(c_mat_3_f[,2])
Prec_1_f
Prec_2_f
Prec_3_f

#Recall / True Positive Rate / Sensitivity

Rec_1_f <- c_mat_1_f[2,2]/sum(c_mat_1_f[2,])
Rec_2_f <- c_mat_2_f[2,2]/sum(c_mat_2_f[2,])
Rec_3_f <- c_mat_3_f[2,2]/sum(c_mat_3_f[2,])
Rec_1_f
Rec_2_f
Rec_3_f

#F1 Score

F1_1_f <- 2 * (Prec_1_f * Rec_1_f)/(Prec_1_f + Rec_1_f)
F1_2_f <- 2 * (Prec_2_f * Rec_2_f)/(Prec_2_f + Rec_2_f)
F1_3_f <- 2 * (Prec_3_f * Rec_3_f)/(Prec_3_f + Rec_3_f)
F1_1_f
F1_2_f
F1_3_f


#ROC curves
roc_i <- roc(test$Attrition_Flag ~ pred_glm_i)
roc_f <- roc(test$Attrition_Flag ~ pred_glm_f)

AUC_i <- auc(roc_i)
AUC_f <- auc(roc_f)


plot(roc_i, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_f,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_i <- coords(roc_i,"best",best.method = "closest.topleft")$threshold
Best_pred_i <- ifelse(pred_glm_i >= Best_Treshold_i , 1,0)
Best_c_mat_i <- table(test$Attrition_Flag,Best_pred_i)
Best_Spec_i <- Best_c_mat_i[1,1]/sum(Best_c_mat_i[1,])
Best_Sens_i <- Best_c_mat_i[2,2]/sum(Best_c_mat_i[2,])

Best_Treshold_f <- coords(roc_f,"best",best.method = "closest.topleft")$threshold
Best_pred_f <- ifelse(pred_glm_f >= Best_Treshold_f , 1,0)
Best_c_mat_f <- table(test$Attrition_Flag,Best_pred_f)
Best_Spec_f <- Best_c_mat_f[1,1]/sum(Best_c_mat_f[1,])
Best_Sens_f <- Best_c_mat_f[2,2]/sum(Best_c_mat_f[2,])

#Table to showing them
Table_mat <-  matrix(c(Best_Treshold_i,Best_Spec_i,Best_Sens_i,Best_Treshold_f,Best_Spec_f,Best_Sens_f), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Initial model","Final model")
Tab <- as.table(Table_mat)
show(Tab)
