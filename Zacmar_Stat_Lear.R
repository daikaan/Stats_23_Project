library("dplyr")
library("corrplot")
library("caTools")
library("ggpubr")
library("ROSE")
library("corpcor")
library("car")
library("e1071")
library("correlation")
library("ppcor")
library("pROC")


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

table(bank_data$Attrition_Flag,bank_data$Gender)

table(bank_data$Attrition_Flag,bank_data$Education_Level)

table(bank_data$Attrition_Flag,bank_data$Marital_Status)

table(bank_data$Attrition_Flag,bank_data$Income_Category)

table(bank_data$Attrition_Flag,bank_data$Card_Category)



bank_data <- subset(bank_data, select = -c(CLIENTNUM,Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))

#We convert categorical variables into numerical
bank_data$Attrition_Flag <- as.numeric(bank_data$Attrition_Flag == "Attrited Customer")

bank_data$Gender <- as.numeric(bank_data$Gender == "F")
bank_data <- bank_data %>% rename("Is_Female" = "Gender")

order_education_level <- list("Unknown" = 0,
                              "Uneducated" = 1,
                              "High School" = 2,
                              "College" = 3,
                              "Graduate" = 4,
                              "Post-Graduate" = 5,
                              "Doctorate" = 6)
bank_data$Education_Level <- unlist(order_education_level[as.character(bank_data$Education_Level)])

order_Marital_Status <- list("Unknown" = 0,
                            "Single" = 1,
                            "Married" = 2,
                            "Divorced" = 3)
bank_data$Marital_Status <- unlist(order_Marital_Status[as.character(bank_data$Marital_Status)])

order_Income_Category <- list("Unknown" = 0,
                              "Less than $40K" = 1,
                              "$40K - $60K" = 2,
                              "$60K - $80K" = 3,
                              "$80K - $120K" = 4,
                              "$120K +" = 5)
bank_data$Income_Category <- unlist(order_Income_Category[as.character(bank_data$Income_Category)])


order_Card_Category <- list("Blue" = 0,
                            "Silver" = 1,
                            "Gold" = 2,
                            "Platinum" = 3)
bank_data$Card_Category <- unlist(order_Card_Category[as.character(bank_data$Card_Category)])


#Remove outliers and Card_Category
age.exc.list <- boxplot.stats(bank_data$Customer_Age)$out

bank_data <- subset(bank_data,!((Customer_Age %in% age.exc.list)))

bank_data <- subset(bank_data,select = -c(Card_Category))

#Correlation matrix
cor_mat_tot <- cor(bank_data)
corrplot(cor_mat_tot,method = "number",type = "upper",number.cex = 0.6, tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)

#Correlation with Attrition_Flag
corrplot(cor_mat_tot[1,,drop=FALSE],method = "number",number.cex = 0.6, cl.pos = "n",tl.col = "black" ,tl.cex=0.5,diag = FALSE)

#Since Credit_Limit and Avg_Open_To_Buy have correlation 1 we can remove Avg_Open_To_Buy
bank_data <- subset(bank_data, select = -c(Avg_Open_To_Buy))

#Partial correlations (Takes time)
correlation(bank_data,partial = TRUE)

#Partial Correlation matrix 
part_cor_mat <- pcor(bank_data)$estimate
corrplot(part_cor_mat, method = "number",type = "upper",number.cex = 0.6, tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)


#We check how the variables with the bigger correlation with Attrition_flag change between Attrited and Existing Customer
Customer <- as.factor(bank_data$Attrition_Flag)
a <- ggplot(bank_data, aes(x = Total_Trans_Ct, fill = Customer)) +
      geom_density(alpha=0.3) + ggtitle("Total_Trans_Ct") +
      scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

b <- ggplot(bank_data, aes(x = Total_Trans_Amt, fill = Customer)) +
  geom_density(alpha=0.3) + ggtitle("Total_Trans_Amt") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

c <- ggplot(bank_data, aes(x = Total_Ct_Chng_Q4_Q1, fill = Customer)) +
      geom_density(alpha=0.3) + ggtitle("Total_Ct_Chng_Q4_Q1")+
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

d <- ggplot(bank_data, aes(x = Total_Revolving_Bal, fill = Customer)) +
      geom_density(alpha=0.3) + ggtitle("Total_Revolving_Bal") + 
    scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

e <- ggplot(bank_data, aes(x = Contacts_Count_12_mon, fill = Customer)) +
      geom_bar(aes(y = after_stat(prop) ),alpha=0.3, position = "dodge") + ggtitle("Contacts_Count_12_mon") +
      scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

f <- ggplot(bank_data, aes(x = Total_Relationship_Count, fill = Customer)) +
    geom_bar(aes(y = after_stat(prop) ),alpha=0.3, position = "dodge") + ggtitle("Total_Relationship_Count") +
    scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))


ggarrange(a,b,c,d,e,f,nrow=2,ncol=3)


#calculate skewness in quant to find which are normally dist
skewness(bank_data$Customer_Age)
skewness(bank_data$Dependent_count)
skewness(bank_data$Months_on_book)
skewness(bank_data$Total_Relationship_Count)
skewness(bank_data$Months_Inactive_12_mon)
skewness(bank_data$Contacts_Count_12_mon)
skewness(bank_data$Total_Revolving_Bal)
skewness(bank_data$Total_Trans_Ct)
skewness(bank_data$Avg_Utilization_Ratio)
skewness(bank_data$Is_Female)
skewness(bank_data$Education_Level)
skewness(bank_data$Marital_Status)
skewness(bank_data$Income_Category)


#we should take log to normalize and calculate skewness again for these
skewness(bank_data$Total_Ct_Chng_Q4_Q1)
skewness(bank_data$Total_Trans_Amt)
skewness(bank_data$Total_Amt_Chng_Q4_Q1)
skewness(bank_data$Credit_Limit)

#they are normally dist now
skewness(log1p(bank_data$Total_Ct_Chng_Q4_Q1))
skewness(log1p(bank_data$Total_Trans_Amt))
skewness(log1p(bank_data$Total_Amt_Chng_Q4_Q1))
skewness(log1p(bank_data$Credit_Limit))


bank_data$Total_Ct_Chng_Q4_Q1 <- log1p(bank_data$Total_Ct_Chng_Q4_Q1)

bank_data$Total_Trans_Amt <- log1p(bank_data$Total_Trans_Amt)

bank_data$Total_Amt_Chng_Q4_Q1 <- log1p(bank_data$Total_Amt_Chng_Q4_Q1)

bank_data$Credit_Limit <- log1p(bank_data$Credit_Limit)



#Build a dataset without missing values
bank_data_withoutNA <- subset(bank_data, Education_Level != 0 & Marital_Status!= 0 & Income_Category != 0  )




# Train-test Split

set.seed(0987)

sample_1 <- sample.split(bank_data$Attrition_Flag,SplitRatio = 0.75)
train_1 <- subset(bank_data,sample_1 == TRUE)
test_1 <- subset(bank_data,sample_1 == FALSE)

sample_2 <- sample.split(bank_data_withoutNA$Attrition_Flag,SplitRatio = 0.75)
train_2 <- subset(bank_data_withoutNA,sample_2 == TRUE)
test_2 <- subset(bank_data_withoutNA,sample_2 == FALSE)


#Proportion of Attrited and Existing Customer
prop.table(table(train_1$Attrition_Flag))
prop.table(table(test_1$Attrition_Flag))

prop.table(table(train_2$Attrition_Flag))
prop.table(table(test_2$Attrition_Flag))


#Original proportion of Attrited and Existing Customer
prop.table(table(bank_data$Attrition_Flag))

#It's an unbalanced dataset.
#It might be better to consider a resampling of the dataset

#Mixed Sampling with 40% of Attrited Customer
train_mix_1 <- ovun.sample(Attrition_Flag~.,data = train_1, method = "both", p = 0.4, N =7594)$data
train_mix_2 <- ovun.sample(Attrition_Flag~.,data = train_2, method = "both", p = 0.4, N =5309)$data


#Thresholds
Threshold1 <- 0.3
Threshold2 <- 0.4
Threshold3 <- 0.5

glm_1 <- glm(data = train_1,Attrition_Flag~ .,family = "binomial")
summary(glm_1)

pred_glm_i <- predict(glm_1,test_1,type="response")
pred_1_i <- ifelse(pred_glm_i >= Threshold1 , 1,0)
pred_2_i <- ifelse(pred_glm_i >= Threshold2 , 1,0)
pred_3_i <- ifelse(pred_glm_i >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_i <- table(test_1$Attrition_Flag,pred_1_i)
c_mat_2_i <- table(test_1$Attrition_Flag,pred_2_i)
c_mat_3_i <- table(test_1$Attrition_Flag,pred_3_i)
c_mat_1_i
c_mat_2_i
c_mat_3_i

#Accuracy

mean(pred_1_i==test_1$Attrition_Flag)*100
mean(pred_2_i==test_1$Attrition_Flag)*100
mean(pred_3_i==test_1$Attrition_Flag)*100

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

glm_2 <- update(glm_1, . ~ . - Education_Level - Customer_Age - Months_on_book)
summary(glm_2)


glm_3 <- update(glm_2, . ~ . + Total_Trans_Ct*Total_Trans_Amt + Total_Trans_Ct*Total_Revolving_Bal + Total_Trans_Ct*Is_Female + Total_Trans_Ct*Avg_Utilization_Ratio + Total_Trans_Ct*Marital_Status)
summary(glm_3)


glm_4 <- update(glm_3, . ~ . + Total_Relationship_Count*Total_Trans_Amt + Total_Relationship_Count*Contacts_Count_12_mon)
summary(glm_4)


glm_5 <- update(glm_4, . ~ . + Total_Revolving_Bal*Avg_Utilization_Ratio + Total_Revolving_Bal*Credit_Limit)
summary(glm_5)


glm_6 <- update(glm_5, . ~ . + Is_Female*Income_Category + Is_Female*Avg_Utilization_Ratio)
summary(glm_6)


glm_7 <- update(glm_6, . ~ . + Dependent_count*Total_Trans_Ct)
summary(glm_7)


glm_8 <- update(glm_7, . ~ . + Credit_Limit*Total_Trans_Amt -Total_Trans_Ct:Avg_Utilization_Ratio)
summary(glm_8)


pred_glm_f <- predict(glm_8,test_1,type="response")
pred_1_f <- ifelse(pred_glm_f >= Threshold1 , 1,0)
pred_2_f <- ifelse(pred_glm_f >= Threshold2 , 1,0)
pred_3_f <- ifelse(pred_glm_f >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_f <- table(test_1$Attrition_Flag,pred_1_f)
c_mat_2_f <- table(test_1$Attrition_Flag,pred_2_f)
c_mat_3_f <- table(test_1$Attrition_Flag,pred_3_f)
c_mat_1_f
c_mat_2_f
c_mat_3_f

#Accuracy

mean(pred_1_f==test_1$Attrition_Flag)*100
mean(pred_2_f==test_1$Attrition_Flag)*100
mean(pred_3_f==test_1$Attrition_Flag)*100

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
roc_i <- roc(test_1$Attrition_Flag ~ pred_glm_i)
roc_f <- roc(test_1$Attrition_Flag ~ pred_glm_f)

AUC_i <- auc(roc_i)
AUC_f <- auc(roc_f)


plot(roc_i, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_f,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_i <- coords(roc_i,"best",best.method = "youden")$threshold
Best_pred_i <- ifelse(pred_glm_i >= Best_Treshold_i , 1,0)
Best_c_mat_i <- table(test_1$Attrition_Flag,Best_pred_i)
Best_Spec_i <- Best_c_mat_i[1,1]/sum(Best_c_mat_i[1,])
Best_Sens_i <- Best_c_mat_i[2,2]/sum(Best_c_mat_i[2,])

Best_Treshold_f <- coords(roc_f,"best",best.method = "youden")$threshold
Best_pred_f <- ifelse(pred_glm_f >= Best_Treshold_f , 1,0)
Best_c_mat_f <- table(test_1$Attrition_Flag,Best_pred_f)
Best_Spec_f <- Best_c_mat_f[1,1]/sum(Best_c_mat_f[1,])
Best_Sens_f <- Best_c_mat_f[2,2]/sum(Best_c_mat_f[2,])

#Table to showing them
Table_mat <-  matrix(c(Best_Treshold_i,Best_Spec_i,Best_Sens_i,Best_Treshold_f,Best_Spec_f,Best_Sens_f), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Initial model","Final model")
Tab <- as.table(Table_mat)
show(Tab)






















#Model without Unknown

glm_1 <- glm(data = train_2,Attrition_Flag~ .,family = "binomial")
summary(glm_1)

pred_glm_i <- predict(glm_1,test_2,type="response")
pred_1_i <- ifelse(pred_glm_i >= Threshold1 , 1,0)
pred_2_i <- ifelse(pred_glm_i >= Threshold2 , 1,0)
pred_3_i <- ifelse(pred_glm_i >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_i <- table(test_2$Attrition_Flag,pred_1_i)
c_mat_2_i <- table(test_2$Attrition_Flag,pred_2_i)
c_mat_3_i <- table(test_2$Attrition_Flag,pred_3_i)
c_mat_1_i
c_mat_2_i
c_mat_3_i

#Accuracy

mean(pred_1_i==test_2$Attrition_Flag)*100
mean(pred_2_i==test_2$Attrition_Flag)*100
mean(pred_3_i==test_2$Attrition_Flag)*100

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


glm_3 <- update(glm_2, . ~ . + Total_Trans_Ct*Total_Trans_Amt + Total_Trans_Ct*Total_Revolving_Bal + Total_Trans_Ct*Is_Female + Total_Trans_Ct*Avg_Utilization_Ratio + Total_Trans_Ct*Marital_Status)
summary(glm_3)


glm_4 <- update(glm_3, . ~ . + Total_Relationship_Count*Total_Trans_Amt )
summary(glm_4)


glm_5 <- update(glm_4, . ~ . + Total_Revolving_Bal*Avg_Utilization_Ratio + Total_Revolving_Bal*Credit_Limit - Total_Trans_Amt:Total_Trans_Ct)
summary(glm_5)


glm_6 <- update(glm_5, . ~ . + Is_Female*Avg_Utilization_Ratio)
summary(glm_6)


glm_7 <- update(glm_6, . ~ . + Dependent_count*Total_Trans_Ct)
summary(glm_7)


glm_8 <- update(glm_7, . ~ . + Credit_Limit*Total_Trans_Amt -Total_Trans_Ct:Avg_Utilization_Ratio - Is_Female:Total_Trans_Ct + Total_Trans_Amt*Total_Trans_Ct)
summary(glm_8)


pred_glm_f <- predict(glm_8,test_2,type="response")
pred_1_f <- ifelse(pred_glm_f >= Threshold1 , 1,0)
pred_2_f <- ifelse(pred_glm_f >= Threshold2 , 1,0)
pred_3_f <- ifelse(pred_glm_f >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_f <- table(test_2$Attrition_Flag,pred_1_f)
c_mat_2_f <- table(test_2$Attrition_Flag,pred_2_f)
c_mat_3_f <- table(test_2$Attrition_Flag,pred_3_f)
c_mat_1_f
c_mat_2_f
c_mat_3_f

#Accuracy

mean(pred_1_f==test_2$Attrition_Flag)*100
mean(pred_2_f==test_2$Attrition_Flag)*100
mean(pred_3_f==test_2$Attrition_Flag)*100

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
roc_i <- roc(test_2$Attrition_Flag ~ pred_glm_i)
roc_f <- roc(test_2$Attrition_Flag ~ pred_glm_f)

AUC_i <- auc(roc_i)
AUC_f <- auc(roc_f)


plot(roc_i, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_f,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_i <- coords(roc_i,"best",best.method = "youden")$threshold
Best_pred_i <- ifelse(pred_glm_i >= Best_Treshold_i , 1,0)
Best_c_mat_i <- table(test_2$Attrition_Flag,Best_pred_i)
Best_Spec_i <- Best_c_mat_i[1,1]/sum(Best_c_mat_i[1,])
Best_Sens_i <- Best_c_mat_i[2,2]/sum(Best_c_mat_i[2,])

Best_Treshold_f <- coords(roc_f,"best",best.method = "youden")$threshold
Best_pred_f <- ifelse(pred_glm_f >= Best_Treshold_f , 1,0)
Best_c_mat_f <- table(test_2$Attrition_Flag,Best_pred_f)
Best_Spec_f <- Best_c_mat_f[1,1]/sum(Best_c_mat_f[1,])
Best_Sens_f <- Best_c_mat_f[2,2]/sum(Best_c_mat_f[2,])

#Table to showing them
Table_mat <-  matrix(c(Best_Treshold_i,Best_Spec_i,Best_Sens_i,Best_Treshold_f,Best_Spec_f,Best_Sens_f), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Initial model","Final model")
Tab <- as.table(Table_mat)
show(Tab)




















#Let's check the model built on the balanced dataset



#Thresholds
Threshold1 <- 0.4
Threshold2 <- 0.5
Threshold3 <- 0.6

glm_1 <- glm(data = train_mix_1,Attrition_Flag~ .,family = "binomial")
summary(glm_1)

pred_glm_i <- predict(glm_1,test_1,type="response")
pred_1_i <- ifelse(pred_glm_i >= Threshold1 , 1,0)
pred_2_i <- ifelse(pred_glm_i >= Threshold2 , 1,0)
pred_3_i <- ifelse(pred_glm_i >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_i <- table(test_1$Attrition_Flag,pred_1_i)
c_mat_2_i <- table(test_1$Attrition_Flag,pred_2_i)
c_mat_3_i <- table(test_1$Attrition_Flag,pred_3_i)
c_mat_1_i
c_mat_2_i
c_mat_3_i

#Accuracy

mean(pred_1_i==test_1$Attrition_Flag)*100
mean(pred_2_i==test_1$Attrition_Flag)*100
mean(pred_3_i==test_1$Attrition_Flag)*100

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

glm_2 <- update(glm_1, . ~ . - Months_on_book - Education_Level)
summary(glm_2)


glm_3 <- update(glm_2, . ~ .  + Total_Trans_Ct*Is_Female + Total_Trans_Ct*Marital_Status)
summary(glm_3)


glm_4 <- update(glm_3, . ~ . + Total_Relationship_Count*Total_Trans_Amt - Credit_Limit)
summary(glm_4)


glm_5 <- update(glm_4, . ~ . + Total_Revolving_Bal*Avg_Utilization_Ratio + Total_Revolving_Bal*Credit_Limit)
summary(glm_5)


glm_6 <- update(glm_5, . ~ . + Is_Female*Credit_Limit + Income_Category*Credit_Limit)
summary(glm_6)


glm_7 <- update(glm_6, . ~ . + Dependent_count*Total_Trans_Ct + Customer_Age*Months_on_book )
summary(glm_7)


glm_8 <- update(glm_7, . ~ . + Credit_Limit*Total_Trans_Amt - Total_Revolving_Bal:Credit_Limit)
summary(glm_8)


glm_9 <- update(glm_8, . ~ . + Avg_Utilization_Ratio*Credit_Limit + Avg_Utilization_Ratio*Total_Trans_Amt - Avg_Utilization_Ratio )
summary(glm_9)


pred_glm_f <- predict(glm_9,test_1,type="response")
pred_1_f <- ifelse(pred_glm_f >= Threshold1 , 1,0)
pred_2_f <- ifelse(pred_glm_f >= Threshold2 , 1,0)
pred_3_f <- ifelse(pred_glm_f >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_f <- table(test_1$Attrition_Flag,pred_1_f)
c_mat_2_f <- table(test_1$Attrition_Flag,pred_2_f)
c_mat_3_f <- table(test_1$Attrition_Flag,pred_3_f)
c_mat_1_f
c_mat_2_f
c_mat_3_f

#Accuracy

mean(pred_1_f==test_1$Attrition_Flag)*100
mean(pred_2_f==test_1$Attrition_Flag)*100
mean(pred_3_f==test_1$Attrition_Flag)*100

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
roc_i <- roc(test_1$Attrition_Flag ~ pred_glm_i)
roc_f <- roc(test_1$Attrition_Flag ~ pred_glm_f)

AUC_i <- auc(roc_i)
AUC_f <- auc(roc_f)


plot(roc_i, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_f,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_i <- coords(roc_i,"best",best.method = "youden")$threshold
Best_pred_i <- ifelse(pred_glm_i >= Best_Treshold_i , 1,0)
Best_c_mat_i <- table(test_1$Attrition_Flag,Best_pred_i)
Best_Spec_i <- Best_c_mat_i[1,1]/sum(Best_c_mat_i[1,])
Best_Sens_i <- Best_c_mat_i[2,2]/sum(Best_c_mat_i[2,])

Best_Treshold_f <- coords(roc_f,"best",best.method = "youden")$threshold
Best_pred_f <- ifelse(pred_glm_f >= Best_Treshold_f , 1,0)
Best_c_mat_f <- table(test_1$Attrition_Flag,Best_pred_f)
Best_Spec_f <- Best_c_mat_f[1,1]/sum(Best_c_mat_f[1,])
Best_Sens_f <- Best_c_mat_f[2,2]/sum(Best_c_mat_f[2,])

#Table to showing them
Table_mat <-  matrix(c(Best_Treshold_i,Best_Spec_i,Best_Sens_i,Best_Treshold_f,Best_Spec_f,Best_Sens_f), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Initial model","Final model")
Tab <- as.table(Table_mat)
show(Tab)



















#Model without Unknown

glm_1 <- glm(data = train_2,Attrition_Flag~ .,family = "binomial")
summary(glm_1)

pred_glm_i <- predict(glm_1,test_2,type="response")
pred_1_i <- ifelse(pred_glm_i >= Threshold1 , 1,0)
pred_2_i <- ifelse(pred_glm_i >= Threshold2 , 1,0)
pred_3_i <- ifelse(pred_glm_i >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_i <- table(test_2$Attrition_Flag,pred_1_i)
c_mat_2_i <- table(test_2$Attrition_Flag,pred_2_i)
c_mat_3_i <- table(test_2$Attrition_Flag,pred_3_i)
c_mat_1_i
c_mat_2_i
c_mat_3_i

#Accuracy

mean(pred_1_i==test_2$Attrition_Flag)*100
mean(pred_2_i==test_2$Attrition_Flag)*100
mean(pred_3_i==test_2$Attrition_Flag)*100

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


glm_3 <- update(glm_2, . ~ . + Total_Trans_Ct*Total_Trans_Amt + Total_Trans_Ct*Total_Revolving_Bal + Total_Trans_Ct*Is_Female + Total_Trans_Ct*Avg_Utilization_Ratio + Total_Trans_Ct*Marital_Status)
summary(glm_3)


glm_4 <- update(glm_3, . ~ . + Total_Relationship_Count*Total_Trans_Amt )
summary(glm_4)


glm_5 <- update(glm_4, . ~ . + Total_Revolving_Bal*Avg_Utilization_Ratio + Total_Revolving_Bal*Credit_Limit - Total_Trans_Amt:Total_Trans_Ct)
summary(glm_5)


glm_6 <- update(glm_5, . ~ . + Is_Female*Avg_Utilization_Ratio)
summary(glm_6)


glm_7 <- update(glm_6, . ~ . + Dependent_count*Total_Trans_Ct)
summary(glm_7)


glm_8 <- update(glm_7, . ~ . + Credit_Limit*Total_Trans_Amt -Total_Trans_Ct:Avg_Utilization_Ratio - Is_Female:Total_Trans_Ct + Total_Trans_Amt*Total_Trans_Ct)
summary(glm_8)


pred_glm_f <- predict(glm_8,test_2,type="response")
pred_1_f <- ifelse(pred_glm_f >= Threshold1 , 1,0)
pred_2_f <- ifelse(pred_glm_f >= Threshold2 , 1,0)
pred_3_f <- ifelse(pred_glm_f >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1_f <- table(test_2$Attrition_Flag,pred_1_f)
c_mat_2_f <- table(test_2$Attrition_Flag,pred_2_f)
c_mat_3_f <- table(test_2$Attrition_Flag,pred_3_f)
c_mat_1_f
c_mat_2_f
c_mat_3_f

#Accuracy

mean(pred_1_f==test_2$Attrition_Flag)*100
mean(pred_2_f==test_2$Attrition_Flag)*100
mean(pred_3_f==test_2$Attrition_Flag)*100

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
roc_i <- roc(test_2$Attrition_Flag ~ pred_glm_i)
roc_f <- roc(test_2$Attrition_Flag ~ pred_glm_f)

AUC_i <- auc(roc_i)
AUC_f <- auc(roc_f)


plot(roc_i, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_f,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_i <- coords(roc_i,"best",best.method = "youden")$threshold
Best_pred_i <- ifelse(pred_glm_i >= Best_Treshold_i , 1,0)
Best_c_mat_i <- table(test_2$Attrition_Flag,Best_pred_i)
Best_Spec_i <- Best_c_mat_i[1,1]/sum(Best_c_mat_i[1,])
Best_Sens_i <- Best_c_mat_i[2,2]/sum(Best_c_mat_i[2,])

Best_Treshold_f <- coords(roc_f,"best",best.method = "youden")$threshold
Best_pred_f <- ifelse(pred_glm_f >= Best_Treshold_f , 1,0)
Best_c_mat_f <- table(test_2$Attrition_Flag,Best_pred_f)
Best_Spec_f <- Best_c_mat_f[1,1]/sum(Best_c_mat_f[1,])
Best_Sens_f <- Best_c_mat_f[2,2]/sum(Best_c_mat_f[2,])

#Table to showing them
Table_mat <-  matrix(c(Best_Treshold_i,Best_Spec_i,Best_Sens_i,Best_Treshold_f,Best_Spec_f,Best_Sens_f), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Initial model","Final model")
Tab <- as.table(Table_mat)
show(Tab)

