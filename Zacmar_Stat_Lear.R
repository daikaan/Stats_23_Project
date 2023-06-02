library("dplyr")
library("corrplot")
library("caTools")
library("ggpubr")
library("ROSE")
library("corpcor")
library("car")
library("correlation")
library("ppcor")


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


order_Card_Category <- list("Blue" = 1,
                            "Silver" = 2,
                            "Gold" = 3,
                            "Platinum" = 4)
bank_data$Card_Category <- unlist(order_Card_Category[as.character(bank_data$Card_Category)])


#Correlation matrix
cor_mat_tot <- cor(bank_data)
corrplot(cor_mat_tot,method = "number",type = "upper",number.cex = 0.6, tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)

#Correlation with Attrition_Flag
corrplot(cor_mat_tot[1,1:20,drop=FALSE],method = "number",number.cex = 0.6, cl.pos = "n",tl.col = "black" ,tl.cex=0.5,diag = FALSE)

#Since Credit_Limit and Avg_Open_To_Buy have correlation 1 we can remove Credit_Limit
bank_data <- subset(bank_data, select = -c(Credit_Limit))

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

b <- ggplot(bank_data, aes(x = Total_Ct_Chng_Q4_Q1, fill = Customer)) +
      geom_density(alpha=0.3) + ggtitle("Total_Ct_Chng_Q4_Q1")+
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

c <- ggplot(bank_data, aes(x = Total_Revolving_Bal, fill = Customer)) +
      geom_density(alpha=0.3) + ggtitle("Total_Revolving_Bal") + 
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

d <- ggplot(bank_data, aes(x = Contacts_Count_12_mon, fill = Customer)) +
      geom_bar(aes(y = after_stat(prop) ),alpha=0.3) + ggtitle("Contacts_Count_12_mon") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

e <- ggplot(bank_data, aes(x = Avg_Utilization_Ratio, fill = Customer)) +
     geom_density(alpha=0.3) + ggtitle("Avg_Utilization_Ratio") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

f <- ggplot(bank_data, aes(x = Total_Trans_Amt, fill = Customer)) +
  geom_density(alpha=0.3) + ggtitle("Total_Trans_Amt") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

ggarrange(a,b,c,d,e,f,nrow=2,ncol=3)



bank_data_NA <- data.frame(bank_data)
bank_data_NA[bank_data_NA=='Unknown'] <- NA

#Build a dataset without missing values
bank_data_withoutNA <- na.omit(bank_data_NA)




# Train-test Split

set.seed(0987)

sample_1 <- sample.split(bank_data_withoutNA$Attrition_Flag,SplitRatio = 0.75)
train_1 <- subset(bank_data_withoutNA,sample_1 == TRUE)
test_1 <- subset(bank_data_withoutNA,sample_1 == FALSE)

sample_2 <- sample.split(bank_data$Attrition_Flag,SplitRatio = 0.75)
train_2 <- subset(bank_data,sample_2 == TRUE)
test_2 <- subset(bank_data,sample_2 == FALSE)

#Proportion of Attrited and Existing Customer
prop.table(table(train_1$Attrition_Flag))
prop.table(table(test_1$Attrition_Flag))

prop.table(table(train_2$Attrition_Flag))
prop.table(table(test_2$Attrition_Flag))


#Original proportion of Attrited and Existing Customer
prop.table(table(bank_data$Attrition_Flag))

#It's an unbalanced dataset.
#It might be better to consider a resampling of the dataset

# Under-sampling
train_under <- ovun.sample(Attrition_Flag~.,data = train_1, method = "under")$data

# Over-sampling
train_over <- ovun.sample(Attrition_Flag~.,data = train_1, method = "over")$data

#Mixed Sampling with 40% of Attrited Customer

train_mix <- ovun.sample(Attrition_Flag~.,data = train_1, method = "both", p = 0.4, N =5311)$data



#Thresholds for classification:
threshold1 <- 0.4
threshold2 <- 0.5
threshold3 <- 0.6

glm_1 <- glm(data = train_1,Attrition_Flag~ .,family = "binomial")
summary(glm_1)

pred_glm_1 <- predict(glm_1,test_1,type="response")
pred_1 <- ifelse(pred_glm_1 > threshold1 , 1,0)
pred_2 <- ifelse(pred_glm_1 > threshold2 , 1,0)
pred_3 <- ifelse(pred_glm_1 > threshold3 , 1,0)

table(test_1$Attrition_Flag,pred_1)
table(test_1$Attrition_Flag,pred_2)
table(test_1$Attrition_Flag,pred_3)

mean(pred_1==test_1$Attrition_Flag)
mean(pred_2==test_1$Attrition_Flag)
mean(pred_3==test_1$Attrition_Flag)


vif(glm_1)


glm_2 <- update(glm_1, . ~ . - Education_Level - Months_on_book -Avg_Utilization_Ratio - Avg_Open_To_Buy)
summary(glm_2)


glm_3 <- update(glm_2, . ~ . + Total_Trans_Amt:Total_Trans_Ct + Total_Trans_Amt:Total_Relationship_Count + Total_Trans_Amt:Total_Revolving_Bal)
summary(glm_3)


glm_4 <- update(glm_3, . ~ . -Card_Category -Customer_Age)
summary(glm_4)


glm_5 <- update(glm_4, . ~ . + Is_Female:Income_Category + Is_Female:Total_Trans_Amt )
summary(glm_5)


glm_6 <- update(glm_5, . ~ . +Total_Amt_Chng_Q4_Q1:Total_Trans_Amt +Marital_Status:Total_Trans_Amt)
summary(glm_6)


pred_glm_6 <- predict(glm_6,test_1,type="response")
pred_1 <- ifelse(pred_glm_6 > threshold1 , 1,0)
pred_2 <- ifelse(pred_glm_6 > threshold2 , 1,0)
pred_3 <- ifelse(pred_glm_6 > threshold3 , 1,0)

table(test_1$Attrition_Flag,pred_1)
table(test_1$Attrition_Flag,pred_2)
table(test_1$Attrition_Flag,pred_3)

mean(pred_1==test_1$Attrition_Flag)
mean(pred_2==test_1$Attrition_Flag)
mean(pred_3==test_1$Attrition_Flag)


#Trying an alternative choice for the model


glm_2_alt <- update(glm_1, . ~ . + Total_Trans_Amt:Total_Trans_Ct + Total_Trans_Amt:Total_Relationship_Count)
summary(glm_2_alt)

glm_3_alt <- update(glm_2_alt, . ~ . + Avg_Open_To_Buy:Total_Revolving_Bal + Avg_Open_To_Buy:Avg_Utilization_Ratio)
summary(glm_3_alt)

glm_4_alt <- update(glm_3_alt, . ~ . + Customer_Age:Months_on_book + Total_Revolving_Bal:Total_Trans_Ct)
summary(glm_4_alt)

glm_5_alt <- update(glm_4_alt, . ~ . - Income_Category - Card_Category - Education_Level - Marital_Status + Dependent_count:Total_Trans_Amt)
summary(glm_5_alt)

pred_glm_5_alt <- predict(glm_5_alt,test_1,type="response")
pred_1 <- ifelse(pred_glm_5_alt > threshold1 , 1,0)
pred_2 <- ifelse(pred_glm_5_alt > threshold2 , 1,0)
pred_3 <- ifelse(pred_glm_5_alt > threshold3 , 1,0)

table(test_1$Attrition_Flag,pred_1)
table(test_1$Attrition_Flag,pred_2)
table(test_1$Attrition_Flag,pred_3)

mean(pred_1==test_1$Attrition_Flag)
mean(pred_2==test_1$Attrition_Flag)
mean(pred_3==test_1$Attrition_Flag)


