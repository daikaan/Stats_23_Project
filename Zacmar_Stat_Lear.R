library("dplyr")
library("corrplot")
library("caTools")
library("ggpubr")
library("ROSE")
library("corpcor")
library("car")


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
corrplot(cor_mat,method = "color",type = "upper", tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)

bank_data <- bank_data %>% mutate_if(is.numeric, scale)

bank_data$Attrition_Flag <- as.numeric(bank_data$Attrition_Flag == "Attrited Customer")


#We convert categorical variables into numerical
new_bank_data <- data.frame(bank_data)


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
new_cor_mat <- cor(new_bank_data[2:21])
corrplot(new_cor_mat[1:20,1:20],method = "color",type = "upper", tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)


#Partial correlation matrix (could be wrong) (Might be better to use correlation package even if the computation is slow)
pcor <- cor2pcor(new_cor_mat)
colnames(pcor) <- colnames(new_cor_mat)
rownames(pcor) <- rownames(new_cor_mat)
corrplot(pcor,method = "color",type = "upper", tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)


#Correlation with Attrition_Flag
corrplot(new_cor_mat[1,1:20,drop=FALSE],method = "number",cl.pos = "n",tl.col = "black" ,tl.cex=0.5,diag = FALSE)

#We check how the variables with the bigger correlation with Attrition_flag change between Attrited and Existing Customer
Customer <- bank_data_origin$Attrition_Flag
a <- ggplot(new_bank_data, aes(x = Total_Trans_Ct, fill = Customer)) +
      geom_density(alpha=0.2) + ggtitle("Total_Trans_Ct")

b <- ggplot(new_bank_data, aes(x = Total_Ct_Chng_Q4_Q1, fill = Customer)) +
      geom_density(alpha=0.2) + ggtitle("Total_Ct_Chng_Q4_Q1")

c <- ggplot(new_bank_data, aes(x = Total_Revolving_Bal, fill = Customer)) +
      geom_density(alpha=0.2) + ggtitle("Total_Revolving_Bal")

d <- ggplot(new_bank_data, aes(x = Contacts_Count_12_mon, fill = Customer)) +
      geom_bar(aes(y = after_stat(prop) )) + ggtitle("Contacts_Count_12_mon")

e <- ggplot(new_bank_data, aes(x = Avg_Utilization_Ratio, fill = Customer)) +
     geom_density(alpha=0.2) + ggtitle("Avg_Utilization_Ratio")

f <- ggplot(new_bank_data, aes(x = Total_Trans_Amt, fill = Customer)) +
  geom_density(alpha=0.2) + ggtitle("Total_Trans_Amt")

ggarrange(a,b,c,d,e,f,nrow=2,ncol=3)



bank_data_NA <- data.frame(bank_data)
bank_data_NA[bank_data_NA=='Unknown'] <- NA

#Build a dataset without missing values
bank_data_withoutNA <- na.omit(bank_data_NA)




# Train-test Split

set.seed(0987)

sample_1 <- sample.split(bank_data_withoutNA$Attrition_Flag,SplitRatio = 0.75)
train_1 <- subset(bank_data_withoutNA[2:21],sample_1 == TRUE)
test_1 <- subset(bank_data_withoutNA[2:21],sample_1 == FALSE)

sample_2 <- sample.split(bank_data$Attrition_Flag,SplitRatio = 0.75)
train_2 <- subset(bank_data[2:21],sample_2 == TRUE)
test_2 <- subset(bank_data[2:21],sample_2 == FALSE)

#Proportion of Attrited and Existing Customer
prop.table(table(train_1$Attrition_Flag))
prop.table(table(test_1$Attrition_Flag))

prop.table(table(train_2$Attrition_Flag))
prop.table(table(test_2$Attrition_Flag))


#Original proportion of Attrited and Existing Customer
prop.table(table(new_bank_data$Attrition_Flag))

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

glm_1 <- glm(data = train_1,Attrition_Flag~ . -Avg_Open_To_Buy,family = "binomial")
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


glm_2 <- glm(data = train_2,Attrition_Flag~ .-Avg_Open_To_Buy,family = "binomial")
summary(glm_2)

pred_glm_2 <- predict(glm_2,test_2,type="response")
pred_1_2 <- ifelse(pred_glm_2 > threshold1 , 1,0)
pred_2_2 <- ifelse(pred_glm_2 > threshold2 , 1,0)
pred_3_2 <- ifelse(pred_glm_2 > threshold3 , 1,0)

table(test_2$Attrition_Flag,pred_1_2)
table(test_2$Attrition_Flag,pred_2_2)
table(test_2$Attrition_Flag,pred_3_2)

mean(pred_1_2==test_2$Attrition_Flag)
mean(pred_2_2==test_2$Attrition_Flag)
mean(pred_3_2==test_2$Attrition_Flag)









s_1 <- summary(glm_1)
r2_1 <- 1 - (s_1$deviance/s_1$null.deviance)

1/(1-r2_1)

vif(glm_1)

s_2 <- summary(glm_2)
r2_2 <- 1 - (s_2$deviance/s_2$null.deviance)

1/(1-r2_2)

vif(glm_2)


glm_1 <- glm(data = train_1,Attrition_Flag~ . -Avg_Open_To_Buy -Customer_Age,family = "binomial")
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


glm_1 <- glm(data = train_1,Attrition_Flag~ . -Avg_Open_To_Buy -Customer_Age -Education_Level,family = "binomial")
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


glm_1 <- glm(data = train_1,Attrition_Flag~ . -Avg_Open_To_Buy -Customer_Age -Education_Level - Total_Amt_Chng_Q4_Q1,family = "binomial")
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






#This part is to check how the rows containg at least one "unknown" are distributed (Probably useless)

#Change Unknown value to NA
bank_data_copy <- data.frame(bank_data)
bank_data_copy[bank_data_copy=='Unknown'] <- NA

#Build a dataset without missing values
bank_data_rev <- na.omit(bank_data_copy)

# Number of rows containing at least one "Unknown"
dim(bank_data_copy)[1] - dim(bank_data_rev)[1]

#Split the initial data based on attrition flag
bank_data_split <- split(bank_data_origin,bank_data_origin$Attrition_Flag)

dim(bank_data_split$`Attrited Customer`)
dim(bank_data_split$`Existing Customer`)

#We check how the rows containing "Unknown" are distributed in relation to the split dataset

bank_data_split$`Attrited Customer`[bank_data_split$`Attrited Customer`=='Unknown'] <- NA
(dim(bank_data_split$`Attrited Customer`)[1]-dim(na.omit(bank_data_split$`Attrited Customer`))[1])/dim(bank_data_split$`Attrited Customer`)[1]

bank_data_split$`Existing Customer`[bank_data_split$`Existing Customer`=='Unknown'] <- NA
(dim(bank_data_split$`Existing Customer`)[1]-dim(na.omit(bank_data_split$`Existing Customer`))[1])/dim(bank_data_split$`Existing Customer`)[1]
