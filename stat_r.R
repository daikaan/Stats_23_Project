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
library("interactions")
library("glmnet")
library("formattable") # for giving a variable dictionary a better look
library("RColorBrewer")# for the visualization colorings

bank_data_origin <- read.csv('~/GitHub/Stats_23_Project/BankChurners.csv')
head(bank_data_origin)

summary(bank_data_origin)

dim(bank_data_origin)
bank_data <- subset(bank_data_origin, select = -c(Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2))
final_dim <- dim(bank_data)
final_dim


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

#change 'Existing Customer' to 1 and 'Attrited Customer' to 0 and add new column to quantitative
bank_data_withoutNA_quan$Attrition_Flag <- as.numeric(bank_data_withoutNA_quan$Attrition_Flag == "Attrited Customer")

bank_data_withoutNA_quan$Gender <- as.numeric(bank_data_withoutNA_quan$Gender == "F")
bank_data_withoutNA_quan <- bank_data_withoutNA_quan %>% rename("Is_Female" = "Gender")

order_education_level <- list("Uneducated" = 1,
                              "High School" = 2,
                              "College" = 3,
                              "Graduate" = 4,
                              "Post-Graduate" = 5,
                              "Doctorate" = 6)
bank_data_withoutNA_quan$Education_Level <- unlist(order_education_level[as.character(bank_data_withoutNA_quan$Education_Level)])

order_Marital_Status <- list("Single" = 1,
                             "Married" = 2,
                             "Divorced" = 3)
bank_data_withoutNA_quan$Marital_Status <- unlist(order_Marital_Status[as.character(bank_data_withoutNA_quan$Marital_Status)])

order_Income_Category <- list("Less than $40K" = 1,
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


#Categorical Value Visualizations

#Bar plots

par(mfrow=c(2,2))
myPalette <- brewer.pal(6, "Set2")
ggplot(bank_data_withoutNA_quan, aes(x = as.factor(Income_Category), fill = factor(Attrition_Flag))) + geom_bar() + labs(fill = "Attrition Flag") 
ggplot(bank_data_withoutNA_quan, aes(x = as.factor(Marital_Status), fill = factor(Attrition_Flag))) + geom_bar() + labs(fill = "Attrition Flag") 
ggplot(bank_data_withoutNA_quan, aes(x = as.factor(Education_Level), fill = factor(Attrition_Flag))) + geom_bar() + labs(fill = "Attrition Flag") 
ggplot(bank_data_withoutNA_quan, aes(x = as.factor(Card_Category), fill = factor(Attrition_Flag))) + geom_bar() + labs(fill = "Attrition Flag")


bank_data_withoutNA_quan %>%
  group_by(Is_Female, Attrition_Flag) %>%
  summarize(Freq=n())

# Card Category

ggplot(bank_data_withoutNA_quan, aes(x=Months_on_book, y= Credit_Limit, shape = as.factor(Card_Category), color= as.factor(Card_Category)))+
  geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


#Numerical columns analysis
attach(bank_data_withoutNA_quan)

cust.age.boxplot <- boxplot(Customer_Age, main="Customer age", ylab = "age")
months.onbook.boxplot <- boxplot(Months_on_book, main="Months on Book", ylab = "months")
reltn.cnt.boxplot <- boxplot(Total_Relationship_Count, main="Total_Relationship_Count", ylab = "#")
mnths.inact.boxplot <- boxplot(Months_Inactive_12_mon,  main="Months_Inactive_12_mon", ylab = "months")
cntc.cnt.boxplot <- boxplot(Contacts_Count_12_mon, main="Contacts_Count_12_mon", ylab = "#")
credit.limit.boxplot <- boxplot(Credit_Limit,  main="Credit_Limit", ylab = "Dollars")
ttl.revbal.boxplot <- boxplot(Total_Revolving_Bal, main="Total_Revolving_Bal", ylab = "Dollars")
avg.opnbuy.boxplot <- boxplot(Avg_Open_To_Buy, main="Avg_Open_To_Buy", ylab = "Dollars")
total.amtchg.boxplot <- boxplot(Total_Amt_Chng_Q4_Q1, main="Total_Amt_Chng_Q4_Q1", ylab = "Dollars")
ttl.transct.boxplot <- boxplot(Total_Trans_Ct, main="Total_Trans_Ct", ylab = "#")
total.cntchg.boxplot <- boxplot(Total_Ct_Chng_Q4_Q1, main="Total_Ct_Chng_Q4_Q1", ylab = "Dollars")
avg.utilrate.boxplot <- boxplot(Avg_Utilization_Ratio, main="Avg_Utilization_Ratio", ylab = "Ratio")

#Since the outliers in most of the numerical columns can be identifying on whether the customer is going to churn we decided to keep them in the data set

#Extracting Outliers from age and Rescoping the study to only focus on Blue Cards

#using the 1st quartile-1.5*IQR and 3rd quartile+1.5*IQR rule, 
#it is seen that customers over the age of 70 are outliers
age.exc.list <- boxplot.stats(bank_data_withoutNA_quan$Customer_Age)$out

#Since most of the data is coming from the Blue cards and there is a visible difference on many parameters among categories, we decided to only focus on blue card category
card.exc.list <- c(2, 3, 4)


cleaned_bank_data_withoutNA_quan <- subset(bank_data_withoutNA_quan,!((Customer_Age %in% age.exc.list)| (Card_Category %in% card.exc.list)))
cleaned_bank_data_withoutNA_quan<- subset(cleaned_bank_data_withoutNA_quan, select = -c(Card_Category))
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

#grouped age bar plot
par(mfrow=c(1,1))
cleaned_bank_data_withoutNA_quan %>%
  group_by(age_group) %>% summarise(N=n()) %>%
  ggplot(aes(x=age_group,y=N,fill=age_group))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  geom_text(aes(label=N),vjust=-0.25,fontface='bold')+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))


# grouped age piechart
age.labels <- c("<=34", "35-44", "45-54", ">=55")
cust.age.piechart <- pie(count(cleaned_bank_data_withoutNA_quan, age_group)$n, border="white", col=myPalette, labels = age.labels)

#Dependent Count

depcount.labels <- c(0, 1, 2, 3, 4, 5)
dependent.count.piechart <- pie(count(cleaned_bank_data_withoutNA_quan, Dependent_count)$n, border="white", col=myPalette, labels = depcount.labels)

#Months inactive
library(yarrr) #to make colors transparent
barplot(table(factor(Months_Inactive_12_mon,levels=min(Months_Inactive_12_mon):max(Months_Inactive_12_mon))), col = yarrr::transparent('red',trans.val = 0.9))
barplot(table(factor(Contacts_Count_12_mon,levels=min(Contacts_Count_12_mon):max(Contacts_Count_12_mon))), col = yarrr::transparent('blue', trans.val = 0.8), add = TRUE)

hist(cleaned_bank_data_withoutNA_quan$Total_Trans_Ct)

int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}


dev.off(dev.list()["RStudioGD"]) #to clear the previous plots on the screen

fig.dim = c(4, 3)

#Histograms
attach(cleaned_bank_data_withoutNA_quan)
par(mfrow=c(3,2))
hist(Avg_Open_To_Buy)
hist(Total_Trans_Amt)
hist(Avg_Utilization_Ratio)
hist(Months_on_book)
hist(Credit_Limit)
hist(Months_Inactive_12_mon)


#Correlation matrix
cor_mat_new <- cor(bank_data_withoutNA_quan[2:15])
corrplot(cor_mat_new,method = "number",type = "upper", tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)


set.seed(0237)

sample <- sample.split(cleaned_bank_data_withoutNA_quan[,2:20]$Attrition_Flag,
                       SplitRatio = 0.75)
train <- subset(cleaned_bank_data_withoutNA_quan[,2:20],sample == TRUE)
test <- subset(cleaned_bank_data_withoutNA_quan[,2:20],sample == FALSE)


#Proportion of Attrited and Existing Customer in Training set
prop.table(table(train$Attrition_Flag))

#Proportion of Attrited and Existing Customer in Test set
prop.table(table(test$Attrition_Flag))


#Mixed Sampling with 50% of Attrited Customer
train_bal <- ovun.sample(Attrition_Flag~.,data = train, method = "both", p = 0.5, N =4948)$data



attach(train)


train$Attrition_Flag <- as.numeric(train$Attrition_Flag)

#Correlation matrix
cor_mat <- cor(train)
corrplot(cor_mat,method = "number",type = "upper",number.cex = 0.6, tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)

#Correlation with Attrition_Flag
corrplot(cor_mat[1,,drop=FALSE],method = "number",number.cex = 0.6, cl.pos = "n",tl.col = "black" ,tl.cex=0.5,diag = FALSE)


#Partial correlations (Takes time)
correlation(train[,-14],partial = TRUE)

#Partial Correlation matrix 
part_cor_mat <- pcor(train[,-14])$estimate
corrplot(part_cor_mat, method = "number",type = "upper",number.cex = 0.6, tl.pos = "td",tl.cex=0.5, tl.col = "black" ,diag = FALSE)

Customer <- as.factor(train$Attrition_Flag)
a <- ggplot(train, aes(x = Total_Trans_Ct, fill = Customer)) +
  geom_density(alpha=0.3) + ggtitle("Total_Trans_Ct") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

b <- ggplot(train, aes(x = Total_Relationship_Count, fill = Customer)) +
  geom_bar(aes(y = after_stat(prop) ),alpha=0.3, position = "dodge") + 
  ggtitle("Total_Relationship_Count") + 
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

c <- ggplot(train, aes(x = Total_Trans_Amt, fill = Customer)) +
  geom_density(alpha=0.3) + ggtitle("Total_Trans_Amt") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

d <- ggplot(train, aes(x = Total_Ct_Chng_Q4_Q1, fill = Customer)) +
  geom_density(alpha=0.3) + ggtitle("Total_Ct_Chng_Q4_Q1")+
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

e <- ggplot(train, aes(x = Contacts_Count_12_mon, fill = Customer)) +
  geom_bar(aes(y = after_stat(prop) ),alpha=0.3, position = "dodge") + 
  ggtitle("Contacts_Count_12_mon") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

f <- ggplot(train, aes(x = Total_Revolving_Bal, fill = Customer)) +
  geom_density(alpha=0.3) + ggtitle("Total_Revolving_Bal") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

g <- ggplot(train, aes(x = Months_Inactive_12_mon, fill = Customer)) +
  geom_bar(aes(y = after_stat(prop) ),alpha=0.3, position = "dodge") + 
  ggtitle("Months_Inactive_12_mon") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))

h <- ggplot(train, aes(x = Is_Female, fill = Customer)) +
  geom_bar(aes(y = after_stat(prop) ),alpha=0.3, position = "dodge") + 
  ggtitle("Is_Female") +
  scale_fill_manual(values = c("darkgrey","red"),labels = c("Existing","Attrited"))


ggarrange(a,b,c,d,e,f,g,h,nrow=4,ncol=2)



# Unbalanced model

glm_1 <- glm(data = train,Attrition_Flag~ . - Avg_Open_To_Buy ,family = "binomial")
summary(glm_1)

#Thresholds
Threshold1 <- 0.3
Threshold2 <- 0.4
Threshold3 <- 0.5

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

glm_2 <- update(glm_1, . ~ . - Avg_Utilization_Ratio - Months_on_book)
summary(glm_2)

vif(glm_2)

glm_3 <- update(glm_2, . ~ . - Months_on_book)
summary(glm_3)


glm_4 <- update(glm_3, . ~ . + Total_Amt_Chng_Q4_Q1*Customer_Age + Total_Amt_Chng_Q4_Q1*Dependent_count 
                + Total_Amt_Chng_Q4_Q1*Total_Trans_Amt + Total_Amt_Chng_Q4_Q1*Total_Trans_Ct)
summary(glm_4)

#Show that there is interaction
interact_plot(glm_4,pred = Total_Amt_Chng_Q4_Q1,modx = Customer_Age, outcome.scale = "link")
interact_plot(glm_4,pred = Total_Amt_Chng_Q4_Q1,modx = Dependent_count, outcome.scale = "link")
interact_plot(glm_4,pred = Total_Amt_Chng_Q4_Q1,modx = Total_Trans_Amt, outcome.scale = "link")
interact_plot(glm_4,pred = Total_Amt_Chng_Q4_Q1,modx = Total_Trans_Ct, outcome.scale = "link")



glm_5 <- update(glm_4, . ~ . + Total_Revolving_Bal*Credit_Limit + Total_Revolving_Bal*Avg_Utilization_Ratio)
summary(glm_5)

#Interaction
interact_plot(glm_5,pred = Total_Revolving_Bal,modx = Credit_Limit, outcome.scale = "link")
interact_plot(glm_5,pred = Total_Revolving_Bal,modx = Avg_Utilization_Ratio, outcome.scale = "link")


glm_6 <- update(glm_5, . ~ . + Credit_Limit*Avg_Utilization_Ratio)
summary(glm_6)

#Show that there is interaction
interact_plot(glm_6,pred = Credit_Limit,modx = Avg_Utilization_Ratio, outcome.scale = "link")


glm_7 <- update(glm_6, . ~ . + Total_Ct_Chng_Q4_Q1*Is_Female)
summary(glm_7)

#Interaction
interact_plot(glm_7,pred = Total_Ct_Chng_Q4_Q1,modx = Is_Female, outcome.scale = "link")

glm_8 <- update(glm_7, . ~ . - Education_Level)
summary(glm_8)


glm_9 <- update(glm_8, . ~ .+ Customer_Age*Marital_Status)
summary(glm_9)

#Interaction
interact_plot(glm_9,pred = Customer_Age,modx = Marital_Status , outcome.scale = "link")

pred_glm_f <- predict(glm_9,test,type="response")
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

#Table to show them
Table_mat <-  matrix(c(Best_Treshold_i,Best_Spec_i,Best_Sens_i,Best_Treshold_f,Best_Spec_f,Best_Sens_f), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Initial model","Final model")
Tab <- as.table(Table_mat)
show(Tab)





#Model with balanced train set

glm_1_bal <- glm(data = train_bal,Attrition_Flag~ . - Avg_Open_To_Buy,family = "binomial")
summary(glm_1_bal)

#Thresholds
Threshold1 <- 0.6
Threshold2 <- 0.7
Threshold3 <- 0.8

pred_glm_bal_i <- predict(glm_1_bal,test,type="response")
pred_1_i <- ifelse(pred_glm_bal_i >= Threshold1 , 1,0)
pred_2_i <- ifelse(pred_glm_bal_i >= Threshold2 , 1,0)
pred_3_i <- ifelse(pred_glm_bal_i >= Threshold3 , 1,0)

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

glm_2_bal <- update(glm_1_bal, . ~ . - Months_on_book)
summary(glm_2_bal)

vif(glm_2_bal)

glm_3_bal <- update(glm_2_bal, . ~ . - Months_on_book)
summary(glm_3_bal)


glm_4_bal <- update(glm_3_bal, . ~ . + Total_Amt_Chng_Q4_Q1*Customer_Age + Total_Amt_Chng_Q4_Q1*Dependent_count 
                    + Total_Amt_Chng_Q4_Q1*Total_Trans_Amt)
summary(glm_4_bal)

#Show that there is interaction
interact_plot(glm_4_bal,pred = Total_Amt_Chng_Q4_Q1,modx = Customer_Age, outcome.scale = "link")
interact_plot(glm_4_bal,pred = Total_Amt_Chng_Q4_Q1,modx = Dependent_count, outcome.scale = "link")
interact_plot(glm_4_bal,pred = Total_Amt_Chng_Q4_Q1,modx = Total_Trans_Amt, outcome.scale = "link")


glm_5_bal <- update(glm_4_bal, . ~ . + Total_Revolving_Bal*Credit_Limit + Total_Revolving_Bal*Avg_Utilization_Ratio)
summary(glm_5_bal)

#Interaction
interact_plot(glm_5_bal,pred = Total_Revolving_Bal,modx = Credit_Limit, outcome.scale = "link")
interact_plot(glm_5_bal,pred = Avg_Utilization_Ratio,modx = Total_Revolving_Bal, outcome.scale = "link")

glm_6_bal <- update(glm_5_bal, . ~ . + Credit_Limit*Avg_Utilization_Ratio)
summary(glm_6_bal)

#Show that there is interaction
interact_plot(glm_6_bal,pred = Avg_Utilization_Ratio,modx = Credit_Limit, outcome.scale = "link")


glm_7_bal <- update(glm_6_bal, . ~ . + Total_Ct_Chng_Q4_Q1*Is_Female)
summary(glm_7_bal)

#Interaction
interact_plot(glm_7_bal,pred = Total_Ct_Chng_Q4_Q1,modx = Is_Female, outcome.scale = "link")


glm_8_bal <- update(glm_7_bal, . ~ . + Customer_Age*Marital_Status)
summary(glm_8_bal)

#Interaction
interact_plot(glm_8_bal,pred = Customer_Age,modx = Marital_Status, outcome.scale = "link")



pred_glm_bal_f <- predict(glm_8_bal,test,type="response")
pred_1_f <- ifelse(pred_glm_bal_f >= Threshold1 , 1,0)
pred_2_f <- ifelse(pred_glm_bal_f >= Threshold2 , 1,0)
pred_3_f <- ifelse(pred_glm_bal_f >= Threshold3 , 1,0)

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
roc_bal_i <- roc(test$Attrition_Flag ~ pred_glm_bal_i)
roc_bal_f <- roc(test$Attrition_Flag ~ pred_glm_bal_f)

AUC_bal_i <- auc(roc_i)
AUC_bal_f <- auc(roc_f)


plot(roc_bal_i, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_bal_f,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_bal_i <- coords(roc_bal_i,"best",best.method = "closest.topleft")$threshold
Best_pred_bal_i <- ifelse(pred_glm_bal_i >= Best_Treshold_bal_i , 1,0)
Best_c_mat_i <- table(test$Attrition_Flag,Best_pred_bal_i)
Best_Spec_bal_i <- Best_c_mat_i[1,1]/sum(Best_c_mat_i[1,])
Best_Sens_bal_i <- Best_c_mat_i[2,2]/sum(Best_c_mat_i[2,])

Best_Treshold_bal_f <- coords(roc_bal_f,"best",best.method = "closest.topleft")$threshold
Best_pred_bal_f <- ifelse(pred_glm_bal_f >= Best_Treshold_bal_f , 1,0)
Best_c_mat_f <- table(test$Attrition_Flag,Best_pred_bal_f)
Best_Spec_bal_f <- Best_c_mat_f[1,1]/sum(Best_c_mat_f[1,])
Best_Sens_bal_f <- Best_c_mat_f[2,2]/sum(Best_c_mat_f[2,])

#Table to show them
Table_mat <-  matrix(c(Best_Treshold_bal_i,Best_Spec_bal_i,Best_Sens_bal_i,Best_Treshold_bal_f,Best_Spec_bal_f,Best_Sens_bal_f), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Initial model","Final model")
Tab <- as.table(Table_mat)
show(Tab)











#LDA


#We can see that the predictor variables don't follow a normal distribution on at least one class
apply(train[train$Attrition_Flag == 1,][2:19],2,shapiro.test )
apply(train[train$Attrition_Flag == 0,][2:19],2,shapiro.test )

#We still try the LDA and QDA models

#Thresholds
Threshold1 <- 0.4
Threshold2 <- 0.5
Threshold3 <- 0.6

lda_1 <- lda(Attrition_Flag ~ Customer_Age + Is_Female + Dependent_count
             + Marital_Status + Income_Category + Total_Relationship_Count 
             + Months_Inactive_12_mon + Contacts_Count_12_mon + Credit_Limit 
             + Total_Revolving_Bal + Total_Amt_Chng_Q4_Q1 
             + Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 
             + Avg_Utilization_Ratio + Customer_Age:Total_Amt_Chng_Q4_Q1 
             + Dependent_count:Total_Amt_Chng_Q4_Q1
             + Total_Amt_Chng_Q4_Q1:Total_Trans_Amt 
             + Total_Amt_Chng_Q4_Q1:Total_Trans_Ct 
             + Credit_Limit:Total_Revolving_Bal 
             + Total_Revolving_Bal:Avg_Utilization_Ratio 
             + Credit_Limit:Avg_Utilization_Ratio 
             + Is_Female:Total_Ct_Chng_Q4_Q1 + Customer_Age:Marital_Status  , data = train, family = "binomial")

lda_1



lda_1_predict <- predict(lda_1,test,type = "response")
lda_predict_1 <- lda_1_predict$posterior
pred_1 <- ifelse(lda_predict_1[,2] >= Threshold1 , 1,0)
pred_2 <- ifelse(lda_predict_1[,2] >= Threshold2 , 1,0)
pred_3 <- ifelse(lda_predict_1[,2] >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1 <- table(test$Attrition_Flag,pred_1)
c_mat_2 <- table(test$Attrition_Flag,pred_2)
c_mat_3 <- table(test$Attrition_Flag,pred_3)
c_mat_1
c_mat_2
c_mat_3

#Accuracy

mean(pred_1==test$Attrition_Flag)*100
mean(pred_2==test$Attrition_Flag)*100
mean(pred_3==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1 <- c_mat_1[1,1]/sum(c_mat_1[1,])
Spec_2 <- c_mat_2[1,1]/sum(c_mat_2[1,])
Spec_3 <- c_mat_3[1,1]/sum(c_mat_3[1,])
Spec_1
Spec_2
Spec_3


#Precision / Positive Predicted Value

Prec_1 <- c_mat_1[2,2]/sum(c_mat_1[,2])
Prec_2 <- c_mat_2[2,2]/sum(c_mat_2[,2])
Prec_3 <- c_mat_3[2,2]/sum(c_mat_3[,2])
Prec_1
Prec_2
Prec_3

#Recall / True Positive Rate / Sensitivity

Rec_1 <- c_mat_1[2,2]/sum(c_mat_1[2,])
Rec_2 <- c_mat_2[2,2]/sum(c_mat_2[2,])
Rec_3 <- c_mat_3[2,2]/sum(c_mat_3[2,])
Rec_1
Rec_2
Rec_3

#F1 Score

F1_1 <- 2 * (Prec_1 * Rec_1)/(Prec_1 + Rec_1)
F1_2 <- 2 * (Prec_2 * Rec_2)/(Prec_2 + Rec_2)
F1_3 <- 2 * (Prec_3 * Rec_3)/(Prec_3 + Rec_3)
F1_1
F1_2
F1_3


# x indicates the linear combinations of the variables obtained by the model
# class indicates the two classes Existing and Attriting Customers.

ldahist(lda_1_predict$x[,1], g = lda_u_predict$class , col = 2)

#LDA Balanced

#Thresholds
Threshold1 <- 0.6
Threshold2 <- 0.7
Threshold3 <- 0.8


#We still try the LDA model
lda_2 <- lda(Attrition_Flag ~ Customer_Age + Is_Female + Dependent_count 
             + Education_Level + Marital_Status + Income_Category 
             + Total_Relationship_Count + Months_Inactive_12_mon 
             + Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal 
             + Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + Total_Trans_Ct 
             + Total_Ct_Chng_Q4_Q1  + Avg_Utilization_Ratio 
             + Customer_Age:Total_Amt_Chng_Q4_Q1 
             + Dependent_count:Total_Amt_Chng_Q4_Q1
             + Total_Amt_Chng_Q4_Q1:Total_Trans_Amt 
             + Credit_Limit:Total_Revolving_Bal 
             + Total_Revolving_Bal:Avg_Utilization_Ratio 
             + Credit_Limit:Avg_Utilization_Ratio 
             + Is_Female:Total_Ct_Chng_Q4_Q1 + Customer_Age:Marital_Status  , data = train_bal, family = "binomial")

lda_2



lda_2_predict <- predict(lda_2,test,type = "response")
lda_predict_2 <- lda_2_predict$posterior
pred_1 <- ifelse(lda_predict_2[,2] >= Threshold1 , 1,0)
pred_2 <- ifelse(lda_predict_2[,2] >= Threshold2 , 1,0)
pred_3 <- ifelse(lda_predict_2[,2] >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1 <- table(test$Attrition_Flag,pred_1)
c_mat_2 <- table(test$Attrition_Flag,pred_2)
c_mat_3 <- table(test$Attrition_Flag,pred_3)
c_mat_1
c_mat_2
c_mat_3

#Accuracy

mean(pred_1==test$Attrition_Flag)*100
mean(pred_2==test$Attrition_Flag)*100
mean(pred_3==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1 <- c_mat_1[1,1]/sum(c_mat_1[1,])
Spec_2 <- c_mat_2[1,1]/sum(c_mat_2[1,])
Spec_3 <- c_mat_3[1,1]/sum(c_mat_3[1,])
Spec_1
Spec_2
Spec_3


#Precision / Positive Predicted Value

Prec_1 <- c_mat_1[2,2]/sum(c_mat_1[,2])
Prec_2 <- c_mat_2[2,2]/sum(c_mat_2[,2])
Prec_3 <- c_mat_3[2,2]/sum(c_mat_3[,2])
Prec_1
Prec_2
Prec_3

#Recall / True Positive Rate / Sensitivity

Rec_1 <- c_mat_1[2,2]/sum(c_mat_1[2,])
Rec_2 <- c_mat_2[2,2]/sum(c_mat_2[2,])
Rec_3 <- c_mat_3[2,2]/sum(c_mat_3[2,])
Rec_1
Rec_2
Rec_3

#F1 Score

F1_1 <- 2 * (Prec_1 * Rec_1)/(Prec_1 + Rec_1)
F1_2 <- 2 * (Prec_2 * Rec_2)/(Prec_2 + Rec_2)
F1_3 <- 2 * (Prec_3 * Rec_3)/(Prec_3 + Rec_3)
F1_1
F1_2
F1_3


# x indicates the linear combinations of the variables obtained by the model
# class indicates the two classes Existing and Attriting Customers.

ldahist(lda_2_predict$x[,1], g = lda_b_predict$class , col = 2)


#ROC curves
roc_lda <- roc(test$Attrition_Flag ~ lda_predict_1[,2])
roc_lda_bal <- roc(test$Attrition_Flag ~ lda_predict_2[,2])

AUC_lda <- auc(roc_lda)
AUC_lda_bal <- auc(roc_lda_bal)


plot(roc_lda, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_lda_bal,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_lda <- coords(roc_lda,"best",best.method = "closest.topleft")$threshold
Best_pred_lda <- ifelse(lda_predict_1[,2] >= Best_Treshold_lda , 1,0)
Best_c_mat <- table(test$Attrition_Flag,Best_pred_lda)
Best_Spec_lda <- Best_c_mat[1,1]/sum(Best_c_mat[1,])
Best_Sens_lda <- Best_c_mat[2,2]/sum(Best_c_mat[2,])

Best_Treshold_lda_bal <- coords(roc_lda_bal,"best",best.method = "closest.topleft")$threshold
Best_pred_lda_bal <- ifelse(lda_predict_2[,2] >= Best_Treshold_lda_bal , 1,0)
Best_c_mat <- table(test$Attrition_Flag,Best_pred_lda_bal)
Best_Spec_lda_bal <- Best_c_mat[1,1]/sum(Best_c_mat[1,])
Best_Sens_lda_bal <- Best_c_mat[2,2]/sum(Best_c_mat[2,])

#Table to show them
Table_mat <-  matrix(c(Best_Treshold_lda,Best_Spec_lda,Best_Sens_lda,Best_Treshold_lda_bal,Best_Spec_lda_bal,Best_Sens_lda_bal), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Unbalanced","Balanced")
Tab <- as.table(Table_mat)
show(Tab)







#QDA


#Thresholds
Threshold1 <- 0.3
Threshold2 <- 0.4
Threshold3 <- 0.5

qda_1 <- qda(Attrition_Flag ~ Customer_Age + Is_Female + Dependent_count
             + Marital_Status + Income_Category + Total_Relationship_Count 
             + Months_Inactive_12_mon + Contacts_Count_12_mon + Credit_Limit 
             + Total_Revolving_Bal + Total_Amt_Chng_Q4_Q1 
             + Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 
             + Avg_Utilization_Ratio + Customer_Age:Total_Amt_Chng_Q4_Q1 
             + Dependent_count:Total_Amt_Chng_Q4_Q1
             + Total_Amt_Chng_Q4_Q1:Total_Trans_Amt 
             + Total_Amt_Chng_Q4_Q1:Total_Trans_Ct 
             + Credit_Limit:Total_Revolving_Bal 
             + Total_Revolving_Bal:Avg_Utilization_Ratio 
             + Credit_Limit:Avg_Utilization_Ratio 
             + Is_Female:Total_Ct_Chng_Q4_Q1 + Customer_Age:Marital_Status  , data = train, family = "binomial")

qda_1



qda_1_predict <- predict(qda_1,test,type = "response")
qda_predict_1 <- qda_1_predict$posterior
pred_1 <- ifelse(qda_predict_1[,2] >= Threshold1 , 1,0)
pred_2 <- ifelse(qda_predict_1[,2] >= Threshold2 , 1,0)
pred_3 <- ifelse(qda_predict_1[,2] >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1 <- table(test$Attrition_Flag,pred_1)
c_mat_2 <- table(test$Attrition_Flag,pred_2)
c_mat_3 <- table(test$Attrition_Flag,pred_3)
c_mat_1
c_mat_2
c_mat_3

#Accuracy

mean(pred_1==test$Attrition_Flag)*100
mean(pred_2==test$Attrition_Flag)*100
mean(pred_3==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1 <- c_mat_1[1,1]/sum(c_mat_1[1,])
Spec_2 <- c_mat_2[1,1]/sum(c_mat_2[1,])
Spec_3 <- c_mat_3[1,1]/sum(c_mat_3[1,])
Spec_1
Spec_2
Spec_3


#Precision / Positive Predicted Value

Prec_1 <- c_mat_1[2,2]/sum(c_mat_1[,2])
Prec_2 <- c_mat_2[2,2]/sum(c_mat_2[,2])
Prec_3 <- c_mat_3[2,2]/sum(c_mat_3[,2])
Prec_1
Prec_2
Prec_3

#Recall / True Positive Rate / Sensitivity

Rec_1 <- c_mat_1[2,2]/sum(c_mat_1[2,])
Rec_2 <- c_mat_2[2,2]/sum(c_mat_2[2,])
Rec_3 <- c_mat_3[2,2]/sum(c_mat_3[2,])
Rec_1
Rec_2
Rec_3

#F1 Score

F1_1 <- 2 * (Prec_1 * Rec_1)/(Prec_1 + Rec_1)
F1_2 <- 2 * (Prec_2 * Rec_2)/(Prec_2 + Rec_2)
F1_3 <- 2 * (Prec_3 * Rec_3)/(Prec_3 + Rec_3)
F1_1
F1_2
F1_3











#QDA Balanced

#Thresholds
Threshold1 <- 0.7
Threshold2 <- 0.8
Threshold3 <- 0.9


#We still try the QDA model
qda_2 <- qda(Attrition_Flag ~ Customer_Age + Is_Female + Dependent_count 
             + Education_Level + Marital_Status + Income_Category 
             + Total_Relationship_Count + Months_Inactive_12_mon 
             + Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal 
             + Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + Total_Trans_Ct 
             + Total_Ct_Chng_Q4_Q1  + Avg_Utilization_Ratio 
             + Customer_Age:Total_Amt_Chng_Q4_Q1 
             + Dependent_count:Total_Amt_Chng_Q4_Q1
             + Total_Amt_Chng_Q4_Q1:Total_Trans_Amt 
             + Credit_Limit:Total_Revolving_Bal 
             + Total_Revolving_Bal:Avg_Utilization_Ratio 
             + Credit_Limit:Avg_Utilization_Ratio 
             + Is_Female:Total_Ct_Chng_Q4_Q1 + Customer_Age:Marital_Status, data = train_bal, family = "binomial")

qda_2



qda_2_predict <- predict(qda_2,test,type = "response")
qda_predict_2 <- qda_2_predict$posterior
pred_1 <- ifelse(qda_predict_2[,2] >= Threshold1 , 1,0)
pred_2 <- ifelse(qda_predict_2[,2] >= Threshold2 , 1,0)
pred_3 <- ifelse(qda_predict_2[,2] >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1 <- table(test$Attrition_Flag,pred_1)
c_mat_2 <- table(test$Attrition_Flag,pred_2)
c_mat_3 <- table(test$Attrition_Flag,pred_3)
c_mat_1
c_mat_2
c_mat_3

#Accuracy

mean(pred_1==test$Attrition_Flag)*100
mean(pred_2==test$Attrition_Flag)*100
mean(pred_3==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1 <- c_mat_1[1,1]/sum(c_mat_1[1,])
Spec_2 <- c_mat_2[1,1]/sum(c_mat_2[1,])
Spec_3 <- c_mat_3[1,1]/sum(c_mat_3[1,])
Spec_1
Spec_2
Spec_3


#Precision / Positive Predicted Value

Prec_1 <- c_mat_1[2,2]/sum(c_mat_1[,2])
Prec_2 <- c_mat_2[2,2]/sum(c_mat_2[,2])
Prec_3 <- c_mat_3[2,2]/sum(c_mat_3[,2])
Prec_1
Prec_2
Prec_3

#Recall / True Positive Rate / Sensitivity

Rec_1 <- c_mat_1[2,2]/sum(c_mat_1[2,])
Rec_2 <- c_mat_2[2,2]/sum(c_mat_2[2,])
Rec_3 <- c_mat_3[2,2]/sum(c_mat_3[2,])
Rec_1
Rec_2
Rec_3

#F1 Score

F1_1 <- 2 * (Prec_1 * Rec_1)/(Prec_1 + Rec_1)
F1_2 <- 2 * (Prec_2 * Rec_2)/(Prec_2 + Rec_2)
F1_3 <- 2 * (Prec_3 * Rec_3)/(Prec_3 + Rec_3)
F1_1
F1_2
F1_3




#ROC curves
roc_qda <- roc(test$Attrition_Flag ~ qda_predict_1[,2])
roc_qda_bal <- roc(test$Attrition_Flag ~ qda_predict_2[,2])

AUC_qda <- auc(roc_qda)
AUC_qda_bal <- auc(roc_qda_bal)


plot(roc_qda, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_qda_bal,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_qda <- coords(roc_qda,"best",best.method = "closest.topleft")$threshold
Best_pred_qda <- ifelse(qda_predict_1[,2] >= Best_Treshold_qda , 1,0)
Best_c_mat <- table(test$Attrition_Flag,Best_pred_qda)
Best_Spec_qda <- Best_c_mat[1,1]/sum(Best_c_mat[1,])
Best_Sens_qda <- Best_c_mat[2,2]/sum(Best_c_mat[2,])

Best_Treshold_qda_bal <- coords(roc_qda_bal,"best",best.method = "closest.topleft")$threshold
Best_pred_qda_bal <- ifelse(qda_predict_2[,2] >= Best_Treshold_qda_bal , 1,0)
Best_c_mat <- table(test$Attrition_Flag,Best_pred_qda_bal)
Best_Spec_qda_bal <- Best_c_mat[1,1]/sum(Best_c_mat[1,])
Best_Sens_qda_bal <- Best_c_mat[2,2]/sum(Best_c_mat[2,])

#Table to show them
Table_mat <-  matrix(c(Best_Treshold_qda,Best_Spec_qda,Best_Sens_qda,Best_Treshold_qda_bal,Best_Spec_qda_bal,Best_Sens_qda_bal), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Unbalanced","Balanced")
Tab <- as.table(Table_mat)
show(Tab)















#Ridge


#Creation of train and test set with interactions
train_int <- data.frame(train)

train_int$Total_Amt_Chng_Q4_Q1_Customer_Age <- 
  train$Total_Amt_Chng_Q4_Q1 * train$Customer_Age
train_int$Total_Amt_Chng_Q4_Q1_Dependent_count <- 
  train$Total_Amt_Chng_Q4_Q1 * train$Dependent_count 
train_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Amt <- 
  train$Total_Amt_Chng_Q4_Q1*train$Total_Trans_Amt
train_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Ct <- 
  train$Total_Amt_Chng_Q4_Q1 * train$Total_Trans_Ct 
train_int$Credit_Limit_Total_Revolving_Bal <- 
  train$Credit_Limit * train$Total_Revolving_Bal
train_int$Total_Revolving_Bal_Avg_Utilization_Ratio <- 
  train$Total_Revolving_Bal * train$Avg_Utilization_Ratio
train_int$Credit_Limit_Avg_Utilization_Ratio <- 
  train$Credit_Limit * train$Avg_Utilization_Ratio
train_int$Is_Female_Total_Ct_Chng_Q4_Q1 <- 
  train$Is_Female * train$Total_Ct_Chng_Q4_Q1
train_int$Customer_Age_Marital_Status <- 
  train$Customer_Age * train$Marital_Status

test_int <- data.frame(test)

test_int$Total_Amt_Chng_Q4_Q1_Customer_Age <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Customer_Age
test_int$Total_Amt_Chng_Q4_Q1_Dependent_count <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Dependent_count 
test_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Amt <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Total_Trans_Amt
test_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Ct <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Total_Trans_Ct 
test_int$Credit_Limit_Total_Revolving_Bal <- 
  test$Credit_Limit * test$Total_Revolving_Bal
test_int$Total_Revolving_Bal_Avg_Utilization_Ratio <- 
  test$Total_Revolving_Bal * test$Avg_Utilization_Ratio
test_int$Credit_Limit_Avg_Utilization_Ratio <- 
  test$Credit_Limit * test$Avg_Utilization_Ratio
test_int$Is_Female_Total_Ct_Chng_Q4_Q1 <- 
  test$Is_Female * test$Total_Ct_Chng_Q4_Q1
test_int$Customer_Age_Marital_Status <- 
  test$Customer_Age * test$Marital_Status


#We transorm them into matrix withou Attrition_Flag, Education_Level , Months_on_book and Avg_Open_To_Buy

train_mat <- data.matrix(train_int[,-c(1,5,8,14)])
test_mat <- data.matrix(test_int[,-c(1,5,8,14)])



ridge <- cv.glmnet(train_mat,train$Attrition_Flag, alpha = 0, family = "binomial", type.measure = "class")

plot(ridge)


opt_lambda_ridge <- ridge$lambda.min
opt_lambda_ridge

ridge_predict <- predict(ridge,test_mat,type = "class", s = opt_lambda_ridge)

#Confusion matrix

c_mat_ridge <- table(test$Attrition_Flag,ridge_predict)
c_mat_ridge

#Accuracy

mean(ridge_predict==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_ridge <- c_mat_ridge[1,1]/sum(c_mat_ridge[1,])
Spec_ridge


#Precision / Positive Predicted Value

Prec_ridge <- c_mat_ridge[2,2]/sum(c_mat_ridge[,2])
Prec_ridge

#Recall / True Positive Rate / Sensitivity

Rec_ridge <- c_mat_ridge[2,2]/sum(c_mat_ridge[2,])
Rec_ridge

#F1 Score

F1_Ridge <- 2 * (Prec_ridge * Rec_ridge)/(Prec_ridge + Rec_ridge)
F1_Ridge



#Thresholds
Threshold1 <- 0.3
Threshold2 <- 0.4
Threshold3 <- 0.5

ridge_predict_2 <- predict(ridge,test_mat,type = "response", s = opt_lambda_ridge)

pred_1 <- ifelse(ridge_predict_2 >= Threshold1 , 1,0)
pred_2 <- ifelse(ridge_predict_2 >= Threshold2 , 1,0)
pred_3 <- ifelse(ridge_predict_2 >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1 <- table(test$Attrition_Flag,pred_1)
c_mat_2 <- table(test$Attrition_Flag,pred_2)
c_mat_3 <- table(test$Attrition_Flag,pred_3)
c_mat_1
c_mat_2
c_mat_3

#Accuracy

mean(pred_1==test$Attrition_Flag)*100
mean(pred_2==test$Attrition_Flag)*100
mean(pred_3==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1 <- c_mat_1[1,1]/sum(c_mat_1[1,])
Spec_2 <- c_mat_2[1,1]/sum(c_mat_2[1,])
Spec_3 <- c_mat_3[1,1]/sum(c_mat_3[1,])
Spec_1
Spec_2
Spec_3


#Precision / Positive Predicted Value

Prec_1 <- c_mat_1[2,2]/sum(c_mat_1[,2])
Prec_2 <- c_mat_2[2,2]/sum(c_mat_2[,2])
Prec_3 <- c_mat_3[2,2]/sum(c_mat_3[,2])
Prec_1
Prec_2
Prec_3

#Recall / True Positive Rate / Sensitivity

Rec_1 <- c_mat_1[2,2]/sum(c_mat_1[2,])
Rec_2 <- c_mat_2[2,2]/sum(c_mat_2[2,])
Rec_3 <- c_mat_3[2,2]/sum(c_mat_3[2,])
Rec_1
Rec_2
Rec_3

#F1 Score

F1_1 <- 2 * (Prec_1 * Rec_1)/(Prec_1 + Rec_1)
F1_2 <- 2 * (Prec_2 * Rec_2)/(Prec_2 + Rec_2)
F1_3 <- 2 * (Prec_3 * Rec_3)/(Prec_3 + Rec_3)
F1_1
F1_2
F1_3









#Ridge balanced




#Creation of train_bal and test set with interactions
train_bal_int <- data.frame(train_bal)

train_bal_int$Total_Amt_Chng_Q4_Q1_Customer_Age <- 
  train_bal$Total_Amt_Chng_Q4_Q1 * train_bal$Customer_Age
train_bal_int$Total_Amt_Chng_Q4_Q1_Dependent_count <- 
  train_bal$Total_Amt_Chng_Q4_Q1 * train_bal$Dependent_count 
train_bal_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Amt <- 
  train_bal$Total_Amt_Chng_Q4_Q1 * train_bal$Total_Trans_Amt 
train_bal_int$Credit_Limit_Total_Revolving_Bal <- 
  train_bal$Credit_Limit * train_bal$Total_Revolving_Bal
train_bal_int$Total_Revolving_Bal_Avg_Utilization_Ratio <- 
  train_bal$Total_Revolving_Bal * train_bal$Avg_Utilization_Ratio
train_bal_int$Credit_Limit_Avg_Utilization_Ratio <- 
  train_bal$Credit_Limit * train_bal$Avg_Utilization_Ratio
train_bal_int$Is_Female_Total_Trans_Ct_Q4_Q1 <- 
  train_bal$Is_Female * train_bal$Total_Ct_Chng_Q4_Q1
train_bal_int$Customer_Age_Marital_Status <- 
  train_bal$Customer_Age * train_bal$Marital_Status

test_bal_int <- data.frame(test)

test_bal_int$Total_Amt_Chng_Q4_Q1_Customer_Age <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Customer_Age
test_bal_int$Total_Amt_Chng_Q4_Q1_Dependent_count <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Dependent_count 
test_bal_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Amt <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Total_Trans_Amt 
test_bal_int$Credit_Limit_Total_Revolving_Bal <- 
  test$Credit_Limit * test$Total_Revolving_Bal
test_bal_int$Total_Revolving_Bal_Avg_Utilization_Ratio <- 
  test$Total_Revolving_Bal * test$Avg_Utilization_Ratio
test_bal_int$Credit_Limit_Avg_Utilization_Ratio <- 
  test$Credit_Limit * test$Avg_Utilization_Ratio
test_bal_int$Is_Female_Total_Trans_Ct_Q4_Q1 <- 
  test$Is_Female * test$Total_Ct_Chng_Q4_Q1
test_bal_int$Customer_Age_Marital_Status <- 
  test$Customer_Age * test$Marital_Status

#We transorm them into matrix withou Attrition_Flag, Months_on_book and Credit_Limit

train_bal_mat <- data.matrix(train_bal_int[,-c(1,8,12)])
test_bal_mat <- data.matrix(test_bal_int[,-c(1,8,12)])



ridge_bal <- cv.glmnet(train_bal_mat,train_bal$Attrition_Flag, alpha = 0, family = "binomial", type.measure = "class")

plot(ridge_bal)


opt_lambda_ridge_bal <- ridge_bal$lambda.min
opt_lambda_ridge_bal

ridge_bal_predict <- predict(ridge_bal,test_bal_mat,type = "class", s = opt_lambda_ridge_bal)

#Confusion matrix

c_mat_ridge_bal <- table(test$Attrition_Flag,ridge_bal_predict)
c_mat_ridge_bal

#Accuracy

mean(ridge_bal_predict==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_ridge_bal <- c_mat_ridge_bal[1,1]/sum(c_mat_ridge_bal[1,])
Spec_ridge_bal


#Precision / Positive Predicted Value

Prec_ridge_bal <- c_mat_ridge_bal[2,2]/sum(c_mat_ridge_bal[,2])
Prec_ridge_bal

#Recall / True Positive Rate / Sensitivity

Rec_ridge_bal <- c_mat_ridge_bal[2,2]/sum(c_mat_ridge_bal[2,])
Rec_ridge_bal

#F1 Score

F1_Ridge_bal <- 2 * (Prec_ridge_bal * Rec_ridge_bal)/(Prec_ridge_bal + Rec_ridge_bal)
F1_Ridge_bal



#Thresholds
Threshold1 <- 0.6
Threshold2 <- 0.7
Threshold3 <- 0.8

ridge_bal_predict_2 <- predict(ridge_bal,test_bal_mat,type = "response", s = opt_lambda_ridge_bal)

pred_1 <- ifelse(ridge_bal_predict_2 >= Threshold1 , 1,0)
pred_2 <- ifelse(ridge_bal_predict_2 >= Threshold2 , 1,0)
pred_3 <- ifelse(ridge_bal_predict_2 >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1 <- table(test$Attrition_Flag,pred_1)
c_mat_2 <- table(test$Attrition_Flag,pred_2)
c_mat_3 <- table(test$Attrition_Flag,pred_3)
c_mat_1
c_mat_2
c_mat_3

#Accuracy

mean(pred_1==test$Attrition_Flag)*100
mean(pred_2==test$Attrition_Flag)*100
mean(pred_3==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1 <- c_mat_1[1,1]/sum(c_mat_1[1,])
Spec_2 <- c_mat_2[1,1]/sum(c_mat_2[1,])
Spec_3 <- c_mat_3[1,1]/sum(c_mat_3[1,])
Spec_1
Spec_2
Spec_3


#Precision / Positive Predicted Value

Prec_1 <- c_mat_1[2,2]/sum(c_mat_1[,2])
Prec_2 <- c_mat_2[2,2]/sum(c_mat_2[,2])
Prec_3 <- c_mat_3[2,2]/sum(c_mat_3[,2])
Prec_1
Prec_2
Prec_3

#Recall / True Positive Rate / Sensitivity

Rec_1 <- c_mat_1[2,2]/sum(c_mat_1[2,])
Rec_2 <- c_mat_2[2,2]/sum(c_mat_2[2,])
Rec_3 <- c_mat_3[2,2]/sum(c_mat_3[2,])
Rec_1
Rec_2
Rec_3

#F1 Score

F1_1 <- 2 * (Prec_1 * Rec_1)/(Prec_1 + Rec_1)
F1_2 <- 2 * (Prec_2 * Rec_2)/(Prec_2 + Rec_2)
F1_3 <- 2 * (Prec_3 * Rec_3)/(Prec_3 + Rec_3)
F1_1
F1_2
F1_3

roc_ridge <- roc(test$Attrition_Flag ~ as.numeric(ridge_predict_2))
roc_ridge_bal <- roc(test$Attrition_Flag ~ as.numeric(ridge_bal_predict_2))

AUC_ridge <- auc(roc_ridge)
AUC_ridge_bal <- auc(roc_ridge_bal)

plot(roc_ridge, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_ridge_bal,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_ridge <- coords(roc_ridge,"best",best.method = "closest.topleft")$threshold
Best_pred_ridge <- ifelse(as.numeric(ridge_predict_2) >= Best_Treshold_ridge , 1,0)
Best_c_mat <- table(test$Attrition_Flag,Best_pred_ridge)
Best_Spec_ridge <- Best_c_mat[1,1]/sum(Best_c_mat[1,])
Best_Sens_ridge <- Best_c_mat[2,2]/sum(Best_c_mat[2,])

Best_Treshold_ridge_bal <- coords(roc_ridge_bal,"best",best.method = "closest.topleft")$threshold
Best_pred_ridge_bal <- ifelse(as.numeric(ridge_bal_predict_2) >= Best_Treshold_ridge_bal , 1,0)
Best_c_mat <- table(test$Attrition_Flag,Best_pred_ridge_bal)
Best_Spec_ridge_bal <- Best_c_mat[1,1]/sum(Best_c_mat[1,])
Best_Sens_ridge_bal <- Best_c_mat[2,2]/sum(Best_c_mat[2,])

#Table to show them
Table_mat <-  matrix(c(Best_Treshold_ridge,Best_Spec_ridge,Best_Sens_ridge,Best_Treshold_ridge_bal,Best_Spec_ridge_bal,Best_Sens_ridge_bal), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Unbalanced","Balanced")
Tab <- as.table(Table_mat)
show(Tab)











#Lasso



#Creation of train and test set with interactions
train_int <- data.frame(train)

train_int$Total_Amt_Chng_Q4_Q1_Customer_Age <- 
  train$Total_Amt_Chng_Q4_Q1 * train$Customer_Age
train_int$Total_Amt_Chng_Q4_Q1_Dependent_count <- 
  train$Total_Amt_Chng_Q4_Q1 * train$Dependent_count 
train_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Amt <- 
  train$Total_Amt_Chng_Q4_Q1*train$Total_Trans_Amt
train_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Ct <- 
  train$Total_Amt_Chng_Q4_Q1 * train$Total_Trans_Ct 
train_int$Credit_Limit_Total_Revolving_Bal <- 
  train$Credit_Limit * train$Total_Revolving_Bal
train_int$Total_Revolving_Bal_Avg_Utilization_Ratio <- 
  train$Total_Revolving_Bal * train$Avg_Utilization_Ratio
train_int$Credit_Limit_Avg_Utilization_Ratio <- 
  train$Credit_Limit * train$Avg_Utilization_Ratio
train_int$Is_Female_Total_Ct_Chng_Q4_Q1 <- 
  train$Is_Female * train$Total_Ct_Chng_Q4_Q1
train_int$Customer_Age_Marital_Status <- 
  train$Customer_Age * train$Marital_Status

test_int <- data.frame(test)

test_int$Total_Amt_Chng_Q4_Q1_Customer_Age <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Customer_Age
test_int$Total_Amt_Chng_Q4_Q1_Dependent_count <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Dependent_count 
test_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Amt <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Total_Trans_Amt
test_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Ct <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Total_Trans_Ct 
test_int$Credit_Limit_Total_Revolving_Bal <- 
  test$Credit_Limit * test$Total_Revolving_Bal
test_int$Total_Revolving_Bal_Avg_Utilization_Ratio <- 
  test$Total_Revolving_Bal * test$Avg_Utilization_Ratio
test_int$Credit_Limit_Avg_Utilization_Ratio <- 
  test$Credit_Limit * test$Avg_Utilization_Ratio
test_int$Is_Female_Total_Ct_Chng_Q4_Q1 <- 
  test$Is_Female * test$Total_Ct_Chng_Q4_Q1
test_int$Customer_Age_Marital_Status <- 
  test$Customer_Age * test$Marital_Status


#We transorm them into matrix withou Attrition_Flag, Education_Level , Months_on_book and Avg_Open_To_Buy

train_mat <- data.matrix(train_int[,-c(1)])
test_mat <- data.matrix(test_int[,-c(1)])



lasso <- cv.glmnet(train_mat,train$Attrition_Flag, alpha = 1, family = "binomial", type.measure = "class")

plot(lasso)


opt_lambda_lasso <- lasso$lambda.min
opt_lambda_lasso

lasso_predict <- predict(lasso,test_mat,type = "class", s = opt_lambda_lasso)

#Confusion matrix

c_mat_lasso <- table(test$Attrition_Flag,lasso_predict)
c_mat_lasso

#Accuracy

mean(lasso_predict==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_lasso <- c_mat_lasso[1,1]/sum(c_mat_lasso[1,])
Spec_lasso


#Precision / Positive Predicted Value

Prec_lasso <- c_mat_lasso[2,2]/sum(c_mat_lasso[,2])
Prec_lasso

#Recall / True Positive Rate / Sensitivity

Rec_lasso <- c_mat_lasso[2,2]/sum(c_mat_lasso[2,])
Rec_lasso

#F1 Score

F1_lasso <- 2 * (Prec_lasso * Rec_lasso)/(Prec_lasso + Rec_lasso)
F1_lasso



#Thresholds
Threshold1 <- 0.3
Threshold2 <- 0.4
Threshold3 <- 0.5

lasso_predict_2 <- predict(lasso,test_mat,type = "response", s = opt_lambda_lasso)

pred_1 <- ifelse(lasso_predict_2 >= Threshold1 , 1,0)
pred_2 <- ifelse(lasso_predict_2 >= Threshold2 , 1,0)
pred_3 <- ifelse(lasso_predict_2 >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1 <- table(test$Attrition_Flag,pred_1)
c_mat_2 <- table(test$Attrition_Flag,pred_2)
c_mat_3 <- table(test$Attrition_Flag,pred_3)
c_mat_1
c_mat_2
c_mat_3

#Accuracy

mean(pred_1==test$Attrition_Flag)*100
mean(pred_2==test$Attrition_Flag)*100
mean(pred_3==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1 <- c_mat_1[1,1]/sum(c_mat_1[1,])
Spec_2 <- c_mat_2[1,1]/sum(c_mat_2[1,])
Spec_3 <- c_mat_3[1,1]/sum(c_mat_3[1,])
Spec_1
Spec_2
Spec_3


#Precision / Positive Predicted Value

Prec_1 <- c_mat_1[2,2]/sum(c_mat_1[,2])
Prec_2 <- c_mat_2[2,2]/sum(c_mat_2[,2])
Prec_3 <- c_mat_3[2,2]/sum(c_mat_3[,2])
Prec_1
Prec_2
Prec_3

#Recall / True Positive Rate / Sensitivity

Rec_1 <- c_mat_1[2,2]/sum(c_mat_1[2,])
Rec_2 <- c_mat_2[2,2]/sum(c_mat_2[2,])
Rec_3 <- c_mat_3[2,2]/sum(c_mat_3[2,])
Rec_1
Rec_2
Rec_3

#F1 Score

F1_1 <- 2 * (Prec_1 * Rec_1)/(Prec_1 + Rec_1)
F1_2 <- 2 * (Prec_2 * Rec_2)/(Prec_2 + Rec_2)
F1_3 <- 2 * (Prec_3 * Rec_3)/(Prec_3 + Rec_3)
F1_1
F1_2
F1_3









#Lasso Balanced






#Creation of train_bal and test set with interactions
train_bal_int <- data.frame(train_bal)

train_bal_int$Total_Amt_Chng_Q4_Q1_Customer_Age <- 
  train_bal$Total_Amt_Chng_Q4_Q1 * train_bal$Customer_Age
train_bal_int$Total_Amt_Chng_Q4_Q1_Dependent_count <- 
  train_bal$Total_Amt_Chng_Q4_Q1 * train_bal$Dependent_count 
train_bal_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Amt <- 
  train_bal$Total_Amt_Chng_Q4_Q1 * train_bal$Total_Trans_Amt 
train_bal_int$Credit_Limit_Total_Revolving_Bal <- 
  train_bal$Credit_Limit * train_bal$Total_Revolving_Bal
train_bal_int$Total_Revolving_Bal_Avg_Utilization_Ratio <- 
  train_bal$Total_Revolving_Bal * train_bal$Avg_Utilization_Ratio
train_bal_int$Credit_Limit_Avg_Utilization_Ratio <- 
  train_bal$Credit_Limit * train_bal$Avg_Utilization_Ratio
train_bal_int$Is_Female_Total_Trans_Ct_Q4_Q1 <- 
  train_bal$Is_Female * train_bal$Total_Ct_Chng_Q4_Q1
train_bal_int$Customer_Age_Marital_Status <- 
  train_bal$Customer_Age * train_bal$Marital_Status

test_bal_int <- data.frame(test)

test_bal_int$Total_Amt_Chng_Q4_Q1_Customer_Age <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Customer_Age
test_bal_int$Total_Amt_Chng_Q4_Q1_Dependent_count <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Dependent_count 
test_bal_int$Total_Amt_Chng_Q4_Q1_Total_Trans_Amt <- 
  test$Total_Amt_Chng_Q4_Q1 * test$Total_Trans_Amt 
test_bal_int$Credit_Limit_Total_Revolving_Bal <- 
  test$Credit_Limit * test$Total_Revolving_Bal
test_bal_int$Total_Revolving_Bal_Avg_Utilization_Ratio <- 
  test$Total_Revolving_Bal * test$Avg_Utilization_Ratio
test_bal_int$Credit_Limit_Avg_Utilization_Ratio <- 
  test$Credit_Limit * test$Avg_Utilization_Ratio
test_bal_int$Is_Female_Total_Trans_Ct_Q4_Q1 <- 
  test$Is_Female * test$Total_Ct_Chng_Q4_Q1
test_bal_int$Customer_Age_Marital_Status <- 
  test$Customer_Age * test$Marital_Status


#We transorm them into matrix withou Attrition_Flag, Months_on_book and Credit_Limit

train_bal_mat <- data.matrix(train_bal_int[,-c(1)])
test_bal_mat <- data.matrix(test_bal_int[,-c(1)])



lasso_bal <- cv.glmnet(train_bal_mat,train_bal$Attrition_Flag, alpha = 1, family = "binomial", type.measure = "class")

plot(lasso_bal)


opt_lambda_lasso_bal <- lasso_bal$lambda.min
opt_lambda_lasso_bal

lasso_bal_predict <- predict(lasso_bal,test_bal_mat,type = "class", s = opt_lambda_lasso_bal)

#Confusion matrix

c_mat_lasso_bal <- table(test$Attrition_Flag,lasso_bal_predict)
c_mat_lasso_bal

#Accuracy

mean(lasso_bal_predict==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_lasso_bal <- c_mat_lasso_bal[1,1]/sum(c_mat_lasso_bal[1,])
Spec_lasso_bal


#Precision / Positive Predicted Value

Prec_lasso_bal <- c_mat_lasso_bal[2,2]/sum(c_mat_lasso_bal[,2])
Prec_lasso_bal

#Recall / True Positive Rate / Sensitivity

Rec_lasso_bal <- c_mat_lasso_bal[2,2]/sum(c_mat_lasso_bal[2,])
Rec_lasso_bal

#F1 Score

F1_lasso_bal <- 2 * (Prec_lasso_bal * Rec_lasso_bal)/(Prec_lasso_bal + Rec_lasso_bal)
F1_lasso_bal



#Thresholds
Threshold1 <- 0.6
Threshold2 <- 0.7
Threshold3 <- 0.8

lasso_bal_predict_2 <- predict(lasso_bal,test_bal_mat,type = "response", s = opt_lambda_lasso_bal)

pred_1 <- ifelse(lasso_bal_predict_2 >= Threshold1 , 1,0)
pred_2 <- ifelse(lasso_bal_predict_2 >= Threshold2 , 1,0)
pred_3 <- ifelse(lasso_bal_predict_2 >= Threshold3 , 1,0)

#Confusion matrix

c_mat_1 <- table(test$Attrition_Flag,pred_1)
c_mat_2 <- table(test$Attrition_Flag,pred_2)
c_mat_3 <- table(test$Attrition_Flag,pred_3)
c_mat_1
c_mat_2
c_mat_3

#Accuracy

mean(pred_1==test$Attrition_Flag)*100
mean(pred_2==test$Attrition_Flag)*100
mean(pred_3==test$Attrition_Flag)*100

#True Negative Rate / Specificity

Spec_1 <- c_mat_1[1,1]/sum(c_mat_1[1,])
Spec_2 <- c_mat_2[1,1]/sum(c_mat_2[1,])
Spec_3 <- c_mat_3[1,1]/sum(c_mat_3[1,])
Spec_1
Spec_2
Spec_3


#Precision / Positive Predicted Value

Prec_1 <- c_mat_1[2,2]/sum(c_mat_1[,2])
Prec_2 <- c_mat_2[2,2]/sum(c_mat_2[,2])
Prec_3 <- c_mat_3[2,2]/sum(c_mat_3[,2])
Prec_1
Prec_2
Prec_3

#Recall / True Positive Rate / Sensitivity

Rec_1 <- c_mat_1[2,2]/sum(c_mat_1[2,])
Rec_2 <- c_mat_2[2,2]/sum(c_mat_2[2,])
Rec_3 <- c_mat_3[2,2]/sum(c_mat_3[2,])
Rec_1
Rec_2
Rec_3

#F1 Score

F1_1 <- 2 * (Prec_1 * Rec_1)/(Prec_1 + Rec_1)
F1_2 <- 2 * (Prec_2 * Rec_2)/(Prec_2 + Rec_2)
F1_3 <- 2 * (Prec_3 * Rec_3)/(Prec_3 + Rec_3)
F1_1
F1_2
F1_3

roc_lasso <- roc(test$Attrition_Flag ~ as.numeric(lasso_predict_2))
roc_lasso_bal <- roc(test$Attrition_Flag ~ as.numeric(lasso_bal_predict_2))

AUC_lasso <- auc(roc_lasso)
AUC_lasso_bal <- auc(roc_lasso_bal)

plot(roc_lasso, col = "black",print.auc = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, lwd=2,print.auc.x = 0.5,print.auc.y = 0.5)
plot(roc_lasso_bal,add = TRUE,col = "blue", print.auc = TRUE, lwd=2, print.auc.x = 0.5,print.auc.y = 0.43)

#Best thresholds and Best Sensitivity and Specificity
Best_Treshold_lasso <- coords(roc_lasso,"best",best.method = "closest.topleft")$threshold
Best_pred_lasso <- ifelse(as.numeric(lasso_predict_2) >= Best_Treshold_ridge , 1,0)
Best_c_mat <- table(test$Attrition_Flag,Best_pred_lasso)
Best_Spec_lasso <- Best_c_mat[1,1]/sum(Best_c_mat[1,])
Best_Sens_lasso <- Best_c_mat[2,2]/sum(Best_c_mat[2,])

Best_Treshold_lasso_bal <- coords(roc_lasso_bal,"best",best.method = "closest.topleft")$threshold
Best_pred_lasso_bal <- ifelse(as.numeric(lasso_bal_predict_2) >= Best_Treshold_lasso_bal , 1,0)
Best_c_mat <- table(test$Attrition_Flag,Best_pred_lasso_bal)
Best_Spec_lasso_bal <- Best_c_mat[1,1]/sum(Best_c_mat[1,])
Best_Sens_lasso_bal <- Best_c_mat[2,2]/sum(Best_c_mat[2,])

#Table to show them
Table_mat <-  matrix(c(Best_Treshold_lasso,Best_Spec_lasso,Best_Sens_lasso,Best_Treshold_lasso_bal,Best_Spec_lasso_bal,Best_Sens_lasso_bal), ncol=3, byrow=TRUE)
colnames(Table_mat) <- c("Threshold","Specificity","Sensitivity")
rownames(Table_mat) <- c("Unbalanced","Balanced")
Tab <- as.table(Table_mat)
show(Tab)





