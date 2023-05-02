bank_data <- read.csv('C:/Users/Bertug/Desktop/BankChurners.csv')

head(bank_data)
dim(bank_data) #10127 rows and 23 columns

col = list('CLIENTNUM', 'Attrition_Flag', 'Customer_Age',	'Gender',	'Dependent_count', 'Education_Level',	'Marital_Status',	'Income_Category', 'Card_Category',	'Months_on_book',	'Total_Relationship_Count', 'Months_Inactive_12_mon',	'Contacts_Count_12_mon', 'Credit_Limit', 'Total_Revolving_Bal',	'Avg_Open_To_Buy', 'Total_Amt_Chng_Q4_Q1', 'Total_Trans_Amt', 'Total_Trans_Ct', 'Total_Ct_Chng_Q4_Q1',	'Avg_Utilization_Ratio',	'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1',	'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2')

splitmix<-function(data){
  data<-data.frame(data,check.names=T)
  class.col <- unlist(lapply(data,class))
  col.quant <- which(class.col %in% c("numeric","integer"))
  col.qual <- which(class.col %in% c("factor","character"))
  if ("integer" %in% class.col) 
    warning("Columns of class integer are considered as quantitative")
  X.quanti <- X.quali <- NULL
  if (length(col.quant)!=0) X.quanti<-data[,col.quant,drop=FALSE]
  if (length(col.qual)!=0) X.quali<-data[,col.qual,drop=FALSE]
  
  return(list(X.quanti=X.quanti,X.quali=X.quali,col.quant=col.quant,col.qual=col.qual))
}

data.split <- splitmix(bank_data)
quantitative <- data.split$X.quanti
qualitative <- data.split$X.quali

length(quantitative)
length(qualitative)
