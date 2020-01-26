#get Working directory

setwd("D:/Edwisor assignments/")

getwd()



#  Load  Require Libraries



#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)

# Install  Libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")
library(tidyr)


#  Function to Converty  Data Types as  Factor  or numeric   based on given type

convert_factor_type= function(df,cat_names,convert_type) {
  
  if (convert_type== "factor")
    df[cat_names] <- lapply(df[cat_names], as.factor)
  else 
    df[cat_names] <- lapply(df[cat_names], as.numeric)
  df
}

# This Function will take input as data frame  and Numeric Columns and gives output as
# box plot relation ship between  Target Variable and  Independent numeric variable

plot_box = function(df, cols, col_x = 'target'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p = ggplot(df, aes_string(col_x, col)) + 
      geom_boxplot() +
      ggtitle(paste('Box plot of', col, '\n vs.', col_x))
    print(p)
  }
}


# This Function will take input as data frame  and Numeric Columns and gives output as
# Violin  plot relation ship between  Target Variable and  Independent numeric variable


plot_violin = function(df, cols, col_x = 'target'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p = ggplot(df, aes_string(col_x, col)) + 
      geom_violin() +
      ggtitle(paste('Box plot of', col, '\n vs.', col_x))
    print(p)
  }
}





# This  Function will  take data frame and categorical columns as input
# and give  group plots between  independenta and target variable

plot_group_bar <- function(data,cat_columns,col_y="target"){
  for(col in cat_columns) {
    
    plot=ggplot(data) + geom_bar(aes_string(x=col,fill=col_y),position = "dodge")
    
    print(plot)
  }
}

# This Function will take dataframe  and numeric columns as input and 
# it treat outliers  using  boxplot and  return dataframe  after treating
treat_outliers  <- function(data,numeric_columns) {
  
  for (col in numeric_columns) {
    val = data[,col][data[,col] %in% boxplot.stats(data[,col])$out]
    
    df_target_out = data[which(!data[,col] %in% val),]
    
  }  
  df_target_out
}

# this  function will take data and categorical variables and gives chisquare
# p values as  output
ttest  <- function(data,numerical,target="target") {
  
  for (col in num_col)
  {
    print(col)
    print(t.test(col,target))
  }
}



# this function  will take data frame and numeric data as input and give 
# dataframe as output after  convering  numeric variables values into standardization form
standardForm_convert  <- function(data,num_col) {
  
  for(col in num_col){
    print(col)
    data[,col] = (data[,col] - mean(data[,col]))/sd(data[,col])
  }
  data
}



# This Function will take Actual y value and Predicted  values and it will give
# Output as  Accuracy , Precision , Recall etc
model_evaluation <- function(test_y,predicted_y) {
  
  table_matrix= table(test_y,predicted_y)
  print (confusionMatrix(table_matrix))
  precision=  table_matrix[4]/(  table_matrix[4] +  table_matrix[3])
  print(paste("Precision  is--" ,precision))
  recall  = table_matrix[4]/(  table_matrix[4] +  table_matrix[2])
  print(paste("recall  is--" ,recall))
  Accuracy = sum(diag(table_matrix)) / sum(table_matrix)
  print(paste("Accuracy   is--" ,Accuracy))
  
  FNR  = table_matrix[2]/(  table_matrix[4] +  table_matrix[2])
  print(paste("FNR     is--" ,FNR))
}

#  This function will take  data frame and categorical  as iput and gives output as data frame with encoded categorical data
encode_categorical  <- function(data,cat_columns) {
  
  for(col in cat_columns ) {
    data[,col]=as.numeric(as.factor(data[,col]))
  }
  data
}

# this function  will take data frame and numeric data as input and give 
# dataframe as output after  convering  numeric variables values into standardization form


standardForm_convert  <- function(data,num_col) {
  
  for(col in num_col){
    print(col)
    data[,col] = (data[,col] - mean(data[,col]))/sd(data[,col])
  }
  data
}






#Load Customer target Train and test Data

df_train=read.csv("Train_data.csv")
df_test=read.csv("Test_data.csv")


# drop  Phone number column from data frame 

df_train= subset(df_train,select=-c(ID_no))
df_test= subset(df_test,select=-c(ID_no))

# understanding  data 

head(df_train)

# Summary Of Data

summary(df_train)

# this data set contains 200000 rows and 202 columns  out of this all columns are categorical columns  
#columns are  Numeric

# It is showing  that  data is closely distributed  min max and standard distribution is looking quick close
#might be less chance of  getting outliers 



# Univariate  Analysis




#analyse   Target Variable
table(df_train$target)

barplot(table(df_train$target), main ="target column Distribution")

#  #Might be chance of facing data imbalance have to be careful during modelling stage whike measuring the accuracy

############################# Bivariate Analysis

# Bivariate  Analysis  between Numerical Variable and target  Variable  using Boxplot
plot_box(df_train,nums_column)

######################

#Bivariate  Analysis  between Numerical Variable and target  Variable  using Violin plot

plot_violin(df_train,nums_column)


#This below plots clearly showing that there are few outliers in the variables 
# but which are very near to the lower/Upper extreme boundries
#This is also one the sign data is closely distributed

#For other features  Boxplot  Median , IQeeriR, Ranges are  looking  almost same. Here it is stating  Feature Engineering is important to find the relationship 
#between  the variables.


################ Missing  Values  ###############

#As  summary Function shows there is not missing  value present in the data

###########  Outlier  Analysis  ###################

#As wecame to know that ring  Univariate  there are utliers in few columns
#so will  treat those outliers and will chick analyse  what is the impact 


# Create  one dummy   Data frame and copy train data frame df_target_T
df_target_T= treat_outliers(df_target_T,nums_column)
# check the  dimensions of data frame
dim(df_target_T)   # it contains 200000 rows and 191 columns


# Asp per the  IQR Range there is no data points has been removed from the data
#No need to remove any data eventhough boxplots is showingfew outliers

##################### Feature Selection ###########################

# verify correleation between   Numeric variable

corrgram(df_train[,nums_column], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#This plot is hsowing almost blue that is there is no relation between the independent variables 
#So its is clearly satisfying the conditions of regression that there should not be any relation between 
#Independent Variables


nums_column

df_train = subset(df_train,select=-c(total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))
df_test = subset(df_test,select=-c(total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))

##  Verify   variable importance of  categorical variable using chi square test

# analyse ttest  test p values for all independent categorical variable

ttest(df_train,cat_ind_columns)


# Important Variables are 181
# Non Important Variables are 19


dim(df_train)
dim(df_test)

#  one more step in  one variable  account length which is  seems like categorical , might be account length are small are 
# old accounts and target rate may be more
# will turn account numbers into ranges and make it as categorical columns


#############################################  Scaling Data ##########################################################
# As we see that almost all the  numeric variables are in  normalal distribution except  two variables
# since our data is also contains  few Outliers  we are better to go standardization for scaling

str(df_train)

#  get numeric columns  from data frame

nums_column_1 <-names(df_train)[sapply(df_train, is.numeric)]

# this function  will take data frame and numeric data as input and give 
# dataframe as output after  convering  numeric variables values into standardization form

df_train = standardForm_convert(df_train,nums_column_1)
df_test=standardForm_convert(df_test,nums_column_1)
View(df_train)



## Preapare  train and test data 

x_train =  subset(df_train,select=-c(target))
y_train = subset(df_train,select=c(target))
x_test = subset(df_test,select = -c(target))
y_test = subset(df_test,select=-c(target))

dim(x_train)
dim(x_test)
dim(y_train)
dim(y_test)

#######################################  Model   logistic  Regression  ###################################


model <- glm (target ~ , data = df_train, family = binomial)
summary(model)


predict_lr <-predict(model, df_test, type = 'class')

############# Model Evaluation  #

model_evaluation(df_test$target, predict_prune)


#"Precision  is-- 0.296"
# "recall  is-- 0.65625"
# "Accuracy   is-- 0.91011037792441"
# "FNR     is-- 0.34375"

############ Build Randam forest Model ###################


random_rf=randomForest(target ~., data =df_train ,ntree=500 ,nodesize =10 ,importance =TRUE)

random_rf

predict_rf <-  predict(random_rf, x_test)
##### Evaluation  Random FOrest


model_evaluation(df_test$target, predict_rf)

# Performance of this model is  good  when compare  to decision  Tree

# Here Precision = 0.13
#  recall is  0.72 
#and  F1 score is 0.56




