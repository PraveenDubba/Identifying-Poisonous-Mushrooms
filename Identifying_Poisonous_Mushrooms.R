#############################################
# Title:  Identifying Poisonous Mushrooms   # 
# Author: Praveen Dubba                     #
# Date:   12/22/2017                        #
#############################################



#############################################
#               Index                       #
#                                           #
#  A. Problem Statement                     #
#  B. Overview of the Dataset               #
#  C. Work environment setup                #
#  D. Data Preprocessing                    #
#  E. Data Visualization                    #
#  F. Model Building                        #
#  G. Model Selection                       #
#  H. Interpretation of the selected model  #
#  I. Additional comments                   #
#                                           #
#############################################

#####
# A #
##########################################################################################
#                                                                                        #
# Problem statement:                                                                     #
#                                                                                        #
# To build a short model to predict if a mushroom is Poisonous or Edible.                #                                                                    #
#                                                                                        #
##########################################################################################


#####
# B #
##########################################################################################
#                                                                                        #
# Overview of the dataset:                                                               #
#                                                                                        #
# This dataset contains 8124 observations and 23 variables. All the variables are        #
# categorical. The dependent variable is dichotomous and its values are p (Poisonous)    #
# and e (Edible). There is no class imbalance in the dataset. Proportion of p & e        #
# observations are 48.2% and 51.8% respectively.                                         #
#                                                                                        #
##########################################################################################



#####
# C #
##########################################################################################
#                                                                                        #
# Work environment setup:                                                                #
#                                                                                        #
# 1. Cleaning or removing all the existing environment variables                         #
# 2. Setting up the working directory to load/save the data                              #
# 3. Installing and loading the required packages                                        #
# 4. Importing the dataset to initiate Data Preprocessing phase                          #
#                                                                                        #
##########################################################################################


# Removing all R environment variables

##############################################
# Warning:                                   #
# Executing the below code wipes out all the #
# environment (Global Environment) variables #
# that are listed in the Environment pane.   #
##############################################
rm(list = ls())


# Setting up the working directory
setwd("C:/Users/prave/Downloads/Praveen/UConn/Predictive modeling/My Learnings")


# Installing are required packages
if (!require("stats")){
  install.packages("stats")
  library(stats)
}

if (!require("utils")){
  install.packages("utils")
  library(utils)
}

# To find summary statistics
if (!require("Matrix")){
  install.packages("Matrix")
  library(Matrix)
}

# To create visualizations
if (!require("graphics")){
  install.packages("graphics")
  library(graphics)
}

# To convert categorical variables into dummies
if (!require("dummies")){
  install.packages("dummies")
  library(dummies)
}


# To split the dataset into subsets while maintaining same proportion of classes
if (!require("caTools")){
  install.packages("caTools")
  library(caTools)
}

# To fit a regularized glm model
if (!require("glmnet")){
  install.packages("glmnet")
  library(glmnet)
}


# To perform k-fold cross validation
if (!require("caret")){
  install.packages("caret")
  library(caret)
}


# To build bayesian glm model
if (!require("arm")){
  install.packages("arm")
  library(arm)
}


# To obtain significant variables using decision tree model
if (!require("rpart")){
  install.packages("rpart")
  library(rpart)
}


# To build Random Forest model
if (!require("randomForest")){
  install.packages("randomForest")
  library(randomForest)
}


# Importing Mushroom Dataset
Original_Dataset = read.csv(file = "mushroom.csv")


#####
# D #
##########################################################################################
#                                                                                        #
# Data Preprocessing:                                                                    #
#                                                                                        #
# 1. Checking data types of each variable and fixing them if found incorrect             #
# 2. Removing monotonous variables from the dataset                                      #
# 3. Installing and loading the required packages                                        #
# 4. Importing the dataset to initiate Data Preprocessing phase                          #
#                                                                                        #
##########################################################################################

# Checking data types of each variable in the dataset
str(Original_Dataset)
# Findings:
# All the variables are categorical and their class is factor


# Checking if there is any class imbalance in the dependent variable
prop.table(table(Original_Dataset$PE))
# Findings:
# Dependent variable, PE, has only two classes
# there is no class imbalance. Proportion of p records are 48.2% while that of e records are 51.8%


# Checking sample values of each variable
summary(Original_Dataset)
# Findings:
# The variable veil.type has just one value. We can delete this variable as this 
# will not help us in understanding the dependent variable

# Deleting the variable
Original_Dataset$veil.type = NULL

# Few more observations made from summary(Original_Dataset) :
# cap.shape has a value "c" which has only 4 records
# cap.surface has a value "g" which has only 4 records
# veil.color has a value "y" which has only 8 records


# Checking variables with more than 7 levels, as summary function on the dataset do not display 
# more than 7 levels for each variable
summary(Original_Dataset$odor)
summary(Original_Dataset$cap.color)
summary(Original_Dataset$gill.color)
summary(Original_Dataset$spore.print.color)
summary(Original_Dataset$stalk.color.above.ring)
summary(Original_Dataset$stalk.color.below.ring)
# Findings: 
# stalk.color.above.ring has a value with "y" which has only 8 records. All other values look good. 
# We have checked all the distinct values of each variable and we didn't see a single missing value.

# Checking how the independent variables are explaining the dependent variable
table(Original_Dataset$PE,Original_Dataset$cap.color)
table(Original_Dataset$PE,Original_Dataset$odor)
table(Original_Dataset$PE,Original_Dataset$cap.surface)
table(Original_Dataset$PE,Original_Dataset$cap.shape)
table(Original_Dataset$PE,Original_Dataset$stalk.shape)
table(Original_Dataset$PE,Original_Dataset$stalk.surface.above.ring)
table(Original_Dataset$PE,Original_Dataset$spore.print.color)
table(Original_Dataset$PE,Original_Dataset$gill.color)
# Findings:
# There are many levels in more than 5 variables that are perfectly classifying the dependent variable,
# which is bad. There is no or least variation in these levels. I can either replace these levels with 
# the variables mode or ignore this issue. Replacing with the mode values can be a serious problem as 
# there are many levels that need this fix and doing so can raise unnecessary complexity.

# Conclusion:
# I choose to ignore this issue for now.


# Checking the number of observations that are related to all the levels (above found variables' levels) 
# having least count
attach(Original_Dataset)
# Attaching the column names makes the conditional query look less cumbersome 

nrow(Original_Dataset[stalk.color.above.ring == 'y' | cap.shape == 'c' | cap.surface == 'g' | veil.color == 'y',])
# Findings: 
# There are only 13 records with above mentioned variables' levels.

# Conclusion: 
# We can choose to either delete or replace above records with the mode value of each categorical variable
# I choose to delete these records as we have sufficient number of records for each level (or value)
# of the categorical variable.

# Removing observations with levels (variables' levels) having least count
Modified_Dataset = Original_Dataset[ ! (stalk.color.above.ring == 'y' | cap.shape == 'c' | 
                                      cap.surface == 'g' | veil.color == 'y') ,]

# Verifying if the records were successfuly removed 
summary(Modified_Dataset)
# Findings: 
# Records were removed but the traces of levels still remain in the dataset

# Removing unused levels that are resulted from the above delete step
Modified_Dataset = droplevels(Modified_Dataset)
summary(Modified_Dataset)
# The traces of the unused levels are now removed


#####
# E #
#############################################################################################
#                                                                                           #
# Data Visualization:                                                                       #
#                                                                                           #
# The objective of data visualization is to understand the influence of independent         #
# variables on the dependent variable.                                                      #
#                                                                                           #
#############################################################################################

# ==================================== Cap Shape ====================================
# Understanding the variable cap.shape and its effect on the dependent variable

# Histogram of Cap shape variable
plot(Modified_Dataset$cap.shape, main = "Histogram of Cap Shape", 
        ylab = "Frequency", xlab = "Cap Shape")
# Findings:
# Levels "b", "k" and "s" have fewer observations than the levels "f" and "x".
# Also, the level "s" has the lowest number of records than other levels.

# Understanding the effect of Mushroom's Cap Shape on the dependent variable (poisonous/edible)
plot(Modified_Dataset$cap.shape, Modified_Dataset$PE, main = "Cap Shape vs PE", 
     ylab = "PE", xlab = "Cap Shape")
# Findings: From the graph,
# Around 85% of cap shape "b" are edible, while only 25% of "k" shaped mushrooms are edible. 
# All Cap shape "s" are edible
# Cap shapes "f" and "x" are neutral, they do not say anything about edible or poisonous

# Conclusion:
# We can only say that cap shapes  "b" and "k" might help a little in identifying a poisonous or a edible mushroom


# ==================================== Cap Surface # ====================================
# Understanding the variable cap surface and its effect on the dependent variable
plot(Modified_Dataset$cap.surface, main = "Histogram of Cap Surface", 
     ylab = "Frequency", xlab = "Cap Surface")
# Findings:
# Levels "f", "s", and "y" have sufficiently large number of records

# Understanding the effect of Mushroom's Cap Surface on the dependent variable (poisonous/edible)
plot(Modified_Dataset$cap.surface, Modified_Dataset$PE, main = "Cap Surface vs PE", 
     ylab = "PE", xlab = "Cap Surface")
# Findings: From the graph,
# The cap Surface "s" and "y" do not convey anything about edible or poisonous
# Around 65% of cap surface "f" mushrooms are edible

# Conclusion: 
# we can only say that mushrooms with cap surface type "f" might help in identifying a poisonous or edible mushroom

# ==================================== gill size ==================================== 
# Understanding the effect of Mushroom's Cap Color on the dependent variable (poisonous/edible)
plot(Modified_Dataset$gill.size, Modified_Dataset$PE, main = "gill size vs PE", 
     ylab = "PE", xlab = "Gill Size")
# Findings:
# Around 90% of mushrooms with gill size "n" seem to be poisonous

# ==================================== Stalk Surface Below Ring ==================================== 
# Understanding the effect of Mushroom's Stalk Surface Below Ring on the dependent variable (poisonous/edible)
plot(Modified_Dataset$stalk.surface.below.ring, Modified_Dataset$PE, main = "Stalk Surface Below Ring vs PE", 
     ylab = "PE", xlab = "Stalk Surface Below Ring")
# Findings:
# Around 95% of mushrooms with Stalk Surface Below Ring of category "k" seem to be poisonous

# ==================================== Population ==================================== 
# Understanding the effect of Mushroom's Population on the dependent variable (poisonous/edible)
plot(Modified_Dataset$population, Modified_Dataset$PE, main = "Population vs PE", 
     ylab = "PE", xlab = "Population")
# Findings:
# More than 90% of mushrooms with Population of categories "a", "c", "n" seem to be edible

# ==================================== Cap Color ==================================== 
# Understanding the variable cap Color and its effect on the dependent variable
plot(Modified_Dataset$cap.color, main = "Histogram of Cap Color", 
     ylab = "Frequency", xlab = "Cap Color")
# Findings:
# Levels "r", "u", "c", "p" and "b" have few records when compared with other levels.
# Also, the level "r", "u" and "c" has the lowest number of records.

# Understanding the effect of Mushroom's Cap Color on the dependent variable (poisonous/edible)
plot(Modified_Dataset$cap.color, Modified_Dataset$PE, main = "Cap Color vs PE", 
     ylab = "PE", xlab = "Cap Color")
# Findings:
# Around 70% of mushrooms with cap color "w" are edible

# Conclusion:
# we can only say that mushrooms with cap color "w" might help in identifying a poisonous or edible mushroom

# ==================== Does Bruises have any relationship with Cap color? ===================
plot(Modified_Dataset$bruises, main = "Histogram of bruises", 
     ylab = "Frequency", xlab = "Bruises")

plot(Modified_Dataset$cap.color, Modified_Dataset$bruises, main = "Cap Color vs Bruises", 
     ylab = "Bruises", xlab = "Cap Color")

# Findings:
# Almost 60% of mushrooms have no bruises
# Also, there is some relationship between color "b" mushrooms and bruise. 


#####
# F #
#################################################################################################
#                                                                                               #
# Model Building:                                                                               #
#                                                                                               #
# 1. We will split the dataset into Train, Validation and Test with 60:20:20 ratio respectively #
#    variables on the dependent variable.                                                       #
# 2. Build few simple machine learning models                                                   #
# 3. Evaluate the accuracies of each model                                                      #
# 4. Comment on models' bias, variance and consistency                                          #
#                                                                                               #
#################################################################################################


# Before building any machine learning model let us calculate the baseline accuracy first. 
# (If our objective is to improve the accuracy of the existing model then set the existing model's
#  accuracy as the benchmark and we aim to obtain a higher accuracy than the benchmark )
# We can then try to improve this baseline accuracy by building machine learning models
# Baseline accuracy in a classification problem is nothing but the percentage of True (or poisonous) 
# cases in the dataset

# Calculating baseline accuracy
count_table = table(Modified_Dataset$PE)
Baseline_Accuracy = count_table[2]/(count_table[1] + count_table[2])
Baseline_Accuracy
# The base line accuracy in predicting a poisonous mushroom is 48.1%

# Handling possible multi-collinearity: Categorical variables can induce multi-collinearity issue
# in the model. Because, when the model splits a categorical variable into all possible levels (as intermediate
# variables), then these intermediate variables can become a linear combination of each other. Thus,
# inducing multi-collinearity in the model. This is can be a serious problem in linear models 
# like logistic and linear regression models.
# Whereas in Tree based models, multi-collinearity is not a problem.

# Converting all categorical independent variables into dummy variables. We shall handle multi-collinearity
# by using various approaches while building models like running backward selection algorithm etc
temp = dummy.data.frame(Modified_Dataset[,c(-1)])
Modified_ds_dummies = data.frame(Modified_Dataset[1],temp)


# ============================== Dataset Split ==============================
# Splitting dataset into training, valiation and test sets

set.seed(5)

split = sample.split(Modified_ds_dummies$PE, SplitRatio = 0.6)
train = subset(Modified_ds_dummies, split == TRUE)
validation = subset(Modified_ds_dummies, split == FALSE)

split = sample.split(validation$PE, SplitRatio = 0.5)
test = subset(validation, split == TRUE)
validation = subset(validation, split == FALSE)

# Now, we shall build few simple machine learning models to improve the baseline accuracy


##########################################################################################
#  ================                                                                      #
# { Logistic Model }: Below is the quick summary of challenges faced while modeling.     #
#  ================                                                                      #
#                                                                                        #
# Logistic regression ran successfully without any errors. But, it gave the below        #
# warnings:                                                                              #
#   1: glm.fit: algorithm did not converge                                               #
#   2: glm.fit: fitted probabilities numerically 0 or 1 occurred                         #
#                                                                                        #
# These warnings are because of complete separation. Complete separation means           #
# that the dependent variable is perfectly or completely separating the                  #
# independent variable or combination of independent variables.                          #
#                                                                                        #
# To deal with this situation, we can follow one of the below approaches:                #
#   1. Introduce noice in the dataset, but this can be little tedious and                #
#      sometimes outcome can be unexpected                                               #
#   2. Identify the variables causing this separation and remove those variables         #
#      from the dataset. But, this may lead to biased estimates of the coefficients.     #
#   3. Introduce regularization term to penalize the variables causing this separation.  #
#   4. Use Bayesian Logistic model, which is nothing but a glm but uses Expectation      #
#      Maximization algorithm to update the coefficients based on the priors.            #
#                                                                                        #
#  Let us build two different models using options 3  & 4                                #
#                                                                                        #
##########################################################################################

#                                     =============================
# ==================================={ Lasso regularized glm model }===================================
#                                     =============================

# Variable Selection: There is no single approach to find significant variables for all kind of models.
#                     Because, each model uses variables based on its own criterion like information gain,
#                     accuracy decrease etc. Here, Lasso will penalize the beta coefficients and make them 
#                     zero, if they are useless. Thereby generating list of significant variables.


# Fitting a Lasso glm model by finding optimal value of lambda (regularization coefficient)
lasso_glm_cv =  cv.glmnet(x = as.matrix(train[-1]), y = as.matrix(train[1]), 
                    family = "binomial", type.measure = "auc" )

lasso_glm_cv$lambda.1se
# best lambda value is 0.00093
# Lambda value might seem very small, almost equal to zero. But, the beta coefficients resulted
# from complete separation are very large (it is believed that in few cases these coefficients 
# might go to infinity).

lasso_glm = glmnet(x = as.matrix(train[-1]), y = as.matrix(train[1]), family = "binomial", 
                   lambda = lasso_glm_cv$lambda.1se)

# Finding most significant variables
lasso_beta_coeff = coef(lasso_glm, s = lasso_glm$lambda)
lasso_beta_coeff = data.frame(as.matrix(lasso_beta_coeff))
lasso_beta_coeff = subset(lasso_beta_coeff, X1 != 0 )
lasso_imp_var = row.names(lasso_beta_coeff)[-1]



# Choice of accuracy metric: Since Mushroom dataset is almost a balanced class dataset, we can simply
#                            go with misclassification rate or overall model accuracy. In case, in  
#                            future, if this dataset becomes an imbalanced class dataset (resulted from
#                            appending new data to the existing dataset) then F score metric will be the 
#                            most logical option to measure accuracy of the models. Fscore tells us how  
#                            well the model is predicting 1 or TRUE.

# Function to do the predictions and to calculate accuracy of each model separately.
# Switch statements are used here to pass the control to specific model section where model performs 
# predictions.
model_accuracy = function(model_name, model_object, dataset)
{
  switch(model_name,
    # Executing Lasso model predictions    
    lasso = 
    {
      # Predictions
      lasso_prob = predict(model_object, newx = as.matrix(dataset[,c(-1)]), s = model_object$lambda, 
                                     type='response')
      # Converting probabilities to predictions
      y_pred = ifelse(lasso_prob > 0.5, 'p','e')
    },
    # Executing Bayes glm model predictions
    bayes_glm = 
    {
      # Predictions
      bayes_prob = predict(model_object,newdata = dataset[,c(-1)], type='response')
      
      # Converting probabilities to predictions
      y_pred = ifelse(bayes_prob > 0.5, 'p','e')
    },
    # Executing random forest model predictions
    random_forest = 
    {
      # Predictions
      rf_prob = predict(model_object,newdata = dataset[,c(-1)], type='prob')
      
      # Converting probabilities to predictions
      y_pred = ifelse(rf_prob[,2] > 0.5, 'p','e')
    }
         
         )
  
  
  #confusion matrix
  y_pred_act = data.frame(act = dataset$PE, pred = y_pred)
  cm = table(y_pred_act)
  cm
  
  # Misclassification/error rate and overall accuracy of the model
  error_rate = (cm[1,2] + cm[2,1])/(cm[1,1] + cm[2,2]+ cm[1,2] + cm[2,1])
  accuracy_rate = 1 - error_rate
  
  # Calculating Fscore 
  # precision = true positives/total predicted positives
  # recall = true positives/total actual positives
  
  p = cm[2,2]/(cm[1,2] + cm[2,2])
  r = cm[2,2]/(cm[2,1] + cm[2,2])
  
  Fscore = 2*p*r/(p + r)
  
  metrics = data.frame(misclassification_rate = error_rate, accuracy = accuracy_rate, fscore = Fscore)
  return(metrics)
}

# Evaluating Training Accuracy
model_accuracy("lasso", lasso_glm, train)
# Train Results: Misclassification is 0.04%, accuracy is 99.96% and Fscore is 0.9995

# Evaluating validation Accuracy
model_accuracy("lasso", lasso_glm, validation)
# Validation Results: Misclassification rate is 0%, accuracy is 100% and Fscore is 1


# K-fold cross validation:
# Purpose of k fold cross validation is to evaluate the model's bias, consistency and variance. This will 
# determine if the model is going perform better on an unseen dataset (like test set) 


# k fold cross validation for Lasso glm model is not required as we already used cross validation to
# find the optimal lambda (regularization parameter) value. While finding the best lambda value, Lasso glm model
# checks for the best accuracy (we used AUC metric) and returns the appropriate lambda value.


# Evaluating Test Accuracy
model_accuracy("lasso", lasso_glm, test)
# Test Results: Misclassification rate is 0.06%, accuracy is 99.94% and Fscore is 0.9994

#############################################################################################
#                                                                                           #
# Conclusion: There is very minimal variance between the predictions of Validation and      #
#             Test datasets, indicating a robust model. This model predicts poisonous       #
#             mushrooms with 99.94% accuracy.                                               #
#                                                                                           #
#############################################################################################

#                                     ====================
# ==================================={ Bayesian glm model }===================================
#                                     ====================
bayes_glm = bayesglm(PE ~ ., data = train , family = binomial)
summary(bayes_glm)

# Variable slection: Using Backward Selection approach to obtain significant variables. We cannot 
#                    use the significant variables from another model as bayesglm model uses 
#                    a different method on its priors to obtain optimal beta coefficients.


# Backward selection algorithm 
bayes_glm_tuning = bayesglm(PE ~ ., data = train , family = binomial)
bayes_glm_tuning_summ = summary(bayes_glm_tuning)

# Obtaining variables with valid p-values
coeff_df = data.frame(bayes_glm_tuning_summ$coefficients)
bayes_imp_vars = row.names.data.frame(coeff_df[!is.na(coeff_df$Pr...z..),])

# Removing variables (one at a time) from the model that have high p-values
for(i in 1:1000 ) {
  bayes_glm_tuned = bayesglm(PE~., data = train[c(bayes_imp_vars[-1],"PE")],family = binomial)
  bayes_glm_tuned_summ = summary(bayes_glm_tuned)
  bayes_glm_tuned_summ
  
  coeff_df = data.frame(bayes_glm_tuned_summ$coefficients)
  if (max(coeff_df$Pr...z..) <= 0.05) {
    break
  }
  bayes_imp_vars = row.names.data.frame(coeff_df[coeff_df$Pr...z.. < max(coeff_df$Pr...z..),])
}
bayes_glm_tuned

# Evaluating Training Accuracy
model_accuracy("bayes_glm", bayes_glm_tuned, train)
# Train Results: Misclassification is 0%, accuracy is 100% and Fscore is 1


# Evaluating validation Accuracy
model_accuracy("bayes_glm", bayes_glm_tuned, validation)
# Validation Results: Misclassification rate is 0%, accuracy is 100% and Fscore is 1


# 5 fold cross validation for bayes glm model
# Creating row indices for the folds 
folds = createFolds(y = train$PE, k = 5)

# Creating train and test folds, and then measuring accuracies for test folds
cross_validation = lapply(folds, function(x) {
  train_fold = train[-x,]
  test_fold = train[x,]
  
  cat("\n Bayes GLM: 5 fold Cross Validation results")
  
  # Using the same significant variables that were obtained earlier
  cv_model = bayesglm(PE ~ ., data = train_fold[c('PE',bayes_imp_vars[-1])], family = binomial)
  cat("\n")
  
  # Using the same model_accuracy function to measure accuracies of test folds
  print(model_accuracy("bayes_glm", cv_model, test_fold))
}
)

# Findings:
# Accuracy on 3 out of 5 test folds is 100% and on other folds, accuracy is 99.9%
# This very less variance among test folds suggests that this model is a robust model

# Evaluating Test Accuracy
model_accuracy("bayes_glm", bayes_glm_tuned, test)
# Test Results: Misclassification rate is 0%, accuracy is 100% and Fscore is 1

#############################################################################################
#                                                                                           #
# Conclusion: There is very minimal variance between the predictions of Validation, cross-  #
#             validation and Test datasets, indicating a robust model. This model predicts  #
#             poisonous mushrooms with almost 100% accuracy on test set.                    #
#                                                                                           #
#############################################################################################



# Let us try one tree based models, decision tree or random forest.
# Decision tree is the most basic tree based model but there is a high chance of overfitting, 
# if pruning is not controlled. 
# On the other hand, Random forest is a bunch of decision tree models. Basically, Random Forest 
# combines "weak learners" or "weak decision trees" into strong learners. It makes the 
# predictions based on majority vote method. Hence, overfitting is reduced by a huge extent.

# Let us build a Random Forest model

#                                     =====================
# ==================================={ Random Forest model }===================================
#                                     =====================

# Variable slection: Since decision tree and other tree based models uses almost similar tree based 
#                    algorithms, we can use decision tree model to find out the significant variables
#                    and use these variables in the random forest model.

# Note: Random Forest may take longer if we do not provide selected features or variables because
#       it will randomly select a subset of variables for each estimator or tree (intermediate 
#       decision tree model).


# Obtaining significant variables from decision tree model
decision_tree = rpart(PE ~ ., data = train)
rf_var_imp = names(decision_tree$variable.importance)

# Fitting random forest model
rf_model = randomForest(x = train[rf_var_imp], y = train[,c(1)])
                  

# Evaluating Training Accuracy
model_accuracy("random_forest", rf_model, train)
# Train Results: Misclassification is 0.57%, accuracy is 99.42% and Fscore is 0.9941

# Evaluating validation Accuracy
model_accuracy("random_forest", rf_model, validation)
# Validation Results: Misclassification rate is 0.67%, accuracy is 99.32% and Fscore is 0.9930


# 5 fold cross validation for random forest model
# Creating train and test folds, and then measuring accuracies for test folds
cross_validation = lapply(folds, function(x) {
  train_fold = train[-x,]
  test_fold = train[x,]
  
  cat("\n Random Forest: 5 fold Cross Validation results")
  
  # Using the same significant variables that were obtained earlier
  cv_model = randomForest(x = train_fold[rf_var_imp], y = train_fold[,c(1)] )
  
  cat("\n")
  
  # Using the same model_accuracy function to measure accuracies of test folds
  print(model_accuracy("random_forest", cv_model, test_fold))
}
)

# Findings: 
# Accuracies of the test folds are 99.57%, 98.9%, 100%, 99.69% and 99.78%
# Test folds' accuracies are not consistent but they are higher than Training accuracy.

# Evaluating Test Accuracy
model_accuracy("random_forest", rf_model, test)
# Test Results: Misclassification rate is 0.25%, accuracy is 99.75% and Fscore is 0.9974

#############################################################################################
#                                                                                           #
# Conclusion: Without performing any parameter tuning, Random Forest gave good results. We  #
#             can perform gridsearch and find the optimal parameter values. This will much  # 
#             more better results(with least variance). Anyway, the variance between Train  #
#             validation and test accuracies seems fine.                                    #
#                                                                                           #
#############################################################################################




#####
# G #
#################################################################################################
#                                                                                               #
# Model Selection:                                                                              #
#                                                                                               #
# 1. Compare model accuracy metrics                                                             #
# 2. Advantages and disadvantages of the models                                                 #
#                                                                                               #
#################################################################################################

#------------------------------------------------------------------------------------
# Model Name   |  Lasso regularized glm  |    Random Forest   |     Bayesian glm    |
# -----------------------------------------------------------------------------------
#              |                         |                    |                     |
# Validation   |      100%               |        99.32%      |        100%         |
#  accuracy    |                         |                    |                     |
#              |                         |                    |                     |
# Test accuracy|      99.94%             |        99.75%      |        100%         |
#              |                         |                    |                     |
#   Fscore     |      0.9994             |        0.9974      |         1           |
#              |                         |                    |                     |
#              |1. Fast                  |1. Slow             |1. Slow              |
#              |                         |                    |                     |
#              |2. Performs feature      |2. Performs feature |2. Doesn't perform   |
# Advantages   |   selection by itself   |   selection        |   feature selection |
#     &        |                         |                    |                     |
# Disadvantages|3.Uses slightly more     |3.Uses more features|3. Uses only few     |
#              |  variables than bayesglm|  than Lasso to     |   variables to      |
#              |  to explain Y           |  to explain Y      |   explain Y         |
#              |                         |                    |                     |
#              |4. Easy to interpret     |4. Little difficult |4. Easy to interpret |
#              |                         |   to interpret     |                     |
#              |                         |                    |                     |
#              |5. Only one parameter    |5. Several paramters|5. No parameters to  |
#              |   to tune(lambda)       |   to tune          |   tune              |
#              |                         |                    |                     |
#              |6. Better accuracy       |6. Good accuracy    |6. Best accuracy     |
#              |                         |                    |                     |
#              |7. consistent results    |7. less consistent  |7. Consistent results|
#              |   on validation and     |   results on cross |   on validation,    |
#              |   test                  |   validation,      |   cross-validation  |
#              |                         |   validation and   |   and test sets     |
#              |                         |   test sets        |                     |
#------------------------------------------------------------------------------------


###############################################################################################
#                                                                                             #
# Conclusion:                                                                                 #
#                                                                                             #
#  1. If speed is the criterion then I will choose Lasso model over Bayes glm model. Because, #
#     Bayesglm takes time in performing feature selection.                                    #      
#  2. If accuracy, consistency, ease of implementation and interpretation are the criterion,  #
#     then I will prefer Bayes glm model over others.                                         #  
#                                                                                             #
# I will choose Bayesglm model for the below reasons:                                         #
#                                                                                             #
#  1. It uses very few variables and gives the best accuracy.                                 #
#     This means that we can identify poisonous mushrooms quickly just by looking at few      #
#     features of the mushroom                                                                #
#                                                                                             #
#  2. Its higher (almost 100%) accuracy results are lot more consistent than other models.    #
#     This means that if I identify an "Edible" mushroom then I can eat it with greater       #
#     confidence.                                                                             #
#                                                                                             #
###############################################################################################


#####
# H #
##########################################################################################
#                                                                                        #
# Interpretation of the selected model:                                                  #
#                                                                                        #
# 1. Identifying an edible mushroom is almost certain if the odor of the                 #
#    mushroom is either type "a" or "l" or "n"                                           #
#                                                                                        #
# 2. Identifying an edible mushroom is easier if the mushroom's gill size is "b"         #  
#                                                                                        #
# 3. Identifying a poisonous mushroom is almost certain if the mushroom's                #  
#    spore print color is "r"                                                            #  
#                                                                                        #
# 4. Odds of identifying a poisonous mushroom increases by a factor of 240 if the        #           
#    mushroom population is of category "c"                                              #      
#                                                                                        #
# 5. Odds of identifying a poisonous mushroom increases by a factor of 77 if the         #  
#    mushroom's stalk surface above the ring is of category "k"                          #
#                                                                                        # 
# 6. Odds of identifying a poisonous mushroom increases by a factor of 101 if the        #
#    mushroom's stalk surface below the ring is of category "y"                          #
#                                                                                        #
##########################################################################################



#####
# I #
##########################################################################################
#                                                                                        #
# Additional comments:                                                                   #
#                                                                                        #
# 1. Review comments can be logged here                                                  #
# 2. Changes planned in the future can also be logged here                               #
# 3. If the changes are implemented, specify what changes were made (in comments)        #
# 4. Any other useful information related to this project can also be logged here        #
#                                                                                        #
##########################################################################################
