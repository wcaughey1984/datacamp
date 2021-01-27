# 
# Title:    Supervised Learning in R - Classification
# Purpose:  (Personal Development) Code for the Supervised Learning in R Course
# Author:   Billy Caughey 
# Date:     2021.01.26 - Initial Build 
# 

##### Chapter 1 - k-Nearest Neighbors #####

# Classification: tasks that require predicting a categorical outcome

# Nearest Neighbors: points that look like each other should have the same type
# I can use the euclidean distance to determine similarity distances
# library(class) - library for knn
# pred <- knn(training_data, testing_data, training_labels)

##### Recognizing a road sign with kNN #####

# Load the 'class' package
library(class)

# Create a vector of labels
sign_types <- signs$sign_type

# Classify the next sign observed
knn(train = signs[-1], test = next_sign, cl = sign_types)

##### Exploring the traffic sign dataset #####

# Examine the structure of the signs dataset
str(signs)

# Count the number of signs of each type
table(signs$sign_types)

# Check r10's average red level by sign type
aggregate(r10 ~ sign_type, data = signs, mean)

##### Classigying a collection of road signs #####

# Use kNN to identify the test road signs
sign_types <- signs$sign_type
signs_pred <- knn(train = signs[-1], test = test_signs[-1], cl = signs$sign_type)

# Create a confusion matrix of the predicted versus actual values
signs_actual <- test_signs[1]
table(signs_actual, signs_pred[1])

# Compute the accuracy
mean(signs_actual == signs_pred)

##### What about the 'k' in kNN? #####

# It is not always the case that large k's are better
# No universal pattern for deciding k

##### Testing other 'k' values #####

# Compute the accuracy of the baseline model (default k = 1)
k_1 <- knn(train = signs, test = signs_test, cl = sign_types)
mean(k_1 == signs_test[1])

# Modify the above to set k = 7
k_7 <- knn(train = signs, test = signs_test, cl = signs$sign_type, k = 7)
mean(k_7 == signs_test)

# Set k = 15 and compare to the above
k_15 <- knn(train = signs, test = signs_test, cl = signs$sign_type, k = 15)
mean(k_15 == signs_test)

##### Seeing how the neighbors voted #####

# Use the prob parameter to get the proportion of votes for the winning class
sign_pred <- knn(train = signs[-1], test = signs_test[-1], cl = signs$sign_type, k = 7, prob = TRUE)

# Get the "prob" attribute from the predicted classes
sign_prob <- attr(sign_pred, "prob")

# Examine the first several predictions
head(sign_pred)

# Examine the proportion of votes for the winning class
head(sign_prob)

##### Data Preparation for kNN #####

# kNN is in NUMERIC FORMAT!
# I will need to scale variables in the data set

##### Understanding Bayesian Methods #####

# Bayesian methods are proability rules based on historical data 
# P(A|B) = P(A and B) / P(B)

##### Computing Probabilities #####

# Compute P(A) 
p_A <- nrow(subset(where9am, location == "office")) / nrow(where9am)

# Compute P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / nrow(where9am)

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, location == "office", daytype == "weekday")) / nrow(where9am)

# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
p_A_given_B

##### A simple Naive Bayes location Model #####

# Load the naivebayes package
library(naivebayes)

# Build the location prediction model
locmodel <- naive_bayes(location ~ daytype, data = where9am)

# Predict Thursday's 9am location
predict(locmodel, thursday9am)

# Predict Saturdays's 9am location
predict(locmodel, saturday9am)

##### Examining Raw Probabilities #####

# The 'naivebayes' package is loaded into the workspace
# and the Naive Bayes 'locmodel' has been built

# Examine the location prediction model
locmodel 

# Obtain the predicted probabilities for Thursday at 9am
predict(locmodel, thursday9am , type = "prob")

# Obtain the predicted probabilities for Saturday at 9am
predict(locmodel, saturday9am , type = "prob")

##### Understanding NB's "naivety" #####

# A naive simplification: The algorithm assumes the events are independent
# The naive assumption doesn't necessarily hold in the real world...
# But the naive model holds true in many really events.

# The Laplace correction is used when a joint probability is 0 in the chain

##### A more sophisticated location model #####

# The 'naivebayes' package is loaded into the workspace already

# Build a NB model of location
locmodel <- naive_bayes(location ~ daytype + hourtype, locations)

# Predict Brett's location on a weekday afternoon
predict(locmodel, weekday_afternoon)

# Predict Brett's location on a weekday evening
predict(locmodel, weekday_evening)

##### Preparing for unforeseen circumstances #####

# The 'naivebayes' package is loaded into the workspace already
# The Naive Bayes location model (locmodel) has already been built

# Observe the predicted probabilities for a weekend afternoon
predict(locmodel, weekend_afternoon, type = "prob")

# Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location ~ daytype + hourtype, 
                         locations, 
                         laplace = 1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2, weekend_afternoon, type = "prob")

##### Applying Naive Bayes to other problems #####

# naive bayes works best when I have to consider large data 
# naive bayes works well in text data 
# binning is the way to bin continuous data 
# for text data, we use "bag of words"

##### Making binary predictions with regression #####

# Logistic regression for binary outcomes 

##### Building simple logistic regression models #####

# Examine the dataset to identify potential independent variables
str(donors)

# Explore the dependent variable
table(donors$donated)

# Build the donation model
donation_model <- glm(donated ~ bad_address + interest_religion + interest_veterans, 
                         data = donors, family = "binomial")

# Summarize the model results
summary(donation_model)

##### making a binary prediction #####

# Estimate the donation probability
donors$donation_prob <- predict(donation_model, type = "response")

# Find the donation probability of the average prospect
mean(donors$donated)

# Predict a donation if probability of donation is greater than average (0.0504)
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donation_pred == donors$donated)

##### Model performance tradeoffs #####

# There are times a models accuracy is misleading
# ROC curves can help
# Shape of the ROC curve matters

##### Calculating ROC Curves and AUC ####

# Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)

##### Dummy variables, missing data, and interactions #####

# Dummy Variables -> categories to binary
# Imputation Strategies -> mean, missing values
# Interaction Effect -> Combination of inputs have greater impact than inputs alone

##### Coding Categorical Features #####

# Convert the wealth rating to a factor
donors$wealth_levels <- factor(donors$wealth_rating, 
                               levels = c(0, 1, 2, 3), 
                               labels = c("Unknown", "Low", "Medium", "High"))

# Use relevel() to change reference category
donors$wealth_levels <- relevel(donors$wealth_levels, ref = "Medium")

# See how our factor coding impacts the model
summary(glm(donated ~ wealth_levels, 
            data = donors,
            family = "binomial"))

##### Handling Missing Data #####

# Find the average age among non-missing values
summary(donors$age)

# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age), 
                             round(mean(donors$age, na.rm = T),2),
                             donors$age)

# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

##### Building a sophisticated model #####

# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ money + recency * frequency, 
                 data = donors,
                 family = "binomial")

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, type = "response")

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated, rfm_prob)
plot(ROC, col = "red")
auc(ROC)

##### Automatic feature selection #####

# Backward Selection 
# Forward Selection
# Neither backward or forward model may produce the 'best' model
# The outcome model may not be the same either
# Stepwise selection is built in the absence of domain knowledge

##### Buidling a stepwise regression model #####

# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, 
                   scope = list(lower = null_model, upper = full_model), 
                   direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model, type = "response")

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)

##### Making decisions with trees #####

# rpart = recursive partitioning

##### Building a simple decision tree #####

# Load the rpart package
library(rpart)

# Build a lending model predicting loan outcome versus loan amount and credit score
loan_model <- rpart(outcome ~ loan_amount + credit_score, 
                    data = loans, 
                    method = "class", 
                    control = rpart.control(cp = 0))

# Make a prediction for someone with good credit
predict(loan_model, good_credit, type = "class")

# Make a prediction for someone with bad credit
predict(loan_model, bad_credit, type = "class")

##### Visualizaing Classification trees #####

# Examine the loan_model object
loan_model

# Load the rpart.plot package
library(rpart.plot)

# Plot the loan_model with default settings
rpart.plot(loan_model)

# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

##### Creating random test datasets #####

# Determine the number of rows for training
nrow(loans) * 0.75

# Create a random sample of row IDs
sample_rows <- sample(x = nrow(loans), 8484)

# Create the training dataset
loans_train <- loans[sample_rows,]

# Create the test dataset
loans_test <- loans[-sample_rows,]

##### Building and evaluating a larger tree #####

# Grow a tree using all of the available applicant data
loan_model <- rpart(outcome ~ ., 
                    data = loans_train, 
                    method = "class", 
                    control = rpart.control(cp = 0, maxdepth = 6))

# Make predictions on the test dataset
loans_test$pred <- predict(loan_model, 
                           loans_test, 
                           type = "class")

# Examine the confusion matrix
table(loans_test$outcome, loans_test$pred)

# Compute the accuracy on the test dataset
mean(loans_test$outcome == loans_test$pred)

##### Tending to classification trees #####

# Pre-pruning - cleaning before the tree grows
# Post-pruning - cleaning after the tree has grown

##### Preventing overgrown trees #####

# Grow a tree with maxdepth of 6
loan_model <- rpart(outcome ~ .,
                    data = loans_train,
                    method = "class",
                    control = rpart.control(cp = 0, maxdepth = 6))

# Make a class prediction on the test set
loans_test$pred <- predict(loan_model,
                           loans_test,
                           type = "class")

# Compute the accuracy of the simpler tree
mean(loans_test$outcome == loans_test$pred)

# Swap maxdepth for a minimum split of 500 
loan_model <- rpart(outcome ~ ., 
                    data = loans_train, 
                    method = "class", 
                    control = rpart.control(cp = 0, minsplit = 500))

# Run this. How does the accuracy change?
loans_test$pred <- predict(loan_model, loans_test, type = "class")
mean(loans_test$pred == loans_test$outcome)

##### Creating a nicely pruned tree #####

# Grow an overly complex tree
loan_model <- rpart(outcome ~ ., 
                    data = loans_train, 
                    method = "class", 
                    control = rpart.control(cp = 0))

# Examine the complexity plot
plotcp(loan_model)

# Prune the tree
loan_model_pruned <- prune(loan_model, cp = 0.0014)

# Compute the accuracy of the pruned tree
loans_test$pred <- predict(loan_model_pruned, loans_test, type = "class")
mean(loans_test$pred == loans_test$outcome)

#### Seeing the forest from the trees #####

# Random Forests vs Classification Trees 

##### Building a random forest model #####

# Load the randomForest package
library(randomForest)

# Build a random forest model
loan_model <- randomForest(outcome ~ ., data = loans_train)

# Compute the accuracy of the random forest
loans_test$pred <- predict(loan_model, 
                           loans_test,
                           type = "class")

mean(loans_test$pred == loans_test$outcome)







