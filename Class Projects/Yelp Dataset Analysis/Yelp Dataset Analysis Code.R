# import data
data <- read.csv("Data_Final")

# checking data
colnames(data)

# import libraries
library(stringr)
library(caret)
library(rpart.plot)
library(randomForest)
library(MASS)
library(ROCR)
library(pROC)
library(class)

# cleaning data
for (i in 1:length(data$Elite)) {
  data$Elite[i] <- str_replace(data$Elite[i], "20,20", "2020")
  data$City[i] <- str_replace(data$City[i], "Santa Barbra", "Santa Barbara")
  data$City[i] <- str_replace(data$City[i], "Santa  Barbara", "Santa Barbara")
  data$City[i] <- str_replace(data$City[i], "Santa Barbara ", "Santa Barbara")
}

# creating factors
for (i in 1:length(data$Elite)) {
  if (data$Elite[i] == "") {
    data$Elite_status[i] <- 0
  } else {
    data$Elite_status[i] <- 1
  }
}

data$Elite_status <- as.factor(data$Elite_status)
levels(data$Elite_status) <- c("No", "Yes")



#Random Forest
set.seed(123)
trainIndex <- createDataPartition(data$Elite_status, p = 0.8, list = FALSE)
data.train <- data[trainIndex,]
data.test <- data[-trainIndex,]

# Tree plot
tree <- rpart(Elite_status ~ User_Review_count + User_Useful_count + User_Funny_count + User_Cool_count + User_Fans, data = data.train, method ='class')
rpart.plot(tree)

# All numerical variables
# Star, Useful, Cool, Funny, Bus_Ave_Star, User_Review_count, User_Useful_count
# User_Funny_count, User_Cool_count, User_Fans, Users_Ave_Star

# Variables important to predict Elite status
# Useful, Cool, Funny, User_Review_count, User_Useful_count, User_Funny_count
# User_Cool_count, User_Fans


# Select parameter values using out-of-bag estimator
oob_train_control <- trainControl(method="oob", classProbs = TRUE, savePredictions = TRUE)

# Finding value for m using cross validation
forestfit <- train(Elite_status ~ User_Review_count + User_Useful_count + User_Funny_count + User_Cool_count + User_Fans, data = data.train, method = 'rf', importance = FALSE, trControl = oob_train_control)
plot(forestfit)

# Importance plots
forestfit.RF <- randomForest(Elite_status ~ User_Review_count + User_Useful_count + User_Funny_count + User_Cool_count + User_Fans, data = data.train, mtry = forestfit$bestTune$mtry, ntree = 500, importance = TRUE)
forestfit.RF
# Permutation importance
varImpPlot(forestfit.RF, type = 1, scale = F)
# Gini importance
varImpPlot(forestfit.RF, type = 2, scale = F)

predictions <- predict(forestfit.RF, data.test)
confusionMatrix(predictions, data.test$Elite_status)



#LDA and QDA

# Split into training and test data
i <- 1:dim(data)[1]
i.train <- sample(i, 43076, replace = F)
X.train <- data[i.train,-19]
Y.train <- data[i.train,19]  
X.test <- data[-i.train,-19]
Y.test <- data[-i.train,19]

# Normalize the dataset
preproc1 <- preProcess(X.train, method=c("center", "scale"))
X.train_norm <- predict(preproc1, X.train)
X.test_norm <- predict(preproc1, X.test)

# LDA vs QDA
X.train_norm$Elite_status <- Y.train
X.test_norm$Elite_status <- Y.test

# LDA model
lda.mod <- lda(Elite_status ~ User_Review_count + User_Useful_count + User_Funny_count + User_Cool_count + User_Fans, data = X.train_norm)
# Evaluate the classification performance of LDA
pred.lda.test <- predict(lda.mod, X.test_norm[,-19])
# Confusion matrix
Conf_Mat_LDA = table('Reference' = X.test_norm[,19], "Predicted" = pred.lda.test$class)
print(paste('The accuracy of LDA is',
            sum(Conf_Mat_LDA[row(Conf_Mat_LDA)==col(Conf_Mat_LDA)])/dim(X.test)[1]))
print(paste('The True Positive rate of LDA is',
            Conf_Mat_LDA[2,2]/sum(Conf_Mat_LDA[2,])))
print(paste('The test error is', 1 - sum(Conf_Mat_LDA[row(Conf_Mat_LDA)==col(Conf_Mat_LDA)])/dim(X.test)[1]))

# QDA models
qda.mod <- qda(Elite_status ~ User_Review_count + User_Useful_count + User_Funny_count + User_Cool_count + User_Fans, data = X.train_norm)
# Evaluate the classification performance of LDA
pred.qda.test <- predict(qda.mod, X.test_norm[,-19])
# Confusion matrix
Conf_Mat_QDA = table('Reference' = X.test_norm[,19], "Predicted" = pred.qda.test$class)
print(paste('The accuracy of QDA is',
            sum(Conf_Mat_QDA[row(Conf_Mat_QDA)==col(Conf_Mat_QDA)])/dim(X.test)[1]))
print(paste('The True Positive rate of QDA is',
            Conf_Mat_QDA[2,2]/sum(Conf_Mat_QDA[2,])))
print(paste('The test error is', 1 - sum(Conf_Mat_QDA[row(Conf_Mat_QDA)==col(Conf_Mat_QDA)])/dim(X.test)[1]))



#Logistic Regression

# Fit logistic regression model to training data.
ml <- glm(Elite_status ~ User_Review_count + User_Useful_count + User_Funny_count + User_Cool_count + User_Fans, data = X.train_norm, family = "binomial")
summary(ml)

# Fit logistic regression model to training data
# Predictions on the training data
pred.log.odds <- predict(ml) # Predicted log-odds
pred.probs <- predict(ml, type = 'response') # Predicted probabilities

# Evaluate the classification performance of LR using test data
my.thres <- 0.5
pred.log.odds.test <- predict(ml, X.test_norm[,-19])
pred.probs.test <- predict(ml, X.test_norm[,-19], type = 'response')
predicted.data <- pred.probs.test > my.thres

X.test_norm[,19] <- ifelse(X.test_norm[,19] == "Yes", TRUE, FALSE)

# Confusion matrix.
Conf_mat_LR = table('Reference' = X.test_norm[,19]==TRUE, "Predicted" = predicted.data)
Conf_mat_LR
sum(Conf_mat_LR[row(Conf_mat_LR)==col(Conf_mat_LR)])/dim(X.test_norm)[1]
print(paste('The accuracy of Logistic Regression is',
            sum(Conf_mat_LR[row(Conf_mat_LR)==col(Conf_mat_LR)])/dim(X.test)[1]))
print(paste('The True Positive rate is',
            Conf_mat_LR[2,2]/sum(Conf_mat_LR[2,])))
print(paste('The test error is', 1 - sum(Conf_mat_LR[row(Conf_mat_LR)==col(Conf_mat_LR)])/dim(X.test)[1]))

# Accuracy
missing_classerr <- mean(predicted.data != X.test_norm[,19])
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(as.numeric(predicted.data), X.test_norm[,19])
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

auc(X.test_norm$Elite_status, as.numeric(predicted.data))

summary(ml)
plot(m1)



#KNN

# Fit KNN on the training dataset
K = 2
m.knn <- knn(X.train_norm[,c(12,13,14,15,17)], X.test_norm[,c(12,13,14,15,17)], X.train_norm[,19], k =K)
table.knn <- table(X.test_norm[,19],m.knn)
print(paste('The accuracy of KNN is',
            sum(table.knn[row(table.knn)==col(table.knn)])/dim(X.test)[1]))
print(paste('The True Positive rate is',
            table.knn[2,2]/sum(table.knn[2,])))
print(paste('The test error is', 1 - sum(table.knn[row(table.knn)==col(table.knn)])/dim(X.test)[1]))
