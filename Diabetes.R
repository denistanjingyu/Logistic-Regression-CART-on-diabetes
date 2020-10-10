library (data.table)
library(ggplot2)

setwd('')
diabetes.dt<-fread('diabetes.csv')

summary(diabetes.dt)
diabetes.dt$Outcome<-factor(diabetes.dt$Outcome)
summary(diabetes.dt)

(sum(diabetes.dt$Glucose==0))/(nrow(diabetes.dt))
diabetes.dt$Glucose <- replace(diabetes.dt$Glucose, diabetes.dt$Glucose == 0, NA)
(sum(diabetes.dt$SkinThickness==0))/(nrow(diabetes.dt))
diabetes.dt$SkinThickness <- replace(diabetes.dt$SkinThickness, diabetes.dt$SkinThickness == 0, NA)
diabetes.dt$SkinThickness <- replace(diabetes.dt$SkinThickness, diabetes.dt$SkinThickness == 99, NA)
diabetes.dt$SkinThickness[is.na(diabetes.dt$SkinThickness)] = mean(diabetes.dt$SkinThickness, na.rm = T)
summary(diabetes.dt$SkinThickness)
(sum(diabetes.dt$BMI == 0))/(nrow(diabetes.dt))
diabetes.dt$BMI <- replace(diabetes.dt$BMI, diabetes.dt$BMI == 0, NA)
(sum(diabetes.dt$BloodPressure == 0))/(nrow(diabetes.dt))
diabetes.dt$BloodPressure <- replace(diabetes.dt$BloodPressure, diabetes.dt$BloodPressure == 0, NA)
diabetes.dt$BloodPressure[is.na(diabetes.dt$BloodPressure)]=mean(diabetes.dt$BloodPressure, na.rm = T)
summary(diabetes.dt)

(sum(diabetes.dt$Glucose == 0))/(nrow(diabetes.dt))
diabetes.dt$Glucose <- replace(diabetes.dt$Glucose, diabetes.dt$Glucose == 0, NA)
(sum(diabetes.dt$SkinThickness==0))/(nrow(diabetes.dt))
diabetes.dt$SkinThickness <- replace(diabetes.dt$SkinThickness, diabetes.dt$SkinThickness == 0, NA)
diabetes.dt$SkinThickness <- replace(diabetes.dt$SkinThickness, diabetes.dt$SkinThickness == 99, NA)
diabetes.dt$SkinThickness[is.na(diabetes.dt$SkinThickness)] = mean(diabetes.dt$SkinThickness, na.rm = T)
summary(diabetes.dt$SkinThickness)
(sum(diabetes.dt$BMI == 0))/(nrow(diabetes.dt))
diabetes.dt$BMI <- replace(diabetes.dt$BMI, diabetes.dt$BMI == 0, NA)
(sum(diabetes.dt$BloodPressure == 0))/(nrow(diabetes.dt))
diabetes.dt$BloodPressure <- replace(diabetes.dt$BloodPressure, diabetes.dt$BloodPressure == 0, NA)
diabetes.dt$BloodPressure[is.na(diabetes.dt$BloodPressure)] = mean(diabetes.dt$BloodPressure, na.rm = T)
summary(diabetes.dt)

diabetes.m1 <- glm(Outcome ~ ., family = binomial, data = diabetes.dt)
summary(diabetes.m1)

# Remove BloodPressure, SkinThickness, Insulin and Age
diabetes.m2 <- glm(Outcome ~ .-BloodPressure-SkinThickness -Insulin - Age , family = binomial, data = diabetes.dt)
summary(diabetes.m2)

OR.m2<-exp(coef(diabetes.m2))
OR.m2

# All don't include 1
OR.m2<-exp(confint(diabetes.m2))
OR.m2

prob<-predict(diabetes.m2, type = 'response')
threshold <- sum(diabetes.dt$Outcome == "1")/length(diabetes.dt$Outcome)
threshold
diabetes.hat <- ifelse(prob>threshold, 1, 0)

# TrainTestsplit
library(caTools)
set.seed(2004)

train <- sample.split(Y = diabetes.dt$Outcome, SplitRatio = 0.7)
trainset <- subset(diabetes.dt, train == T)
testset <- subset(diabetes.dt, train == F)

summary(trainset$Outcome)
summary(testset$Outcome)

# Model 3 with outcome against all variables
diabetes.m3 <- glm(Outcome ~ . , family = binomial, data = trainset)
summary(diabetes.m3)

# Model 4 with outcome against all except skinthickness, BloodPressure, insulin, age
diabetes.m4 <- glm(Outcome ~ .-SkinThickness - BloodPressure - Insulin - Age , family = binomial, data = trainset, na.action = na.exclude)
summary(diabetes.m4)

prob.train <- predict(diabetes.m4, type = "response")
predict.diabetes.train<-ifelse(prob.train > threshold, "1", "0")
table3 <- table(trainset$Outcome,predict.diabetes.train)
table3
mean(predict.diabetes.train == trainset$Outcome, na.rm = T)

# Confusion Matrix
prob.test <- predict(diabetes.m4, newdata = testset, type = 'response')
predict.diabetes.test <- ifelse(prob.test > threshold, "1", "0")
table4 <- table(testset$Outcome, predict.diabetes.test)
table4
prop.table(table4)

# Overall Accuracy For Logistic Model
accuracy = mean(predict.diabetes.test == testset$Outcome, na.rm = T)

# For CART:
library(rpart)
library(rpart.plot)	# For Enhanced tree plots via PRP()

options(digits = 3) # show only 3 decimal places

m1_c <- rpart(Outcome ~ . , data = diabetes.dt, method = 'class', cp = 0) 

rpart.plot(m1_c, nn = T, main = "Maximal Tree in m1_c") # nn = T means we want the plot to show the node number

# Results of CART as Decision Rules
print(m1_c)

# Effects of Cost Complexity Pruning at important cp values.
printcp(m1_c, digits = 3)

# Plot CV error vs cp values
plotcp(m1_c)

# Prune the max tree m2_c using a particular CP value (i.e. a specified penalty cost for model complexity)
m2_c <- prune(m1_c, cp = Inf)
print(m2_c)
printcp(m2_c, digits = 3)
rpart.plot(m2_c, nn = T, main = "Minimal Tree in m2_c")

# Find the optimal CART via cp Table
cp.min <- m1_c$cptable[which.min(m1_c$cptable[, "xerror"]), "CP"]
m3_c <- prune(m1_c, cp = cp.min)
print(m3_c)
printcp(m3_c, digits = 3)

rpart.plot(m3_c, nn = T, main = "Optimal Tree in m3_c with 5 Splits")

# -------- Trainset Error & CV Error ----------
# Root node error: 268/768 = 0.349
# cart trainset error = 0.604 * 0.349 = 0.211
# cart CV error = 0.716 * 0.349 = 0.25

m3_c$variable.importance

predicted <- predict(m3_c, newdata = diabetes.dt, type = 'class')
table1<-table(diabetes.dt$Outcome, predicted)
prop.table(table1)

# Accuracy of the Cart Model
mean(diabetes.dt$Outcome == predicted)

library(data.table)
library(ggplot2)
library(rpart)
library(rpart.plot)	# For Enhanced tree plots via PRP()
set.seed(2004)

setwd("Team Challenge Assignment 2")
diabetes.dt <- fread ("diabetes.csv")

# For Data cleaning
(sum(diabetes.dt$Glucose == 0))/(nrow(diabetes.dt))
diabetes.dt$Glucose <- replace(diabetes.dt$Glucose, diabetes.dt$Glucose == 0, NA)
(sum(diabetes.dt$SkinThickness == 0))/(nrow(diabetes.dt))
diabetes.dt$SkinThickness <- replace(diabetes.dt$SkinThickness, diabetes.dt$SkinThickness == 0, NA)
diabetes.dt$SkinThickness <- replace(diabetes.dt$SkinThickness, diabetes.dt$SkinThickness == 99, NA)
diabetes.dt$SkinThickness[is.na(diabetes.dt$SkinThickness)]=mean(diabetes.dt$SkinThickness, na.rm = T)
summary(diabetes.dt$SkinThickness)
(sum(diabetes.dt$BMI == 0))/(nrow(diabetes.dt))
diabetes.dt$BMI <- replace(diabetes.dt$BMI, diabetes.dt$BMI == 0, NA)
(sum(diabetes.dt$BloodPressure == 0))/(nrow(diabetes.dt))
diabetes.dt$BloodPressure <- replace(diabetes.dt$BloodPressure, diabetes.dt$BloodPressure == 0, NA)
diabetes.dt$BloodPressure[is.na(diabetes.dt$BloodPressure)] = mean(diabetes.dt$BloodPressure, na.rm = T)
summary(diabetes.dt)
diabetes.m1 <- glm(Outcome ~ ., family = binomial, data = diabetes.dt)
summary(diabetes.m1)

# For Logistic Regression:
library (data.table)
setwd('')
diabetes.dt<-fread('diabetes.csv')
summary(diabetes.dt)

diabetes.dt$Outcome<-factor(diabetes.dt$Outcome)
summary(diabetes.dt)

(sum(diabetes.dt$Glucose == 0))/(nrow(diabetes.dt))
diabetes.dt$Glucose <- replace(diabetes.dt$Glucose, diabetes.dt$Glucose == 0, NA)
(sum(diabetes.dt$SkinThickness==0))/(nrow(diabetes.dt))
diabetes.dt$SkinThickness <- replace(diabetes.dt$SkinThickness, diabetes.dt$SkinThickness == 0, NA)
diabetes.dt$SkinThickness <- replace(diabetes.dt$SkinThickness, diabetes.dt$SkinThickness == 99, NA)
diabetes.dt$SkinThickness[is.na(diabetes.dt$SkinThickness)]=mean(diabetes.dt$SkinThickness, na.rm = T)
summary(diabetes.dt$SkinThickness)
(sum(diabetes.dt$BMI == 0))/(nrow(diabetes.dt))
diabetes.dt$BMI <- replace(diabetes.dt$BMI, diabetes.dt$BMI == 0, NA)
(sum(diabetes.dt$BloodPressure == 0))/(nrow(diabetes.dt))
diabetes.dt$BloodPressure <- replace(diabetes.dt$BloodPressure, diabetes.dt$BloodPressure == 0, NA)
diabetes.dt$BloodPressure[is.na(diabetes.dt$BloodPressure)] = mean(diabetes.dt$BloodPressure, na.rm = T)
summary(diabetes.dt)

diabetes.m1 <- glm(Outcome ~ ., family = binomial, data = diabetes.dt)
summary(diabetes.m1)

# Remove BloodPressure, SkinThickness, Insulin and Age
diabetes.m2 <- glm(Outcome ~ .-BloodPressure - SkinThickness - Insulin - Age , family = binomial, data = diabetes.dt)
summary(diabetes.m2)

OR.m2 <- exp(coef(diabetes.m2))
OR.m2

# All don't include 1
OR.m2 <- exp(confint(diabetes.m2))
OR.m2

prob <- predict(diabetes.m2, type = 'response')
threshold <- sum(diabetes.dt$Outcome == "1")/length(diabetes.dt$Outcome)
threshold
diabetes.hat <- ifelse(prob>threshold, 1, 0)

# TrainTestsplit
library(caTools)

train <- sample.split(Y = diabetes.dt$Outcome, SplitRatio = 0.7)
trainset <- subset(diabetes.dt, train == T)
testset <- subset(diabetes.dt, train == F)

summary(trainset$Outcome)
summary(testset$Outcome)

# Model 3 with outcome against all variables
diabetes.m3 <- glm(Outcome ~ . , family = binomial, data = trainset)
summary(diabetes.m3)

# Model 4 with outcome against all except skinthickness, insulin, age
diabetes.m4 <- glm(Outcome ~ .-SkinThickness - Insulin - Age , family = binomial, data = trainset, na.action = na.exclude)
summary(diabetes.m4)

prob.train <- predict(diabetes.m4, type = "response")
predict.diabetes.train<-ifelse(prob.train > threshold, "1", "0")
table3 <- table(trainset$Outcome,predict.diabetes.train)
table3
mean(predict.diabetes.train == trainset$Outcome, na.rm = T)

# Confusion Matrix
prob.test <- predict(diabetes.m4, newdata = testset, type = 'response')
predict.diabetes.test <- ifelse(prob.test > threshold, "1", "0")
table4 <- table(testset$Outcome, predict.diabetes.test)
table4
prop.table(table4)

# Overall Accuracy
mean(predict.diabetes.test == testset$Outcome, na.rm = T)
#Accuracy = 0.77

# For CART
# Set seed above already
options(digits = 3) #show only 3 digits (ie 3 significant digits)

m1_c <- rpart(Outcome ~ . , data = diabetes.dt, method = 'class', cp = 0) 

rpart.plot(m1_c, nn = T, main = "Maximal Tree in m1") # nn = T means we want the plot to show the node number

# Results of CART as Decision Rules
print(m1_c)

# Effects of Cost Complexity Pruning at important cp values.
printcp(m1_c, digits = 3)

# Plot CV error vs cp values
plotcp(m1_c)

cp.min <- m1_c$cptable[which.min(m1_c$cptable[, "xerror"]), "CP"]
cp.min
cp.opt1 <- 0.01586  # Simplest tree within 1SE
# cp.opt2 <- 0.00280  # if cp.opt1 is too simple

# Prune the max tree m2 using a particular CP value (i.e. a specified penalty cost for model complexity)
m2_c <- prune(m1_c, cp = cp.opt1)
print(m2_c)
printcp(m2_c, digits = 3)
rpart.plot(m2_c, nn = T, main = "Minimal Tree in m2")
