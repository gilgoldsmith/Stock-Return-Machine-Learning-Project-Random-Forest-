rm(list=ls())
setwd("C:/Users/goldsg/Desktop/Machine Learning for Finance/Project")
data <- read.csv('Factors and Stock returns.csv')
data <- na.omit(data)
data$Date <- as.Date(data$Date)

# Programs
library("ggplot2")
library("reshape")
library("plm")
library("rpart")
library("zoo")
library("plyr")
library("dplyr")
library("stringr")
library("reshape2")
library("pander")
library("DataCombine")
library("plm")
library("quantmod")
library("caret")
library("ipred")
library("e1071")
library("ranger")

# Data
# Separating by dates
my.dates1 <- as.Date(data$Date)

model.df <- list(NA)

# ------------------------------------------------------------------------------------------------
# 2010
d11 <- min(my.dates1)
d21 <- as.Date("2006-12-31")
d31 <- as.Date("2009-12-31")
d41 <- as.Date("2010-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                                  method="ranger",
                                  trControl = fitControl,
                                  metric="RMSE",
                                  tuneGrid=grid.tmp,
                                  oob.error=F,
                                  importance="impurity",
                                  max.depth=grid[i,4],
                                  num.trees=grid[i,5])
  print(i)

}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[1]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# ---------------------------------------------------------------------------------------------------
# Table 1
monthly <- 0.33

graph <- c(test.R2,monthly)
bar <- 0.6
barplot(graph, 
        main = "Monthly Performance Project vs. Paper Rsquared",
        xlab = "Project                                                 Paper",
        ylab = "Rsquared",
        width = bar,
        col = "skyblue")

# --------------------------------------------------------------------------------------------------
# Annual Data
annualData <- data[format(data$Date, "%m") == "01", ]
annual.my.dates3 <- as.Date(annualData$Date)

ad13 <- min(annual.my.dates3)
ad23 <- as.Date("2006-12-31")
ad33 <- as.Date("2009-12-31")
ad43 <- as.Date("2010-12-31")
aidx13 <- which(annual.my.dates3 >= ad13 & annual.my.dates3 <= ad23)
aidx23 <- which(annual.my.dates3 > ad23 & annual.my.dates3 <= ad33)
aidx33 <- which(annual.my.dates3 > ad33 & annual.my.dates3 <= ad43)

#data splits
ax3 <- subset(data, select = -c(return, return..t.1., Date))
ay3 <- as.data.frame(data$return)

#train-validate-test split
annual.train_x3 <- ax3[aidx13,]
annual.validate_x3 <- ax3[aidx23,]
annual.test_x3 <- ax3[aidx33,]

annual.train_y3 <- ay3[aidx13,]
annual.validate_y3 <- ay3[aidx23,]
annual.test_y3 <- ay3[aidx33,]

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=annual.train_x3,y=annual.train_y3,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity")
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- annual.train_y3
y.validate <- annual.validate_y3

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=annual.train_x3)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=annual.validate_x3)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=annual.test_x3)
test.R2 <- R2(test.predictions,annual.test_y3)
print(test.R2)

# Table 2
annual <- 03.28
test <- test.R2

graph <- c(test,annual)
bar <- 0.6
barplot(graph, 
        main = "Annual Performance Project vs. Paper Rsquared",
        xlab = "Project                                                 Paper",
        ylab = "Rsquared",
        width = bar,
        col = "red")

# ------------------------------------------------------------------------------------------------
# 2011
d11 <- min(my.dates1)
d21 <- as.Date("2007-12-31")
d31 <- as.Date("2010-12-31")
d41 <- as.Date("2011-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity",
                              max.depth=grid[i,4],
                              num.trees=grid[i,5])
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[2]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# ------------------------------------------------------------------------------------------------
# 2012
d11 <- min(my.dates1)
d21 <- as.Date("2008-12-31")
d31 <- as.Date("2011-12-31")
d41 <- as.Date("2012-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity",
                              max.depth=grid[i,4],
                              num.trees=grid[i,5])
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[3]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# ------------------------------------------------------------------------------------------------
# 2013
d11 <- min(my.dates1)
d21 <- as.Date("2009-12-31")
d31 <- as.Date("2012-12-31")
d41 <- as.Date("2013-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity",
                              max.depth=grid[i,4],
                              num.trees=grid[i,5])
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[4]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# ------------------------------------------------------------------------------------------------
# 2014
d11 <- min(my.dates1)
d21 <- as.Date("2010-12-31")
d31 <- as.Date("2013-12-31")
d41 <- as.Date("2014-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity",
                              max.depth=grid[i,4],
                              num.trees=grid[i,5])
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[5]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# ------------------------------------------------------------------------------------------------
# 2015
d11 <- min(my.dates1)
d21 <- as.Date("2011-12-31")
d31 <- as.Date("2014-12-31")
d41 <- as.Date("2015-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity",
                              max.depth=grid[i,4],
                              num.trees=grid[i,5])
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[6]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# -------------------------------------------------------------------------------------------------
# 2016
d11 <- min(my.dates1)
d21 <- as.Date("2012-12-31")
d31 <- as.Date("2015-12-31")
d41 <- as.Date("2016-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity",
                              max.depth=grid[i,4],
                              num.trees=grid[i,5])
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[7]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# -------------------------------------------------------------------------------------------------
# 2017
d11 <- min(my.dates1)
d21 <- as.Date("2013-12-31")
d31 <- as.Date("2016-12-31")
d41 <- as.Date("2017-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity",
                              max.depth=grid[i,4],
                              num.trees=grid[i,5])
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[8]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# -------------------------------------------------------------------------------------------------
# 2018
d11 <- min(my.dates1)
d21 <- as.Date("2014-12-31")
d31 <- as.Date("2017-12-31")
d41 <- as.Date("2018-12-31")
idx11 <- which(my.dates1 >= d11 & my.dates1 <= d21)
idx21 <- which(my.dates1 > d21 & my.dates1 <= d31)
idx31 <- which(my.dates1 > d31 & my.dates1 <= d41)

#data splits
x1 <- subset(data, select = -c(return, return..t.1., Date))
y1 <- as.data.frame(data$return)

#train-validate-test split
train_x1 <- x1[idx11,]
validate_x1 <- x1[idx21,]
test_x1 <- x1[idx31,]

train_y1 <- y1[idx11,]
validate_y1 <- y1[idx21,]
test_y1 <- y1[idx31,]

#combine train & validate for after training (if needed)
combined_x1 <- rbind(train_x1, validate_x1)
combined_y1 <- c(train_y1, validate_y1)

# -----------------------------------------------------------------------------------------------------
# Training
fitControl <- trainControl(method = "cv",number=5)
grid <- expand.grid(.mtry=c(3,4,5),
                    .splitrule="variance",
                    .min.node.size=c(100,150,200),
                    .max.depth=c(2,3),
                    .num.trees=c(5,10,20))

model.number <- nrow(grid)

models <- list(NA)

for (i in 1:model.number){
  grid.tmp <- grid[i,c(1:3)]
  models[[i]] <- caret::train(x=train_x1,y=train_y1,
                              method="ranger",
                              trControl = fitControl,
                              metric="RMSE",
                              tuneGrid=grid.tmp,
                              oob.error=F,
                              importance="impurity",
                              max.depth=grid[i,4],
                              num.trees=grid[i,5])
  print(i)
  
}

# -----------------------------------------------------------------------------------------------------
# Validating
training.R2 <- list(NA)
validating.R2 <- list(NA)

y.train <- train_y1 
y.validate <- validate_y1

my.R2 <- as.data.frame(matrix(NA,ncol=2,nrow=model.number))
names(my.R2) <- c("Training","Validation")

R2 <- function(pred,act)
{
  rss <- sum((pred - act)^2)
  tss <- sum((act - 0)^2)
  rsq <- 1 - rss/tss
}

for (i in 1:model.number){
  training.R2[[i]] <- predict(models[[i]],newdata=train_x1)
  my.R2[i,1] <- R2(training.R2[[i]],y.train)
  
  validating.R2[[i]] <- predict(models[[i]],newdata=validate_x1)
  my.R2[i,2] <- R2(validating.R2[[i]],y.validate)
  
  print(i)
}

best.model <- which.max(my.R2[,2])
print(best.model)
models[[best.model]]

model.df[[9]] <- models[[best.model]]

# -------------------------------------------------------------------------------------------------
# Testing
test.predictions <- predict(models[[best.model]],newdata=test_x1)
test.R2 <- R2(test.predictions,test_y1)
print(test.R2)

# -------------------------------------------------------------------------------------------------
# Variable Importance
# Linear Graph
importance <- caret::varImp(model.df[[9]]) #Choose which ever model from model.df to check the variable importance for that model
ggplot(importance)

# Pie Chart
total <- sum(importance$importance$Overall)
variables <- colnames(train_x1)
scores <- c()
for (i in 1:length(variables)){
  scores <- c(scores, importance$importance$Overall[i] / total)
}
pie(scores, labels = variables, main="Pie Chart of Variable Importance")

# -------------------------------------------------------------------------------------------------
# End Table
end <- data.frame(matrix(nrow=length(model.df), ncol=3))
names <- c("Year", "Out of Sample R-Squared", "Max Depth")
colnames(end) <- names
years <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
rsq <- c(0.03689, -0.0070, 0.0207, 0.0483, -0.0006, -0.0140, 0.0231, 0.0161, -0.0163)
depth <- c(3, 3, 3, 3, 3, 3, 2, 3, 3)
end[,1] <- years
end[,2] <- rsq
end[,3] <- depth

plot(years, rsq,
     type = "l",
     col = "red",
     xlab = "Year",
     ylab = "Out of Sample R-Squared",
     main = "Out of Sample R-Squared by Year")

plot(years, depth,
     type = "l",
     col = "blue",
     xlab = "Year",
     ylab = "Max Depth of Best Model",
     main = "Model Complexity by Year")
