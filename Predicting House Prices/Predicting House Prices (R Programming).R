rm(list=ls())

# Set Working Directory
setwd("C:/")

# Install required packages
requiredPackages <- c("ggplot2","plyr","dplyr","corrplot","caret","Boruta","randomForest",
                      "naniar","plotly","questionr","glmnet", "kernlab","Metrics","arules")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))
}

# Load required libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(Boruta)
library(randomForest)
library(naniar)
library(questionr)
library(glmnet)
library(kernlab)
library(Metrics)
library(arules)

##### LOADING THE DATA
house <- read.csv("train.csv", stringsAsFactors = FALSE)

# Structure of data
str(house)

# Getting rid of id
house$Id <- NULL

#########################################################################
#####                DATA CLEANING AND EXPLORATION                  #####
#########################################################################

# Look at Sale Price
ggplot(data = house, aes(x=SalePrice)) +
  geom_histogram(fill="blue", bins = 50, col = I("blue"))
# Sale price is right skewed, suggest to log transform

##### HANDLING MISSING DATA #####

#Visualise missing data
house[,colnames(house)[apply(house, 2, anyNA)]] %>% vis_miss(sort_miss = TRUE)

#Look at NAs
freq.na(house)

# Remove columns with a lot of NAs if they exist
house <- house %>% select(-intersect(names(house), c("Alley", "Fence", "MiscFeature","FireplaceQu", "PoolQC")))

# Remove columns with no predictive power if they exist
house <- house %>% select(-intersect(names(house), c("Street", "LandContour", "LandSlope", "Condition1", "Condition2", "RoofMatl", "Heating", "Electrical", "Functional")))

# Study summary of data
summary(house)

#Fill the following variables with 'None' in their missing values as they are qualitative features and their null values likely indicates no presence of feature
cols_to_fill_none <- c('GarageFinish', 'GarageQual', 'GarageCond', 'GarageType',
                       'BsmtCond', 'BsmtExposure', 'BsmtQual', 'BsmtFinType2',
                       'BsmtFinType1', 'MasVnrType')

for (col in cols_to_fill_none) {
  if (col %in% colnames(house)) {
    house[[col]][is.na(house[[col]])] <- "None"
  }
}

#Fill the following variables with '0' in their missing values as they are quantitative features and their null values likely indicates no presence of feature
# Columns to replace NAs with 0s
cols_to_fill_zero <- c('MasVnrArea', 'BsmtFullBath', 'BsmtHalfBath',
                       'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
                       'TotalBsmtSF', 'GarageCars', 'GarageArea')


for (col in cols_to_fill_zero) {
  if (col %in% colnames(house)) {
    house[[col]][is.na(house[[col]])] <- 0
  }
}


#Fill the following variable with the year the house is built, assuming garage is built with the house
# GarageYrBlt: Replace with year house is built
house$GarageYrBlt[is.na(house$GarageYrBlt)] <- house$YearBuilt[is.na(house$GarageYrBlt)]

#Fill LotFrontage with the median of their neighbourhoods, simulate results from an average household in the same neighbourhood
# LotFrontage
house <- house %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), median(LotFrontage, na.rm = TRUE), LotFrontage))

#Fill MSZoning with the mode of their neighbourhoods, simulate results from an average household in the same neighbourhood
# MSZoning
house <- house %>%
  group_by(Neighborhood) %>%
  mutate(MSZoning = ifelse(is.na(MSZoning), names(sort(table(MSZoning), decreasing = TRUE))[1], MSZoning))

#Fill the following variables with the mode of the column, simulate results from an average household in the data
# List of columns to fill with the most frequent value
cols_to_fill_mode <- c('Utilities', 'Functional', 'Exterior1st', 'Exterior2nd',
                       'Electrical', 'KitchenQual', 'SaleType')

# Replacing missing values with mode
for (col in cols_to_fill_mode) {
  house[[col]][is.na(house[[col]])] <- names(sort(table(house[[col]]), decreasing = TRUE))[1]
}

# Confirm NAs are handled
freq.na(house)

##### Handling Ordinal/Nominal variables #####

# Convert the following to factors
cols_to_factor <- c('MSZoning', 'Street', 'Alley', 'LandContour', 'Utilities',
                    'LotConfig', 'Neighborhood', 'Condition1', 'Condition2',
                    'BldgType', 'HouseStyle', 'RoofStyle', 'RoofMatl',
                    'Exterior1st', 'Exterior2nd', 'MasVnrType', 'Foundation',
                    'Heating', 'CentralAir', 'Electrical', 'GarageType',
                    'GarageFinish', 'PavedDrive', 'Fence', 'MiscFeature',
                    'SaleType', 'SaleCondition', 'LotShape', 'LandSlope',
                    'Functional', 'MSSubClass', 'MoSold')

# Loop through each column and convert to factor
for (col in cols_to_factor) {
  if (col %in% colnames(house)) {
    house[[col]] <- as.factor(house[[col]])
  }
}

# Convert the following to Ordinal Variables
# BsmtExposure
house$BsmtExposure <- as.numeric(revalue(house$BsmtExposure, c("None"=0, "No"=1, "Mn"=2, "Av"=3, "Gd"=4)))
# BsmtFinType1
house$BsmtFinType1 <- as.numeric(revalue(house$BsmtFinType1, c("None"=0, "Unf"=1, "LwQ"=2, "Rec"=3, "BLQ"=4, "ALQ"=5, "GLQ"=6)))
# BsmtFinType2
house$BsmtFinType2 <- as.numeric(revalue(house$BsmtFinType2, c("None"=0, "Unf"=1, "LwQ"=2, "Rec"=3, "BLQ"=4, "ALQ"=5, "GLQ"=6)))
# ExterQual
house$ExterQual <- as.numeric(revalue(house$ExterQual, c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)))
# ExterCond
house$ExterCond <- as.numeric(revalue(house$ExterCond, c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)))
# BsmtQual
house$BsmtQual <- as.numeric(revalue(house$BsmtQual, c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)))
# BsmtCond
house$BsmtCond <- as.numeric(revalue(house$BsmtCond, c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)))
# HeatingQC
house$HeatingQC <- as.numeric(revalue(house$HeatingQC, c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)))
# KitchenQual
house$KitchenQual <- as.numeric(revalue(house$KitchenQual, c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)))
# GarageQual
house$GarageQual <- as.numeric(revalue(house$GarageQual, c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)))
# GarageCond
house$GarageCond <- as.numeric(revalue(house$GarageCond, c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)))


##### Handling Outliers #####
set.seed(123)
variables <- randomForest(x= house[!is.na(house$SalePrice), ! names(house) %in% c("SalePrice")], y=house$SalePrice[!is.na(house$SalePrice)], ntree=500,importance=TRUE)
varImpPlot(variables)

# We will study the some top variables for outliers
# GrLivArea
# Ensure columns are numeric vectors
gr_liv_area <- as.numeric(unlist(house[!is.na(house$SalePrice), "GrLivArea"]))
sale_price <- as.numeric(unlist(house[!is.na(house$SalePrice), "SalePrice"]))

# Plot GrLivArea vs SalePrice
plot(gr_liv_area, sale_price,
     xlab = "GrLivArea", ylab = "SalePrice",
     main = "GrLivArea vs SalePrice")

# Remove outliers
house <- house %>% filter(!(GrLivArea > 4000 & SalePrice <= 300000 & !is.na(SalePrice)))

# Neighbourhood
neighbourhood <- as.factor(unlist(house[!is.na(house$SalePrice), "Neighborhood"]))
sale_price <- as.numeric(unlist(house[!is.na(house$SalePrice), "SalePrice"]))

# Plot Neighbourhood vs SalePrice
plot(neighbourhood, sale_price,
     xlab = "GrLivArea", ylab = "SalePrice",
     main = "GrLivArea vs SalePrice")
# Seems reasonable, there are no boxplots that are far from each other

#########################################################################
#####                     FEATURE ENGINEERING                      #####
#########################################################################
# Generating New Features
# Taking a look at the correlation table before feature engineering
numvar <- house %>% sapply(is.numeric) %>% which() %>% names()
correlation <- house[,numvar] %>% cor(use = "pairwise.complete.obs")
p_cor <- correlation[,"SalePrice"] %>% sort(decreasing = TRUE) %>% as.matrix()
high_cor <- p_cor %>% apply(1, function(x) abs(x) > 0.5) %>% which %>% names
correlation[high_cor, high_cor] %>% corrplot.mixed(tl.col="black", tl.pos = "lt")

# We shall try to generate new features that might potentially be useful for our model
# Create new feature total area from area variables in data
house$Totalarea <- house$GrLivArea + house$TotalBsmtSF
# Remove features used to avoid colinearity
house <- house[, !(names(house) %in% c("GrLivArea", "TotalBsmtSF"))]

# Create new feature average year the house is built based on remodelling and building a garage as these affects the quality of house
house$avgYear <- (house$YearBuilt + house$GarageYrBlt + house$YearRemodAdd) / 3
# Remove features used to avoid colinearity
house <- house[, !(names(house) %in% c("YearBuilt", "GarageYrBlt","YearRemodAdd"))]

# Taking a look at the correlation table after feature engineering
numvar <- house %>% sapply(is.numeric) %>% which() %>% names()
correlation <- house[,numvar] %>% cor(use = "pairwise.complete.obs")
p_cor <- correlation[,"SalePrice"] %>% sort(decreasing = TRUE) %>% as.matrix()
high_cor <- p_cor %>% apply(1, function(x) abs(x) > 0.5) %>% which %>% names
correlation[high_cor, high_cor] %>% corrplot.mixed(tl.col="black", tl.pos = "lt")
# Our new features have improved correlation with saleprice, with Total Area at 0.83 and Avg Year at 0.57

#check the final proportion of the SalePrice
prop.table(table(house$SalePrice))

#########################################################################
#####                Create train and test datasets                 #####
#########################################################################
set.seed(12345)

# Create train and test datasets with 80% use for training
train_sample <- createDataPartition(house$SalePrice, p = 0.80, list = FALSE)
house_train <- house[train_sample, ]  # Form the training dataset; 80 percent
house_test  <- house[-train_sample, ] # Form the test dataset; 20 percent

# Combine train and test datasets with a label
house_train$dataset <- "Train"
house_test$dataset <- "Test"
combined_data <- rbind(house_train, house_test)

# Plot the distribution, to confirm that distribution across train and test datasets are the same
ggplot(combined_data, aes(x = SalePrice, fill = dataset)) +
  geom_histogram(position = "dodge", binwidth = 50000) +
  labs(title = "Distribution of SalePrice in Train and Test Datasets",
       x = "SalePrice", y = "Count") +
  theme_minimal()

# Study the summary statistics across both train and test datasets
# Summary statistics for training dataset
summary(house_train$SalePrice)
# Summary statistics for test dataset
summary(house_test$SalePrice)

#########################################################################
#####                       FEATURE SELECTION                       #####
#########################################################################
# We will use boruta to determine the important variables

boruta <- Boruta(SalePrice ~ ., data = house_train, doTrace = 2, maxRuns = 500)
print(boruta)
plot(boruta)

# Take top variables from boruta
getConfirmedFormula(boruta)

# Run top variables from boruta to determine the top 20 variables using random forest VarImpPlot
house.rf=randomForest(getConfirmedFormula(boruta),data=house_train,mtry=2,ntree=1000,importance=TRUE)
#variable importance
varImpPlot(house.rf)

#We will run all our models on the top 20 variables from the VarImpPlot result
importance_df <- data.frame(
  Variable = c("Totalarea", "OverallQual", "Neighborhood","X1stFlrSF","GarageCars", 
               "GarageArea", "ExterQual", "avgYear", "KitchenQual","BsmtQual", 
               "BsmtFinSF1",  "LotArea", "FullBath", "TotRmsAbvGrd", "GarageFinish", 
               "Fireplaces", "OpenPorchSF", "LotFrontage", "MasVnrArea", "MSSubClass")
)

top_20_variables <- importance_df$Variable

# Our training dataset will be set to only these 20 variables
house_train_top20 <- house_train[, c(top_20_variables, "SalePrice")]

head(house_train_top20)

#########################################################################
##### Model Testing    --   Multiple Linear Regression              #####
#########################################################################
# Now, we will try our model with multiple linear regression
# Define train control with cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Linear Regression, with 5 fold cross-validation (No tuning required)
model_lm <- train(SalePrice ~., house_train_top20, method = "lm", trControl = train_control)

# Ridge Regression with hyperparameter tuning, with 5 fold cross-validation
model_ridge <- train(
  SalePrice ~., house_train_top20, method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 10, length = 100)),
  trControl = train_control
)

# Lasso Regression with hyperparameter tuning, with 5 fold cross-validation
model_lasso <- train(
  SalePrice ~., house_train_top20, method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 10, length = 100)),
  trControl = train_control
)

# Evaluate on test data
pred_lm <- predict(model_lm, house_test)
pred_ridge <- predict(model_ridge, house_test)
pred_lasso <- predict(model_lasso, house_test)

# Calculate R-squared values
r2_lm <- R2(pred_lm, house_test$SalePrice)
r2_ridge <- R2(pred_ridge, house_test$SalePrice)
r2_lasso <- R2(pred_lasso, house_test$SalePrice)

################### Evaluation for Linear Models #######################
## Plot RMSE and residuals for linear regression
# Calculate RMSE value
houseRMSE <- RMSE(pred_lm, house_test$SalePrice)

print(paste("RMSE:", houseRMSE))

plot(house_test$SalePrice, pred_lm,
     main = "Actual VS Predicted Sale Price (Linear Regression)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price")
# Add a reference line
abline(a = 0,b = 1,col = "red")
# Add a legend next to the plot
legend("topright", paste("RMSE:", round(houseRMSE, digits = 2)),
       bty = "n")

# Plot residuals
house_r <- house_test$SalePrice - pred_lm
plot(house_r, main = "Residuals Plot",
     xlab = "number",
     ylab = "Residual")
# Add a reference line at y=0
abline(h = 0, col = "red")

# Calculate MAE value
house_mae <- MAE(house_test$SalePrice, pred_lm)
print(paste("MAE:", house_mae))

## Plot RMSE and residuals for ridge regression
# Calculate RMSE value
houseRMSE <- RMSE(pred_ridge, house_test$SalePrice)

print(paste("RMSE:", houseRMSE))

plot(house_test$SalePrice, pred_ridge,
     main = "Actual VS Predicted Sale Price (Ridge Regression)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price")
# Add a reference line
abline(a = 0,b = 1,col = "red")
# Add a legend next to the plot
legend("topright", paste("RMSE:", round(houseRMSE, digits = 2)),
       bty = "n")

# Plot residuals
house_r <- house_test$SalePrice - pred_ridge
plot(house_r, main = "Residuals Plot",
     xlab = "number",
     ylab = "Residual")
# Add a reference line at y=0
abline(h = 0, col = "red")

# Calculate MAE value
house_mae <- MAE(house_test$SalePrice, pred_ridge)
print(paste("MAE:", house_mae))

## Plot RMSE and residuals for lasso regression
# Calculate RMSE value
houseRMSE <- RMSE(pred_lasso, house_test$SalePrice)

print(paste("RMSE:", houseRMSE))

plot(house_test$SalePrice, pred_lasso,
     main = "Actual VS Predicted Sale Price (Lasso Regression)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price")
# Add a reference line
abline(a = 0,b = 1,col = "red")
# Add a legend next to the plot
legend("topright", paste("RMSE:", round(houseRMSE, digits = 2)),
       bty = "n")

# Plot residuals
house_r <- house_test$SalePrice - pred_lasso
plot(house_r, main = "Residuals Plot",
     xlab = "number",
     ylab = "Residual")
# Add a reference line at y=0
abline(h = 0, col = "red")

# Calculate MAE value
house_mae <- MAE(house_test$SalePrice, pred_lasso)
print(paste("MAE:", house_mae))

# Number of observations
n <- nrow(house_test)

# Number of predictors (features) in the training data
p <- ncol(house_train_top20)

# Calculate Adjusted R-squared values
adj_r2_lm <- 1 - ((1 - r2_lm) * (n - 1) / (n - p - 1))
adj_r2_ridge <- 1 - ((1 - r2_ridge) * (n - 1) / (n - p - 1))
adj_r2_lasso <- 1 - ((1 - r2_lasso) * (n - 1) / (n - p - 1))

# Collect results into a data frame
lmresults <- data.frame(
  Model = c("Linear Regression", "Ridge", "Lasso"),
  RMSE = c(RMSE(pred_lm, house_test$SalePrice), RMSE(pred_ridge, house_test$SalePrice), RMSE(pred_lasso, house_test$SalePrice)),
  MAE = c(MAE(house_test$SalePrice, pred_lm),MAE(house_test$SalePrice, pred_ridge),MAE(house_test$SalePrice, pred_lasso)),
  R2 = c(r2_lm, r2_ridge, r2_lasso),
  Adjusted_R2 = c(adj_r2_lm, adj_r2_ridge, adj_r2_lasso)
)

print(lmresults)

#########################################################################
##### Model Testing    --    SVM                                    #####
#########################################################################
# Building the model
set.seed(123)

# Try with linear kernel
house_classifier <- ksvm(SalePrice ~ .,
                         data = house_train_top20,
                         kernel = "vanilladot")  # linear
house_classifier

################### Evaluation for Linear Kernal Model #######################
pred_linear <- predict(house_classifier, house_test)
head(pred_linear)

table(pred_linear, house_test$SalePrice)

# Plot a density distribution
ggplot(house_test, aes(x = house_test$SalePrice)) +
  geom_density(aes(color="Actual"),alpha = 0.5)+
  geom_density(aes(x = pred_linear, color = "Predicted"), alpha = 0.5)+
  labs(title = "Distribution of Actual vs Predicted Sale Prices",
       subtitle = "Density Plot",
       x = "Price",
       y = "Density",
       color = "Distribution Type")+
  scale_color_manual( values = c("Actual"="blue", "Predicted"="red"))+
  theme_minimal()+
  theme(legend.position = "bottom")

## Plot RMSE
# Calculate RMSE value
houseRMSE <- RMSE(pred_linear, house_test$SalePrice)

print(paste("RMSE:", houseRMSE))

plot(house_test$SalePrice, pred_linear,
     main = "Actual VS Predicted Sale Price (Linear Kernal)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price")
# Add a reference line
abline(a = 0,b = 1,col = "red")
# Add a legend next to the plot
legend("topright", paste("RMSE:", round(houseRMSE, digits = 2)),
       bty = "n")

# Plot residuals
house_r <- house_test$SalePrice - pred_linear
plot(house_r, main = "Residuals Plot",
     xlab = "number",
     ylab = "Residual")
# Add a reference line at y=0
abline(h = 0, col = "red")

# Calculate MAE value
house_mae <- MAE(house_test$SalePrice, pred_linear)
print(paste("MAE:", house_mae))

#################### Improving the model #######################
# Using different kernels

##### Try radial kernel #####
house_classifier2 <- ksvm(SalePrice ~ .,
                          data = house_train_top20,
                          kernel = "rbfdot")

pred_radial <- predict(house_classifier2, house_test)


################### Evaluation for Radial Kernal Model #######################
# Calculate RMSE value
houseRMSE2 <- RMSE(pred_radial, house_test$SalePrice)

print(paste("RMSE:", houseRMSE2))

plot(house_test$SalePrice, pred_radial,
     main = "Actual VS Predicted Sale Price (Radial Basis)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price")
# Add a reference line
abline(a = 0,b = 1,col = "red")
# Add a legend next to the plot
legend("topright", paste("RMSE:", round(houseRMSE2, digits = 2)),
       bty = "n")

# Plot residuals
house_r <- house_test$SalePrice - pred_radial
plot(house_r, main = "Residuals Plot",
     xlab = "number",
     ylab = "Residual")
# Add a reference line at y=0
abline(h = 0, col = "red")

# Calculate MAE value
house_mae <- MAE(house_test$SalePrice, pred_radial)
print(paste("MAE:", house_mae))

##### Try polynomial kernel #####
house_classifier3 <- ksvm(SalePrice ~ .,
                          data = house_train_top20,
                          kernel = "polydot")

pred_poly <- predict(house_classifier3, house_test)

################### Evaluation for Polynomial Kernal Model #######################
# Calculate RMSE value
houseRMSE3 <- RMSE(pred_poly, house_test$SalePrice)

print(paste("RMSE:", houseRMSE3))

plot(house_test$SalePrice, pred_poly,
     main = "Actual VS Predicted Sale Price (Polynomial Basis)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price")
# Add a reference line
abline(a = 0,b = 1,col = "red")
# Add a legend next to the plot
legend("topright", paste("RMSE:", round(houseRMSE3, digits = 2)),
       bty = "n")

# Plot residuals
house_r <- house_test$SalePrice - pred_poly
plot(house_r, main = "Residuals Plot",
     xlab = "number",
     ylab = "Residual")
# Add a reference line at y=0
abline(h = 0, col = "red")

# Calculate MAE value
house_mae <- MAE(house_test$SalePrice, pred_poly)
print(paste("MAE:", house_mae))

##### Try hyperbolic tangent kernel #####
house_classifier4 <- ksvm(SalePrice ~ .,
                          data = house_train_top20,
                          kernel = "tanhdot")

pred_hyperbolic <- predict(house_classifier4, house_test)

################### Evaluation for hyperbolic tangent Model #######################
# Calculate RMSE value
houseRMSE4 <- RMSE(pred_hyperbolic, house_test$SalePrice)

print(paste("RMSE:", houseRMSE4))

plot(house_test$SalePrice, pred_hyperbolic,
     main = "Actual VS Predicted Sale Price (Hyperbolic Basis)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price")
# Add a reference line
abline(a = 0,b = 1,col = "red")
# Add a legend next to the plot
legend("topright", paste("RMSE:", round(houseRMSE4, digits = 2)),
       bty = "n")

# Plot residuals
house_r <- house_test$SalePrice - pred_hyperbolic
plot(house_r, main = "Residuals Plot",
     xlab = "number",
     ylab = "Residual")
# Add a reference line at y=0
abline(h = 0, col = "red")

# Calculate MAE value
house_mae <- MAE(house_test$SalePrice, pred_hyperbolic)
print(paste("MAE:", house_mae))

#Comparison across all SVM models

# Calculate R-squared values
r2_linear<- R2(pred_linear, house_test$SalePrice)
r2_radial <- R2(pred_radial, house_test$SalePrice)
r2_poly <- R2(pred_poly, house_test$SalePrice)
r2_hyperbolic <- R2(pred_hyperbolic, house_test$SalePrice)

# Number of observations
n <- nrow(house_test)

# Number of predictors (features) in the training data
p <- ncol(house_train_top20)

# Calculate Adjusted R-squared values
adj_r2_linear <- 1 - ((1 - r2_linear) * (n - 1) / (n - p - 1))
adj_r2_radial <- 1 - ((1 - r2_radial) * (n - 1) / (n - p - 1))
adj_r2_poly <- 1 - ((1 - r2_poly) * (n - 1) / (n - p - 1))
adj_r2_hyperbolic <- 1 - ((1 - r2_hyperbolic) * (n - 1) / (n - p - 1))

# Collect results into a data frame
SVMresults <- data.frame(
  Model = c("SVM-Linear", "SVM-Radial", "SVM-Polynomial", "SVM-Hyperbolic"),
  RMSE = c(RMSE(pred_linear, house_test$SalePrice), RMSE(pred_radial, house_test$SalePrice), RMSE(pred_poly, house_test$SalePrice),RMSE(pred_hyperbolic, house_test$SalePrice)),
  MAE = c(MAE(pred_linear, house_test$SalePrice),MAE(house_test$SalePrice, pred_radial),MAE(house_test$SalePrice, pred_poly),MAE(house_test$SalePrice, pred_hyperbolic)),
  R2 = c(r2_linear, r2_radial, r2_poly, r2_hyperbolic),
  Adjusted_R2 = c(adj_r2_linear, adj_r2_radial, adj_r2_poly, adj_r2_hyperbolic)
)

print(SVMresults)


#########################################################################
##### Model Testing    --    Random Forest                          #####
#########################################################################
# Now, we will try our model with randomforest
# We do a randomforest with a 5 fold cross validation 
# Define the grid of hyperparameters
tuneGrid <- expand.grid(mtry = c(3, 6, 9, 12, 15))

# Set up 5-fold cross-validation
trainControl <- trainControl(method = "cv", number = 5)

# Perform the grid search with cross-validation
rf_gridsearch_house_train_top20 <- train(SalePrice ~ .,
                                         data = house_train_top20,
                                         method = "rf",
                                         trControl = trainControl,
                                         tuneGrid = tuneGrid,
                                         ntree = 500,          # Number of trees
                                         nodesize = 5,         # Minimum size of terminal nodes
                                         importance = TRUE
)

# Print the best model and its parameters
print(rf_gridsearch_house_train_top20)

# We will use mtry=15 for our best rf model according to the results
rf_15_house_train_top20 <- randomForest(SalePrice ~ .,data = house_train_top20,mtry=15,ntree=500,importance=TRUE)

pred_rfwithcv <- predict(rf_15_house_train_top20, house_test)
# Prediction results
pred_rfwithcv
################### Evaluation for Random Forest Model #######################
## Plot RMSE
# Calculate RMSE value
rfRMSE <- RMSE(pred_rfwithcv, house_test$SalePrice)

print(paste("RMSE:", rfRMSE))

plot(house_test$SalePrice, pred_rfwithcv,
     main = "Actual VS Predicted Sale Price (Random Forest)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price")
# Add a reference line
abline(a = 0,b = 1,col = "red")
# Add a legend next to the plot
legend("topright", paste("RMSE:", round(rfRMSE, digits = 2)),
       bty = "n")

# Plot residuals
house_r <- house_test$SalePrice - pred_rfwithcv
plot(house_r, main = "Residuals Plot",
     xlab = "number",
     ylab = "Residual")
# Add a reference line at y=0
abline(h = 0, col = "red")

# Calculate MAE value
rf_mae <- MAE(house_test$SalePrice, pred_rfwithcv)
print(paste("MAE:", rf_mae))

# Calculate R-squared values
r2_rfwithcv <- R2(pred_rfwithcv, house_test$SalePrice)

# Number of observations
n <- nrow(house_test)

# Number of predictors (features) in the training data
p <- ncol(house_train_top20)

# Calculate Adjusted R-squared values
adj_r2_rfwithcv <- 1 - ((1 - r2_rfwithcv) * (n - 1) / (n - p - 1))

# Collect results into a data frame
rfresults <- data.frame(
  Model = c("Random Forest"),
  RMSE = c(RMSE(pred_rfwithcv, house_test$SalePrice)),
  MAE = c(MAE(pred_rfwithcv, house_test$SalePrice)),
  R2 = c(r2_rfwithcv),
  Adjusted_R2 = c(adj_r2_rfwithcv)
)

print(rfresults)


#########################################################################
##### Evaluating Across All Models                                  #####
#########################################################################
options("scipen"=100, "digits"=4)
# Evaluate all models in a data frame
allresults <- data.frame(
  Model = c("Linear Regression", "Ridge", "Lasso",
            "SVM-Linear", "SVM-Radial", "SVM-Polynomial", "SVM-Hyperbolic",
            "Random Forest"),
  MSE = c(mse(pred_lm, house_test$SalePrice), mse(pred_ridge, house_test$SalePrice), mse(pred_lasso, house_test$SalePrice),
          mse(pred_linear, house_test$SalePrice), mse(pred_radial, house_test$SalePrice), mse(pred_poly, house_test$SalePrice),mse(pred_hyperbolic, house_test$SalePrice),
          mse(pred_rfwithcv, house_test$SalePrice)),
  RMSE = c(RMSE(pred_lm, house_test$SalePrice), RMSE(pred_ridge, house_test$SalePrice), RMSE(pred_lasso, house_test$SalePrice),
           RMSE(pred_linear, house_test$SalePrice), RMSE(pred_radial, house_test$SalePrice), RMSE(pred_poly, house_test$SalePrice),RMSE(pred_hyperbolic, house_test$SalePrice),
           RMSE(pred_rfwithcv, house_test$SalePrice)),
  MAE = c(MAE(pred_lm,house_test$SalePrice),MAE(pred_ridge,house_test$SalePrice),MAE(pred_lasso,house_test$SalePrice),
          MAE(pred_linear, house_test$SalePrice),MAE(house_test$SalePrice, pred_radial),MAE(house_test$SalePrice, pred_poly),MAE(house_test$SalePrice, pred_hyperbolic),
          MAE(pred_rfwithcv, house_test$SalePrice)),
  R2 = c(r2_lm, r2_ridge, r2_lasso,
         r2_linear, r2_radial, r2_poly, r2_hyperbolic,
         r2_rfwithcv),
  Adjusted_R2 = c(adj_r2_lm, adj_r2_ridge, adj_r2_lasso,
                  adj_r2_linear, adj_r2_radial, adj_r2_poly, adj_r2_hyperbolic,
                  adj_r2_rfwithcv)
)

print(allresults)

#########################################################################
#####                     Association Rule Mining                   #####
#########################################################################

# Create a dataframe with selected features
armdf <- data.frame(
  house$Totalarea, house$OverallQual, house$Neighborhood, house$GarageCars,
  house$X1stFlrSF, house$GarageArea, house$avgYear, house$SalePrice
)

# Discretize all features except GarageCars
armdf_discretized <- discretizeDF(
  armdf[-4],
  default = list(method = "frequency", breaks = 3, labels = c("Low", "Medium", "High"))
)

# Retain original GarageCars as factor
armdf_discretized$GarageCars <- as.factor(armdf$house.GarageCars)

# Check the bin ranges for SalePrice
levels(cut(armdf$house.SalePrice, breaks = 3))

# Verify the structure of the discretized dataframe
str(armdf_discretized)

# Apply the Apriori algorithm with support, confidence, and maxlen parameters
rules <- apriori(armdf_discretized, parameter = list(support = 0.002, conf = 0.8, maxlen = 20))

# Subset rules where RHS contains SalePrice using grepl
SalePriceRules <- subset(rules, subset = grepl("SalePrice", labels(rhs(rules))))

# Inspect the top 50 SalePrice-related rules sorted by support
inspect(sort(SalePriceRules, by = "support")[1:50])

# Extract and analyze support, confidence, and lift values
support_values <- quality(SalePriceRules)$support
confidence_values <- quality(SalePriceRules)$confidence
lift_values <- quality(SalePriceRules)$lift

# Set thresholds for support, confidence, and lift
support_threshold <- 0.008
confidence_threshold <- 0.99
lift_threshold <- 2.9

# Filter rules based on these thresholds
filtered_SalePriceRules <- subset(SalePriceRules,
                                  support > support_threshold &
                                    confidence > confidence_threshold &
                                    lift > lift_threshold)

# Inspect the filtered rules
inspect(filtered_SalePriceRules)

# ---- Analyze High SalePrice Rules ----
rules_high <- subset(filtered_SalePriceRules, rhs %pin% "SalePrice=High")
sorted_rules_high <- sort(rules_high, by = "support", decreasing = TRUE)
inspect(head(sorted_rules_high, 10))

# Extract the LHS of the top 200 rules for high SalePrice
lhs_rules_high <- lhs(head(sorted_rules_high, 200))
lhs_components_high <- unique(unlist(as(lhs_rules_high, "list")))

# ---- Analyze Medium SalePrice Rules ----
rules_medium <- subset(filtered_SalePriceRules, rhs %pin% "SalePrice=Medium")
sorted_rules_medium <- sort(rules_medium, by = "support", decreasing = TRUE)
inspect(head(sorted_rules_medium, 10))

lhs_rules_medium <- lhs(head(sorted_rules_medium, 200))
lhs_components_medium <- unique(unlist(as(lhs_rules_medium, "list")))

# ---- Analyze Low SalePrice Rules ----
rules_low <- subset(filtered_SalePriceRules, rhs %pin% "SalePrice=Low")
sorted_rules_low <- sort(rules_low, by = "support", decreasing = TRUE)
inspect(head(sorted_rules_low, 10))

lhs_rules_low <- lhs(head(sorted_rules_low, 200))
lhs_components_low <- unique(unlist(as(lhs_rules_low, "list")))

# ---- Compare LHS Components Across Price Levels ----
high_vs_low <- setdiff(lhs_components_high, lhs_components_low)
high_vs_medium <- setdiff(lhs_components_high, lhs_components_medium)
medium_vs_low <- setdiff(lhs_components_medium, lhs_components_low)

# View comparison results
print("High vs Low:")
print(high_vs_low)
print("High vs Medium:")
print(high_vs_medium)
print("Medium vs Low:")
print(medium_vs_low)

# Features differentiating SalePrice=High with SalePrice=Medium & SalePrice=Low
# GarageArea(High), TotalArea(High), Neighborhood(NdrigHt), GarageCars(3)

