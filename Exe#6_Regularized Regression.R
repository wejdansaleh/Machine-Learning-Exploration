# Exe Regularized Regression

# Helper packages
library(recipes)   # for feature engineering
library(tidyverse) # general data munging
# Modeling packages
library(glmnet)   # for implementing regularized regression
library(caret)    # for automating the tuning process
library(rsample)  # for sampling
library(vip)
library(ISLR)
# Using the Hitters dataset from the ISLR package


data(Hitters)
set.seed(123)  # for reproducibility
Hitters = na.omit(Hitters)  ## clean dataset
split <- initial_split(Hitters, 0.7, strata = "Salary")
Hitters_train <- training(split)
Hitters_test <- testing(split)

### 1. Apply a ridge model with glmnet with Salary being the response variable.
# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept
X <- model.matrix(Salary ~ ., Hitters_train)[, -1]
# transform y with log transformation
Y <- log(Hitters_train$Salary)

ridge <- glmnet(
 x = X,
 y = Y,
 alpha = 0
)
plot(ridge, xvar = "lambda")
###
ridge <- cv.glmnet(
 x = X,
 y = Y,
 alpha = 0
)
plot(ridge)
# What is the minimum MSE?
min(ridge$cvm)
# What is the minimum MSE within 1 standard error?
ridge$lambda.1se
ridge$cvm[ridge$lambda == ridge$lambda.1se]
# What are the lambda values for these MSEs?
ridge$lambda.min


### 2. Apply a lasso model with glmnet.
lasso <- glmnet(
 x = X,
 y = Y,
 alpha = 1
)
plot(lasso, xvar = "lambda")
###
lasso <- cv.glmnet(
 x = X,
 y = Y,
 alpha = 1
)
plot(lasso)
# What is the minimum MSE?
min(lasso$cvm)
# What is the minimum MSE within 1 standard error?
lasso$cvm[lasso$lambda == lasso$lambda.1se]
# What are the lambda values for these MSEs?
lasso$lambda.min


### 3. Perform a grid search across alpha parameter values ranging between 0–1
# What is the optimal alpha and lambda values?
# What is the MSE and RMSE for this optimal model?
hyper_grid <- expand.grid(
 alpha = seq(0, 1, by = .25),
 lambda = c(0.1, 10, 100, 1000, 10000)
)
# perform resampling
set.seed(123)
cv_glmnet <- train(
 x = X,
 y = Y,
 method = "glmnet",
 preProc = c("zv", "center", "scale"),
 trControl = trainControl(method = "cv", number = 10),
 tuneLength = 10
)
# best model
cv_glmnet$results %>%
 filter(
  alpha == cv_glmnet$bestTune$alpha,
  lambda == cv_glmnet$bestTune$lambda
 )

# How does it compare to your previous models?
# predict Salary price on training data
pred <- predict(cv_glmnet, X)
# compute RMSE of transformed predicted
RMSE(exp(pred), exp(Y))

### 4. Plot the top 10 most influential features.
ggplot(cv_glmnet, num_features = 10, geom = "point")

#Do these features have positive or negative impacts on your response variable?
vip(cv_glmnet, num_features = 10, geom = "point")

