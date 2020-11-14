# Exe Modeling Process

# loading data + package
library(rsample)
library(caret)
library(tidyverse)
library(h2o)
# h2o set-up
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o

library(pdp)


# Load the Boston housing data set
boston <- pdp::boston

# initial dimension
dim(boston)

#Perform some exploratory data analysis on this data set
str(boston)

#be sure to assess the distribution of the target variable cmedv
ggplot(boston, aes(x=cmedv)) +
 geom_histogram(aes(y=..density..),
                binwidth=.5,
                colour="black",
                fill="white")

 geom_density(alpha=.2, fill="#FF6666")

#Split the Boston housing data into a training set and test set using a 70-30% split.
set.seed(123)
split <- initial_split(boston, strata = "cmedv", prop = 0.7)
boston_train <- training(split)
boston_test  <- testing(split)

# How many observations are in the training set and test set?

dim(boston_train)
dim(boston_test)

# Compare the distribution of cmedv between the training set and test set.
ggplot(boston_train, aes(x = cmedv)) +
 geom_line(stat = "density",
           trim = TRUE) +
 geom_line(data = boston_test,
           stat = "density",
           trim = TRUE, col = "red")


# Create a model with lm(), glm(), and caret::train()
model1 <- lm(cmedv ~ ., data = boston)
summary(model1)
model1
model2 <-  glm(cmedv ~ ., data = boston,
                          family = gaussian)
model2

lm_caret <- train(cmedv ~ ., data = boston,
                  method = "lm")
lm_caret
#How do the coefficients compare across these models?
coefficients(model1)
coefficients(model2)
coef(lm_caret$finalModel)

#How does the MSE/RMSE compare across these models?

sigma(model1)^2
sigma(model2)^2

sigma(model1)
sigma(model2)

# Which method is caret::train() using to fit a linear regression model?
#### the method is lm

#perform a 10-fold cross-validated linear regression model,
#repeated 5 times, that uses all available features to predict cmedv
# modelval <- train(  ## train function
#  cmedv ~ .,
#  boston,
#  method = "lm",
#  trControl = trainControl(
#   method = "cv",
#   number = 10,
#   verboseIter = TRUE
#
#  )
# )

boston.h2o <- as.h2o(boston)
split_2 <- h2o.splitFrame(boston.h2o, ratios = 0.7,
                          seed = 123)
train_4 <- split_2[[1]]
test_4  <- split_2[[2]]

h2o.cv <- h2o.glm(
  x = x,
  y = y,
  training_frame = boston.h2o,
  nfolds = 10  # perform 10-fold CV
)

vfold_cv(boston, v = 10)
#What is the average RMSE across all 50 model iterations?
# Specify resampling strategy
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

# Create grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 50, by = 1))

knn_fit <- train(
 cmedv ~ .,
 data = boston_train,
 method = "knn",
 trControl = cv,
 tuneGrid = hyper_grid,
 metric = "RMSE"
)
knn_fit
#Plot the distribution of the RMSE across all 50 model iterations.
ggplot(knn_fit)
#Describe the results.
###  we can see that the best model coincided with k=
# 2, which resulted in an RMSE of 6.075656.

#use a k-nearest neighbor model that executes
#a hyperparameter grid search where k ranges from 2–20.

cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)
hyper_grid <- expand.grid(k = seq(2, 20, by = 1))
knn_fit1 <- train(
  cmedv ~ .,
  data = boston_train,
  method = "knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metric = "RMSE"
)

ggplot(knn_fit1)
