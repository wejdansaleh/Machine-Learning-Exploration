# Exe Linear Regression


# loading package, Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics

# Modeling packages
library(caret)    # for cross-validation, etc.
library(rsample)  # for sampling


# load data set
library(pdp)
boston <- pdp::boston
summary(boston)

set.seed(123)
split <- initial_split(boston, prop = 0.7, strata = "cmedv")
boston_train <- training(split)
boston_test <- testing(split)
### 1. Pick a single feature and apply simple linear regression model.
model1 <- lm(cmedv ~ crim  , data = boston_train)

#Interpret the feature’s coefficient
summary(model1)
#What is the model’s performance?
   ##**************** The estimated coefficients from our model are βˆ0= 24.15928 and βˆ1=-0.46090
   ##  we estimate that the mean selling price deccreases by -0.46090 for each additional one square foot

#How does it compare to the KNN in the last module?

set.seed(123)  # for reproducibility
(cv_model1 <- train(
 form = cmedv ~ crim,
 data = boston_train,
 method = "lm",
 trControl = trainControl(method = "cv", number = 10)
))

### 2. Pick another feature to add to the model.
model2 <- lm(cmedv ~ crim + age, data = boston_train)
summary(model2)

#Before applying the module why do you think this feature will help?

#Apply a linear regression model with the two features and compare to the simple linear model.
model2 <- lm(cmedv ~ crim + age, data = boston_train)
#Interpret the coefficients.
summary(model2)

### 3. Now apply a model that includes all the predictors.
model3 <- lm(cmedv ~ ., data = boston_train)
tidy(model3)
#How does this model compare to the previous two?
set.seed(123)  # for reproducibility
cv_model1 <- train(
        form = cmedv ~ crim,
        data = boston_train,
        method = "lm",
        trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model2 <- train(
        cmedv ~ crim + age,
        data = boston_train,
        method = "lm",
        trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model3 <- train(
        cmedv ~ .,
        data = boston_train,
        method = "lm",
        trControl = trainControl(method = "cv", number = 10)
)

summary(resamples(list(
        model1 = cv_model1,
        model2 = cv_model2,
        model3 = cv_model3
)))

