# EXE_ Logistic Regression



library(dplyr)     # for data wrangling
library(ggplot2)   # for awesome plotting
library(rsample)   # for data splitting

library(caret)
library(ROCR)
library(caret)

# load data set and spliting


#install.packages('kernlab')
 library(kernlab)
 data(spam)
 head(spam)
 nRow <- dim(spam)[1]
 nCol <- dim(spam)[2]

 ###### Adding a new column (for later)
 table(spam$type)
 table(as.integer(spam$type))
 ### nonspam is coded as 1
 ### spam is coded as 2
 spam$typeNumeric <- as.integer(spam$type) - 1
 ### nonspam as 0
 ### spam as 1
 summary(spam)

 set.seed(123)  # for reproducibility
 spam_split <- initial_split(spam, prop = .7, strata = "type")
 spam_train <- training(spam_split)
 spam_test  <- testing(spam_split)


 ###1. Pick a single feature and apply simple logistic regression model
 model1 <- glm(type ~ receive, data=spam_train, family='binomial')

 p1 <- ggplot(trainingSet, aes(receive, type)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE) +
  scale_y_continuous("email", labels = scales::dollar) +
  xlab("type") +
  ggtitle(paste("Non-transformed variables with a\n",
                "non-linear relationship."))

# Interpret the feature’s coefficient
tidy(model1)
exp(coef(model1))

# What is the model’s performance?

###2. Pick another feature to add to the model.
# Before applying the module why do you think this feature will help?
 ## I chose the receive and direct feature to predict how many people recive the spam by the direct, also both of the feature hvav the mean with more 0.05
# Apply a logistic regression model with the two features and compare to the simple linear model.
model2 <- glm(
 type ~ receive + direct ,
 family = "binomial",
 data = spam_train
)
tidy(model2)
# Interpret the coefficients
tidy(model2)
exp(coef(model2))

### 3.Now apply a model that includes all the predictors

model3 <- glm(type ~ . , data=spam_train, family='binomial')
tidy(model3)
# How does this model compare to the previous two
install.packages('e1071', dependencies=TRUE)
set.seed(123)
cv_model1 <- train(
 type ~ receive,
 data = spam_train,
 method = "glm",
 family = "binomial",
 trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model2 <- train(
 type ~ receive + direct,
 data = spam_train,
 method = "glm",
 family = "binomial",
 trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model3 <- train(
 type ~  .,
 data = trainingSet,
 method = "glm",
 family = "binomial",
 trControl = trainControl(method = "cv", number = 10)
)

# extract out of sample performance measures
summary(
 resamples(
  list(
   model1 = cv_model1,
   model2 = cv_model2,
   model3 = cv_model3
  )
 )
)$statistics$Accuracy
###4.Plot an ROC curve comparing the performance of all three models

# Compute predicted probabilities
m1_prob <- predict(cv_model1, spam_train, type = "prob")$spam
m3_prob <- predict(cv_model3, spam_train, type = "prob")$spam

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, spam_train$type) %>%
 performance(measure = "tpr", x.measure = "fpr")

perf2 <- prediction(m3_prob, spam_train$type) %>%
 performance(measure = "tpr", x.measure = "fpr")

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = "black", lty = 2)
plot(perf2, add = TRUE, col = "blue")

legend(0.8, 0.2, legend = c("cv_model1", "cv_model3"),
       col = c("black", "blue"), lty = 2:1, cex = 0.6)

### 5. Compute and interpret the following performance metrics:
pred_class <- predict(cv_model3, spam_train)
confusionMatrix(
 data = relevel(pred_class, ref = "spam"),
 reference = relevel(spam_train$type, ref = "spam")
)












