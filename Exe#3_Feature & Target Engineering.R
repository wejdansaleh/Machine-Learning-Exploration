# Exe Feature & Target Engineering

library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics
library(visdat)   # for additional visualizations

# Feature engineering packages
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks
library(h2o)       # for resampling and model training

# h2o set-up
h2o.no_progress()  # turn off h2o progress bars
h2o.init()
# load Ames dataset
ames <- AmesHousing::make_ames()

#Rather than use a 70% stratified training split, try an 80% unstratified training split.
set.seed(123)  # for reproducibility
split  <- rsample::initial_split(ames, prop = 0.8)
ames_train  <- rsample::training(split)
ames_test   <- rsample::testing(split)

#How does your cross-validated results compare
ames.h2o <- as.h2o(ames)
split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7,    seed = 123)
train_4 <- split_2[[1]]
test_4  <- split_2[[2]]

h2o.cv <- h2o.glm(
  x = x,
  y = y,
  training_frame = ames.h2o,
  nfolds = 10  # perform 10-fold CV
)

vfold_cv(ames, v = 10)

#one-hot encode these features.
recipe(Sale_Price ~ ., data = ames_train) %>%
 step_dummy(all_nominal(), one_hot = TRUE)

# Identify three new step_xxx functions that recipes provides:
blueprint <- recipe(Sale_Price ~ ., data = ames_train) %>%
 step_nzv(all_nominal())  %>%
 step_integer(matches("Qual|Cond|QC|Qu")) %>%
 step_center(all_numeric(), -all_outcomes()) %>%
 step_scale(all_numeric(), -all_outcomes()) %>%
 step_pca(all_numeric(), -all_outcomes())

blueprint


