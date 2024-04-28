setwd("C:/Users/kimbe/Desktop/Repo/eco395m-data-mining-project")

library(tidyverse)
library(car)
library(rsample)
library(glmnet)
library(caret)
library(modelr)
library(randomForest)
library(xgboost)
library(quantregForest)


listing_clean = read.csv("data/listing_clean.csv")


# Remove NA
# Remove unused variables

listing_model = listing_clean %>%
  filter(price <= 1999 & price >= 30) %>%   # remove outliers at 99%
  na.omit() %>%
  select(-c(id, last_scraped, name, description, host_id, host_name, latitude, longitude, 
            neighbourhood_cleansed)) %>%
  select(price, everything()) %>%
  mutate(neighbourhood_group_cleansed = factor(neighbourhood_group_cleansed))


# Check variable correlation, remove highly correlated variables

lm_vif = lm(price ~ ., data = listing_model)
vif_values = vif(lm_vif)
print(vif_values)

listing_model = listing_model %>%
  select(-c(dist_park, dist_dt, review_scores_rating))


# Scale numerical variables

scale_var = c("host_listings_count", "accommodates", "bathrooms", "bedrooms",
              "beds", "minimum_nights", "maximum_nights", "number_of_reviews",
              "review_scores_accuracy", "review_scores_cleanliness", 
              "review_scores_checkin", "review_scores_communication",
              "review_scores_location", "review_scores_value", "days_first_review",
              "days_last_review", "len_neighborhood_overview", "len_host_about",
              "dist_empire")

listing_model = listing_model %>%
  mutate(across(all_of(scale_var), scale))



# Model 1: LASSO

# Encode categorical variables before splitting
data_encoded = model.matrix(~ . - 1, data = listing_model)
data_encoded = as.data.frame(data_encoded)

set.seed(123)
airbnb_split = initial_split(data_encoded, prop = 0.8)
airbnb_train = training(airbnb_split)
airbnb_test = testing(airbnb_split)

x_train = model.matrix(price ~ ., data = airbnb_train)
y_train = airbnb_train$price

# Choose best lambda
lasso_cv = cv.glmnet(x_train, y_train, alpha = 1, family = "gaussian")
best_lambda = lasso_cv$lambda.min

# Best model
lm_lasso = glmnet(x_train, y_train, alpha = 1, family = "gaussian", lambda = best_lambda)

x_test = model.matrix(price ~ ., data = airbnb_test)
y_test = airbnb_test$price
pred_lasso = predict(lm_lasso, newx = x_test)
rmse_lasso = sqrt(mean((y_test - pred_lasso)^2))
print(rmse_lasso)  # around 120

coeff_lasso = coef(lm_lasso, s = best_lambda)
print(coeff_lasso)



# Model 2: Random forest

set.seed(123)
airbnb_split = initial_split(listing_model, prop = 0.8)
airbnb_train = training(airbnb_split)
airbnb_test = testing(airbnb_split)

# Default model, ntree=500, mtry=13
airbnb_forest = randomForest(price ~ ., data = airbnb_train, importance = TRUE)
plot(airbnb_forest)
modelr::rmse(airbnb_forest, airbnb_test) # around 91.5

# Feature importance
varImpPlot(airbnb_forest, type=1)



# Model 3: XGBoost

sparse_matrix = model.matrix(price ~ . -1, data = airbnb_train)
dtrain = xgb.DMatrix(data = sparse_matrix, label = airbnb_train$price)

new_sparse_matrix = model.matrix(price ~ . - 1, data = airbnb_test)
dnew = xgb.DMatrix(data = new_sparse_matrix)

params = list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 5,
  subsample = 1,
  colsample_bytree = 1
)
nrounds = 500

set.seed(123)
xgb_model = xgb.train(params = params, data = dtrain, nrounds = nrounds)

pred_xgboost = predict(xgb_model, dnew)
rmse_xgboost = sqrt(mean((pred_xgboost - airbnb_test$price)^2))
print(rmse_xgboost)  # around 91

# Feature importance
importance_matrix = xgb.importance(feature_names = colnames(sparse_matrix), model = xgb_model)
xgb.plot.importance(importance_matrix)



# Model 4: Quantile random forest

x_train = airbnb_train[, -1]
y_train = airbnb_train$price
x_test = airbnb_test[, -1]

qrf_model = quantregForest(x = x_train, y = y_train)

pred_qrf = predict(qrf_model, newdata = x_test, what=0.5)

rmse_qrf = sqrt(mean((pred_qrf - airbnb_test$price)^2))
print(rmse_qrf)




##############

# XGBoost Tuning (takes too much time)

# Data replaces NA with missing
listing_model1 = listing_clean %>%
  filter(price <= 1999 & price >= 30) %>%
  select(-c(id, last_scraped, name, description, host_id, host_name, latitude, longitude, 
            neighbourhood_cleansed, dist_park, dist_dt, review_scores_rating)) %>%
  select(price, everything()) %>%
  mutate(neighbourhood_group_cleansed = factor(neighbourhood_group_cleansed)) %>%
  mutate(across(all_of(scale_var), scale))

listing_model1 = listing_model1 %>%
  mutate(across(where(is.factor), ~fct_na_value_to_level(.x, "Missing")))

listing_model1[is.na(listing_model1)] = "Missing"

set.seed(123)
airbnb_split = initial_split(listing_model1, prop = 0.8)
airbnb_train = training(airbnb_split)
airbnb_test = testing(airbnb_split)

sparse_matrix = model.matrix(price ~ . -1, data = airbnb_train)
dtrain = xgb.DMatrix(data = sparse_matrix, label = airbnb_train$price)

grid = expand.grid(
  nrounds = c(100, 200, 500),
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(3, 4, 5),
  min_child_weight = c(5, 7, 9),
  subsample = c(0.5, 0.8, 1),
  colsample_bytree = c(0.5, 0.8, 1),
  gamma = 0
)

train_control = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "none",
  allowParallel = TRUE
)

tune_model = function(param) {
  model = train(
    price ~ .,
    data = airbnb_train,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = data.frame(param),
    metric = "RMSE"
  )
  list(Model = model, BestRMSE = min(model$results$RMSE))
}

results = lapply(1:nrow(grid), function(i) tune_model(grid[i, ]))
best_model = results[[which.min(sapply(results, function(x) x$BestRMSE))]]

final_params = best_model$Model$finalModel$params
nrounds = best_model$Model$bestTune$nrounds
xgb_model_best = xgb.train(final_params, dtrain, nrounds = nrounds)



################

# Model 3: Gradient Boosting (not good)

airbnb_boost = gbm(price ~ ., data=airbnb_train, distribution = "gaussian",
                   interaction.depth=5, n.trees=1000, shrinkage=0.1)

# Number of trees vs error
gbm.perf(airbnb_boost)

# RMSE greater than random forest defaults
modelr::rmse(airbnb_boost, airbnb_test)

# Tuning Boosting model

train_control = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  summaryFunction = defaultSummary
)

gbm_grid = expand.grid(
  interaction.depth = c(1, 3, 5),  # Max depth of trees
  n.trees = c(50, 100, 150),       # Number of trees
  shrinkage = c(0.01, 0.05, 0.1),  # Learning rate
  n.minobsinnode = c(10, 20)       # Minimum number of observations in the terminal nodes
)

set.seed(123)
gbm_model = train(
  price ~ ., 
  data = training_data, 
  method = "gbm",
  trControl = train_control,
  tuneGrid = gbm_grid,
  metric = "RMSE"  # Change this if you are focusing on another metric like R-squared
)

print(gbm_model$bestTune)



##############

# CV Quantile Lasso (too unstable, doesn't work)

qr_test <- rq(price ~ ., data = airbnb_train, tau = 0.5, method = "lasso", lambda = 0.0001)

lambda_sequence <- seq(0.0001, 0.1, length = 100)
cv_errors <- numeric(length(lambda_sequence))

set.seed(123)  # for reproducibility
folds <- createFolds(airbnb_train$price, k = 5)

# Loop through each lambda value
for (i in seq_along(lambda_sequence)) {
  lambda_value <- lambda_sequence[i]
  fold_errors <- numeric(length(folds))
  
  # Perform k-fold cross-validation
  for (j in seq_along(folds)) {
    # Split the data
    test_indices <- folds[[j]]
    train_indices <- setdiff(seq_len(nrow(data_encoded)), test_indices)
    train_data <- data_encoded[train_indices, ]
    test_data <- data_encoded[test_indices, ]
    
    # Fit model on training data
    model <- rq(price ~ ., data = train_data, tau = 0.5, method = "lasso", lambda = lambda_value)
    
    # Predict on test data and calculate error (e.g., RMSE)
    predictions <- predict(model, newdata = test_data)
    fold_errors[j] <- sqrt(mean((test_data$price - predictions)^2))
  }
  
  # Calculate mean CV error for this lambda
  cv_errors[i] <- mean(fold_errors)
}

# Find the lambda that minimizes the CV error
best_lambda_index <- which.min(cv_errors)
best_lambda <- lambda_sequence[best_lambda_index]

# Print the best lambda value
print(best_lambda)


