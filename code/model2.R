library(tidyverse)
library(car)
library(rsample)
library(glmnet)
library(caret)
library(modelr)
library(randomForest)
library(xgboost)
library(quantregForest)
library(pdp)
library(gridExtra)
library(ICEbox)


listing_clean = read.csv("data/listing_clean.csv")
quantile(listing_clean$price, c(0.025, 0.975))


# Remove NA
# Remove unused variables

listing_model = listing_clean %>%
  filter(price <= 790 & price >= 36) %>%   # remove outliers at 95%
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
print(rmse_lasso)  # around 84

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
rmse_forest = modelr::rmse(airbnb_forest, airbnb_test) 
print(rmse)  # 69.84506

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
print(rmse_xgboost)  # 67.43793

# Feature importance
importance_matrix = xgb.importance(feature_names = colnames(sparse_matrix), model = xgb_model)
#xgb.plot.importance(importance_matrix)

ggplot(importance_matrix, aes(x = reorder(Feature, Importance), y = Importance))+
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(x = "Importance", 
       y = "Feature", 
       title = "Feature importance in XGBoost model") +
  theme_minimal()


# Partial dependency plot

pdp1 = partial(xgb_model, pred.var = "accommodates", plot = TRUE, train = sparse_matrix)
pdp2 = partial(xgb_model, pred.var = "dist_empire", plot = TRUE, train = sparse_matrix)
pdp3 = partial(xgb_model, pred.var = "bathrooms", plot = TRUE, train = sparse_matrix)
pdp4 = partial(xgb_model, pred.var = "minimum_nights", plot = TRUE, train = sparse_matrix)

grid.arrange(pdp1, pdp2, pdp3, pdp4, nrow = 2, ncol = 2)


# ICE plot

ice1 = ice(object = xgb_model, X = sparse_matrix, y = airbnb_train$price, 
           predictor = "accommodates",frac_to_build = .1)
ice2 = ice(object = xgb_model, X = sparse_matrix, y = airbnb_train$price, 
           predictor = "dist_empire",frac_to_build = .1)
ice3 = ice(object = xgb_model, X = sparse_matrix, y = airbnb_train$price, 
           predictor = "bathrooms",frac_to_build = .1)
ice4 = ice(object = xgb_model, X = sparse_matrix, y = airbnb_train$price, 
           predictor = "minimum_nights",frac_to_build = .1)

iceplot1 = plot(ice1, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.25)
iceplot2 = plot(ice2, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.25)
iceplot3 = plot(ice3, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.25)
iceplot4 = plot(ice4, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.25)



# Model 4: Quantile random forest

x_train = airbnb_train[, -1]
y_train = airbnb_train$price
x_test = airbnb_test[, -1]

qrf_model = quantregForest(x = x_train, y = y_train)

pred_qrf = predict(qrf_model, newdata = x_test, what=0.5)

rmse_qrf = sqrt(mean((pred_qrf - airbnb_test$price)^2))
print(rmse_qrf)  # 71.27767


