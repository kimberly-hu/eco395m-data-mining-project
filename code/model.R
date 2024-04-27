setwd("C:/Users/kimbe/Desktop/Repo/eco395m-data-mining-project")

library(tidyverse)
library(rsample)
library(quantreg)
library(glmnet)
library(car)

library(rpart)
library(rpart.plot)
library(randomForest)
library(scales)
library(gbm)
library(kableExtra)
library(pdp)


listing_clean = read.csv("data/listing_clean.csv")


# Remove NA
# Remove unused variables
listing_model = listing_clean %>%
  filter(price <= 1999 & price >= 30) %>%   # remove outliers
  na.omit() %>%
  select(-c(id, last_scraped, name, description, host_id, host_name, latitude, longitude, 
            neighbourhood_group_cleansed)) %>%
  select(price, everything())


# Check variable correlation
# No VIF above 10
lm_vif = lm(price ~ ., data = listing_model)
vif_values = vif(lm_vif)
print(vif_values)


# Scale numerical variables
scale_var = c("")





# Train-test split
set.seed(9)
airbnb_split = initial_split(listing_model, prop = 0.8)
airbnb_train = training(airbnb_split)
airbnb_test = testing(airbnb_split)


# Quantile LASSO

x_train = model.matrix(price ~ ., data = airbnb_train)  # remove intercept in x
y_train = airbnb_train$price

# Lasso model with CV, choose best lambda
lm_lasso = cv.glmnet(x_train, y_train, alpha = 1, family = "gaussian")
best_lambda = lm_lasso$lambda.min

# Fit quantile regression
qr_lasso = rq(price ~ ., data = airbnb_train, tau = 0.5, method = "lasso", lambda = best_lambda)


# Predictions
predictions <- predict(qr_lasso, newdata = new_df)  # 'new_df' is the new data frame for prediction
  
