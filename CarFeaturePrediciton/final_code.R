car_data <- read.csv("data/train2024.csv")
test_car_data <- read.csv("data/test2024.csv")

# Extract the 'No' column from the test data
No_column <- test_car_data$No

# Create Choice column
car_data$Choice <- ifelse(car_data$Ch1 == 1, 1,ifelse(car_data$Ch2 == 1, 2,ifelse(car_data$Ch3 == 1, 3,4)))

set.seed(123)

# Load necessary libraries
if (!require(glmnet)) {
  install.packages("glmnet")
  library(glmnet)
}

if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

# Ensure all predictors for each choice are numeric in the training data
predictor_prefixes <- c("CC", "GN", "NS", "BU", "FA", "LD", "FC", "FP", "RP", "PP", "KA", "SC", "TS", "NV", "MA", "LB", "AF", "HU", "Price")
additional_columns <- c("segmentind", "yearind", "milesind", "nightind", "pparkind", "genderind", "ageind", "educind", "regionind", "Urbind", "incomeind")

# Convert factor/character columns to numeric
for (col_prefix in c(predictor_prefixes, additional_columns)) {
  if (col_prefix %in% additional_columns) {
    if (is.factor(car_data[[col_prefix]]) || is.character(car_data[[col_prefix]])) {
      car_data[[col_prefix]] <- as.numeric(as.factor(car_data[[col_prefix]]))
    }
  } else {
    for (i in 1:3) {
      col <- paste0(col_prefix, i)
      if (is.factor(car_data[[col]]) || is.character(car_data[[col]])) {
        car_data[[col]] <- as.numeric(as.factor(car_data[[col]]))
      }
    }
  }
}

# Create the model matrix for the training data
predictor_columns <- c(paste0(rep(predictor_prefixes, each = 4), rep(1:3, length(predictor_prefixes))), additional_columns)
X <- model.matrix(as.formula(paste("~", paste(predictor_columns, collapse = "+"), "- 1")), data = car_data)
y <- factor(car_data$Choice)

# Create folds for cross-validation
set.seed(123)
folds <- createFolds(y, k = 5, list = TRUE, returnTrain = TRUE)

# Initialize a vector to store log loss for each fold
log_losses <- numeric(length(folds))

# Function to calculate log loss
log_loss <- function(actual, predicted) {
  -mean(log(predicted[cbind(seq_along(actual), actual)]))
}

# Perform cross-validation
for (i in seq_along(folds)) {
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(car_data)), train_indices)
  
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  X_test <- X[test_indices, ]
  y_test <- y[test_indices]
  
  # Fit the model on the training data
  cv_fit <- cv.glmnet(X_train, y_train, family = "multinomial", type.multinomial = "grouped", alpha = 1)
  
  # Get the best lambda value
  best_lambda <- cv_fit$lambda.min
  
  # Predict probabilities on the test data
  Final_predict <- predict(cv_fit, newx = X_test, s = best_lambda, type = "response")
  
  # Ensure Final_predict is a matrix with probabilities for each choice
  if (is.list(Final_predict)) {
    Final_predict <- do.call(rbind, Final_predict)
  }
  
  # Final_predict has dimensions [1:4997, 1:4, 1], so extract the correct slice
  Final_predict_matrix <- Final_predict[, , 1]
  
  # Calculate log loss for the current fold
  log_losses[i] <- log_loss(as.numeric(y_test), Final_predict_matrix)
}

# Print the average log loss
mean_log_loss <- mean(log_losses)
print(paste("Average Log Loss: ", mean_log_loss))

# Prepare the test data
No_column <- test_car_data$No
test_car_data$Choice <- 1  # Dummy variable for compatibility

# Ensure all predictors for each choice are numeric in the test data
for (col_prefix in c(predictor_prefixes, additional_columns)) {
  if (col_prefix %in% additional_columns) {
    if (is.factor(test_car_data[[col_prefix]]) || is.character(test_car_data[[col_prefix]])) {
      test_car_data[[col_prefix]] <- as.numeric(as.factor(test_car_data[[col_prefix]]))
    }
  } else {
    for (i in 1:4) {
      col <- paste0(col_prefix, i)
      if (is.factor(test_car_data[[col]]) || is.character(test_car_data[[col]])) {
        test_car_data[[col]] <- as.numeric(as.factor(test_car_data[[col]]))
      }
    }
  }
}

# Create the model matrix for the test data
X_test <- model.matrix(as.formula(paste("~", paste(predictor_columns, collapse = "+"), "- 1")), data = test_car_data)

# Fit the model on the entire training data using the best lambda value from cross-validation
best_model <- glmnet(X, y, family = "multinomial", type.multinomial = "grouped", alpha = 1, lambda = best_lambda)

# Predict probabilities on the test data
Final_predict <- predict(best_model, newx = X_test, type = "response")

# Ensure Final_predict is a matrix with probabilities for each choice
if (is.list(Final_predict)) {
  Final_predict <- do.call(rbind, Final_predict)
}

# Final_predict has dimensions [1:4997, 1:4, 1], so extract the correct slice
Final_predict_matrix <- Final_predict[, , 1]

# Format the results into the required format
result_glmnet <- data.frame(No = No_column,
                            Ch1 = Final_predict_matrix[, 1],
                            Ch2 = Final_predict_matrix[, 2],
                            Ch3 = Final_predict_matrix[, 3],
                            Ch4 = Final_predict_matrix[, 4])

# Print the first few rows of the result to verify
head(result_glmnet)



#------------------------
#------------------------
#------------------------
#------------------------
#------------------------

if (!require(nnet)) {
  install.packages("nnet")
  library(nnet)
}

library(nnet)
library(caret)

# Prepare the data for the neural network
train_data <- car_data

# Select columns dynamically based on prefixes and exclude certain columns as needed
predictor_prefixes <- c("CC", "GN", "NS", "BU", "FA", "LD", "RP", "PP", "KA", "TS", "MA", "LB", "HU", "Price")
additional_columns <- c("segmentind", "yearind", "milesind", "nightind", "pparkind", "genderind", "ageind", "educind", "regionind", "Urbind", "incomeind")

# Create the full list of predictor columns dynamically
predictor_columns <- c(unlist(lapply(predictor_prefixes, function(prefix) {
  paste0(prefix, 1:4)
})), additional_columns)

# Ensure all predictors for each choice are numeric in the training data
for (col_prefix in c(predictor_prefixes, additional_columns)) {
  if (col_prefix %in% additional_columns) {
    if (is.factor(car_data[[col_prefix]]) || is.character(car_data[[col_prefix]])) {
      car_data[[col_prefix]] <- as.numeric(as.factor(car_data[[col_prefix]]))
      test_car_data[[col_prefix]] <- as.numeric(as.factor(test_car_data[[col_prefix]]))
    }
  } else {
    for (i in 1:4) {
      col <- paste0(col_prefix, i)
      if (is.factor(car_data[[col]]) || is.character(car_data[[col]])) {
        car_data[[col]] <- as.numeric(as.factor(car_data[[col]]))
        test_car_data[[col]] <- as.numeric(as.factor(test_car_data[[col]]))
      }
    }
  }
}

# Ensure correct labels for the training set
train_matrix <- train_data[, predictor_columns]
train_matrix$Choice <- as.factor(train_data$Choice)

# Function to calculate log loss
log_loss <- function(actual, predicted) {
  -mean(log(predicted[cbind(seq_along(actual), actual)]))
}

# Custom function to include rang and maxit in model training
custom_train_nnet <- function(data, size, decay, maxit, rang) {
  model <- nnet(
    Choice ~ ., data = data,
    size = size,
    decay = decay,
    maxit = maxit,
    rang = rang,
    trace = FALSE,
    MaxNWts = 1000 # Increase the maximum number of weights if necessary
  )
  return(model)
}

# Perform cross-validation and calculate log loss for each fold
cross_validation <- function(data, k, params) {
  folds <- createFolds(data$Choice, k = k, list = TRUE, returnTrain = TRUE)
  results <- list()
  
  for (i in 1:nrow(params)) {
    log_losses <- c()
    
    for (fold in folds) {
      train_fold <- data[fold, ]
      test_fold <- data[-fold, ]
      
      model <- custom_train_nnet(
        data = train_fold,
        size = params$size[i],
        decay = params$decay[i],
        maxit = params$maxit[i],
        rang = params$rang[i]
      )
      
      pred <- predict(model, newdata = test_fold[, predictor_columns], type = "raw")
      actual <- as.numeric(test_fold$Choice)
      log_losses <- c(log_losses, log_loss(actual, pred))
    }
    
    results[[i]] <- list(
      model = model,
      size = params$size[i],
      decay = params$decay[i],
      maxit = params$maxit[i],
      rang = params$rang[i],
      log_loss = mean(log_losses)
    )
  }
  
  return(results)
}

# Set up the parameters to test
params <- expand.grid(
  size = c(3, 5), # Reduced number of hidden units
  decay = c(0.00001), # Weight decay parameter
  maxit = c(100, 200), # Reduced maximum number of iterations
  rang = c(0.1, 0.3) # Initial random weights range
)

# Perform cross-validation with 5 folds
set.seed(123)
cv_results <- cross_validation(train_matrix, k = 5, params = params)

# Find the best model based on log loss
best_result <- cv_results[[which.min(sapply(cv_results, function(x) x$log_loss))]]

# Print the best parameters and corresponding log loss
print(best_result)

# Prepare the test data
for (col_prefix in c(predictor_prefixes, additional_columns)) {
  if (col_prefix %in% additional_columns) {
    if (is.factor(test_car_data[[col_prefix]]) || is.character(test_car_data[[col_prefix]])) {
      test_car_data[[col_prefix]] <- as.numeric(as.factor(test_car_data[[col_prefix]]))
    }
  } else {
    for (i in 1:4) {
      col <- paste0(col_prefix, i)
      if (is.factor(test_car_data[[col]]) || is.character(test_car_data[[col]])) {
        test_car_data[[col]] <- as.numeric(as.factor(test_car_data[[col]]))
      }
    }
  }
}

test_matrix <- test_car_data[, predictor_columns]
nn_prob_predictions <- predict(best_result$model, newdata = test_matrix, type = "raw")

# Format the results into the required format
result_nn <- data.frame(
  No = test_car_data$No,
  Ch1 = nn_prob_predictions[, 1],
  Ch2 = nn_prob_predictions[, 2],
  Ch3 = nn_prob_predictions[, 3],
  Ch4 = nn_prob_predictions[, 4]
)

# Print the first few rows of the result to verify
head(result_nn)


#----------------------
#----------------------
#----------------------
#----------------------
#----------------------


# Load necessary library
if (!require(xgboost)) {
  install.packages("xgboost")
  library(xgboost)
}

# Define the response and predictors
response <- "Choice"
predictor_prefixes <- c("CC", "GN", "NS", "BU", "FA", "LD", "BZ", "FC", "FP", "RP", "PP", "KA", "SC", "TS", "NV", "MA", "LB", "AF", "HU", "Price")
additional_columns <- c("segmentind", "yearind", "milesind", "nightind", "pparkind", "genderind", "ageind", "educind", "regionind", "Urbind", "incomeind")

# Create the full list of predictor columns dynamically
predictors <- unlist(lapply(predictor_prefixes, function(prefix) {
  paste0(prefix, 1:4) # Include only 1 to 3
}))

# Add additional columns to predictors
predictors <- c(predictors, additional_columns)

# Ensure all predictors for each choice are numeric in the training data
for (col_prefix in c(predictor_prefixes, additional_columns)) {
  if (col_prefix %in% additional_columns) {
    if (is.factor(car_data[[col_prefix]]) || is.character(car_data[[col_prefix]])) {
      car_data[[col_prefix]] <- as.numeric(as.factor(car_data[[col_prefix]]))
      test_car_data[[col_prefix]] <- as.numeric(as.factor(test_car_data[[col_prefix]]))
    }
  } else {
    for (i in 1:3) {
      col <- paste0(col_prefix, i)
      if (is.factor(car_data[[col]]) || is.character(car_data[[col]])) {
        car_data[[col]] <- as.numeric(as.factor(car_data[[col]]))
        test_car_data[[col]] <- as.numeric(as.factor(test_car_data[[col]]))
      }
    }
  }
}

# Prepare the data for XGBoost
train_data <- subset(car_data, Task <= 12)
validation_data <- subset(car_data, Task > 12)

# Ensure correct labels for the training and test sets
train_matrix <- model.matrix(as.formula(paste(response, "~", paste(predictors, collapse = "+"), "- 1")), data = train_data)
train_label <- as.numeric(train_data$Choice) - 1

validation_matrix <- model.matrix(as.formula(paste(response, "~", paste(predictors, collapse = "+"), "- 1")), data = validation_data)
validation_label <- as.numeric(validation_data$Choice) - 1

# Convert to DMatrix
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dvalidation <- xgb.DMatrix(data = validation_matrix, label = validation_label)

# Set parameters
params <- list(
  objective = "multi:softprob",
  num_class = 4,
  eval_metric = "mlogloss"
)

# Train the model
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)

# Predict on the validation data
xgb_prob_predictions_validation <- predict(xgb_model, newdata = dvalidation)
xgb_prob_predictions_validation <- matrix(xgb_prob_predictions_validation, ncol = 4, byrow = TRUE)

# Function to calculate log loss
log_loss <- function(actual_choices, predicted_matrix) {
  n <- length(actual_choices)
  total <- 0
  
  for (i in 1:n) {
    total <- total + log(predicted_matrix[i, actual_choices[i]])
  }
  
  return(-total/n)
}

# Calculate and print log loss for XGBoost on validation set
xgb_log_loss <- log_loss(validation_data$Choice, xgb_prob_predictions_validation)
print(paste("XGBoost Log Loss on Validation Set: ", xgb_log_loss))

# Prepare the test data
test_matrix <- model.matrix(as.formula(paste(response, "~", paste(predictors, collapse = "+"), "- 1")), data = test_car_data)

# Convert test data to DMatrix
dtest <- xgb.DMatrix(data = test_matrix)

# Predict on the test data
xgb_prob_predictions_test <- predict(xgb_model, newdata = dtest)
xgb_prob_predictions_test <- matrix(xgb_prob_predictions_test, ncol = 4, byrow = TRUE)

# Extract the 'No' column from the test data
No_column <- test_car_data$No

# Format the results into the required format
result_xgb <- data.frame(
  No = No_column,
  Ch1 = xgb_prob_predictions_test[, 1],
  Ch2 = xgb_prob_predictions_test[, 2],
  Ch3 = xgb_prob_predictions_test[, 3],
  Ch4 = xgb_prob_predictions_test[, 4]
)


# Print the first few rows of the result to verify
head(result_xgb)

# Ensemble model
result_ensemble <- (result_xgb + result_nn + result_glmnet)/3

# Save the results to a CSV file
write.csv(result_ensemble, "submission_ensemble_best.csv", row.names = FALSE)

print(result_ensemble)

