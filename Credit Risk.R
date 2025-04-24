#########################################
#### EXPLORATORY DATA ANALYSIS ####
#########################################
####Violin Plot - Distribution of intrest Rate Across Loan Grades ####
data <- read.csv("Q:/Final.csv")
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(reshape2)
#### 1. Distribution of Loan Amounts ####
ggplot(data, aes(x = `Loan.Amount`)) +
  geom_histogram(aes(y = ..density..), binwidth = 1000, fill = 'skyblue', color = 'black', alpha = 0.5) +
  geom_density(color = 'black', size = 1.5) +
  labs(title = 'Distribution of Loan Amounts', x = 'Loan Amount', y = 'Density') +
  theme_minimal()



#### 2. Interest Rate vs Loan Amount ####
# Step 1: Stratified Sampling
set.seed(40423910)  # For reproducibility
sampled_data <- data %>%
  group_by(Grade) %>%
  sample_frac(size = 0.03)  # Adjust the size as needed (e.g., 10% of each grade)

# Step 2: Create the Scatter Plot with the Subsampled Data
ggplot(sampled_data, aes(x = `Loan.Amount`, y = `Interest.Rate`, color = Grade)) +
  geom_point(alpha = 0.7) +
  labs(title = 'Interest Rate vs Loan Amount', x = 'Loan Amount', y = 'Interest Rate') +
  theme_minimal()
#### 3. Sunburst Plot of Home Ownership, Grade, and Loan Status ####
# Create a hierarchical dataset for the Sunburst plot
sunburst_data <- data %>%
  group_by(Home.Ownership, Grade, Loan.Status) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup()
# Create the hierarchical structure for the Sunburst plot
sunburst_data$parent <- paste(sunburst_data$Home.Ownership, sunburst_data$Grade, sep = " - ")
sunburst_data$label <- paste(sunburst_data$parent, sunburst_data$Loan.Status, sep = " - ")

# Prepare the data for the Sunburst plot
sunburst_plot_data <- bind_rows(
  # Adding the top-level (Home Ownership)
  sunburst_data %>% 
    group_by(Home.Ownership) %>% 
    summarise(count = sum(count)) %>%
    mutate(parent = "", label = Home.Ownership),
  # Adding the second-level (Grade under Home Ownership)
  sunburst_data %>%
    group_by(Home.Ownership, Grade) %>%
    summarise(count = sum(count)) %>%
    mutate(parent = Home.Ownership, label = paste(Home.Ownership, Grade, sep = " - ")),
  
  # Adding the third-level (Loan Status under Grade)
  sunburst_data
)
# Create the Sunburst plot
fig <- plot_ly(
  labels = sunburst_plot_data$label,
  parents = sunburst_plot_data$parent,
  values = sunburst_plot_data$count,
  type = 'sunburst',
  branchvalues = 'total'
)
# Display the plot
fig <- fig %>%
  layout(title = "Sunburst Plot of Home Ownership, Grade, and Loan Status",
         margin = list(l = 0, r = 0, b = 0, t = 30))
fig
#### 4.Comparison of Scaled Loan Metrics Across Interest Rate Bins #####

# Bin the Interest Rate into 10 bins
data$Interest.Rate.Bins <- cut(data$Interest.Rate, breaks = 10)

# Calculate mean values for each bin
trend_data_extended <- data %>%
  group_by(Interest.Rate.Bins) %>%
  summarise(
    Loan.Amount = mean(Loan.Amount, na.rm = TRUE),
    Total.Received.Interest = mean(Total.Received.Interest, na.rm = TRUE),
    Total.Revolving.Credit.Limit = mean(Total.Revolving.Credit.Limit, na.rm = TRUE)
  )
# Normalize the variables for better comparison
trend_data_extended <- trend_data_extended %>%
  mutate(
    Loan.Amount.Scaled = Loan.Amount / max(Loan.Amount),
    Total.Received.Interest.Scaled = Total.Received.Interest / max(Total.Received.Interest),
    Total.Revolving.Credit.Limit.Scaled = Total.Revolving.Credit.Limit / max(Total.Revolving.Credit.Limit)
  )
# Create the plot with normalization
ggplot(trend_data_extended, aes(x = as.factor(Interest.Rate.Bins))) +
  geom_line(aes(y = Loan.Amount.Scaled, group = 1, color = "Loan Amount"), size = 1.2) +
  geom_line(aes(y = Total.Received.Interest.Scaled, group = 1, color = "Total Received Interest"), size = 1.2) +
  geom_line(aes(y = Total.Revolving.Credit.Limit.Scaled, group = 1, color = "Total Revolving Credit Limit"), size = 1.2) +
  labs(x = "Interest Rate Bins", y = "Scaled Values", title = "Comparison of Scaled Loan Metrics Across Interest Rate Bins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(name = "Metrics", 
                     values = c("Loan Amount" = "blue", "Total Received Interest" = "green", "Total Revolving Credit Limit" = "red"))
#### 5. Loan Amount Distribution by Grade ####
ggplot(data, aes(x = Grade, y = `Loan.Amount`, fill = Grade)) +
  geom_boxplot() +
  labs(title = 'Loan Amount Distribution by Grade', x = 'Grade', y = 'Loan Amount') +
  theme_minimal()
#### 6. Recoveries vs Loan Amount ###
ggplot(data, aes(x = `Loan.Amount`, y = Recoveries, color = factor(`Loan.Status`))) +
  geom_point(alpha = 0.7) +
  labs(title = 'Recoveries vs Loan Amount', x = 'Loan Amount', y = 'Recoveries') +
  scale_color_manual(values = c("red", "blue")) + # Adjust colors as needed
  theme_minimal()
######################
#### DATA QUALITY ####
######################
# Load necessary libraries
library(dplyr)

# Load dataset
data <- train

# Define the numerical columns to check for outliers
numerical_cols <- c('Loan Amount', 'Funded Amount', 'Interest Rate', 'Debit to Income', 
                    'Delinquency - two years', 'Revolving Utilities', 'Open Account', 
                    'Public Record', 'Revolving Balance', 'Total Accounts', 
                    'Total Collection Amount', 'Total Current Balance', 
                    'Total Revolving Credit Limit')
# Calculate Z-scores for the numerical columns
z_scores <- data %>% select(all_of(numerical_cols)) %>% mutate(across(everything(), scale))

# Define a threshold for identifying outliers (commonly 3)
outliers <- rowSums(abs(z_scores) > 3) == 0
# Filter out the outliers
data_clean <- data[outliers, ]
# Display the cleaned data
head(data_clean)
write.csv(data_clean, "final.csv", row.names = FALSE)




#####################################################
#### FEATURE SELECTION - LASSO REGRESSION ####
#####################################################
# Load necessary libraries
library(glmnet)
# Load the data
data <- train
# Remove "Payment Plan" column if present
if ("Payment.Plan" %in% names(data)) {
  data <- data[, !names(data) %in% "Payment.Plan"]
}
# Convert categorical variables to factors
data$`Batch Enrolled` <- as.factor(data$`Batch Enrolled`)
data$Grade <- as.factor(data$Grade)
data$`Sub Grade` <- as.factor(data$`Sub Grade`)
data$`Loan Title` <- as.factor(data$`Loan Title`)
data$`Application Type` <- as.factor(data$`Application Type`)
data$`Initial List Status` <- as.factor(data$`Initial List Status`)
data$`Loan Status` <- as.factor(data$`Loan Status`)
data$`Verification Status` <- as.factor(data$`Verification Status`) 
data$`Payment Plan` <- as.factor(data$`Payment Plan`)
# Remove factors with only one level
data <- data[, sapply(data, function(col) !(is.factor(col) && length(levels(col)) < 2))]
single_level_factors <- sapply(data, function(col) is.factor(col) && length(levels(col)) < 2)
single_level_factors
# Separate features and target variable
X <- model.matrix(`Loan Status` ~ . - ID - `Loan Status`, data = data)[, -1]
y <- data$`Loan Status`
# Split the data into training and testing sets
set.seed(40423910)
train_index <- sample(1:nrow(X), size = 0.8 * nrow(X))
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]
# Apply Lasso regression
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1, family = "binomial")
# Get the coefficients of the features
lasso_coefficients <- coef(lasso_model, s = "lambda.min")
important_features <- rownames(lasso_coefficients)[which(lasso_coefficients != 0)]
# Display the important features
print(important_features)
####################
##### SMOTE #######
####################
# Load necessary libraries
library(smotefamily)
library(ggplot2)
library(dplyr) # For data manipulation
library(caret) # For dummyVars function
# Load the dataset
df <- read_csv("Q:/final.csv")
# Print column names to confirm the correct name
print(colnames(df))
# Assign the correct column name directly
loan_status_column <- "Loan Status"
# Convert 'Loan Status' to factor
df[[loan_status_column]] <- as.factor(df[[loan_status_column]])
# Identify and separate categorical columns
categorical_cols <- sapply(df, is.factor) | sapply(df, is.character)
categorical_cols <- names(df)[categorical_cols]
# Preserve the original format of categorical columns
original_categoricals <- df[categorical_cols]
# Remove categorical columns from the dataframe for SMOTE processing
df_numeric <- df[ , !(names(df) %in% categorical_cols)]
# Apply one-hot encoding
df_encoded <- dummyVars(~ ., data = df_numeric, fullRank = TRUE)
df_transformed <- predict(df_encoded, newdata = df_numeric)
# Convert the transformed data to a dataframe
df_transformed <- as.data.frame(df_transformed)
# Apply SMOTE to balance the dataset, excluding the Loan Status column
smote_result <- SMOTE(df_transformed, df[[loan_status_column]], K = 5, dup_size = 0)
# Combine the SMOTE result into a new dataframe
smote_data <- data.frame(smote_result$data, stringsAsFactors = FALSE)
smote_data[[loan_status_column]] <- as.factor(smote_data$class)
smote_data$class <- NULL
# Add categorical columns back to the SMOTE data
original_categoricals <- original_categoricals[rep(1:nrow(original_categoricals), length.out = nrow(smote_data)), ]
original_categoricals <- original_categoricals[, !colnames(original_categoricals) %in% colnames(smote_data)] # Remove any columns that already exist in smote_data
smote_data_original <- cbind(original_categoricals, smote_data)
# Plot the distribution of loan defaulters in the original data
original_distribution <- ggplot(df, aes(x = `Loan Status`)) +
  geom_bar() +
  ggtitle("Original Distribution of Loan Defaulters") +
  theme_minimal()
# Plot the distribution of loan defaulters in the SMOTE data
smote_distribution <- ggplot(smote_data_original, aes(x = `Loan Status`)) +
  geom_bar() +
  ggtitle("SMOTE Distribution of Loan Defaulters") +
  theme_minimal()
# Display the plots
print(original_distribution)
print(smote_distribution)
# Write the SMOTE-processed dataset to a CSV file
write_csv(smote_data_original, "SMOTE.csv")

######################
#### BASE MODEL ####
######################
#### LOGISTIC REGRESSION ####
# Load necessary libraries
library(pROC)
library(dplyr)
library(caret)

# Load dataset
data <- read.csv("Q:/SMOTE.csv")
# Define the selected features with backticks for spaces
selected_features <- c(
  'Grade', 'Sub.Grade', 'Delinquency...two.years',
  'Open.Account', 'Public.Record', 'Initial.List.Status', 'Total.Received.Late.Fee',
  'Last.week.Pay', 'Total.Collection.Amount', 'Total.Current.Balance', 'Home.Ownership'
)
# Remove "Payment.Plan" column if present
if ("Payment.Plan" %in% names(data)) {
  data <- data[, !names(data) %in% "Payment.Plan"]
}
# Convert categorical variables to factors
data$Grade <- as.factor(data$Grade)
data$Sub.Grade <- as.factor(data$Sub.Grade)
data$Application.Type <- as.factor(data$Application.Type)
data$Initial.List.Status <- as.factor(data$Initial.List.Status)
data$Loan.Status <- as.factor(data$`Loan.Status`)
data$Home.Ownership <- as.factor(data$Home.Ownership)

# Stratify and split the data into training and testing sets
set.seed(40423910) # For reproducibility
trainIndex <- createDataPartition(data$`Loan.Status`, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data[trainIndex,]
data_test  <- data[-trainIndex,]
# Check stratification
cat("Original Data Distribution:\n")
print(prop.table(table(data$`Loan.Status`)))
cat("Training Data Distribution:\n")
print(prop.table(table(data_train$`Loan.Status`)))
cat("Testing Data Distribution:\n")
print(prop.table(table(data_test$`Loan.Status`)))
# Perform logistic regression
formula <- as.formula(paste("`Loan.Status` ~", paste(selected_features, collapse = " + ")))
logistic_model <- glm(formula, data = data_train, family = binomial)
# Summary of the logistic regression model
model_summary <- summary(logistic_model)

# Make predictions on the test set
predictions <- predict(logistic_model, newdata = data_test, type = "response")
# Convert predictions to binary outcome based on a threshold (e.g., 0.5)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
# Evaluate the model
conf_matrix <- confusionMatrix(factor(predicted_classes), factor(data_test$`Loan.Status`))
# Calculate ROC and AUC
roc_obj <- roc(data_test$`Loan.Status`, predictions)
auc_value <- auc(roc_obj)
# Calculate Precision and Recall
precision <- conf_matrix$byClass['Positive Predictive Value']
recall <- conf_matrix$byClass['Sensitivity']  # Recall is the same as Sensitivity
# Save accuracy, sensitivity, specificity, precision, recall, and AUC
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
# Save the metrics as a list
logistic_metrics <- list(
  model_summary = summary(logistic_model),
  accuracy = accuracy,
  sensitivity = sensitivity,
  specificity = specificity,
  precision = precision,
  recall = recall,
  auc = auc_value,
  roc_curve = roc_obj,
  confusion_matrix = conf_matrix
)
# Print the metrics
print(logistic_metrics)
# Save the list to a file for later use
save(logistic_metrics, file = "logistic_metrics.RData")
# Alternatively, save as a CSV if only specific components are needed
metrics_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
  Value = c(accuracy, sensitivity, specificity, auc_value)
)
write.csv(metrics_df, "logistic_metrics.csv", row.names = FALSE)
# Save ROC curve details (FPR, TPR) to a CSV for later reproduction
roc_details <- data.frame(
  Threshold = roc_obj$thresholds,
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)
write.csv(roc_details, "logistic_roc_details.csv", row.names = FALSE)
# Plot and save the ROC curve
roc_plot_file <- "ROC_Curve_Logistic_Regression.png"
png(roc_plot_file)
plot(roc_obj, main = "ROC Curve for Logistic Regression")
dev.off()
# Output to confirm ROC plot is saved
cat("ROC curve has been saved as:", roc_plot_file, "\n")
cat("ROC curve details have been saved as: roc_curve_details.csv\n")

#### RANDOMFOREST ####
# Load necessary libraries
library(randomForest)
# Stratify and split the data into training and testing sets
set.seed(40423910) # For reproducibility
trainIndex <- createDataPartition(data$Loan.Status, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data[trainIndex,]
data_test  <- data[-trainIndex,]

# Check stratification
cat("Original Data Distribution:\n")
print(prop.table(table(data$Loan.Status)))
cat("Training Data Distribution:\n")
print(prop.table(table(data_train$Loan.Status)))
cat("Testing Data Distribution:\n")
print(prop.table(table(data_test$Loan.Status)))
# Check the number of levels for each factor variable
num_levels <- sapply(data_train[, selected_features], function(x) if(is.factor(x)) length(levels(x)) else NA)
print(num_levels)
# Build Random Forest model
rf_model <- randomForest(as.formula(paste("Loan.Status ~", paste(selected_features, collapse = " + "))),
                         data = data_train, importance = TRUE, ntree = 500)

# Summary of the Random Forest model
print(rf_model)
# Make predictions on the test set (probabilities)
rf_probabilities <- predict(rf_model, newdata = data_test, type = "prob")

# Convert probabilities to binary class predictions
rf_predictions <- ifelse(rf_probabilities[,2] > 0.5, "1", "0")
# Evaluate the model
conf_matrix <- confusionMatrix(factor(rf_predictions), factor(data_test$Loan.Status))
# Calculate ROC and AUC
roc_obj <- roc(data_test$Loan.Status, rf_probabilities[,2])
auc_value <- auc(roc_obj)
# Save accuracy, sensitivity, specificity, and AUC
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
# Save the metrics as a list
rf_metrics <- list(
  model_summary = rf_model,
  accuracy = accuracy,
  sensitivity = sensitivity,
  specificity = specificity,
  auc = auc_value,
  roc_curve = roc_obj,
  confusion_matrix = conf_matrix
)
# Save the list to a file for later use
save(rf_metrics, file = "rf_metrics.RData")
# Save metrics to a CSV file
metrics_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
  Value = c(accuracy, sensitivity, specificity, auc_value)
)
write.csv(metrics_df, "rf_metrics.csv", row.names = FALSE)
# Save ROC curve details (FPR, TPR) to a CSV for later reproduction
roc_details <- data.frame(
  Threshold = roc_obj$thresholds,
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)
write.csv(roc_details, "rf_roc_details.csv", row.names = FALSE)
# Plot and save the ROC curve
roc_plot_file <- "ROC_Curve_Random_Forest.png"
png(roc_plot_file)
plot(roc_obj, main = "ROC Curve for Random Forest")
dev.off()

# Output to confirm ROC plot is saved
cat("ROC curve has been saved as:", roc_plot_file, "\n")
cat("ROC curve details have been saved as: rf_roc_details.csv\n")
#### Tree Visual ####
# Load necessary libraries for tree visualization
library(rpart)
library(rattle)
library(rpart.plot)
# Extract and plot a single tree from the Random Forest model
# Select a specific tree, for example, the first tree
single_tree <- getTree(rf_model, k = 1, labelVar = TRUE)
# Convert the tree to a decision tree object
tree_model <- rpart(as.formula(paste("Loan.Status ~", paste(selected_features, collapse = " + "))),
                    data = data_train,
                    method = "class",
                    control = rpart.control(cp = 0, maxdepth = 4))

# Visualize the single decision tree
fancyRpartPlot(tree_model)


#### XGBOOST  ####
# Load necessary libraries
library(xgboost)
# Stratify and split the data into training and testing sets
set.seed(40423910) # For reproducibility
trainIndex <- createDataPartition(data$Loan.Status, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data[trainIndex,]
data_test  <- data[-trainIndex,]
# Check stratification
cat("Original Data Distribution:\n")
print(prop.table(table(data$Loan.Status)))
cat("Training Data Distribution:\n")
print(prop.table(table(data_train$Loan.Status)))
cat("Testing Data Distribution:\n")
print(prop.table(table(data_test$Loan.Status)))
# Convert data to matrix format for XGBoost
train_matrix <- model.matrix(~ . - 1, data_train[, selected_features])
train_label <- as.numeric(data_train$Loan.Status) - 1  # XGBoost requires numeric labels starting from 0

test_matrix <- model.matrix(~ . - 1, data_test[, selected_features])
test_label <- as.numeric(data_test$Loan.Status) - 1
# DMatrix for XGBoost
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)
# Set XGBoost parameters
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 6,
  eval_metric = "error"
)
# Train the XGBoost model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)
# Make predictions on the test set
xgb_predictions <- predict(xgb_model, newdata = dtest)
xgb_pred_labels <- ifelse(xgb_predictions > 0.5, 1, 0)
# Evaluate the model
conf_matrix <- confusionMatrix(factor(xgb_pred_labels), factor(test_label))
# Extract accuracy, sensitivity, and specificity
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
# Calculate ROC and AUC
roc_obj <- roc(test_label, xgb_predictions)
auc_value <- auc(roc_obj)
# Create a data frame with these metrics
xgb_metrics_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
  Value = c(accuracy, sensitivity, specificity, auc_value)
)
# Print to check the data frame
print(xgb_metrics_df)
# Save the metrics to a CSV file
write.csv(xgb_metrics_df, "xgb_metrics.csv", row.names = FALSE)
# Save the XGBoost model to a file
save(xgb_model, file = "xgb_model.RData")
# Save ROC curve details (FPR, TPR) to a CSV for later reproduction
roc_details <- data.frame(
  Threshold = roc_obj$thresholds,
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)
write.csv(roc_details, "xgb_roc_details.csv", row.names = FALSE)
# Plot and save the ROC curve
roc_plot_file <- "ROC_Curve_XGBoost.png"
png(roc_plot_file)
plot(roc_obj, main = "ROC Curve for XGBoost")
dev.off()
# Output to confirm ROC plot is saved
cat("ROC curve has been saved as:", roc_plot_file, "\n")
cat("ROC curve details have been saved as: xgb_roc_details.csv\n")


#### SUPPORT VECTOR MACHINE ####
# Load necessary libraries
library(e1071)
# Stratify and split the data into training and testing sets
set.seed(40423910) # For reproducibility
trainIndex <- createDataPartition(data$Loan.Status, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data[trainIndex,]
data_test  <- data[-trainIndex,]

# Check stratification
cat("Original Data Distribution:\n")
print(prop.table(table(data$Loan.Status)))
cat("Training Data Distribution:\n")
print(prop.table(table(data_train$Loan.Status)))
cat("Testing Data Distribution:\n")
print(prop.table(table(data_test$Loan.Status)))
# Prepare data for SVM
train_data <- data_train[, c(selected_features, 'Loan.Status')]
test_data <- data_test[, c(selected_features, 'Loan.Status')]

# Train SVM model
svm_model <- svm(Loan.Status ~ ., data = train_data, kernel = "radial", cost = 1, scale = TRUE, probability = TRUE)
# Summary of the SVM model
print(summary(svm_model))
# Make predictions on the test set
svm_predictions <- predict(svm_model, newdata = test_data, probability = TRUE)
svm_probabilities <- attr(svm_predictions, "probabilities")[,2]
# Evaluate the model
conf_matrix <- confusionMatrix(svm_predictions, test_data$Loan.Status)
print(conf_matrix)
# Save accuracy and other metrics
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
# Calculate AUC
roc_curve <- roc(test_data$Loan.Status, svm_probabilities)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for SVM Model", col = "blue", lwd = 2)
# Save the ROC plot
roc_plot_path <- "roc_curve.png"
png(roc_plot_path)
plot(roc_curve, main = "ROC Curve for SVM Model", col = "blue", lwd = 2)
dev.off()
# Save the metrics as a list
svm_metrics <- list(
  model_summary = summary(svm_model),
  accuracy = accuracy,
  sensitivity = sensitivity,
  specificity = specificity,
  auc = auc_value,
  confusion_matrix = conf_matrix
)
# Save the list to a file for later use
save(svm_metrics, file = "svm_metrics.RData")
# Alternatively, save as a CSV if only specific components are needed
metrics_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
  Value = c(accuracy, sensitivity, specificity, auc_value)
)
write.csv(metrics_df, "svm_metrics.csv", row.names = FALSE)
# Save the ROC details (FPR, TPR, Thresholds) for future reproduction
roc_details <- data.frame(
  Threshold = roc_curve$thresholds,
  FPR = 1 - roc_curve$specificities,
  TPR = roc_curve$sensitivities
)
write.csv(roc_details, "svm_roc_details.csv", row.names = FALSE)
cat("SVM model training, evaluation, and ROC curve plotting complete. Metrics saved to 'svm_metrics.RData' and 'svm_metrics.csv'. ROC details saved to 'svm_roc_details.csv'.")
# Reduce the dataset to two features for visualization
plot_data <- data_train[, c("Total.Current.Balance", "Total.Collection.Amount", "Loan.Status")]
# Train SVM on these two features
svm_model_2d <- svm(Loan.Status ~ Total.Current.Balance + Total.Collection.Amount, data = plot_data, kernel = "radial", cost = 1, scale = TRUE)
# Create a grid of points to plot the decision boundary
grid <- expand.grid(
  Total.Current.Balance = seq(min(plot_data$Total.Current.Balance), max(plot_data$Total.Current.Balance), length.out = 100),
  Total.Collection.Amount = seq(min(plot_data$Total.Collection.Amount), max(plot_data$Total.Collection.Amount), length.out = 100)
)
# Predict the classes on the grid
grid$Prediction <- predict(svm_model_2d, newdata = grid)
# Plot the decision boundary with the original points
ggplot() +
  geom_tile(data = grid, aes(x = Total.Current.Balance, y = Total.Collection.Amount, fill = Prediction), alpha = 0.3) +
  geom_point(data = plot_data, aes(x = Total.Current.Balance, y = Total.Collection.Amount, color = Loan.Status)) +
  labs(title = "SVM Decision Boundary", x = "Total Current Balance", y = "Total Collection Amount") +
  theme_minimal()
### ARTIFICAL NEURAL NETWORK ####
# Load necessary libraries
library(nnet)
library(pROC)
# Stratify and split the data into training and testing sets
set.seed(40423910) # For reproducibility
trainIndex <- createDataPartition(data$Loan.Status, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data[trainIndex,]
data_test  <- data[-trainIndex,]
# Check stratification
cat("Original Data Distribution:\n")
print(prop.table(table(data$Loan.Status)))
cat("Training Data Distribution:\n")
print(prop.table(table(data_train$Loan.Status)))
cat("Testing Data Distribution:\n")
print(prop.table(table(data_test$Loan.Status)))
# Prepare data for ANN
train_data <- data_train[, c(selected_features, 'Loan.Status')]
test_data <- data_test[, c(selected_features, 'Loan.Status')]
# Standardize the features
preProc <- preProcess(train_data[, selected_features], method = c("center", "scale"))
train_data[, selected_features] <- predict(preProc, train_data[, selected_features])
test_data[, selected_features] <- predict(preProc, test_data[, selected_features])
# Convert the target variable to binary numeric (0, 1)
train_data$Loan.Status <- as.numeric(train_data$Loan.Status) - 1
test_data$Loan.Status <- as.numeric(test_data$Loan.Status) - 1

# Train ANN model with fewer hidden units
set.seed(40423910) # For reproducibility
ann_model <- nnet(Loan.Status ~ ., data = train_data, size = 3, maxit = 200, linout = FALSE)
# Summary of the ANN model
print(summary(ann_model))
# Make predictions on the test set (predict probabilities)
ann_probabilities <- predict(ann_model, newdata = test_data, type = "raw")
# Convert probabilities to binary class predictions
ann_predictions <- ifelse(ann_probabilities > 0.5, 1, 0)
# Evaluate the model
conf_matrix <- confusionMatrix(as.factor(ann_predictions), as.factor(test_data$Loan.Status))
# Calculate ROC and AUC
roc_obj <- roc(test_data$Loan.Status, ann_probabilities)
auc_value <- auc(roc_obj)
# Save accuracy, sensitivity, specificity, and AUC
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
# Save the metrics as a list
ann_metrics <- list(
  model_summary = summary(ann_model),
  accuracy = accuracy,
  sensitivity = sensitivity,
  specificity = specificity,
  auc = auc_value,
  roc_curve = roc_obj,
  confusion_matrix = conf_matrix
)
# Save the list to a file for later use
save(ann_metrics, file = "ann_metrics.RData")
# Save the model object
save(ann_model, file = "ann_model.RData")
# Save metrics to a CSV file
metrics_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
  Value = c(accuracy, sensitivity, specificity, auc_value)
)
write.csv(metrics_df, "ann_metrics.csv", row.names = FALSE)
# Save ROC curve details (FPR, TPR) to a CSV for later reproduction
roc_details <- data.frame(
  Threshold = roc_obj$thresholds,
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)
write.csv(roc_details, "ann_roc_details.csv", row.names = FALSE)
# Plot and save the ROC curve
roc_plot_file <- "ROC_Curve_ANN.png"
png(roc_plot_file)
plot(roc_obj, main = "ROC Curve for ANN")
dev.off()
# Output to confirm ROC plot is saved
cat("ROC curve has been saved as:", roc_plot_file, "\n")
cat("ROC curve details have been saved as: ann_roc_details.csv\n")

#### NAVIES BAYES ####
# Load necessary libraries
library(e1071)
# Stratify and split the data into training and testing sets
set.seed(40423910) # For reproducibility
trainIndex <- createDataPartition(data$Loan.Status, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data[trainIndex,]
data_test  <- data[-trainIndex,]
# Check stratification
cat("Original Data Distribution:\n")
print(prop.table(table(data$Loan.Status)))
cat("Training Data Distribution:\n")
print(prop.table(table(data_train$Loan.Status)))
cat("Testing Data Distribution:\n")
print(prop.table(table(data_test$Loan.Status)))
# Prepare data for Naive Bayes
train_data <- data_train[, c(selected_features, 'Loan.Status')]
test_data <- data_test[, c(selected_features, 'Loan.Status')]
# Train Naive Bayes model
naive_bayes_model <- naiveBayes(Loan.Status ~ ., data = train_data)
# Summary of the Naive Bayes model
print(naive_bayes_model)
# Make predictions on the test set
nb_predictions <- predict(naive_bayes_model, newdata = test_data, type = "raw")
# Convert predictions to binary class based on threshold (e.g., 0.5)
nb_pred_labels <- ifelse(nb_predictions[,2] > 0.5, "1", "0")
# Evaluate the model
conf_matrix <- confusionMatrix(factor(nb_pred_labels), factor(test_data$Loan.Status))
# Calculate ROC and AUC
roc_obj <- roc(test_data$Loan.Status, nb_predictions[,2])
auc_value <- auc(roc_obj)
# Save accuracy, sensitivity, specificity, and AUC
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
# Save the metrics as a list
naive_bayes_metrics <- list(
  model_summary = naive_bayes_model,
  accuracy = accuracy,
  sensitivity = sensitivity,
  specificity = specificity,
  auc = auc_value,
  roc_curve = roc_obj,
  confusion_matrix = conf_matrix
)
print(naive_bayes_metrics)
# Save the list to a file for later use
save(naive_bayes_metrics, file = "naive_bayes_metrics.RData")

# Save metrics to a CSV file
metrics_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
  Value = c(accuracy, sensitivity, specificity, auc_value)
)
write.csv(metrics_df, "naive_bayes_metrics.csv", row.names = FALSE)
# Save ROC curve details (FPR, TPR) to a CSV for later reproduction
roc_details <- data.frame(
  Threshold = roc_obj$thresholds,
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)
write.csv(roc_details, "naive_bayes_roc_details.csv", row.names = FALSE)
# Plot and save the ROC curve
roc_plot_file <- "ROC_Curve_Naive_Bayes.png"
png(roc_plot_file)
plot(roc_obj, main = "ROC Curve for Naive Bayes")
dev.off()
# Output to confirm ROC plot is saved
cat("ROC curve has been saved as:", roc_plot_file, "\n")
cat("ROC curve details have been saved as: naive_bayes_roc_details.csv\n")

######################################################################
#### HYPER-PARAMETER TUNNNING & BAYESIAN OPTIMZATION ####
#######################################################################
# Stratify and split the data into training and testing sets
set.seed(40423910) # For reproducibility
trainIndex <- createDataPartition(data$Loan.Status, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data[trainIndex,]
data_test  <- data[-trainIndex,]
# Check stratification
cat("Original Data Distribution:\n")
print(prop.table(table(data$Loan.Status)))
cat("Training Data Distribution:\n")
print(prop.table(table(data_train$Loan.Status)))
cat("Testing Data Distribution:\n")
print(prop.table(table(data_test$Loan.Status)))
# Convert data to matrix format for XGBoost
train_matrix <- model.matrix(~ . - 1, data_train[, selected_features])
train_label <- as.numeric(data_train$Loan.Status) - 1  # XGBoost requires numeric labels starting from 0
test_matrix <- model.matrix(~ . - 1, data_test[, selected_features])
test_label <- as.numeric(data_test$Loan.Status) - 1
# DMatrix for XGBoost
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)
# Set XGBoost parameters
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 6,
  eval_metric = "error"
)
# Train the base XGBoost model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)
# Make predictions on the test set
xgb_predictions <- predict(xgb_model, newdata = dtest)
xgb_pred_labels <- ifelse(xgb_predictions > 0.5, 1, 0
                          # Evaluate the base model
                          conf_matrix <- confusionMatrix(factor(xgb_pred_labels), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity
                          accuracy <- conf_matrix$overall['Accuracy']
                          sensitivity <- conf_matrix$byClass['Sensitivity']
                          specificity <- conf_matrix$byClass['Specificity']
                          # Calculate ROC and AUC for the base model
                          roc_obj <- roc(test_label, xgb_predictions)
                          auc_value <- auc(roc_obj)
                          # Create a data frame with these metrics
                          xgb_metrics_df <- data.frame(
                            Metric = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
                            Value = c(accuracy, sensitivity, specificity, auc_value)
                          )
                          
                          # Print to check the data frame
                          print(xgb_metrics_df)
                          # Save the base metrics to a CSV file
                          write.csv(xgb_metrics_df, "xgb_metrics.csv", row.names = FALSE)
                          # Save the base XGBoost model to a file
                          save(xgb_model, file = "xgb_model.RData")
                          # Save ROC curve details (FPR, TPR) to a CSV for later reproduction
                          roc_details <- data.frame(
                            Threshold = roc_obj$thresholds,
                            FPR = 1 - roc_obj$specificities,
                            TPR = roc_obj$sensitivities
                          )
                          write.csv(roc_details, "xgb_roc_details.csv", row.names = FALSE)
                          # Plot and save the ROC curve for the base model
                          roc_plot_file <- "ROC_Curve_XGBoost.png"
                          png(roc_plot_file)
                          plot(roc_obj, main = "ROC Curve for XGBoost")
                          dev.off()
                          # Output to confirm ROC plot is saved
                          cat("ROC curve has been saved as:", roc_plot_file, "\n")
                          cat("ROC curve details have been saved as: xgb_roc_details.csv\n")
                          #Step 1: Hyperparameter Tuning (Lower Learning Rate `eta`) 
                          # Set XGBoost parameters for tuning eta
                          params_tuned_eta <- list(
                            booster = "gbtree",
                            objective = "binary:logistic",
                            eta = 0.01,  # Lower learning rate
                            max_depth = 6,
                            eval_metric = "error"
                          )
                          # Train the XGBoost model with tuned eta
                          xgb_model_tuned_eta <- xgb.train(
                            params = params_tuned_eta,
                            data = dtrain,
                            nrounds = 5000,  # Increased rounds due to lower learning rate
                            watchlist = list(train = dtrain, test = dtest),
                            early_stopping_rounds = 10,
                            verbose = 1
                          )
                          # Make predictions on the test set
                          xgb_predictions_tuned_eta <- predict(xgb_model_tuned_eta, newdata = dtest)
                          xgb_pred_labels_tuned_eta <- ifelse(xgb_predictions_tuned_eta > 0.5, 1, 0)
                          
                          # Evaluate the tuned model
                          conf_matrix_tuned_eta <- confusionMatrix(factor(xgb_pred_labels_tuned_eta), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity for the tuned model
                          accuracy_tuned_eta <- conf_matrix_tuned_eta$overall['Accuracy']
                          sensitivity_tuned_eta <- conf_matrix_tuned_eta$byClass['Sensitivity']
                          specificity_tuned_eta <- conf_matrix_tuned_eta$byClass['Specificity']
                          # Calculate ROC and AUC for the tuned model
                          roc_obj_tuned_eta <- roc(test_label, xgb_predictions_tuned_eta)
                          auc_value_tuned_eta <- auc(roc_obj_tuned_eta)
                          # Create a data frame with these metrics for comparison
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", "Tuned XGBoost (eta = 0.01)"),
                            Accuracy = c(accuracy, accuracy_tuned_eta),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta),
                            Specificity = c(specificity, specificity_tuned_eta),
                            AUC = c(auc_value, auc_value_tuned_eta)
                          )
                          # Print the comparison data frame
                          print(comparison_df)
                          
                          # Save the comparison to a CSV file
                          write.csv(comparison_df, "xgb_comparison_eta_tuning.csv", row.names = FALSE)
                          cat("Comparison between base model and tuned model (eta = 0.01) has been saved as: xgb_comparison_eta_tuning.csv\n")
                          # Step 2: Tunning   
                          # Set XGBoost parameters for tuning eta and max_depth
                          params_tuned_eta_depth <- list(
                            booster = "gbtree",
                            objective = "binary:logistic",
                            eta = 0.05,  # Slightly higher learning rate than before
                            max_depth = 4,  # Reduce max depth to avoid overfitting
                            subsample = 0.8,  # Add subsampling to prevent overfitting
                            colsample_bytree = 0.8,  # Fraction of features used per tree
                            eval_metric = "error"
                          )
                          # Train the XGBoost model with tuned eta and max_depth
                          xgb_model_tuned_eta_depth <- xgb.train(
                            params = params_tuned_eta_depth,
                            data = dtrain,
                            nrounds = 2000,  # Adjust rounds to account for higher learning rate
                            watchlist = list(train = dtrain, test = dtest),
                            early_stopping_rounds = 10,
                            verbose = 1
                          )
                          # Make predictions on the test set
                          xgb_predictions_tuned_eta_depth <- predict(xgb_model_tuned_eta_depth, newdata = dtest)
                          xgb_pred_labels_tuned_eta_depth <- ifelse(xgb_predictions_tuned_eta_depth > 0.5, 1, 0)
                          # Evaluate the tuned model
                          conf_matrix_tuned_eta_depth <- confusionMatrix(factor(xgb_pred_labels_tuned_eta_depth), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity for the tuned model
                          accuracy_tuned_eta_depth <- conf_matrix_tuned_eta_depth$overall['Accuracy']
                          sensitivity_tuned_eta_depth <- conf_matrix_tuned_eta_depth$byClass['Sensitivity']
                          specificity_tuned_eta_depth <- conf_matrix_tuned_eta_depth$byClass['Specificity']
                          # Calculate ROC and AUC for the tuned model
                          roc_obj_tuned_eta_depth <- roc(test_label, xgb_predictions_tuned_eta_depth)
                          auc_value_tuned_eta_depth <- auc(roc_obj_tuned_eta_depth)
                          # Create a data frame with these metrics for comparison
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", "Tuned XGBoost (eta = 0.05, max_depth = 4)"),
                            Accuracy = c(accuracy, accuracy_tuned_eta_depth),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta_depth),
                            Specificity = c(specificity, specificity_tuned_eta_depth),
                            AUC = c(auc_value, auc_value_tuned_eta_depth)
                          )
                          # Print the comparison data frame
                          print(comparison_df)
                          # Save the comparison to a CSV file
                          write.csv(comparison_df, "xgb_comparison_eta_depth_tuning.csv", row.names = FALSE)
                          cat("Comparison between base model and tuned model (eta = 0.05, max_depth = 4) has been saved as: xgb_comparison_eta_depth_tuning.csv\n")
                          # step -3 tunning 
                          # Set XGBoost parameters for further tuning
                          params_tuned_subsample_colsample <- list(
                            booster = "gbtree",
                            objective = "binary:logistic",
                            eta = 0.05,  # Same eta as the last tuned model
                            max_depth = 4,  # Same max depth as the last tuned model
                            subsample = 0.7,  # Further reduce subsample to avoid overfitting
                            colsample_bytree = 0.7,  # Reduce colsample_bytree to use fewer features per tree
                            eval_metric = "error"
                          )
                          
                          # Train the XGBoost model with tuned subsample and colsample_bytree
                          xgb_model_tuned_subsample_colsample <- xgb.train(
                            params = params_tuned_subsample_colsample,
                            data = dtrain,
                            nrounds = 2000,  # Adjust rounds based on previous learning rate
                            watchlist = list(train = dtrain, test = dtest),
                            early_stopping_rounds = 10,
                            verbose = 1
                          )
                          # Make predictions on the test set
                          xgb_predictions_tuned_subsample_colsample <- predict(xgb_model_tuned_subsample_colsample, newdata = dtest)
                          xgb_pred_labels_tuned_subsample_colsample <- ifelse(xgb_predictions_tuned_subsample_colsample > 0.5, 1, 0)
                          # Evaluate the tuned model
                          conf_matrix_tuned_subsample_colsample <- confusionMatrix(factor(xgb_pred_labels_tuned_subsample_colsample), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity for the tuned model
                          accuracy_tuned_subsample_colsample <- conf_matrix_tuned_subsample_colsample$overall['Accuracy']
                          sensitivity_tuned_subsample_colsample <- conf_matrix_tuned_subsample_colsample$byClass['Sensitivity']
                          specificity_tuned_subsample_colsample <- conf_matrix_tuned_subsample_colsample$byClass['Specificity']
                          # Calculate ROC and AUC for the tuned model
                          roc_obj_tuned_subsample_colsample <- roc(test_label, xgb_predictions_tuned_subsample_colsample)
                          auc_value_tuned_subsample_colsample <- auc(roc_obj_tuned_subsample_colsample)
                          # Create a data frame with these metrics for comparison
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", "Tuned XGBoost (eta = 0.05, max_depth = 4)", "Tuned XGBoost (subsample = 0.7, colsample_bytree = 0.7)"),
                            Accuracy = c(accuracy, accuracy_tuned_eta_depth, accuracy_tuned_subsample_colsample),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta_depth, sensitivity_tuned_subsample_colsample),
                            Specificity = c(specificity, specificity_tuned_eta_depth, specificity_tuned_subsample_colsample),
                            AUC = c(auc_value, auc_value_tuned_eta_depth, auc_value_tuned_subsample_colsample)
                          )
                          # Print the comparison data frame
                          print(comparison_df)
                          # Save the comparison to a CSV file
                          write.csv(comparison_df, "xgb_comparison_subsample_colsample_tuning.csv", row.names = FALSE)
                          
                          cat("Comparison between base model and further tuned model (subsample = 0.7, colsample_bytree = 0.7) has been saved as: xgb_comparison_subsample_colsample_tuning.csv\n")
                          #Step -4 Tunnig 
                          # Set XGBoost parameters for rebalancing the model
                          params_tuned_rebalance <- list(
                            booster = "gbtree",
                            objective = "binary:logistic",
                            eta = 0.07,  # Slightly higher learning rate
                            max_depth = 6,  # Increase max depth to allow more complex relationships
                            subsample = 0.8,  # Revert to a moderate subsample value
                            colsample_bytree = 0.8,  # Revert to a moderate colsample_bytree value
                            eval_metric = "error"
                          )
                          # Train the XGBoost model with rebalanced parameters
                          xgb_model_tuned_rebalance <- xgb.train(
                            params = params_tuned_rebalance,
                            data = dtrain,
                            nrounds = 1500,  # Adjust rounds based on the new learning rate
                            watchlist = list(train = dtrain, test = dtest),
                            early_stopping_rounds = 10,
                            verbose = 1
                          )
                          # Make predictions on the test set
                          xgb_predictions_tuned_rebalance <- predict(xgb_model_tuned_rebalance, newdata = dtest)
                          xgb_pred_labels_tuned_rebalance <- ifelse(xgb_predictions_tuned_rebalance > 0.5, 1, 0)
                          
                          # Evaluate the tuned model
                          conf_matrix_tuned_rebalance <- confusionMatrix(factor(xgb_pred_labels_tuned_rebalance), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity for the tuned model
                          accuracy_tuned_rebalance <- conf_matrix_tuned_rebalance$overall['Accuracy']
                          sensitivity_tuned_rebalance <- conf_matrix_tuned_rebalance$byClass['Sensitivity']
                          specificity_tuned_rebalance <- conf_matrix_tuned_rebalance$byClass['Specificity']
                          # Calculate ROC and AUC for the tuned model
                          roc_obj_tuned_rebalance <- roc(test_label, xgb_predictions_tuned_rebalance)
                          auc_value_tuned_rebalance <- auc(roc_obj_tuned_rebalance)
                          # Create a data frame with these metrics for comparison
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", 
                                      "Tuned XGBoost (eta = 0.05, max_depth = 4)", 
                                      "Tuned XGBoost (subsample = 0.7, colsample_bytree = 0.7)", 
                                      "Rebalanced XGBoost (eta = 0.07, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)"),
                            Accuracy = c(accuracy, accuracy_tuned_eta_depth, accuracy_tuned_subsample_colsample, accuracy_tuned_rebalance),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta_depth, sensitivity_tuned_subsample_colsample, sensitivity_tuned_rebalance),
                            Specificity = c(specificity, specificity_tuned_eta_depth, specificity_tuned_subsample_colsample, specificity_tuned_rebalance),
                            AUC = c(auc_value, auc_value_tuned_eta_depth, auc_value_tuned_subsample_colsample, auc_value_tuned_rebalance)
                          )
                          # Print the comparison data frame
                          print(comparison_df)
                          # Save the comparison to a CSV file
                          write.csv(comparison_df, "xgb_comparison_rebalance_tuning.csv", row.names = FALSE)
                          cat("Comparison between base model and rebalanced model has been saved as: xgb_comparison_rebalance_tuning.csv\n")
                          # Step -5 
                          # Set XGBoost parameters for fine-tuning eta and nrounds
                          params_fine_tune <- list(
                            booster = "gbtree",
                            objective = "binary:logistic",
                            eta = 0.08,  # Slightly higher learning rate for faster convergence
                            max_depth = 6,  # Keep the depth the same as the rebalanced model
                            subsample = 0.8,  # Same as rebalanced model
                            colsample_bytree = 0.8,  # Same as rebalanced model
                            eval_metric = "error"
                          )
                          # Train the XGBoost model with fine-tuned eta and nrounds
                          xgb_model_fine_tune <- xgb.train(
                            params = params_fine_tune,
                            data = dtrain,
                            nrounds = 1750,  # Adjust rounds slightly higher to account for new eta
                            watchlist = list(train = dtrain, test = dtest),
                            early_stopping_rounds = 10,
                            verbose = 1
                          )
                          # Make predictions on the test set
                          xgb_predictions_fine_tune <- predict(xgb_model_fine_tune, newdata = dtest)
                          xgb_pred_labels_fine_tune <- ifelse(xgb_predictions_fine_tune > 0.5, 1, 0)
                          # Evaluate the tuned model
                          conf_matrix_fine_tune <- confusionMatrix(factor(xgb_pred_labels_fine_tune), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity for the tuned model
                          accuracy_fine_tune <- conf_matrix_fine_tune$overall['Accuracy']
                          sensitivity_fine_tune <- conf_matrix_fine_tune$byClass['Sensitivity']
                          specificity_fine_tune <- conf_matrix_fine_tune$byClass['Specificity']
                          # Calculate ROC and AUC for the tuned model
                          roc_obj_fine_tune <- roc(test_label, xgb_predictions_fine_tune)
                          auc_value_fine_tune <- auc(roc_obj_fine_tune)
                          # Create a data frame with these metrics for comparison
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", 
                                      "Tuned XGBoost (eta = 0.05, max_depth = 4)", 
                                      "Tuned XGBoost (subsample = 0.7, colsample_bytree = 0.7)", 
                                      "Rebalanced XGBoost (eta = 0.07, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)", 
                                      "Fine-Tuned XGBoost (eta = 0.08, max_depth = 6)"),
                            Accuracy = c(accuracy, accuracy_tuned_eta_depth, accuracy_tuned_subsample_colsample, accuracy_tuned_rebalance, accuracy_fine_tune),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta_depth, sensitivity_tuned_subsample_colsample, sensitivity_tuned_rebalance, sensitivity_fine_tune),
                            Specificity = c(specificity, specificity_tuned_eta_depth, specificity_tuned_subsample_colsample, specificity_tuned_rebalance, specificity_fine_tune),
                            AUC = c(auc_value, auc_value_tuned_eta_depth, auc_value_tuned_subsample_colsample, auc_value_tuned_rebalance, auc_value_fine_tune)
                          )
                          
                          # Print the comparison data frame
                          print(comparison_df)
                          # Save the comparison to a CSV file
                          write.csv(comparison_df, "xgb_comparison_fine_tune.csv", row.names = FALSE)
                          cat("Comparison between base model and fine-tuned model has been saved as: xgb_comparison_fine_tune.csv\n")
                          # Step-6 
                          # Set XGBoost parameters for tuning gamma and min_child_weight
                          params_tuned_gamma_min_child_weight <- list(
                            booster = "gbtree",
                            objective = "binary:logistic",
                            eta = 0.08,  # Keep eta as in the fine-tuned model
                            max_depth = 6,  # Keep max_depth the same
                            subsample = 0.8,  # Keep subsample the same
                            colsample_bytree = 0.8,  # Keep colsample_bytree the same
                            gamma = 1,  # Introduce gamma for more regularization
                            min_child_weight = 2,  # Introduce min_child_weight to prevent overfitting
                            eval_metric = "error"
                          )
                          # Train the XGBoost model with tuned gamma and min_child_weight
                          xgb_model_tuned_gamma_min_child_weight <- xgb.train(
                            params = params_tuned_gamma_min_child_weight,
                            data = dtrain,
                            nrounds = 1750,  # Use the same nrounds as the fine-tuned model
                            watchlist = list(train = dtrain, test = dtest),
                            early_stopping_rounds = 10,
                            verbose = 1
                          )
                          # Make predictions on the test set
                          xgb_predictions_tuned_gamma_min_child_weight <- predict(xgb_model_tuned_gamma_min_child_weight, newdata = dtest)
                          xgb_pred_labels_tuned_gamma_min_child_weight <- ifelse(xgb_predictions_tuned_gamma_min_child_weight > 0.5, 1, 0)
                          # Evaluate the tuned model
                          conf_matrix_tuned_gamma_min_child_weight <- confusionMatrix(factor(xgb_pred_labels_tuned_gamma_min_child_weight), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity for the tuned model
                          accuracy_tuned_gamma_min_child_weight <- conf_matrix_tuned_gamma_min_child_weight$overall['Accuracy']
                          sensitivity_tuned_gamma_min_child_weight <- conf_matrix_tuned_gamma_min_child_weight$byClass['Sensitivity']
                          specificity_tuned_gamma_min_child_weight <- conf_matrix_tuned_gamma_min_child_weight$byClass['Specificity']
                          
                          # Calculate ROC and AUC for the tuned model
                          roc_obj_tuned_gamma_min_child_weight <- roc(test_label, xgb_predictions_tuned_gamma_min_child_weight)
                          auc_value_tuned_gamma_min_child_weight <- auc(roc_obj_tuned_gamma_min_child_weight)
                          # Create a data frame with these metrics for comparison
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", 
                                      "Tuned XGBoost (eta = 0.05, max_depth = 4)", 
                                      "Tuned XGBoost (subsample = 0.7, colsample_bytree = 0.7)", 
                                      "Rebalanced XGBoost (eta = 0.07, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)", 
                                      "Fine-Tuned XGBoost (eta = 0.08, max_depth = 6)", 
                                      "Tuned XGBoost (gamma = 1, min_child_weight = 2)"),
                            Accuracy = c(accuracy, accuracy_tuned_eta_depth, accuracy_tuned_subsample_colsample, accuracy_tuned_rebalance, accuracy_fine_tune, accuracy_tuned_gamma_min_child_weight),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta_depth, sensitivity_tuned_subsample_colsample, sensitivity_tuned_rebalance, sensitivity_fine_tune, sensitivity_tuned_gamma_min_child_weight),
                            Specificity = c(specificity, specificity_tuned_eta_depth, specificity_tuned_subsample_colsample, specificity_tuned_rebalance, specificity_fine_tune, specificity_tuned_gamma_min_child_weight),
                            AUC = c(auc_value, auc_value_tuned_eta_depth, auc_value_tuned_subsample_colsample, auc_value_tuned_rebalance, auc_value_fine_tune, auc_value_tuned_gamma_min_child_weight)
                          )
                          # Print the comparison data frame
                          print(comparison_df)
                          # Save the comparison to a CSV file
                          write.csv(comparison_df, "xgb_comparison_gamma_min_child_weight.csv", row.names = FALSE)
                          cat("Comparison between base model and tuned model (gamma and min_child_weight) has been saved as: xgb_comparison_gamma_min_child_weight.csv\n")
                          #### Bayesian ####
                          install.packages("rBayesianOptimization")
                          library(rBayesianOptimization)
                          # Define the search space
                          optimize_function <- function(max_depth, eta, gamma, subsample, colsample_bytree) {
                            params <- list(
                              booster = "gbtree",
                              objective = "binary:logistic",
                              eval_metric = "auc",
                              max_depth = as.integer(max_depth),
                              eta = eta,
                              gamma = gamma,
                              subsample = subsample,
                              colsample_bytree = colsample_bytree
                            )
                            cv_result <- xgb.cv(
                              params = params,
                              data = dtrain,
                              nrounds = 200,
                              nfold = 5,
                              verbose = FALSE,
                              early_stopping_rounds = 10,
                              maximize = TRUE
                            )
                            list(Score = max(cv_result$evaluation_log$test_auc_mean), Pred = 0)
                          }
                          # Perform Bayesian optimization
                          bayes_opt <- BayesianOptimization(
                            FUN = optimize_function,
                            bounds = list(
                              max_depth = c(3L, 10L),
                              eta = c(0.01, 0.2),
                              gamma = c(0, 5),
                              subsample = c(0.6, 1),
                              colsample_bytree = c(0.6, 1)
                            ),
                            init_points = 10,
                            n_iter = 30,
                            acq = "ucb",
                            kappa = 2.576,
                            eps = 0.0
                          )
                          # Train final model with best parameters
                          best_params <- list(
                            booster = "gbtree",
                            objective = "binary:logistic",
                            max_depth = bayes_opt$Best_Par["max_depth"],
                            eta = bayes_opt$Best_Par["eta"],
                            gamma = bayes_opt$Best_Par["gamma"],
                            subsample = bayes_opt$Best_Par["subsample"],
                            colsample_bytree = bayes_opt$Best_Par["colsample_bytree"],
                            eval_metric = "auc"
                          )
                          xgb_model_optimized <- xgb.train(
                            params = best_params,
                            data = dtrain,
                            nrounds = 2000,
                            watchlist = list(train = dtrain, test = dtest),
                            early_stopping_rounds = 10,
                            verbose = 1
                          )
                          # Evaluate the optimized model and compare
                          # Make predictions on the test set using the optimized model
                          xgb_predictions_optimized <- predict(xgb_model_optimized, newdata = dtest)
                          xgb_pred_labels_optimized <- ifelse(xgb_predictions_optimized > 0.5, 1, 0)
                          # Evaluate the optimized model
                          conf_matrix_optimized <- confusionMatrix(factor(xgb_pred_labels_optimized), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity for the optimized model
                          accuracy_optimized <- conf_matrix_optimized$overall['Accuracy']
                          sensitivity_optimized <- conf_matrix_optimized$byClass['Sensitivity']
                          specificity_optimized <- conf_matrix_optimized$byClass['Specificity']
                          # Calculate ROC and AUC for the optimized model
                          roc_obj_optimized <- roc(test_label, xgb_predictions_optimized)
                          auc_value_optimized <- auc(roc_obj_optimized)
                          #### Bayesian Parameters Table ####
                          # Install necessary package if you haven't already
                          install.packages("kableExtra")
                          library(kableExtra)
                          # Assuming 'bayes_opt$Best_Par' contains the best parameters from Bayesian Optimization
                          best_params <- bayes_opt$Best_Par
                          # Convert the list of best parameters to a data frame for better formatting
                          best_params_df <- data.frame(Parameter = names(best_params), Value = unlist(best_params))
                          # Create the colorful table
                          best_params_table <- best_params_df %>%
                            kbl(caption = "Best Hyperparameters from Bayesian Optimization") %>%
                            kable_classic(full_width = F, html_font = "Cambria") %>%
                            row_spec(0, bold = T, color = "white", background = "#4F81BD") %>%
                            row_spec(1:nrow(best_params_df), background = "#D9EAD3", color = "black") %>%
                            column_spec(1, bold = T, border_right = TRUE) %>%
                            column_spec(2, border_left = TRUE, border_right = TRUE) %>%
                            kable_styling(latex_options = "hold_position")
                          # Print the table
                          print(best_params_table)
                          # Optionally, save the table as an HTML file if needed
                          save_kable(best_params_table, "Best_Hyperparameters_Table.html")
                          #### Metrics Comparison ####
                          # Create a data frame with these metrics for comparison
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", 
                                      "Tuned XGBoost (eta = 0.05, max_depth = 4)", 
                                      "Tuned XGBoost (subsample = 0.7, colsample_bytree = 0.7)", 
                                      "Rebalanced XGBoost (eta = 0.07, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)", 
                                      "Fine-Tuned XGBoost (eta = 0.08, max_depth = 6)", 
                                      "Tuned XGBoost (gamma = 1, min_child_weight = 2)", 
                                      "Optimized XGBoost (Bayesian Optimization)"),
                            Accuracy = c(accuracy, accuracy_tuned_eta_depth, accuracy_tuned_subsample_colsample, accuracy_tuned_rebalance, accuracy_fine_tune, accuracy_tuned_gamma_min_child_weight, accuracy_optimized),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta_depth, sensitivity_tuned_subsample_colsample, sensitivity_tuned_rebalance, sensitivity_fine_tune, sensitivity_tuned_gamma_min_child_weight, sensitivity_optimized),
                            Specificity = c(specificity, specificity_tuned_eta_depth, specificity_tuned_subsample_colsample, specificity_tuned_rebalance, specificity_fine_tune, specificity_tuned_gamma_min_child_weight, specificity_optimized),
                            AUC = c(auc_value, auc_value_tuned_eta_depth, auc_value_tuned_subsample_colsample, auc_value_tuned_rebalance, auc_value_fine_tune, auc_value_tuned_gamma_min_child_weight, auc_value_optimized)
                          )
                          # Print the comparison data frame
                          print(comparison_df)
                          # Save the comparison to a CSV file
                          write.csv(comparison_df, "model_comparison_optimized.csv", row.names = FALSE)
                          cat("Comparison between base model and optimized model has been saved as: model_comparison_optimized.csv\n")
                          # Plot and save the ROC curve for the optimized model
                          roc_plot_file <- "ROC_Curve_Optimized_XGBoost.png"
                          png(roc_plot_file)
                          plot(roc_obj_optimized, main = "ROC Curve for Optimized XGBoost")
                          dev.off()
                          cat("ROC curve for the optimized XGBoost model has been saved as:", roc_plot_file, "\n")
                          # Make predictions on the test set using the optimized model
                          xgb_predictions_optimized <- predict(xgb_model_optimized, newdata = dtest)
                          xgb_pred_labels_optimized <- ifelse(xgb_predictions_optimized > 0.5, 1, 0)
                          # Evaluate the optimized model
                          conf_matrix_optimized <- confusionMatrix(factor(xgb_pred_labels_optimized), factor(test_label))
                          # Extract accuracy, sensitivity, and specificity for the optimized model
                          accuracy_optimized <- conf_matrix_optimized$overall['Accuracy']
                          sensitivity_optimized <- conf_matrix_optimized$byClass['Sensitivity']
                          specificity_optimized <- conf_matrix_optimized$byClass['Specificity']
                          # Calculate ROC and AUC for the optimized model
                          roc_obj_optimized <- roc(test_label, xgb_predictions_optimized)
                          auc_value_optimized <- auc(roc_obj_optimized)
                          # Install and load the necessary package if you haven't already
                          install.packages("kableExtra")
                          library(kableExtra)
                          # Create the data frame with your metrics
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", 
                                      "Tuned XGBoost (eta = 0.05, max_depth = 4)", 
                                      "Tuned XGBoost (subsample = 0.7, colsample_bytree = 0.7)", 
                                      "Rebalanced XGBoost (eta = 0.07, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)", 
                                      "Fine-Tuned XGBoost (eta = 0.08, max_depth = 6)", 
                                      "Tuned XGBoost (gamma = 1, min_child_weight = 2)", 
                                      "Optimized XGBoost (Bayesian Optimization)"),
                            Accuracy = c(accuracy, accuracy_tuned_eta_depth, accuracy_tuned_subsample_colsample, accuracy_tuned_rebalance, accuracy_fine_tune, accuracy_tuned_gamma_min_child_weight, accuracy_optimized),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta_depth, sensitivity_tuned_subsample_colsample, sensitivity_tuned_rebalance, sensitivity_fine_tune, sensitivity_tuned_gamma_min_child_weight, sensitivity_optimized),
                            Specificity = c(specificity, specificity_tuned_eta_depth, specificity_tuned_subsample_colsample, specificity_tuned_rebalance, specificity_fine_tune, specificity_tuned_gamma_min_child_weight, specificity_optimized),
                            AUC = c(auc_value, auc_value_tuned_eta_depth, auc_value_tuned_subsample_colsample, auc_value_tuned_rebalance, auc_value_fine_tune, auc_value_tuned_gamma_min_child_weight, auc_value_optimized)
                          )
                          # Print the comparison data frame as a colorful table
                          comparison_df %>%
                            kbl(caption = "Model Comparison Table") %>%
                            kable_classic(full_width = F, html_font = "Cambria") %>%
                            row_spec(0, bold = T, color = "white", background = "#4F81BD") %>%
                            row_spec(1:nrow(comparison_df), background = "#F7F7F7", color = "black") %>%
                            column_spec(1, bold = T, border_right = TRUE) %>%
                            column_spec(2:5, border_left = TRUE, border_right = TRUE) %>%
                            kable_styling(latex_options = "hold_position")
                          # Save the comparison to a CSV file
                          write.csv(comparison_df, "model_comparison_optimized.csv", row.names = FALSE)
                          cat("Comparison between base model and optimized model has been saved as: model_comparison_optimized.csv\n")
                          # Plot and save the ROC curve for the optimized model
                          roc_plot_file <- "ROC_Curve_Optimized_XGBoost.png"
                          png(roc_plot_file)
                          plot(roc_obj_optimized, main = "ROC Curve for Optimized XGBoost")
                          dev.off()
                          cat("ROC curve for the optimized XGBoost model has been saved as:", roc_plot_file, "\n")
                          ### Metrics Graph ###
                          # Load necessary libraries
                          library(ggplot2)
                          library(dplyr)
                          library(pROC)
                          # Function to calculate F1 Score
                          calculate_f1_score <- function(conf_matrix) {
                            precision <- conf_matrix$byClass['Pos Pred Value']
                            recall <- conf_matrix$byClass['Sensitivity']
                            f1_score <- 2 * ((precision * recall) / (precision + recall))
                            return(f1_score)
                          }
                          # Calculate F1 scores for all models
                          f1_base <- calculate_f1_score(conf_matrix)
                          f1_tuned_eta <- calculate_f1_score(conf_matrix_tuned_eta)
                          f1_tuned_eta_depth <- calculate_f1_score(conf_matrix_tuned_eta_depth)
                          f1_tuned_subsample_colsample <- calculate_f1_score(conf_matrix_tuned_subsample_colsample)
                          f1_tuned_rebalance <- calculate_f1_score(conf_matrix_tuned_rebalance)
                          f1_fine_tune <- calculate_f1_score(conf_matrix_fine_tune)
                          f1_tuned_gamma_min_child_weight <- calculate_f1_score(conf_matrix_tuned_gamma_min_child_weight)
                          f1_optimized <- calculate_f1_score(conf_matrix_optimized)
                          # Create a data frame with all metrics for comparison
                          comparison_df <- data.frame(
                            Model = c("Base XGBoost", 
                                      "Tuned XGBoost -1", 
                                      "Tuned XGBoost - 2", 
                                      "Rebalanced XGBoost", 
                                      "Fine-Tuned XGBoost", 
                                      "Tuned XGBoost", 
                                      "Bayesian Optimised"),
                            Accuracy = c(accuracy, accuracy_tuned_eta_depth, accuracy_tuned_subsample_colsample, accuracy_tuned_rebalance, accuracy_fine_tune, accuracy_tuned_gamma_min_child_weight, accuracy_optimized),
                            Sensitivity = c(sensitivity, sensitivity_tuned_eta_depth, sensitivity_tuned_subsample_colsample, sensitivity_tuned_rebalance, sensitivity_fine_tune, sensitivity_tuned_gamma_min_child_weight, sensitivity_optimized),
                            Specificity = c(specificity, specificity_tuned_eta_depth, specificity_tuned_subsample_colsample, specificity_tuned_rebalance, specificity_fine_tune, specificity_tuned_gamma_min_child_weight, specificity_optimized),
                            F1_Score = c(f1_base, f1_tuned_eta, f1_tuned_eta_depth, f1_tuned_subsample_colsample, f1_tuned_rebalance, f1_fine_tune, f1_optimized),
                            AUC = c(auc_value, auc_value_tuned_eta_depth, auc_value_tuned_subsample_colsample, auc_value_tuned_rebalance, auc_value_fine_tune, auc_value_tuned_gamma_min_child_weight, auc_value_optimized)
                          )
                          # Function to create a bar plot with values on top
                          plot_metric <- function(metric) {
                            ggplot(comparison_df, aes(x = reorder(Model, !!sym(metric)), y = !!sym(metric), fill = Model)) +
                              geom_bar(stat = "identity") +
                              geom_text(aes(label = round(!!sym(metric), 4)), vjust = -0.5, size = 3) +
                              labs(title = paste("Comparison of", metric), y = metric, x = "Model") +
                              theme_minimal() +
                              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                              scale_fill_brewer(palette = "Set1")
                          }
                          # Plot, print, and save each metric separately
                          metrics_list <- c("Accuracy", "Sensitivity", "Specificity", "F1_Score")
                          for (metric in metrics_list) {
                            plot <- plot_metric(metric)
                            # Print the plot
                            print(plot)
                            
                            # Save the plot
                            ggsave(paste0("model_comparison_", tolower(metric), ".png"), plot = plot, width = 8, height = 6)
                          }
                          cat("Individual bar charts for Accuracy, Sensitivity, Specificity, and F1 Score have been saved.\n")
                          # Calculate ROC curves for each model
                          roc_base <- roc(test_label, xgb_predictions)
                          roc_tuned_eta <- roc(test_label, xgb_predictions_tuned_eta)
                          roc_tuned_eta_depth <- roc(test_label, xgb_predictions_tuned_eta_depth)
                          roc_tuned_subsample_colsample <- roc(test_label, xgb_predictions_tuned_subsample_colsample)
                          roc_tuned_rebalance <- roc(test_label, xgb_predictions_tuned_rebalance)
                          roc_fine_tune <- roc(test_label, xgb_predictions_fine_tune)
                          roc_tuned_gamma_min_child_weight <- roc(test_label, xgb_predictions_tuned_gamma_min_child_weight)
                          roc_optimized <- roc(test_label, xgb_predictions_optimized)
                          # Combine ROC data into a single data frame for plotting
                          roc_data <- bind_rows(
                            data.frame(FPR = 1 - roc_base$specificities, TPR = roc_base$sensitivities, Model = paste0("Base XGBoost (AUC = ", round(auc_value, 3), ")")),
                            data.frame(FPR = 1 - roc_tuned_eta$specificities, TPR = roc_tuned_eta$sensitivities, Model = paste0("Tuned XGBoost -1 (AUC = ", round(auc_value_tuned_eta_depth, 3), ")")),
                            data.frame(FPR = 1 - roc_tuned_eta_depth$specificities, TPR = roc_tuned_eta_depth$sensitivities, Model = paste0("Tuned XGBoost - 2 (AUC = ", round(auc_value_tuned_subsample_colsample, 3), ")")),
                            data.frame(FPR = 1 - roc_tuned_subsample_colsample$specificities, TPR = roc_tuned_subsample_colsample$sensitivities, Model = paste0("Rebalanced XGBoost (AUC = ", round(auc_value_tuned_rebalance, 3), ")")),
                            data.frame(FPR = 1 - roc_tuned_rebalance$specificities, TPR = roc_tuned_rebalance$sensitivities, Model = paste0("Fine-Tuned XGBoost (AUC = ", round(auc_value_fine_tune, 3), ")")),
                            data.frame(FPR = 1 - roc_fine_tune$specificities, TPR = roc_fine_tune$sensitivities, Model = paste0("Tuned XGBoost (AUC = ", round(auc_value_tuned_gamma_min_child_weight, 3), ")")),
                            data.frame(FPR = 1 - roc_tuned_gamma_min_child_weight$specificities, TPR = roc_tuned_gamma_min_child_weight$sensitivities, Model = paste0("Tuned XGBoost (AUC = ", round(auc_value_tuned_gamma_min_child_weight, 3), ")")),
                            data.frame(FPR = 1 - roc_optimized$specificities, TPR = roc_optimized$sensitivities, Model = paste0("Bayesian Optimised (AUC = ", round(auc_value_optimized, 3), ")"))
                          )
                          # Plot the combined ROC curves with AUC in the legend
                          roc_plot <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
                            geom_line(size = 1.2) +
                            geom_abline(linetype = "dashed", color = "gray") +
                            labs(title = "ROC Curve Comparison: All Models", x = "False Positive Rate", y = "True Positive Rate") +
                            theme_minimal() +
                            theme(legend.position = "bottom") +
                            scale_color_brewer(palette = "Set1")
                          # Print the ROC plot
                          print(roc_plot)
                          # Save the ROC plot
                          ggsave("roc_curve_comparison_with_auc.png", plot = roc_plot, width = 10, height = 8)
                          cat("ROC curve comparison with AUC values has been saved as: roc_curve_comparison_with_auc.png\n")
                          #### Interpretable AI ####
                          #### Partial Dependency Plot ####
                          # Load the necessary libraries
                          library(iml)
                          library(ggplot2)
                          library(xgboost)  # Ensure xgboost package is loaded
                          # Convert train_matrix to data frame
                          train_data <- as.data.frame(train_matrix)
                          # Get the feature names used by the XGBoost model
                          model_feature_names <- colnames(train_matrix)
                          # Ensure column names in train_data match those in the model
                          colnames(train_data) <- model_feature_names
                          
                          # Add the target variable back to the data frame
                          train_data$Loan.Status <- as.factor(train_label)
                          # Define a custom prediction function for iml
                          custom_predict <- function(model, newdata) {
                            data_matrix <- as.matrix(newdata)  # Ensure data is in matrix format
                            predict(model, newdata = data_matrix)
                          }
                          # Create the Predictor object using the custom prediction function
                          predictor <- Predictor$new(
                            model = xgb_model_optimized,
                            data = train_data[, model_feature_names],  # Use the correct feature names
                            y = train_data$Loan.Status,
                            predict.fun = custom_predict,  # Use the custom prediction function
                            type = "prob"  # Specify that we want probability predictions
                          )
                          # Generate and save Dependency (PDP) plots for each feature
                          for (feature in model_feature_names) {
                            pdp <- FeatureEffect$new(predictor, feature = feature, method = "pdp")
                            pdp_plot <- pdp$plot()
                            # Print the plot
                            print(pdp_plot)
                            # Save each PDP plot
                            ggsave(paste0("pdp_plot_", gsub("[^[:alnum:]]", "_", feature), ".png"), plot = pdp_plot, width = 8, height = 6)
                          }
                          cat("PDP plots for the best-performing XGBoost model have been generated and saved.\n")
                          #Create a plot after remapping all the factors to the variables so that we get a proper single graph for each variable
                          #### Feature Importance ####
                          # Step 1: Extract feature importance from the optimized XGBoost model
                          feature_importance <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model_optimized)
                          # Step 2: Create a mapping for the selected features
                          mapping <- c(
                            'Grade' = 'Grade',
                            'Sub.Grade' = 'Sub.Grade',
                            'Delinquency...two.years' = 'Delinquency...two.years',
                            'Open.Account' = 'Open.Account',
                            'Public.Record' = 'Public.Record',
                            'Initial.List.Status' = 'Initial.List.Status',
                            'Total.Received.Late.Fee' = 'Total.Received.Late.Fee',
                            'Last.week.Pay' = 'Last.week.Pay',
                            'Total.Collection.Amount' = 'Total.Collection.Amount',
                            'Total.Current.Balance' = 'Total.Current.Balance',
                            'Home.Ownership' = 'Home.Ownership'
                          )
                          # Step 3: Replace the feature names with their original variable names using the mapping
                          feature_importance$Original_Variable <- sapply(feature_importance$Feature, function(f) {
                            matched_variable <- unlist(lapply(names(mapping), function(x) {
                              if (grepl(x, f)) return(mapping[[x]])
                            }))
                            return(matched_variable[1])  # Return the first match
                          })
                          # Step 4: Aggregate importance by original variable
                          # Sum the Gain for each original variable
                          variable_importance <- aggregate(Gain ~ Original_Variable, data = feature_importance, FUN = sum)
                          # Step 5: Sort the variable importance for better visualization
                          variable_importance <- variable_importance[order(variable_importance$Gain, decreasing = TRUE, ]
                          # Print the variable importance to check
                          print(variable_importance)
                          # Step 6: Plot the variable importance using ggplot2
                          library(ggplot2)
                          ggplot(variable_importance, aes(x = reorder(Original_Variable, Gain), y = Gain)) +
                            geom_bar(stat = "identity", fill = "skyblue") +
                            coord_flip() +
                            labs(title = "Variable Importance in Optimized XGBoost Model",
                                 x = "Original Variable",
                                 y = "Aggregated Importance (Gain)") +
                            theme_minimal()
                          # Save the variable importance plot as a PNG file
                          ggsave("Variable_Importance_Optimized_XGBoost.png", width = 8, height = 6)
                          cat("Variable importance plot has been saved as: Variable_Importance_Optimized_XGBoost.png\n")
                          #### ALE ####
                          # Load necessary library
                          library(ALEPlot)
                          # Step 1: Convert the training data used in the XGBoost model back to a data frame
                          train_df <- as.data.frame(train_matrix)
                          colnames(train_df) <- colnames(train_matrix)  # Set correct column names
                          # Step 2: Define the prediction function for your XGBoost model
                          predict_function <- function(X.model, newdata) {
                            predict(X.model, as.matrix(newdata))
                          }
                          
                          # Step 3: Plot ALE for each selected feature
                          for (feature in selected_features) {
                            # Check if the feature exists in the training data
                            if (feature %in% colnames(train_df)) {
                              # Determine the column index of the feature in the train_df
                              feature_index <- which(colnames(train_df) == feature)
                              # Plot the ALE
                              ALEPlot(
                                X = train_df,                  # Training data
                                pred.fun = predict_function,   # Prediction function for your model
                                J = feature_index,             # Index of the feature to plot ALE for
                                X.model = xgb_model_optimized, # Your trained XGBoost model
                                K = 50                         # Number of intervals, can be adjusted
                              )
                              # Add title and labels
                              title(main = paste("ALE Plot for", feature))
                              mtext("Accumulated Local Effects", side = 2, line = 3)
                              # Optionally save each plot
                              file_name <- paste0("ALE_Plot_", gsub("\\.", "_", feature), ".png")
                              dev.copy(png, file_name)
                              dev.off()
                              
                              cat("ALE plot has been saved as:", file_name, "\n")
                            } else {
                              cat("Feature not found in training data:", feature, "\n")
                            }
                          }
                          #### CIME ####
                          # Step 1: Define a function to calculate permutation importance and aggregate by variable
                          calculate_permutation_importance <- function(model, data, target, n_repeats = 5) {
                            # Original model accuracy
                            original_predictions <- predict(model, as.matrix(data))
                            original_accuracy <- confusionMatrix(factor(ifelse(original_predictions > 0.5, 1, 0)), target)$overall['Accuracy']
                            # Initialize a data frame to store importance results
                            feature_importance <- data.frame(Variable = colnames(data), Importance = numeric(ncol(data)))
                            # Iterate through each feature
                            for (feature in colnames(data)) {
                              feature_importance_value <- numeric(n_repeats)
                              for (i in 1:n_repeats) {
                                # Shuffle the feature column
                                permuted_data <- data
                                permuted_data[[feature]] <- sample(permuted_data[[feature]])
                                # Calculate accuracy after shuffling
                                permuted_predictions <- predict(model, as.matrix(permuted_data))
                                permuted_accuracy <- confusionMatrix(factor(ifelse(permuted_predictions > 0.5, 1, 0)), target)$overall['Accuracy']
                                # Importance is the drop in accuracy
                                feature_importance_value[i] <- original_accuracy - permuted_accuracy
                              }
                              # Average importance over the repetitions
                              feature_importance[feature_importance$Variable == feature, "Importance"] <- mean(feature_importance_value)
                            }
                            # Step 2: Map features to their original variables
                            mapping <- c(
                              'Grade' = 'Grade',
                              'Sub.Grade' = 'Sub.Grade',
                              'Delinquency...two.years' = 'Delinquency...two.years',
                              'Open.Account' = 'Open.Account',
                              'Public.Record' = 'Public.Record',
                              'Initial.List.Status' = 'Initial.List.Status',
                              'Total.Received.Late.Fee' = 'Total.Received.Late.Fee',
                              'Last.week.Pay' = 'Last.week.Pay',
                              'Total.Collection.Amount' = 'Total.Collection.Amount',
                              'Total.Current.Balance' = 'Total.Current.Balance',
                              'Home.Ownership' = 'Home.Ownership'
                            )
                            feature_importance$Original_Variable <- sapply(feature_importance$Variable, function(f) {
                              matched_variable <- unlist(lapply(names(mapping), function(x) {
                                if (grepl(x, f)) return(mapping[[x]])
                              }))
                              return(matched_variable[1])  # Return the first match
                            })
                            # Step 3: Aggregate importance by original variable
                            variable_importance <- feature_importance %>%
                              group_by(Original_Variable) %>%
                              summarize(Importance = sum(Importance)) %>%
                              arrange(desc(Importance))
                            return(variable_importance)
                          }
                          # Step 4: Prepare the data
                          train_df <- as.data.frame(train_matrix)
                          target <- as.factor(data_train$Loan.Status)  # Assuming the target is binary and stored in 'Loan.Status'
                          # Step 5: Calculate permutation importance for each variable
                          permutation_importance <- calculate_permutation_importance(xgb_model_optimized, train_df, target)
                          # Step 6: Sort and display the importance scores
                          print(permutation_importance)
                          # Step 7: Plot the permutation importance using ggplot2
                          library(ggplot2)
                          ggplot(permutation_importance, aes(x = reorder(Original_Variable, Importance), y = Importance)) +
                            geom_bar(stat = "identity", fill = "skyblue") +
                            coord_flip() +
                            labs(title = "Permutation Importance in Optimized XGBoost Model",
                                 x = "Variable",
                                 y = "Importance (Accuracy Drop)") +
                            theme_minimal()
                          # Save the permutation importance plot as a PNG file
                          ggsave("Permutation_Importance_XGBoost_Aggregated.png", width = 8, height = 6)
                          cat("Permutation importance plot has been saved as: Permutation_Importance_XGBoost_Aggregated.png\n")
                          
                          
                          
                          #####################################
                          #### K-FOLD CROSSVALIDATION ####
                          #####################################
                          #### LOGISTIC REGRESSION ####
                          # Ensure valid R variable names for Loan.Status levels
                          data$Loan.Status <- as.factor(make.names(data$`Loan.Status`))
                          # Define the formula for the logistic regression model
                          formula <- as.formula(paste("`Loan.Status` ~", paste(selected_features, collapse = " + ")))
                          # Set up the cross-validation control
                          cv_control <- trainControl(
                            method = "cv",   # K-fold cross-validation
                            number = 5,     # Number of folds
                            classProbs = TRUE,  # For classification
                            summaryFunction = twoClassSummary,  # To calculate AUC
                            savePredictions = "final",
                            returnResamp = "all"  # Return all fold-wise results
                          )
                          # Train the model using K-fold cross-validation
                          set.seed(40423910) # For reproducibility
                          cv_model <- train(
                            formula, 
                            data = data, 
                            method = "glm", 
                            family = "binomial",
                            trControl = cv_control,
                            metric = "ROC"  # Optimize by ROC (AUC)
                          )
                          # Extract fold-wise results
                          fold_results <- cv_model$resample
                          # Calculate metrics for each fold
                          metrics_list <- lapply(1:nrow(fold_results), function(i) {
                            resample <- fold_results$Resample[i]
                            preds <- cv_model$pred[cv_model$pred$Resample == resample, ]
                            # Confusion matrix
                            conf_mat <- confusionMatrix(as.factor(preds$pred), as.factor(preds$obs))
                            # Metrics
                            accuracy <- conf_mat$overall['Accuracy']
                            sensitivity <- conf_mat$byClass['Sensitivity']
                            specificity <- conf_mat$byClass['Specificity']
                            precision <- conf_mat$byClass['Pos Pred Value']
                            recall <- conf_mat$byClass['Sensitivity']
                            f1_score <- 2 * ((precision * recall) / (precision + recall))
                            
                            # Dynamically reference the column for the positive class probabilities
                            positive_class <- levels(preds$obs)[2]  # Assumes 2nd level is the positive class
                            roc_obj <- roc(as.numeric(preds$obs), as.numeric(preds[[positive_class]]))
                            auc_value <- auc(roc_obj)
                            return(data.frame(
                              Resample = resample,
                              Accuracy = accuracy,
                              Sensitivity = sensitivity,
                              Specificity = specificity,
                              Precision = precision,
                              Recall = recall,
                              F1_Score = f1_score,
                              ROC = fold_results$ROC[i],
                              AUC = auc_value
                            ))
                          })
                          # Combine the fold metrics into a single data frame
                          metrics_df <- do.call(rbind, metrics_list)
                          
                          # Calculate the mean of each metric
                          mean_metrics <- metrics_df %>%
                            summarise(
                              Resample = "Mean",
                              Accuracy = mean(Accuracy, na.rm = TRUE),
                              Sensitivity = mean(Sensitivity, na.rm = TRUE),
                              Specificity = mean(Specificity, na.rm = TRUE),
                              Precision = mean(Precision, na.rm = TRUE),
                              Recall = mean(Recall, na.rm = TRUE),
                              F1_Score = mean(F1_Score, na.rm = TRUE),
                              ROC = mean(ROC, na.rm = TRUE),
                              AUC = mean(AUC, na.rm = TRUE)
                            )
                          # Combine fold metrics and mean metrics
                          final_results <- rbind(metrics_df, mean_metrics)
                          # Save the final results to a CSV file
                          write.csv(final_results, "logistic_kfold_detailed_results.csv", row.names = FALSE)
                          # Save the full model object for later use if needed
                          save(cv_model, file = "logistic_kfold_model.RData")
                          # Output to confirm files are saved
                          cat("Detailed fold-wise results and their mean have been saved as: logistic_kfold_detailed_results.csv\n")
                          ####RANDOMFOREST ####
                          # Ensure valid R variable names for Loan.Status levels
                          data$Loan.Status <- as.factor(make.names(data$Loan.Status))
                          # Set up the cross-validation control
                          cv_control <- trainControl(
                            method = "cv",   # K-fold cross-validation
                            number = 5,     # Number of folds
                            classProbs = TRUE,  # For classification
                            summaryFunction = twoClassSummary,  # To calculate AUC
                            savePredictions = "final",
                            returnResamp = "all"  # Return all fold-wise results
                          )
                          # Train the Random Forest model using K-fold cross-validation
                          set.seed(40423910) # For reproducibility
                          rf_model <- train(
                            as.formula(paste("Loan.Status ~", paste(selected_features, collapse = " + "))),
                            data = data,
                            method = "rf",   # Random Forest
                            trControl = cv_control,
                            metric = "ROC",  # Optimize by ROC (AUC)
                            importance = TRUE,
                            ntree = 500
                          )
                          # Extract fold-wise results
                          fold_results <- rf_model$resample
                          # Calculate metrics for each fold
                          metrics_list <- lapply(1:nrow(fold_results), function(i) {
                            resample <- fold_results$Resample[i]
                            preds <- rf_model$pred[rf_model$pred$Resample == resample, ]
                            # Confusion matrix
                            conf_mat <- confusionMatrix(as.factor(preds$pred), as.factor(preds$obs))
                            # Metrics
                            accuracy <- conf_mat$overall['Accuracy']
                            sensitivity <- conf_mat$byClass['Sensitivity']
                            specificity <- conf_mat$byClass['Specificity']
                            precision <- conf_mat$byClass['Pos Pred Value']
                            recall <- conf_mat$byClass['Sensitivity']
                            f1_score <- 2 * ((precision * recall) / (precision + recall))
                            # Calculate ROC and AUC for the current fold
                            positive_class <- levels(preds$obs)[2]  # Assumes 2nd level is the positive class
                            roc_obj <- roc(preds$obs, preds[[positive_class]])
                            auc_value <- auc(roc_obj)
                            return(data.frame(
                              Resample = resample,
                              Accuracy = accuracy,
                              Sensitivity = sensitivity,
                              Specificity = specificity,
                              Precision = precision,
                              Recall = recall,
                              F1_Score = f1_score,
                              ROC = fold_results$ROC[i],
                              AUC = auc_value
                            ))
                          })
                          # Combine the fold metrics into a single data frame
                          metrics_df <- do.call(rbind, metrics_list)
                          # Calculate the mean of each metric
                          mean_metrics <- metrics_df %>%
                            summarise(
                              Resample = "Mean",
                              Accuracy = mean(Accuracy, na.rm = TRUE),
                              Sensitivity = mean(Sensitivity, na.rm = TRUE),
                              Specificity = mean(Specificity, na.rm = TRUE),
                              Precision = mean(Precision, na.rm = TRUE),
                              Recall = mean(Recall, na.rm = TRUE),
                              F1_Score = mean(F1_Score, na.rm = TRUE),
                              ROC = mean(ROC, na.rm = TRUE),
                              AUC = mean(AUC, na.rm = TRUE)
                            )
                          # Combine fold metrics and mean metrics
                          final_results <- rbind(metrics_df, mean_metrics)
                          # Save the final results to a CSV file
                          write.csv(final_results, "rf_kfold_detailed_results.csv", row.names = FALSE)
                          # Save the full model object for later use if needed
                          save(rf_model, file = "rf_kfold_model.RData")
                          # Output to confirm files are saved
                          cat("Detailed fold-wise results and their mean have been saved as: rf_kfold_detailed_results.csv\n")
                          #### Tree Visual ####
                          # Load necessary libraries for tree visualization
                          library(rpart)
                          library(rattle)
                          library(rpart.plot)
                          
                          # Extract and plot a single tree from the Random Forest model
                          # Select a specific tree, for example, the first tree
                          single_tree <- getTree(rf_model$finalModel, k = 1, labelVar = TRUE)
                          # Convert the tree to a decision tree object
                          tree_model <- rpart(as.formula(paste("Loan.Status ~", paste(selected_features, collapse = " + "))),
                                              data = data_train,
                                              method = "class",
                                              control = rpart.control(cp = 0, maxdepth = 4))
                          # Visualize the single decision tree
                          fancyRpartPlot(tree_model)
                          #### XGBOOST ####
                          # Stratify and split the data into training and testing sets
                          set.seed(40423910) # For reproducibility
                          trainIndex <- createDataPartition(data$Loan.Status, p = 0.8, 
                                                            list = FALSE, 
                                                            times = 1)
                          data_train <- data[trainIndex,]
                          data_test  <- data[-trainIndex,]
                          # Convert data to matrix format for XGBoost
                          train_matrix <- model.matrix(~ . - 1, data_train[, selected_features])
                          train_label <- as.numeric(data_train$Loan.Status) - 1  # XGBoost requires numeric labels starting from 0
                          test_matrix <- model.matrix(~ . - 1, data_test[, selected_features])
                          test_label <- as.numeric(data_test$Loan.Status) - 1
                          # DMatrix for XGBoost
                          dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
                          dtest <- xgb.DMatrix(data = test_matrix, label = test_label)
                          # Set XGBoost parameters
                          params <- list(
                            booster = "gbtree",
                            objective = "binary:logistic",
                            eta = 0.1,
                            max_depth = 6,
                            eval_metric = "error"
                          )
                          # Set up cross-validation
                          train_control <- trainControl(
                            method = "cv", 
                            number = 5, # 5-fold cross-validation
                            verboseIter = TRUE,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary
                          )
                          # Train the XGBoost model with cross-validation
                          xgb_model_cv <- train(
                            x = train_matrix,
                            y = factor(train_label, labels = c("Class0", "Class1")),
                            method = "xgbTree",
                            trControl = train_control,
                            tuneGrid = expand.grid(
                              nrounds = 500, # Number of boosting iterations
                              max_depth = 6, # Max depth of the trees
                              eta = 0.1, # Learning rate
                              gamma = 0, 
                              colsample_bytree = 1, 
                              min_child_weight = 1, 
                              subsample = 1
                            ),
                            metric = "ROC" # Evaluate the model based on AUC
                          )
                          
                          # Initialize a list to store metrics for each fold
                          metrics_list <- list()
                          # Loop through the folds to calculate metrics
                          for (i in 1:5) {
                            fold_predictions <- xgb_model_cv$pred %>% filter(Resample == paste0("Fold", i))
                            fold_actuals <- fold_predictions$obs
                            fold_probs <- fold_predictions$Class1
                            fold_labels <- ifelse(fold_probs > 0.5, 1, 0)
                            # Confusion matrix for the fold
                            conf_matrix <- confusionMatrix(factor(fold_labels), factor(as.numeric(fold_actuals) - 1))
                            # Extract metrics
                            accuracy <- conf_matrix$overall['Accuracy']
                            sensitivity <- conf_matrix$byClass['Sensitivity']
                            specificity <- conf_matrix$byClass['Specificity']
                            # Calculate F1 Score
                            precision <- conf_matrix$byClass['Pos Pred Value']
                            f1_score <- 2 * ((precision * sensitivity) / (precision + sensitivity))
                            # Calculate AUC
                            roc_obj <- roc(as.numeric(fold_actuals) - 1, fold_probs)
                            auc_value <- auc(roc_obj)
                            
                            # Store the metrics
                            metrics_list[[i]] <- c(accuracy, sensitivity, specificity, f1_score, auc_value)
                          }
                          # Convert metrics list to data frame
                          metrics_df <- do.call(rbind, metrics_list)
                          colnames(metrics_df) <- c("Accuracy", "Sensitivity", "Specificity", "F1_Score", "AUC")
                          # Add fold number
                          metrics_df <- cbind(Fold = 1:5, metrics_df)
                          # Calculate mean metrics across folds
                          mean_metrics <- colMeans(metrics_df[, -1])
                          # Append mean metrics to the data frame
                          metrics_df <- rbind(metrics_df, c(Fold = "Mean", mean_metrics))
                          # Save the metrics to a CSV file
                          write.csv(metrics_df, "xgb_metrics_cv_folds.csv", row.names = FALSE)
                          # Print to check the data frame
                          print(metrics_df)
                          #### SUPPORT VECTOR MACHINE ####
                          # Set up cross-validation
                          set.seed(40423910)  # For reproducibility
                          folds <- createFolds(data$Loan.Status, k = 5, list = TRUE, returnTrain = TRUE)
                          
                          # Initialize lists to store metrics for each fold
                          accuracy_list <- c()
                          sensitivity_list <- c()
                          specificity_list <- c()
                          f1_score_list <- c()
                          auc_list <- c()
                          # Loop over each fold
                          for(i in 1:5) {
                            # Split data
                            train_indices <- folds[[i]]
                            test_indices <- setdiff(seq_len(nrow(data)), train_indices)
                            train_data <- data[train_indices, ]
                            test_data <- data[test_indices, ]
                            # Train SVM model
                            svm_model <- svm(Loan.Status ~ ., data = train_data, kernel = "radial", cost = 1, scale = TRUE, probability = TRUE)
                            # Make predictions
                            svm_predictions <- predict(svm_model, newdata = test_data, probability = TRUE)
                            svm_probabilities <- attr(svm_predictions, "probabilities")[,2]
                            # Evaluate the model
                            conf_matrix <- confusionMatrix(svm_predictions, test_data$Loan.Status)
                            # Calculate metrics
                            accuracy <- conf_matrix$overall['Accuracy']
                            sensitivity <- conf_matrix$byClass['Sensitivity']
                            specificity <- conf_matrix$byClass['Specificity']
                            precision <- conf_matrix$byClass['Pos Pred Value']
                            recall <- sensitivity
                            f1_score <- 2 * (precision * recall) / (precision + recall)
                            roc_curve <- roc(test_data$Loan.Status, svm_probabilities)
                            auc_value <- auc(roc_curve)
                            # Store metrics
                            accuracy_list <- c(accuracy_list, accuracy)
                            sensitivity_list <- c(sensitivity_list, sensitivity)
                            specificity_list <- c(specificity_list, specificity)
                            f1_score_list <- c(f1_score_list, f1_score)
                            auc_list <- c(auc_list, auc_value)
                          }
                          # Calculate mean metrics
                          mean_accuracy <- mean(accuracy_list)
                          mean_sensitivity <- mean(sensitivity_list)
                          mean_specificity <- mean(specificity_list)
                          mean_f1_score <- mean(f1_score_list)
                          mean_auc <- mean(auc_list)
                          # Create a data frame to store all the metrics
                          metrics_df <- data.frame(
                            Fold = 1:5,
                            Accuracy = accuracy_list,
                            Sensitivity = sensitivity_list,
                            Specificity = specificity_list,
                            F1_Score = f1_score_list,
                            AUC = auc_list
                          )
                          # Add a row with the mean metrics
                          metrics_df <- rbind(metrics_df, data.frame(
                            Fold = "Mean",
                            Accuracy = mean_accuracy,
                            Sensitivity = mean_sensitivity,
                            Specificity = mean_specificity,
                            F1_Score = mean_f1_score,
                            AUC = mean_auc
                          ))
                          # Save metrics to CSV file
                          write.csv(metrics_df, "svm_cross_validation_metrics.csv", row.names = FALSE)
                          
                          cat("5-fold cross-validation complete. Metrics saved to 'svm_cross_validation_metrics.csv'.")
                          #### ATRIFICAL NEURAL NETWORK ###
                          # Remove "Payment.Plan" column if present
                          if ("Payment.Plan" %in% names(data)) {
                            data <- data[, !names(data) %in% "Payment.Plan"]
                          }
                          # Convert 'Loan.Status' column to a factor with appropriate variable names
                          data$`Loan.Status` <- factor(data$`Loan.Status`, levels = c(0, 1), labels = c("NoDefault", "Default"))
                          # Define the selected features
                          selected_features <- c(
                            'Grade', 'Sub.Grade', 'Delinquency...two.years',
                            'Open.Account', 'Public.Record', 'Initial.List.Status', 'Total.Received.Late.Fee',
                            'Last.week.Pay', 'Total.Collection.Amount', 'Total.Current.Balance', 'Home.Ownership'
                          )
                          # Prepare data for modeling
                          full_data <- data[, c(selected_features, 'Loan.Status')]
                          # Standardize the features
                          preProc <- preProcess(full_data[, selected_features], method = c("center", "scale"))
                          full_data[, selected_features] <- predict(preProc, full_data[, selected_features])
                          
                          # Set up cross-validation
                          set.seed(40423910) # For reproducibility
                          train_control <- trainControl(
                            method = "cv",
                            number = 5, #5-fold cross-validation
                            savePredictions = "final",
                            classProbs = TRUE, # Save class probabilities
                            summaryFunction = twoClassSummary # Use AUC and other metrics
                          )
                          # Define a tuning grid for 'size' (number of units in the hidden layer)
                          tune_grid <- expand.grid(size = c(1, 3, 5), decay = c(0, 0.1, 1e-4))
                          # Train ANN model using cross-validation
                          ann_cv_model <- train(
                            `Loan.Status` ~ .,
                            data = full_data,
                            method = "nnet",
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            metric = "ROC",
                            linout = FALSE,
                            trace = FALSE,
                            maxit = 200
                          )
                          # Output model details
                          print(ann_cv_model)
                          # Save the cross-validated model
                          save(ann_cv_model, file = "ann_cv_model.RData")
                          # Extract saved predictions and true classes
                          predictions <- ann_cv_model$pred
                          # Calculate accuracy for each fold
                          accuracy_per_fold <- aggregate(correct ~ Resample, data = transform(predictions, correct = obs == pred), mean)
                          # Calculate mean ROC, Sensitivity, and Specificity from the results
                          mean_sens <- mean(ann_cv_model$results$Sens)
                          mean_spec <- mean(ann_cv_model$results$Spec)
                          mean_auc <- mean(ann_cv_model$results$ROC)
                          # Extract mean Accuracy from the aggregated data
                          mean_accuracy <- mean(accuracy_per_fold$correct)
                          # Combine all mean metrics into a single dataframe
                          mean_metrics <- data.frame(
                            MeanAccuracy = mean_accuracy,
                            MeanSensitivity = mean_sens,
                            MeanSpecificity = mean_spec,
                            MeanAUC = mean_auc
                          )
                          # Output mean metrics
                          print(mean_metrics)
                          # Save metrics to a CSV file
                          write.csv(mean_metrics, "ann_cv_mean_metrics.csv", row.names = FALSE)
                          #### NAVIES BAYES ####
                          # Rename levels of Loan.Status to valid R variable names
                          levels(data$Loan.Status) <- make.names(levels(data$Loan.Status))
                          # Define the control for k-fold cross-validation (5 folds)
                          train_control <- trainControl(method = "cv", number = 5, 
                                                        classProbs = TRUE, 
                                                        summaryFunction = twoClassSummary, 
                                                        savePredictions = TRUE)
                          # Train the Naive Bayes model with k-fold cross-validation
                          nb_model <- train(Loan.Status ~ ., data = data[, c(selected_features, 'Loan.Status')], 
                                            method = "nb", 
                                            trControl = train_control, 
                                            metric = "ROC")
                          
                          # Extract predictions and calculate metrics for each fold
                          fold_metrics <- data.frame(Resample = paste0("Fold", 1:5))
                          # Initialize columns for metrics
                          fold_metrics$Accuracy <- NA
                          fold_metrics$Specificity <- NA
                          fold_metrics$Precision <- NA
                          fold_metrics$Recall <- NA
                          fold_metrics$F1_Score <- NA
                          fold_metrics$ROC <- NA
                          fold_metrics$AUC <- NA
                          # Calculate metrics for each fold
                          for (i in 1:5) {
                            fold_data <- nb_model$pred[nb_model$pred$Resample == paste0("Fold", i), ]
                            conf_matrix <- confusionMatrix(fold_data$pred, fold_data$obs)
                            accuracy <- conf_matrix$overall["Accuracy"]
                            precision <- conf_matrix$byClass["Pos Pred Value"]
                            recall <- conf_matrix$byClass["Sensitivity"]
                            specificity <- conf_matrix$byClass["Specificity"]
                            f1_score <- 2 * ((precision * recall) / (precision + recall))
                            
                            # Determine the correct column name for the positive class probability
                            positive_class <- levels(fold_data$obs)[2]
                            roc_auc <- roc(fold_data$obs, fold_data[[positive_class]], levels = rev(levels(fold_data$obs)))$auc
                            fold_metrics$Accuracy[i] <- accuracy
                            fold_metrics$Precision[i] <- precision
                            fold_metrics$Recall[i] <- recall
                            fold_metrics$Specificity[i] <- specificity
                            fold_metrics$F1_Score[i] <- f1_score
                            fold_metrics$AUC[i] <- roc_auc
                            fold_metrics$ROC[i] <- roc_auc
                          }
                          # Calculate the mean of each metric across all folds
                          mean_metrics <- data.frame(Resample = "Mean",
                                                     Accuracy = mean(fold_metrics$Accuracy, na.rm = TRUE),
                                                     Specificity = mean(fold_metrics$Specificity, na.rm = TRUE),
                                                     Precision = mean(fold_metrics$Precision, na.rm = TRUE),
                                                     Recall = mean(fold_metrics$Recall, na.rm = TRUE),
                                                     F1_Score = mean(fold_metrics$F1_Score, na.rm = TRUE),
                                                     ROC = mean(fold_metrics$ROC, na.rm = TRUE),
                                                     AUC = mean(fold_metrics$AUC, na.rm = TRUE))
                          
                          # Combine the individual fold metrics with the mean metrics
                          final_metrics <- rbind(fold_metrics, mean_metrics)
                          # Save the metrics to a CSV file
                          write.csv(final_metrics, "naive_bayes_kfold_detailed_results.csv", row.names = FALSE)
                          # Save the model and results for future use
                          save(nb_model, file = "naive_bayes_kfold_model.RData")
                          # Output to confirm the metrics have been saved
                          cat("Metrics have been saved to naive_bayes_kfold_detailed_results.csv\n")
                          cat("Model has been saved to naive_bayes_kfold_model.RData\n")
                          ##############################################################
                          #### METRICS COMPARISOM VISUALIZATIONS & TABLES ####
                          #############################################################
                          #### BASE MODELS - METRICS COMPARISION GRAPH AND TABLE ####
                          # Load necessary libraries
                          library(ggplot2)
                          library(dplyr)
                          library(pROC)
                          library(kableExtra)
                          library(tidyr)
                          
                          
                          # Load metrics from CSV files
                          logistic_metrics <- read.csv("logistic_metrics.csv")
                          naive_bayes_metrics <- read.csv("naive_bayes_metrics.csv")
                          rf_metrics <- read.csv("rf_metrics.csv")
                          svm_metrics <- read.csv("svm_metrics.csv")
                          xgb_metrics <- read.csv("xgb_metrics.csv")
                          ann_metrics <- read.csv("ann_metrics.csv")
                          # Load ROC data from CSV files
                          logistic_roc <- read.csv("logistic_roc_details.csv")
                          naive_bayes_roc <- read.csv("naive_bayes_roc_details.csv")
                          rf_roc <- read.csv("rf_roc_details.csv")
                          svm_roc <- read.csv("svm_roc_details.csv")
                          xgb_roc <- read.csv("xgb_roc_details.csv")
                          ann_roc <- read.csv("ann_roc_details.csv")
                          # Add a column to each metrics dataframe indicating the model
                          logistic_metrics$Model <- "Logistic Regression"
                          naive_bayes_metrics$Model <- "Naive Bayes"
                          rf_metrics$Model <- "Random Forest"
                          svm_metrics$Model <- "SVM"
                          xgb_metrics$Model <- "XGBoost"
                          ann_metrics$Model <- "ANN"
                          # Add a column to each ROC dataframe indicating the model
                          logistic_roc$Model <- "Logistic Regression"
                          naive_bayes_roc$Model <- "Naive Bayes"
                          rf_roc$Model <- "Random Forest"
                          svm_roc$Model <- "SVM"
                          xgb_roc$Model <- "XGBoost"
                          ann_roc$Model <- "ANN"
                          # Combine all ROC data into one dataframe
                          all_roc <- bind_rows(logistic_roc, naive_bayes_roc, rf_roc, svm_roc, xgb_roc, ann_roc)
                          # Dictionary of AUC values
                          auc_values <- list(
                            "ANN" = 0.7096,
                            "Logistic Regression" = 0.5263,
                            "Naive Bayes" = 0.5804,
                            "Random Forest" = 0.9193,
                            "SVM" = 0.6879,
                            "XGBoost" = 0.9436
                          )
                          # Adjust model names to include AUC in the legend
                          all_roc <- all_roc %>%
                            mutate(Model = paste0(Model, " (AUC = ", auc_values[Model], ")"))
                          # Combine all metrics into one dataframe
                          all_metrics <- bind_rows(logistic_metrics, naive_bayes_metrics, rf_metrics, svm_metrics, xgb_metrics, ann_metrics)
                          # Rearrange columns for consistency
                          all_metrics <- all_metrics %>%
                            select(Model, Metric, Value)
                          # Plot the ROC curves for all models, including AUC in the legend
                          ggplot(all_roc, aes(x = FPR, y = TPR, color = Model)) +
                            geom_line(size = 1.2) +
                            geom_abline(linetype = "dashed", color = "gray") +
                            labs(title = "ROC Curve Comparison: All Models", x = "False Positive Rate", y = "True Positive Rate") +
                            theme_minimal() +
                            theme(legend.position = "bottom") +
                            scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"))
                          # Plot Accuracy for each model in increasing order
                          ggplot(all_metrics %>% filter(Metric == "Accuracy") %>%
                                   arrange(Value), 
                                 aes(x = reorder(Model, Value), y = Value, fill = Model)) +
                            geom_bar(stat = "identity") +
                            geom_text(aes(label = round(Value, 3)), vjust = -0.5) +
                            labs(title = "Model Comparison: Accuracy", y = "Accuracy", x = "Model") +
                            theme_minimal() +
                            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                            scale_fill_brewer(palette = "Set1")
                          # Plot Sensitivity for each model in increasing order
                          ggplot(all_metrics %>% filter(Metric == "Sensitivity") %>%
                                   arrange(Value), 
                                 aes(x = reorder(Model, Value), y = Value, fill = Model)) +
                            geom_bar(stat = "identity") +
                            geom_text(aes(label = round(Value, 3)), vjust = -0.5) +
                            labs(title = "Model Comparison: Sensitivity", y = "Sensitivity", x = "Model") +
                            theme_minimal() +
                            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                            scale_fill_brewer(palette = "Set1")
                          # Plot Specificity for each model in increasing order
                          ggplot(all_metrics %>% filter(Metric == "Specificity") %>%
                                   arrange(Value), 
                                 aes(x = reorder(Model, Value), y = Value, fill = Model)) +
                            geom_bar(stat = "identity") +
                            geom_text(aes(label = round(Value, 3)), vjust = -0.5) +
                            labs(title = "Model Comparison: Specificity", y = "Specificity", x = "Model") +
                            theme_minimal() +
                            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                            scale_fill_brewer(palette = "Set1")
                          # Combine all metrics into one dataframe
                          all_metrics <- bind_rows(logistic_metrics, naive_bayes_metrics, rf_metrics, svm_metrics, xgb_metrics, ann_metrics)
                          # Rearrange columns for consistency
                          all_metrics <- all_metrics %>%
                            select(Model, Metric, Value)
                          # Pivot the metrics data to wide format
                          metrics_wide <- all_metrics %>%
                            pivot_wider(names_from = Metric, values_from = Value)
                          # Create a colorful table of all metrics with blue and white combination
                          metrics_wide %>%
                            kbl(caption = "Model Performance Metrics", digits = 3) %>%
                            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                            row_spec(0, bold = TRUE, background = "#004080", color = "white") %>%  # Blue header with white text
                            column_spec(1, bold = TRUE, background = "#cce6ff", color = "black") %>%  # Light blue for model names
                            column_spec(2:ncol(metrics_wide), background = "#e6f2ff", color = "black") %>%  # Light blue for metric values
                            column_spec(2:ncol(metrics_wide), border_right = TRUE)  # Add borders to columns
                          #### CROSS-VALIDATED MODELS ####
                          # Load necessary libraries
                          library(kableExtra)
                          library(dplyr)
                          # Load the CSV files
                          ann_cv <- read.csv("Model R/ann_cv_metrics.csv")
                          logistic_kfold <- read.csv("Model R/logistic_kfold_detailed_results.csv")
                          rf_kfold <- read.csv("Model R/rf_kfold_detailed_results.csv")
                          nb_kfold <- read.csv("Model R/naive_bayes_kfold_detailed_results.csv")
                          svm_cv <- read.csv("Q:/svm_cross_validation_metrics.csv")
                          xgb_cv <- read.csv("xgb_metrics_cv_folds.csv")
                          # Calculate the mean metrics for each model
                          mean_metrics <- data.frame(
                            Model = c("Logistic Regression", "Random Forest", "Naive Bayes", "SVM", "XGBoost"),
                            Accuracy = c(mean(logistic_kfold$Accuracy, na.rm = TRUE),
                                         mean(rf_kfold$Accuracy, na.rm = TRUE),
                                         mean(nb_kfold$Accuracy, na.rm = TRUE),
                                         mean(svm_cv$Accuracy, na.rm = TRUE),
                                         mean(xgb_cv$Accuracy, na.rm = TRUE)),
                            Sensitivity = c(mean(logistic_kfold$Sensitivity, na.rm = TRUE),
                                            mean(rf_kfold$Sensitivity, na.rm = TRUE),
                                            mean(nb_kfold$Sensitivity, na.rm = TRUE), 
                                            mean(svm_cv$Sensitivity, na.rm = TRUE),
                                            mean(xgb_cv$Sensitivity, na.rm = TRUE)),
                            Specificity = c(mean(logistic_kfold$Specificity, na.rm = TRUE),
                                            mean(rf_kfold$Specificity, na.rm = TRUE),
                                            mean(nb_kfold$Specificity, na.rm = TRUE),
                                            mean(svm_cv$Specificity, na.rm = TRUE),
                                            mean(xgb_cv$Specificity, na.rm = TRUE)),
                            F1_Score = c(mean(logistic_kfold$F1_Score, na.rm = TRUE),
                                         mean(rf_kfold$F1_Score, na.rm = TRUE),
                                         mean(nb_kfold$F1_Score, na.rm = TRUE),
                                         mean(svm_cv$F1_Score, na.rm = TRUE),
                                         mean(xgb_cv$F1_Score, na.rm = TRUE)),
                            AUC = c(mean(logistic_kfold$AUC, na.rm = TRUE),
                                    mean(rf_kfold$AUC, na.rm = TRUE),
                                    mean(nb_kfold$AUC, na.rm = TRUE),
                                    mean(svm_cv$AUC, na.rm = TRUE),
                                    mean(xgb_cv$AUC, na.rm = TRUE))
                          )
                          # Input the ANN metrics directly
                          ann_metrics <- data.frame(
                            Model = "ANN",
                            Accuracy = 0.66696402,
                            Sensitivity = 0.936208791,
                            Specificity = 0.370894004,
                            F1_Score = 0.727,
                            AUC = 0.696398747
                          )
                          # Combine ANN metrics with other models
                          combined_metrics <- rbind(ann_metrics, mean_metrics)
                          # Create a colorful table using kableExtra
                          combined_metrics %>%
                            kbl(align = 'c', caption = "Metrics of Machine Learning Models With K-Fold Cross-Validation") %>%
                            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                                          full_width = F, font_size = 15) %>%
                            row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") %>%
                            column_spec(1, bold = TRUE, color = "white", background = "#4CAF50") %>%
                            column_spec(2:6, background = "#E8F5E9") %>%
                            row_spec(1:nrow(combined_metrics), color = "black", background = "#E8F5E9")
                          #### METRICS EXPLANATION TABLE ####
                          # Install necessary packages if not already installed
                          if (!require("kableExtra")) install.packages("kableExtra", dependencies = TRUE)
                          # Load necessary libraries
                          library(kableExtra)
                          # Define the data
                          df <- data.frame(
                            Metric = c("Accuracy", "Precision", "Recall (Sensitivity)", "F1-Score", "ROC-AUC"),
                            Formula = c(
                              "(TP + TN) / (TP + TN + FP + FN)",
                              "TP / (TP + FP)",
                              "TP / (TP + FN)",
                              "2 * (Precision * Recall) / (Precision + Recall)",
                              "AUC of the ROC curve"
                            ),
                            Description = c(
                              "Accuracy measures the overall correctness of the model, indicating the proportion of true results (both true positives and true negatives) among the total number of cases examined.",
                              "Precision, also known as Positive Predictive Value, measures the proportion of positive predictions that are actually correct, i.e., how many selected items are relevant.",
                              "Recall, or Sensitivity, measures the proportion of actual positives that are correctly identified by the model, i.e., how many relevant items are selected.",
                              "F1-Score is the harmonic mean of Precision and Recall, providing a single metric that balances both concerns, particularly useful in cases of imbalanced datasets.",
                              "ROC-AUC measures the area under the ROC curve, which plots the true positive rate against the false positive rate at various threshold settings, indicating the model's ability to discriminate between classes."
                            )
                          )
                          # Create the table using kableExtra
                          df %>%
                            kable("html", escape = FALSE, col.names = c("Metric", "Formula", "Description")) %>%
                            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
                            row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") %>%
                            column_spec(1, bold = TRUE, color = "#00796B") %>%
                            column_spec(2, color = "#E64A19") %>%
                            column_spec(3, width = "30em", color = "#37474F")
                          #### K-Fold Crossvalidation - IMAGE ####
                          # Load necessary libraries
                          library(ggplot2)
                          library(grid)
                          
                          # Define the number of folds
                          k <- 5
                          # Create a data frame to represent the structure of k-fold cross-validation
                          fold_data <- expand.grid(Split = factor(1:k), Fold = factor(1:k))
                          fold_data$Role <- ifelse(fold_data$Split == fold_data$Fold, "Validation", "Training")
                          # Create a label column to show fold numbers inside the grid
                          fold_data$Label <- ifelse(fold_data$Role == "Validation", paste("Fold", fold_data$Fold), "")
                          # Base plot
                          p <- ggplot(fold_data, aes(x = Split, y = Fold, fill = Role)) +
                            geom_tile(color = "black", size = 0.5) +
                            scale_fill_manual(values = c("Training" = "lightgreen", "Validation" = "lightblue")) +
                            geom_text(aes(label = Label), size = 5, color = "black") +  # Add fold numbers inside the grid
                            labs(x = "Data Splits", y = "Folds", fill = "Role") +
                            theme_minimal() +
                            theme(
                              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                              axis.text = element_text(size = 12),
                              axis.title = element_text(size = 14),
                              legend.title = element_text(size = 12),
                              legend.text = element_text(size = 10)
                            )
                          # Add custom annotations to explain each part
                          p <- p + annotate("text", x = 6, y = 3, label = "Finding Parameters", angle = 90, size = 5, fontface = "bold") +
                            annotate("text", x = 3, y = -0.5, label = "Each fold used as Validation\nAll other folds used as Training", size = 4, fontface = "italic", color = "darkgray") +
                            annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0.5, ymax = 5.5, alpha = .2, fill = "yellow") +
                            annotate("text", x = 7, y = 3, label = "Test Data\n(Final Evaluation)", color = "black", size = 4, fontface = "bold") +
                            labs(title = "Illustration of K-Fold Cross-Validation")
                          # Draw the plot
                          print(p)
                          #### Bayesian Optimization - IMAGE  ####
                          # Load necessary libraries
                          library(ggplot2)
                          library(dplyr)
                          # Define the objective function to be optimized (e.g., a simple quadratic function)
                          objective_function <- function(x) {
                            return((x - 2) ^ 2 + sin(5 * x))
                          }
                          # Generate data points for plotting the function
                          x_values <- seq(-1, 5, length.out = 100)
                          y_values <- objective_function(x_values)
                          data <- data.frame(x = x_values, y = y_values)
                          # Simulate the Bayesian optimization process
                          # Starting with some initial points
                          initial_points <- data.frame(x = c(0, 4), y = objective_function(c(0, 4)))
                          # Surrogate model (fit a linear model to the initial points)
                          surrogate_model <- lm(y ~ poly(x, 1), data = initial_points)
                          # Predict values using the surrogate model across the entire x range
                          predicted_values <- predict(surrogate_model, newdata = data.frame(x = x_values))
                          # Acquisition function (we'll simulate it as the negative of the predicted values)
                          acquisition_function <- -predicted_values + min(predicted_values)
                          # Determine the next point to evaluate (where the acquisition function is highest)
                          next_point <- data.frame(x = x_values[which.max(acquisition_function)], 
                                                   y = objective_function(x_values[which.max(acquisition_function)]))
                          # Plot the objective function, surrogate model, and acquisition function
                          p <- ggplot(data, aes(x = x, y = y)) +
                            geom_line(color = "blue", size = 1, alpha = 0.6, linetype = "solid") +  # Objective function
                            geom_line(aes(y = predicted_values), color = "red", linetype = "dashed", size = 1) +  # Surrogate model
                            geom_line(aes(y = acquisition_function), color = "green", linetype = "dotted", size = 1) +  # Acquisition function
                            geom_point(data = initial_points, aes(x = x, y = y), color = "black", size = 3) +  # Initial points
                            geom_point(data = next_point, aes(x = x, y = y), color = "purple", size = 4, shape = 17) +  # Next point
                            annotate("text", x = 4.5, y = max(acquisition_function), label = "Acquisition Function", color = "green", size = 4, vjust = -1) +
                            annotate("text", x = 4.5, y = max(predicted_values), label = "Surrogate Model", color = "red", size = 4, vjust = -1) +
                            annotate("text", x = 4.5, y = max(y_values), label = "Objective Function", color = "blue", size = 4, vjust = -1) +
                            annotate("text", x = next_point$x, y = next_point$y, label = "Next Point", color = "purple", size = 4, vjust = -1) +
                            labs(title = "Bayesian Optimization Process", x = "Parameter Space", y = "Objective Value") +
                            theme_minimal()
                          print(p)
                          #### SUMMARY STATISTICS - TABLE ####
                          # Assuming the data is already loaded into a data frame called 'data'
                          data_info <- data.frame(
                            Variable = names(data),
                            Type = sapply(data, class),
                            Description = c(
                              "Unique Identifier", "Loan Amount Requested", "Amount Funded by Lenders", 
                              "Amount Funded by Investors", "Loan Term (months)", "Batch Enrollment Status",
                              "Interest Rate of Loan", "Credit Grade", "Sub-Grade of Credit",
                              "Home Ownership Status", "Verification of Income Status", "Whether on Payment Plan",
                              "Title of Loan", "Debt-to-Income Ratio", "Delinquencies in Last 2 Years",
                              "Inquiries in Last 6 Months", "Number of Open Accounts", "Public Records",
                              "Revolving Balance", "Revolving Credit Utilization", "Total Number of Accounts",
                              "Initial Listing Status", "Total Interest Received", "Total Late Fees Received",
                              "Recoveries from Charged-off Loans", "Fee for Collection Recovery", 
                              "Medical Collection in Last 12 Months", "Type of Application", "Last Week's Payment",
                              "Number of Delinquent Accounts", "Total Amount in Collections", 
                              "Total Current Balance", "Total Revolving Credit Limit", "Loan Status"
                            ) ) # Ensure the count of descriptions matches the number of variables
                          # Create a colorful table using kable and kableExtra
                          kable(data_info, "html") %>%
                            kable_classic(full_width = FALSE, html_font = "Cambria") %>%
                            row_spec(0, bold = TRUE, color = "white", background = "#4F81BD") %>%
                            row_spec(which(data_info$Type == "character"), background = "lightpink") %>%
                            row_spec(which(data_info$Type == "numeric"), background = "lightblue") %>%
                            column_spec(1, bold = TRUE, border_right = TRUE) %>%
                            column_spec(2:3, border_left = TRUE, border_right = TRUE) %>%
                            kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE, font_size = 16) %>%
                            scroll_box(width = "100%", height = "600px")
                          