if (!require("tools"))
{
  install.packages("tools", dependencies = TRUE)
}
library(tools)

if (!require("base"))
{
  install.packages("base", dependencies = TRUE)
}
library(base)

if (!require("dplyr"))
{
  install.packages("dplyr", dependencies = TRUE)
}
library(dplyr)

if (!require("ggplot2"))
{
  install.packages("ggplot2", dependencies = TRUE)
}
library(ggplot2)

if (!require("ggthemes"))
{
  install.packages("ggthemes", dependencies = TRUE)
}
library(ggthemes)

if (!require("caret"))
{
  install.packages("caret", dependencies = TRUE)
}
library(caret)

if (!require("readxl"))
{
  install.packages("readxl", dependencies = TRUE)
}
library(readxl)

if (!require("tidyr"))
{
  install.packages("tidyr", dependencies = TRUE)
}
library(tidyr)

if (!require("kableExtra"))
{
  install.packages("kableExtra", dependencies = TRUE)
}
library(kableExtra)

if (!require("gridExtra"))
{
  install.packages("gridExtra", dependencies = TRUE)
}
library(gridExtra)

if (!require("reshape2"))
{
  install.packages("reshape2", dependencies = TRUE)
}
library(reshape2)

if (!require("useful"))
{
  install.packages("useful", dependencies = TRUE)
}
library(useful)

if (!require("tidymodels"))
{
  install.packages("tidymodels", dependencies = TRUE)
}
library(tidymodels)
tidymodels_prefer()

if (!require("fastDummies"))
{
  install.packages("fastDummies", dependencies = TRUE)
}
library(fastDummies)

if (!require("glmnet"))
{
  install.packages("glmnet", dependencies = TRUE)
}
library(glmnet)

if (!require("e1071"))
{
  install.packages("e1071", dependencies = TRUE)
}
library(e1071)

if (!require("pROC"))
{
  install.packages("pROC", dependencies = TRUE)
}
library(pROC)

if (!require("rpart"))
{
  install.packages("rpart", dependencies = TRUE)
}
library(rpart)

if (!require("randomForest"))
{
  install.packages("randomForest", dependencies = TRUE)
}
library(randomForest)


# Reads the data
data <- read_excel("./Tourism.xlsx",sheet = "Tourism")

# Shows the first five rows
head(data)

# Dataset size
dim(data)

# Check missing values
as.data.frame(colSums(is.na(data))) %>% kable(col.names = c("# of NA"))

# Check unique values in each column
data %>% summarise_all((n_distinct)) %>% pivot_longer(everything(), names_to = c("Column Name"), values_to = c("# Unique Values"))

# Drop CustomerID since it has an unique value for each row
data <- select(data, -"CustomerID")

# Get data summary
summary(data)

# Values on each categorical variable
table(data['TypeofContact']) %>% kable()
table(data['CityTier']) %>% kable()
table(data['Occupation']) %>% kable()
table(data['Gender']) %>% kable()
table(data['NumberOfPersonVisiting']) %>% kable()
table(data['NumberOfFollowups']) %>% kable()
table(data['ProductPitched']) %>% kable()
table(data['PreferredPropertyStar']) %>% kable()
table(data['MaritalStatus']) %>% kable()
table(data['Passport']) %>% kable()
table(data['PitchSatisfactionScore']) %>% kable()
table(data['OwnCar']) %>% kable()
table(data['NumberOfChildrenVisiting']) %>% kable()
table(data['Designation']) %>% kable()


# Fix Gender by replacing "Fe Male" with "Female"
data$Gender[data$Gender == "Fe Male"] <- "Female"
table(data['Gender']) %>% kable()

# Remove 'Free Lancer'
data <- data %>% filter(Occupation != 'Free Lancer')
table(data['Occupation']) %>% kable()

# Make categorical columns a factor
factor_variables = c('TypeofContact', 'CityTier','Occupation', 'Gender', 'NumberOfPersonVisiting', 'NumberOfFollowups', 'ProductPitched',
                     'PreferredPropertyStar', 'MaritalStatus', 'Passport', 'PitchSatisfactionScore', 'OwnCar', 'NumberOfChildrenVisiting', 'Designation')

data[factor_variables] <- lapply(data[factor_variables], factor)

# Plot the distribution of continuous variables
plot_distribution <- function(data, column, columnName)
{
  # Create a histogram to analyze distribution
  hist_plot <- data %>% ggplot(aes(x = column)) +
    geom_histogram(aes(y = ..density..), fill = '#0000DD', binwidth = 0.5) +
    geom_density(alpha = 0.2, fill = '#DD0000') +
    xlab(columnName) +
    ylab("Density") +
    theme_economist()
  
  # Create a boxplot to analyze outliers and common values
  box_plot <- data %>% ggplot(aes(x = column)) +
    geom_boxplot(fill = '#3377EE', color='#000077', width = 0.05) +
    xlab('') +
    theme_economist() +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
  
  # Create the QQ-plot to determine if the variable follows the normal distribution
  qq_plot <- data %>% ggplot(aes(sample = column)) +
    stat_qq() +
    stat_qq_line() +
    theme_economist()
  
  # Combine all plots
  grid.arrange(box_plot, hist_plot, qq_plot, ncol = 1, nrow = 3, heights = c(1, 3, 3))
}

# Plot the number of occurrences for categorical variables
plot_count <- function(data, column, columnName)
{
  # Create a histogram with the number of occurrences of each category
  data %>% ggplot(aes(x = column)) +
    geom_bar(fill = '#3377EE') +
    geom_text(aes(label = ..count..), stat='count', vjust = -0.5, color = '#000077') +
    xlab(columnName) +
    theme_economist()
}

# Age distribution
plot_distribution(data, data$Age, "Age")

# Duration of pitch distribution
plot_distribution(data, data$DurationOfPitch, "Duration of Pitch")

# Get DurationOfPitch outliers
data %>% filter(DurationOfPitch > 40)

# Remove DurationOfPitch outliers
data <- data %>% filter(DurationOfPitch <= 40)

# Monthly income distribution
plot_distribution(data, data$MonthlyIncome, "Monthly Income")

# Get MonthlyIncome outliers
data %>% filter(MonthlyIncome < 12000 | MonthlyIncome > 40000)

# Remove MonthlyIncome outliers
data <- data %>% filter(MonthlyIncome >= 12000 & MonthlyIncome <= 40000)

# Number of trips distribution
plot_distribution(data, data$NumberOfTrips, "Number of Trips")

# Get NumberOfTrips outliers
data %>% filter(NumberOfTrips > 10)

# Remove NumberOfTrips outliers
data <- data %>% filter(NumberOfTrips <= 10)

# Number of persons distribution
plot_count(data, data$NumberOfPersonVisiting, "Persons Visiting")

# Occupation distribution
plot_count(data, data$Occupation, "Occupation")

# City tier distribution
plot_count(data, data$CityTier, "City Tier")

# Gender distribution
plot_count(data, data$Gender, "Gender")

# Number of follow-ups distribution
plot_count(data, data$NumberOfFollowups, "Number of Follow-ups")

# Product pitched distribution
plot_count(data, data$ProductPitched, "Product Pitched")

# Type of contact distribution
plot_count(data, data$TypeofContact, "Type of Contact")

# Designation distribution
plot_count(data, data$Designation, "Designation")

# Product taken distribution
plot_count(data, data$ProdTaken, "Product Taken")

# Set the missing values to the median on the Age column
data$Age <- simple.impute(data$Age, fun = median)

# Set the missing values to the most frequent value on categorical columns
Mode <- function(x)
{
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

data$NumberOfFollowups <- simple.impute(data$NumberOfFollowups, fun = Mode)
data$PreferredPropertyStar <- simple.impute(data$PreferredPropertyStar, fun = Mode)
data$NumberOfChildrenVisiting <- simple.impute(data$NumberOfChildrenVisiting, fun = Mode)

# Check that there are no missing values anymore
as.data.frame(colSums(is.na(data))) %>% kable(col.names = c("# of NA"))

# Plot the correlation matrix
numeric_data <- select_if(data, is.numeric)
correlation_matrix <- round(cor(numeric_data), 2)
distance <- as.dist((1-correlation_matrix)/2)
hc <- hclust(distance)

correlation_matrix <- correlation_matrix[hc$order, hc$order]

melt(correlation_matrix) %>% ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color='#FFFFFF', size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) 


# Convert ProdTaken to factor
data['ProdTaken'] <- lapply(data['ProdTaken'], factor)

# Plot a variable against ProdTaken
plot_var_vs_prod_taken <- function(data, variable, variableName)
{
  data %>% ggplot(aes(x = variable, fill = ProdTaken)) +
    geom_bar(position = 'fill', stat = 'count') +
    labs(y = "Percentage") +
    labs(title = paste(variableName, 'vs ProdTaken')) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)
}

# MaritalStatus vs ProdTaken
plot_var_vs_prod_taken(data, data$MaritalStatus, "MaritalStatus")

# ProductPitched vs ProdTaken
plot_var_vs_prod_taken(data, data$ProductPitched, 'ProductPitched')

# Passport vs ProdTaken
plot_var_vs_prod_taken(data, data$Passport, 'Passport')

# Designation vs ProdTaken
plot_var_vs_prod_taken(data, data$Designation, 'Designation')


# Remove unnecessary variables
data <- select(data, -c('DurationOfPitch','NumberOfFollowups','ProductPitched','PitchSatisfactionScore'))

# Create dummy variables
data <- dummy_cols(data, select_columns = c('TypeofContact', 'Occupation', 'Gender', 'MaritalStatus', 'Designation', 'CityTier'), remove_selected_columns = TRUE)


# Separate training and validation sets
# Splits the data in 70% for training and 30% for testing
data_split <- initial_split(data, prop = 0.7, strata = ProdTaken)
data_train <- training(data_split)
data_test <- testing(data_split)

# Separate the independent variable
# Creates a X dataset containing the dependent variables and Y containing the independent variable for both training and testing
X_train <- select(data_train, -'ProdTaken')
Y_train <- data_train['ProdTaken']

X_test <- select(data_test, -'ProdTaken')
Y_test <- data_test['ProdTaken']

# Show the performance of a model by comparing its predictions with the actual values
show_results <- function(actual, predicted)
{
  # Generate the confusion matrix
  cm <- confusionMatrix(predicted, actual)
  plt <- as.data.frame(cm$table)
  plt$Prediction <- factor(plt$Prediction, levels = levels(plt$Prediction))
  
  # Plot the confusion matrix in a graphical way
  print(ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
        geom_tile() + geom_text(aes(label=Freq)) +
        scale_fill_gradient(low="white", high="#009194") +
        labs(x = "Actual",y = "Prediction") +
        scale_x_discrete(labels=c("Won't Purchase","Will Purchase")) +
        scale_y_discrete(labels=c("Won't Purchase","Will Purchase")))
  
  # Return the accuracy and the recall so that we can compare the models
  return(list(accuracy = cm$overall['Accuracy'], recall = caret::recall(actual, predicted)))
}

# Train the logistic regression model using the training data
lg_fit <- glm(ProdTaken ~ ., data = data_train, family = "binomial")

# Predict the target variable using the training data
lg_scores <- predict(lg_fit, data_train, type="response")
y_hat_train <- ifelse(lg_scores > 0.5, 1, 0) |> factor()

# Evaluate the model performance on the training data
lg_train_result <- show_results(Y_train$ProdTaken, y_hat_train)

# Store the accuracy and the recall
lg_train_accuracy <- lg_train_result$accuracy
lg_train_recall <- lg_train_result$recall

# Predict the target variable on the test set
y_hat_test <- ifelse(predict(lg_fit, data_test, type="response") > 0.5, 1, 0) |> factor()

# Evaluate the model performance on the test set
lg_test_result <- show_results(Y_test$ProdTaken, y_hat_test)

# Store the accuracy and the recall
lg_test_accuracy <- lg_test_result$accuracy
lg_test_recall <- lg_test_result$recall

# Create a data frame with the predicted scores and actual labels
pr_df <- data.frame(scores = as.numeric(lg_scores), labels = as.numeric(levels(data_train$ProdTaken)))

# Calculate precision and recall for different threshold values
precision_recall <- lapply(seq(0, 1, 0.01), function(threshold) {
  # Compute confusion matrix
  cm <- matrix(c(table(pr_df$labels, pr_df$scores > threshold)), ncol = 2, nrow = 2)

  # Calculate precision and recall
  precision <- cm[2, 2] / sum(cm[, 2])
  recall <- cm[2, 2] / sum(cm[2, ])

  # Return a data frame with threshold, precision, and recall
  data.frame(threshold = threshold, precision = precision, recall = recall)
})

# Combine the results into a single data frame
precision_recall_df <- do.call(rbind, precision_recall)

# Calculate F1 score
precision_recall_df$F1 <- 2 * precision_recall_df$precision * precision_recall_df$recall /
  (precision_recall_df$precision + precision_recall_df$recall)

# Find the threshold value that maximizes the F1 score to have precision and recall balanced
optimal_threshold <- precision_recall_df$threshold[which.max(precision_recall_df$F1)]

# Plot precision-recall curve
ggplot(data = precision_recall_df, aes(x = threshold)) +
  geom_line(aes(y = precision, color = "Precision")) +
  geom_line(aes(y = recall, color = "Recall")) +
  scale_x_continuous(name = "Threshold", limits = c(0, 1)) +
  scale_y_continuous(name = "Value", limits = c(0, 1)) +
  labs(title = "Precision-Recall Curve with Optimal Threshold") +
  geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "red") +
  scale_color_manual(name = "", values = c("Precision" = "blue", "Recall" = "red")) +
  annotate("text", x = optimal_threshold, y = max(precision_recall_df$precision),
           label = paste("Threshold:", round(optimal_threshold, 2)), vjust = -1, hjust = -0.1)



# Create an SVM model using the linear kernel and train it using the training data
svm_fit_linear <- svm(ProdTaken ~ ., data = data_train, kernel = "linear", probability=TRUE)

# Use the model to predict the target variable on the training set
y_hat_svm_linear_train <- predict(svm_fit_linear, data_train, probability=TRUE)

#Evaluate the model performance on the training data
svm_linear_train_result <- show_results(data_train$ProdTaken, y_hat_svm_linear_train)

# Store the accuracy and the recall
svm_linear_train_accuracy <- svm_linear_train_result$accuracy
svm_linear_train_recall <- svm_linear_train_result$recall

# Use the model to predict the target variable on the test set
y_hat_svm_linear_test <- predict(svm_fit_linear, data_test, probability=TRUE)

# Evaluate the model performance on the test set
svm_linear_test_result <- show_results(data_test$ProdTaken, y_hat_svm_linear_test)

# Store the accuracy and the recall
svm_linear_test_accuracy <- svm_linear_test_result$accuracy
svm_linear_test_recall <- svm_linear_test_result$recall

svm_scores <- attr(y_hat_svm_linear_train, 'probabilities')
y_scores_pos <- svm_scores[, 2]

# Create a data frame with the predicted scores and actual labels
pr_df <- data.frame(scores = y_scores_pos, labels = data_train$ProdTaken)

# Calculate precision and recall for different threshold values
precision_recall <- lapply(seq(0, 1, 0.01), function(threshold) {
  # Compute confusion matrix
  cm <- matrix(c(table(pr_df$labels, pr_df$scores > threshold)), ncol = 2, nrow = 2)
  
  # Calculate precision and recall
  precision <- cm[2, 2] / sum(cm[, 2])
  recall <- cm[2, 2] / sum(cm[2, ])
  
  # Return a data frame with threshold, precision, and recall
  data.frame(threshold = threshold, precision = precision, recall = recall)
})

# Combine the results into a single data frame
precision_recall_df <- do.call(rbind, precision_recall)

# Calculate F1 score
precision_recall_df$F1 <- 2 * precision_recall_df$precision * precision_recall_df$recall /
  (precision_recall_df$precision + precision_recall_df$recall)

# Find the threshold value that maximizes the F1 score to have precision and recall balanced
optimal_threshold <- precision_recall_df$threshold[which.max(precision_recall_df$F1)]

# Plot precision-recall curve
ggplot(data = precision_recall_df, aes(x = threshold)) +
  geom_line(aes(y = precision, color = "Precision")) +
  geom_line(aes(y = recall, color = "Recall")) +
  scale_x_continuous(name = "Threshold", limits = c(0, 1)) +
  scale_y_continuous(name = "Value", limits = c(0, 1)) +
  labs(title = "Precision-Recall Curve with Optimal Threshold") +
  geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "red") +
  scale_color_manual(name = "", values = c("Precision" = "blue", "Recall" = "red")) +
  annotate("text", x = optimal_threshold, y = max(precision_recall_df$precision),
           label = paste("Threshold:", round(optimal_threshold, 2)), vjust = -1, hjust = -0.1)

# Predict the target variable
y_hat_svm_linear_opt <- predict(svm_fit_linear, data_test, probability=TRUE)
#y_hat_svm_linear_opt <- y_hat_svm_linear_opt[,2] > optimal_threshold

# Check the model performance using the optimal threshold.
svm_linear_opt_result <- show_results(data_test$ProdTaken, factor(as.numeric(attr(y_hat_svm_linear_opt, 'probabilities')[,2] > optimal_threshold)))

# Store the accuracy and the recall
svm_linear_opt_accuracy <- svm_linear_opt_result$accuracy
svm_linear_opt_recall <- svm_linear_opt_result$recall


# Create an SVM model using the RBF kernel and train it using the training data
svm_fit_rbf <- svm(ProdTaken ~ ., data = data_train, kernel = "radial", probability=TRUE)

# Predict the target variable on the training data
y_hat_svm_rbf_train <- predict(svm_fit_rbf, data_train, probability=TRUE)

# Evaluate the performance of the model
svm_rbf_train_result <- show_results(data_train$ProdTaken, y_hat_svm_rbf_train)

# Store the accuracy and the recall
svm_rbf_train_accuracy <- svm_rbf_train_result$accuracy
svm_rbf_train_recall <- svm_rbf_train_result$recall

# Predict the target variable using the test data
y_hat_svm_rbf_test <- predict(svm_fit_rbf, data_test, probability=TRUE)

# Evaluate the model performance on the test data
svm_rbf_test_result <- show_results(data_test$ProdTaken, y_hat_svm_rbf_test)

# Store the accuracy and the recall
svm_rbf_test_accuracy <- svm_rbf_test_result$accuracy
svm_rbf_test_recall <- svm_rbf_test_result$recall

svm_scores <- attr(y_hat_svm_linear_train, 'probabilities')
y_scores_pos <- svm_scores[, 2]

# Create a data frame with the predicted scores and actual labels
pr_df <- data.frame(scores = y_scores_pos, labels = data_train$ProdTaken)

# Calculate precision and recall for different threshold values
precision_recall <- lapply(seq(0, 1, 0.01), function(threshold) {
  # Compute confusion matrix
  cm <- matrix(c(table(pr_df$labels, pr_df$scores > threshold)), ncol = 2, nrow = 2)
  
  # Calculate precision and recall
  precision <- cm[2, 2] / sum(cm[, 2])
  recall <- cm[2, 2] / sum(cm[2, ])
  
  # Return a data frame with threshold, precision, and recall
  data.frame(threshold = threshold, precision = precision, recall = recall)
})

# Combine the results into a single data frame
precision_recall_df <- do.call(rbind, precision_recall)

# Calculate F1 score
precision_recall_df$F1 <- 2 * precision_recall_df$precision * precision_recall_df$recall /
  (precision_recall_df$precision + precision_recall_df$recall)

# Find the threshold value that maximizes the F1 score to have precision and recall balanced
optimal_threshold <- precision_recall_df$threshold[which.max(precision_recall_df$F1)]

# Plot precision-recall curve
ggplot(data = precision_recall_df, aes(x = threshold)) +
  geom_line(aes(y = precision, color = "Precision")) +
  geom_line(aes(y = recall, color = "Recall")) +
  scale_x_continuous(name = "Threshold", limits = c(0, 1)) +
  scale_y_continuous(name = "Value", limits = c(0, 1)) +
  labs(title = "Precision-Recall Curve with Optimal Threshold") +
  geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "red") +
  scale_color_manual(name = "", values = c("Precision" = "blue", "Recall" = "red")) +
  annotate("text", x = optimal_threshold, y = max(precision_recall_df$precision),
           label = paste("Threshold:", round(optimal_threshold, 2)), vjust = 0, hjust = -0.1)

# Predict the target variable
y_hat_svm_rbf_opt <- predict(svm_fit_rbf, data_test, probability=TRUE)

# Check the model performance using the optimal threshold.
svm_rbf_opt_result <- show_results(data_test$ProdTaken, factor(as.numeric(attr(y_hat_svm_rbf_opt, 'probabilities')[,2] > optimal_threshold)))

# Store the accuracy and the recall
svm_rbf_opt_accuracy <- svm_rbf_opt_result$accuracy
svm_rbf_opt_recall <- svm_rbf_opt_result$recall



# Train the decision tree model using the training data
model_dt <- rpart(ProdTaken ~ ., data = data_train, method = "class")

# Use the model to predict the training data
y_hat_dt_train <- predict(model_dt, newdata = data_train, type = "class")

# Evaluate the model performance on the training data
dt_train_result <- show_results(data_train$ProdTaken, y_hat_dt_train)

# Store the accuracy and the recall
dt_train_accuracy <- dt_train_result$accuracy
dt_train_recall <- dt_train_result$recall

set.seed(1)

# Use the model to perform predictions on the test data
y_hat_dt_test <- predict(model_dt, newdata = data_test, type = "class")

# Evaluate the model performance on the test data
dt_test_result <- show_results(data_test$ProdTaken, y_hat_dt_test)

# Store the accuracy and the recall
dt_test_accuracy <- dt_test_result$accuracy
dt_test_recall <- dt_test_result$recall

# Grid of parameters to choose from
cp_values <- seq(0, 0.1, 0.01)
parameters <- expand.grid(cp = cp_values, maxdepth = seq(10, 30, 10))

# Run the grid search, testing the model with different parameters
recall_values <- vector(mode = "numeric", length = nrow(parameters))

for(i in 1:nrow(parameters)) 
{
  model <- rpart(ProdTaken ~., data = data_train, method = "class",
                 cp = parameters$cp[i], maxdepth = parameters$maxdepth[i])
  pred <- predict(model, newdata = data_train, type = "class")
  recall_values[i] <- caret::recall(Y_train$ProdTaken, pred)
}

# Get the best model
best_dt_model <- rpart(ProdTaken ~., data = data_train, method = "class",
                    cp = parameters$cp[which.max(recall_values)], 
                    maxdepth = parameters$maxdepth[which.max(recall_values)])

# Perform predictions using the training data
y_hat_dt_train_opt <- predict(best_dt_model, newdata = data_train, type = "class")

# Evaluate the model performance on the training data
dt_train_opt_result <- show_results(data_train$ProdTaken, y_hat_dt_train_opt)

# Store the accuracy and the recall
dt_train_opt_accuracy <- dt_train_opt_result$accuracy
dt_train_opt_recall <- dt_train_opt_result$recall

# Predict the target variable on the test data
y_hat_dt_test_opt <- predict(best_dt_model, newdata = data_test, type = "class")

# Check the performance of the model on the test data
dt_test_opt_result <- show_results(data_test$ProdTaken, y_hat_dt_test_opt)

# Store the accuracy and the recall
dt_test_opt_accuracy <- dt_test_opt_result$accuracy
dt_test_opt_recall <- dt_test_opt_result$recall

# Show the features ordered by their importance
plot_feature_importance <- function(importances, features)
{
  # Create dataframe for plotting
  importances_df <- data.frame(
    feature = features,
    importance = importances,
    stringsAsFactors = FALSE
  )
  
  # Order by importance in descending order
  importances_df <- importances_df %>% arrange(desc(importance))
  
  # Create plot
  importances_df %>% ggplot(aes(x = importance, y = reorder(feature, sort(importance), decreasing = TRUE))) +
    geom_bar(stat = "identity", fill = "#3377EE") +
    xlab("Relative Importance") +
    ylab("") +
    ggtitle("Feature Importances") +
    theme_minimal()
}

# Show a list of features ordered by their importance
plot_feature_importance(best_dt_model$variable.importance, names(best_dt_model$variable.importance))


# Train the random forest model
model_rf <- randomForest(X_train, Y_train$ProdTaken, importance=TRUE, ntree=500, mtry=floor(sqrt(ncol(X_train))))

# Predict the training data
y_hat_rf_train <- predict(model_rf, newdata = data_train)

# Evaluate the model performance on the training data
rf_train_result <- show_results(data_train$ProdTaken, y_hat_rf_train)

# Store the accuracy and the recall
rf_train_accuracy <- rf_train_result$accuracy
rf_train_recall <- rf_train_result$recall

# Predict the test data
y_hat_rf_test <- predict(model_rf, newdata = data_test)

# Evaluate the model performance on the test data
rf_test_result <- show_results(data_test$ProdTaken, y_hat_rf_test)

# Store the accuracy and the recall
rf_test_accuracy <- rf_test_result$accuracy
rf_test_recall <- rf_test_result$recall

# Show a list of features ordered by their importance
plot_feature_importance(model_rf$importance[, "MeanDecreaseGini"], rownames(model_rf$importance))


# Create a dataframe to compare the performance of the models
comparison_df <- data.frame(
  Model = c('Logistic Regression', 'SVM - Linear Kernel', 'SVM - RBF Kernel', 'Decision Tree', 'Random Forest'),
  Recall = c(lg_test_recall, svm_linear_opt_recall, svm_rbf_opt_recall, dt_test_opt_recall, rf_test_recall),
  Accuracy = c(lg_test_accuracy, svm_linear_opt_accuracy, svm_rbf_opt_accuracy, dt_test_opt_accuracy, rf_test_accuracy)
)

# Format recall and accuracy to show as percentages
comparison_df <- comparison_df %>%
  mutate(
    Recall = lapply(Recall, function(x) sprintf("%.2f%%", x * 100)),
    Accuracy = lapply(Accuracy, function(x) sprintf("%.2f%%", x * 100))
  ) 

# Show the comparison table
comparison_df %>%
  arrange(desc(unlist(Recall))) %>%
  select(Model, Recall, Accuracy) %>%
  kable(booktabs = TRUE, escape = FALSE, format = "pandoc") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(2:3) %>%
  row_spec(0, bold = TRUE)
