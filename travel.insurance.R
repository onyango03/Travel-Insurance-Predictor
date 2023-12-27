#Load Required Libraries

library("tidyverse")
library("caret")
library("GGally")
library("ggplot2")
library("rpart")
library("skimr")
library("caTools")
library("dplyr")
library("iml")
library(scales)
library("summarytools")
library("randomForest")
library(shapper)
library(gridExtra)
library(glmnet)
library(corrplot)
library(knitr)

#Load data
travel_data <- read.csv("C:/Users/USER/Downloads/TRAVEL INSURANCE/TravelInsurancePrediction.csv")
View(travel_data)

#Exploratory data analysis
str(travel_data)
skim_without_charts(travel_data)
summary(travel_data)

#Bar Graph To compare the number o Travel Insurance purchased.
filtered_df <- travel_data %>%
  filter(TravelInsurance %in% c(0, 1))
grouped_data <- filtered_df %>%
  group_by(TravelInsurance) %>%
  summarise(Count = n())
ggplot(data = grouped_data, aes(x = factor(TravelInsurance), y = Count)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(title = "Count Plot of TravelInsurance", x = "TravelInsurance", y = "Count") +
  scale_x_discrete(labels = c("No", "Yes"))

# Grouped bar chart on travel insurance purchase by age
ggplot(travel_data, aes(x=Age, fill = factor(TravelInsurance))) +
  geom_bar(position ="dodge")+
  labs(title = "Travel Insurance Purchase by Age Group",
       x = "Age",
       y = "Count") +
  scale_fill_discrete((name = "Travel Insurance"),
                      labels= c("Not Purchased", "Purchased"))

#Grouped Chart on travel Insurance purchase by number of Family Members
ggplot(travel_data, aes(x=FamilyMembers, fill = factor(TravelInsurance))) +
  geom_bar(position ="dodge")+
  labs(title = "Travel Insurance Purchase by family members",
       x = "No of Family Members",
       y = "Count") +
  scale_fill_discrete((name = "Travel Insurance"),
                      labels= c("Not Purchased", "Purchased"))

#Bar Chart showing those who have and have not graduated
filteredGraduate_df <- travel_data %>%
  filter(GraduateOrNot %in% c(0, 1))
groupedGraduate_data <- filteredGraduate_df %>%
  group_by(GraduateOrNot) %>%
  summarise(Count = n())
ggplot(data = groupedGraduate_data, aes(x = factor(GraduateOrNot), y = Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Count Plot of Graduants", x = "Graduated", y = "Count") +
  scale_x_discrete(labels = c("No", "Yes"))

# Replace "Yes" with 1 and "No" with 0 for specific columns so as to make dataframe have only one datatype
travel_data$GraduateOrNot <- ifelse(travel_data$GraduateOrNot == "Yes", 1, 0)
travel_data$EverTravelledAbroad <- ifelse(travel_data$EverTravelledAbroad == "Yes", 1, 0)
travel_data$FrequentFlyer <- ifelse(travel_data$FrequentFlyer == "Yes", 1, 0)
travel_data$Employment.Type <- ifelse(travel_data$Employment.Type == "Government Sector", 1, 0)
# Continue to do the same for other columns if needed
head(travel_data)

#Pie chart showing the relationship between other variables and purchasing of travel insurance.
# Step 1: Group data by 'Employment Type' and calculate the percentage of customers who purchased travel insurance
employmenttype_purchase_pct <- travel_data %>%
  group_by(`Employment.Type`) %>%
  summarize(purchase_pct = (sum(TravelInsurance) / n()) * 100)
# Step 2: Group data by 'GraduateOrNot' and calculate the percentage of customers who purchased travel insurance
graduateornot_purchase_pct <- travel_data %>%
  group_by(`GraduateOrNot`) %>%
  summarize(purchase_pct = (sum(TravelInsurance) / n()) * 100)
# Step 3: Group data by 'ChronicDiseases' and calculate the percentage of customers who purchased travel insurance
chronicdiseases_purchase_pct <- travel_data %>%
  group_by(`ChronicDiseases`) %>%
  summarize(purchase_pct = (sum(TravelInsurance) / n()) * 100)
# Step 4: Group data by 'FrequentFlyer' and calculate the percentage of customers who purchased travel insurance
frequentflyer_purchase_pct <- travel_data %>%
  group_by(`FrequentFlyer`) %>%
  summarize(purchase_pct = (sum(TravelInsurance) / n()) * 100)
# Step 5: Group data by 'EverTravelledAbroad' and calculate the percentage of customers who purchased travel insurance
evertravelabroad_purchase_pct <- travel_data %>%
  group_by(`EverTravelledAbroad`) %>%
  summarize(purchase_pct = (sum(TravelInsurance) / n()) * 100)
# Step 6: Define colors for pie chart
colors <- c('grey', 'black')
labels <- c('Government', 'Private/Self-Employed')
# Step 7: Create the pie charts
par(mfrow=c(2, 3), mar=c(4, 4, 2, 1))

# Pie chart for Employment Type
pie(employmenttype_purchase_pct$purchase_pct, labels =  c('Government', 'Private'), col = colors, main = "Employment Type VS Travel Insurance")
# Pie chart for GraduateOrNot
pie(graduateornot_purchase_pct$purchase_pct, labels = c('Not Graduate', 'Graduate'), col = colors, main = "Education Level VS Travel Insurance")
# Pie chart for ChronicDiseases
pie(chronicdiseases_purchase_pct$purchase_pct, labels = c('No Chronic Diseases', 'Chronic Diseases'), col = colors, main = "Chronic Diseases VS Travel Insurance")
# Pie chart for FrequentFlyer
pie(frequentflyer_purchase_pct$purchase_pct, labels = c('Not Frequent Flyer', 'Frequent Flyer'), col = colors, main = "FrequentFlyer Status VS Travel Insurance")
# Pie chart for EverTravelledAbroad
pie(evertravelabroad_purchase_pct$purchase_pct, labels = c('Not Ever Travelled Abroad', 'Ever Travelled Abroad'), col = colors, main = "Travel History VS Travel Insurance")


#Check For Potential Outliers
z_scores <- abs(scale(travel_data))
# Identify any rows with z-score greater than 3
potential_outliers <- travel_data[apply(z_scores, 1, function(x) any(x > 3)), ]
# Print the potential outliers
print(potential_outliers)

# scaling to normalize it
cols_to_scale <- c("Age", "AnnualIncome", "FamilyMembers")
scaled_df <- travel_data %>%
  mutate(across(all_of(cols_to_scale), rescale))
View(scaled_df)

#Calculate the Correlation
correlation_matrix <- cor(scaled_df)
print(correlation_matrix)

corrplot(correlation_matrix, method="color")
cor_table <- kable(correlation_matrix, format = "markdown")
print(cor_table)


#Arrange output in order of highest correlation to lowest correlation
target_variable <- "TravelInsurance"
# Get correlations with the target variable
correlations_with_target <- correlation_matrix[target_variable, ]
# Sort variables by correlation in descending order
sorted_variables <- names(sort(correlations_with_target, decreasing = TRUE))
# Remove the target variable itself from the list (if present)
sorted_variables <- sorted_variables[sorted_variables != target_variable]
print(sorted_variables)

#Create a pairplot for the correlation
# Use only Top 5 variables with highest correlation to travel insurance
travel_pairplot <- subset(scaled_df, select = -c(GraduateOrNot, Employment.Type, ChronicDiseases))
View(travel_pairplot)

# Convert the numeric variable to a factor
travel_pairplot$TravelInsurance <- as.factor(travel_pairplot$TravelInsurance)
# Create the pair plot
g_correlation <- ggpairs(travel_pairplot, columns = 1:6, aes(color = TravelInsurance))
# Print the pair plot
print(g_correlation)

# MODEL BUILDING.

# RANDOM FOREST MODEL.
# Define features and target variable
features <- c('AnnualIncome', 'Age', 'FamilyMembers', 'FrequentFlyer', 'EverTravelledAbroad')
target <- 'TravelInsurance'

X <- travel_pairplot[, features]
y <- travel_pairplot[, target]

# Split the data into training and testing sets
set.seed(42)
split_index <- sample(1:nrow(travel_pairplot), size = 0.8 * nrow(travel_pairplot))
X_train <- X[split_index, ]
y_train <- y[split_index]
X_test <- X[-split_index, ]
y_test <- y[-split_index]

rf_model <- randomForest(y_train ~ ., data = cbind(X_train, y_train))

# Make predictions
predictions <- predict(rf_model, newdata = X_test)

# Output predictions
print(predictions)
Rf_df<-data.frame(predictions,y_test)

View(Rf_df)
# Calculate accuracy
Rf_accuracy <- mean(predictions == y_test)

# Create a data frame to display results
results <- data.frame(Predicted = predictions, Actual = y_test, Correct = (predictions == y_test))

# Output results
print(results)
print(paste("Accuracy:", Rf_accuracy))

#Creating a confusion matrix
conf_matrix1 <- confusionMatrix(predictions, y_test)

# Plot confusion matrix
plot_confusion_matrix1 <- function(cm) {
  cm_table <- as.table(cm)
  ggplot(data = as.data.frame(cm_table), aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.0f", Freq)), vjust = 0.5, color = "black") +
    labs(title = "RF Confusion Matrix", x = "Reference", y = "Prediction") +
    scale_fill_gradient(low = "white", high = "grey") +
    theme_minimal()
}

# Plot confusion matrix
plot_confusion_matrix1(conf_matrix1$table)
print(paste("Accuracy:", conf_matrix1$overall["Accuracy"]))

#precision
Rf_precision <- conf_matrix1$byClass["Pos Pred Value"]
print(paste("Precision:", Rf_precision))

# Calculate Recall (Sensitivity)
Rf_recall <- conf_matrix1$byClass["Sensitivity"]
print(paste("Recall (Sensitivity):", Rf_recall))

# Calculate Specificity
Rf_specificity <- conf_matrix1$byClass["Specificity"]
print(paste("Specificity:", Rf_specificity))

# Calculate F1-score
Rf_f1_score <- conf_matrix1$byClass["F1"]
print(paste("F1-Score:", Rf_f1_score))

# Find false positives
false_positives <- Rf_df[predictions == 1 & y_test == 0, ]
print("False Positives:")
print(false_positives)

#LOGISTIC REGRESSION MODEL
# Define features and target variable
features <- c('AnnualIncome', 'Age', 'FamilyMembers', 'FrequentFlyer', 'EverTravelledAbroad')
target <- 'TravelInsurance'

# Extract features and target
A <- travel_pairplot[, features]
B <- as.numeric(as.character(travel_pairplot[, target]))

# Split the data into training and testing sets
set.seed(42)
split_index <- sample(1:nrow(travel_pairplot), size = 0.8 * nrow(travel_pairplot))
A_train <- A[split_index, ]
B_train <- B[split_index]
A_test <- A[-split_index, ]
B_test <- B[-split_index]

# Create and train the Logistic Regression model using LASSO regularization
lambda_values <- 10^seq(10, -2, length = 100)
lasso_model <- cv.glmnet(as.matrix(A_train), B_train, alpha = 1, lambda = lambda_values)
best_lambda <- lasso_model$lambda.min
final_model <- glmnet(as.matrix(A_train), B_train, alpha = 1, lambda = best_lambda)

# Make predictions
probabilities <- predict(final_model, s = best_lambda, newx = as.matrix(A_test))
predictions <- ifelse(probabilities > 0.5, 1, 0)

# Calculate accuracy
lr_accuracy <- mean(predictions == B_test)
print(paste("Accuracy:", lr_accuracy))

# Create a confusion matrix
conf_matrix <- confusionMatrix(factor(predictions, levels = c(0, 1)), factor(B_test, levels = c(0, 1)))

# Output confusion matrix
print(conf_matrix)

# Plot confusion matrix
plot_confusion_matrix <- function(cm) {
  cm_table <- as.table(cm)
  ggplot(data = as.data.frame(cm_table), aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.0f", Freq)), vjust = 0.5, color = "black") +
    labs(title = "LR Confusion Matrix", x = "Reference", y = "Prediction") +
    scale_fill_gradient(low = "white", high = "grey") +
    theme_minimal()
}

# Plot confusion matrix
plot_confusion_matrix(conf_matrix$table)

# Calculate precision
lr_precision <- conf_matrix$byClass["Pos Pred Value"]
print(paste("Precision:", lr_precision))

# Calculate Recall (Sensitivity)
lr_recall <- conf_matrix$byClass["Sensitivity"]
print(paste("Recall (Sensitivity):", lr_recall))

# Calculate Specificity
lr_specificity <- conf_matrix$byClass["Specificity"]
print(paste("Specificity:", lr_specificity))

# Calculate F1-score
lr_f1_score <- 2 * (lr_precision * lr_recall) / (lr_precision + lr_recall)
print(paste("F1-Score:", lr_f1_score))

lr_df <- data.frame(B_test, predictions)
# Output the comparison dataframe
print(lr_df)
false_positives <- lr_df[predictions == 1 & y_test == 0, ]
print("False Positives:")
print(false_positives)

#K-NEAREST-NEIGHBOUR MODEL

# Define features and target variable
features <- c('AnnualIncome', 'Age', 'FamilyMembers', 'FrequentFlyer', 'EverTravelledAbroad')
target <- 'TravelInsurance'

# Extract data
data <- travel_pairplot[, c(features, target)]

# Split the data into training and testing sets
set.seed(42)
split_index <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[split_index, ]
test_data <- data[-split_index, ]

# Create and train the k-Nearest Neighbors model
k <- 5  # Set the number of neighbors
knn_model <- train(train_data[, features], train_data[, target],
                   method = "knn",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv"),
                   tuneGrid = data.frame(k = k))

# Make predictions
predictions <- predict(knn_model, newdata = test_data)

# Calculate accuracy
knn_accuracy <- mean(predictions == test_data[, target])
print(paste("Accuracy:", knn_accuracy))

# Create a data frame to display results
results <- data.frame(Predicted = predictions, Actual = test_data[, target], Correct = (predictions == test_data[, target]))

# Output results
print(results)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(predictions, test_data[, target])

# Print confusion matrix
print(conf_matrix)

# Calculate precision
knn_precision <- conf_matrix$byClass["Pos Pred Value"]
print(paste("Precision:", knn_precision))

# Calculate recall (Sensitivity)
knn_recall <- conf_matrix$byClass["Sensitivity"]
print(paste("Recall (Sensitivity):", knn_recall))

# Calculate specificity (True Negative Rate)
knn_specificity <- conf_matrix$byClass["Specificity"]
print(paste("Specificity:", knn_specificity))

# Calculate F1-score
knn_f1_score <- 2 * (knn_precision * knn_recall) / (knn_precision + knn_recall)
print(paste("Knn F1_score:", knn_f1_score))


# Plot confusion matrix
plot_confusion_matrix <- function(cm) {
  cm_table <- as.table(cm)
  cm_df <- as.data.frame(cm_table)
  ggplot(data = cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = 0.5, color = "black") +
    labs(title = "Knn Confusion Matrix", x = "Reference", y = "Prediction") +
    scale_fill_gradient(low = "white", high = "grey") +
    theme_minimal()
}

# Plot confusion matrix to get a better understanding of the spread of our results
plot_confusion_matrix(conf_matrix)

false_positives <- results[results$Predicted == 1 & results$Actual == 0, ]

# Output false positives
print("False Positives:")
print(false_positives)

# Compare results of the different types of models used
comparison_df <- data.frame(Model = c("Random Forest", "Logistic Regression", "K-Nearest Neighbors"),
                            Accuracy = c(Rf_accuracy, lr_accuracy, knn_accuracy),
                            Precision = c(Rf_precision, lr_precision, knn_precision),
                            Recall = c( Rf_recall,lr_recall, knn_recall),
                            specificity = c(Rf_specificity, lr_specificity, knn_specificity),
                            f1_score = c(Rf_f1_score, lr_f1_score, knn_f1_score))

print(comparison_df)
ggplot(comparison_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Comparison: Accuracy",
       y = "Value", x = "Model") +
  theme_minimal()

ggplot(comparison_df, aes(x = Model, y = Precision, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Comparison: Precision",
       y = "Value", x = "Model") +
  theme_minimal()

ggplot(comparison_df, aes(x = Model, y = Recall, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Comparison: Recall",
       y = "Value", x = "Model") +
  theme_minimal()

ggplot(comparison_df, aes(x = Model, y = specificity, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Comparison: Specificity",
       y = "Value", x = "Model") +
  theme_minimal()

ggplot(comparison_df, aes(x = Model, y = f1_score, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Comparison: F1_score",
       y = "Value", x = "Model") +
  theme_minimal()
#comparison false positives
print(Rf_df)
false_positives <- Rf_df[predictions == 1 & y_test == 0, ]
print("False Positives:")
print(false_positives)
print(lr_df)
false_positives <- lr_df[predictions == 1 & y_test == 0, ]
print("False Positives:")
print(false_positives)

