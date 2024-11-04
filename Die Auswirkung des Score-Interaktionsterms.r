
# Load libraries
library(caret)
library(nnet)
library(dplyr)
library(readxl)

# Load the data (replace 'path_to_your_file.xlsx' with your actual file path)
data <- read_excel("D:/tcd/data mining/444.xlsx")

# Select relevant features and create an interaction term
data <- data %>%
  select(price, room_type, availability_365, number_of_reviews, review_scores_rating) %>%
  mutate(interaction_term = number_of_reviews * review_scores_rating)

# Handle missing values (if any)
data <- na.omit(data)

# Convert room_type to numeric since neural networks need numerical inputs
data$room_type <- as.numeric(as.factor(data$room_type))

# Normalize the numerical features for better neural network performance
preProcess_range_model <- preProcess(data, method = c("center", "scale"))
data_normalized <- predict(preProcess_range_model, data)

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123)
trainIndex <- createDataPartition(data_normalized$price, p = 0.8, list = FALSE)
trainData <- data_normalized[trainIndex, ]
testData <- data_normalized[-trainIndex, ]

# Train the neural network using caret's train function
nn_model <- train(price ~ room_type + availability_365 + number_of_reviews + review_scores_rating + interaction_term,
                  data = trainData,
                  method = "nnet",
                  linout = TRUE,   # For regression
                  trace = FALSE,   # Suppresses training output
                  tuneGrid = expand.grid(size = c(5), decay = c(0.1)))  # Adjust parameters as needed

# Make predictions on the test set
predictions <- predict(nn_model, newdata = testData)

# Compare actual vs predicted prices
results <- data.frame(Actual = testData$price, Predicted = predictions)

# Print the actual vs predicted results
print("Actual vs Predicted Prices:")
print(results)

# Evaluate the model using Root Mean Square Error (RMSE)
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
cat("Root Mean Square Error (RMSE):", rmse, "\n")

# Optional: View the first 10 rows of actual vs predicted values
print("First 10 rows of Actual vs Predicted Prices:")
print(head(results, 10))
