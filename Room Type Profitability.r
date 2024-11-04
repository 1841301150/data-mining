
# Load necessary libraries
library(dplyr)
library(class)  # For KNN
library(caret)  # For train-test split and evaluation
library(readxl)  # For reading Excel files

# Load the data from the Excel file
data <- read_excel("D:/tcd/data mining/444.xlsx")

# Select relevant columns (room_type, price, reviews_per_month,accommodates,bathrooms,bedrooms,beds)
data <- data %>%
  select(room_type, price, reviews_per_month,accommodates,bathrooms,bedrooms,beds)

# Remove rows with missing data
data <- na.omit(data)

# Convert room_type to a factor
data$room_type <- as.factor(data$room_type)

# Normalize the numeric variables (price, reviews_per_month) for better KNN performance
preProcess_range_model <- preProcess(data[, c("price", "reviews_per_month","accommodates","bathrooms","bedrooms","beds")], method = c("center", "scale"))
data_normalized <- predict(preProcess_range_model, data)

# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)
trainIndex <- createDataPartition(data_normalized$room_type, p = 0.80, list = FALSE)
trainData <- data_normalized[trainIndex, ]
testData <- data_normalized[-trainIndex, ]

# Define predictor variables (price, reviews_per_month) and the target variable (room_type)
train_x <- trainData[, c("price", "reviews_per_month","accommodates","bathrooms","bedrooms","beds")]
train_y <- trainData$room_type
test_x <- testData[, c("price", "reviews_per_month","accommodates","bathrooms","bedrooms","beds")]
test_y <- testData$room_type

# App105
k<-30
knn_model <- knn(train = train_x, test = test_x, cl = train_y, k = k)

# Evaluate the model by creating a confusion matrix
confusionMatrix(knn_model, test_y)

# Calculate the average price and reviews per month for each room type
aggregate(cbind(price, reviews_per_month,accommodates,bathrooms,bedrooms,beds) ~ room_type, data = data, FUN = mean)
