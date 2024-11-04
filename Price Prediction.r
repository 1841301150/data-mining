
# Load necessary libraries
library(readxl)
library(dplyr)

# Load the data from the Excel file
# Replace "path_to_your_file.xlsx" with the actual file path on your machine
data <- read_excel("D:/tcd/data mining/444.xlsx")

# View the first few rows of the data to understand its structure
head(data)

# Convert room_type to a factor if it's categorical
# (Assuming room_type is numeric but represents categories)
data$room_type <- as.factor(data$room_type)

# Check the structure of the dataset to ensure data types are appropriate
str(data)

# Build a linear regression model to predict price based on room_type, availability, and number_of_reviews
model <- lm(price ~ room_type + availability_365 + number_of_reviews, data = data)

# Summarize the model to understand the coefficients and model performance
summary(model)

# Optionally, make predictions using the model with new data
# Replace these values with actual values for prediction
new_data <- data.frame(room_type = factor(c(1, 2)), availability_365 = c(100, 200), number_of_reviews = c(50, 100))

# Predict prices based on the new data
predictions <- predict(model, newdata = new_data)

# Print the predicted prices
print(predictions)

# If you want to save the model or the results, you can write it to a CSV file:
# write.csv(predictions, "predicted_prices.csv")
