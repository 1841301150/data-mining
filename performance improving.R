### clean the data
# read csv
listing <- read.csv("listings.csv", header = TRUE, sep = ",")
View(listing)

# clear columns full of null or NA values
listing <- listing[, colSums(is.na(listing)) < nrow(listing)]
listing <- listing[complete.cases(listing),]

# output the cleaned data
write.csv(listing, file = "listings_cleaned.csv", row.names = FALSE)

##### performance analysis
# input selected final dataset
airb <- read.csv("D:\\Afile\\1硕士学业\\Data Mining\\group assign\\cleaned_listing_final_3.csv", header = TRUE, sep = ",")
View(airb)
# standardize the data
library(caret)
norm_value <- preProcess(airb[, c(3:20,28)], method=c("center", "scale"))
airb[, c(3:20,28)] <- predict(norm_value, airb[, c(3:20,28)])

### decision tree -- what influences the score rating?
library(rpart)
library(rpart.plot)
tree_rating <- rpart(review_scores_rating ~ host_response_time+host_response_rate+host_acceptance_rate+
                       host_is_superhost+host_listings_count+host_has_profile_pic
                     +host_identity_verified+neighbourhood+instant_bookable +
                       room_type + accommodates + bathrooms + beds + price + number_of_reviews 
                     + minimum_nights + maximum_nights + availability_365, data=airb, method="anova")
tree_rating_pruned <- prune(tree_rating, cp=tree_rating$cptable[which.min(tree_rating$cptable[,"xerror"]),"CP"])
prp(tree_rating_pruned, box.palette=c('lightpink','lightblue'),type=1,extra=1,under.cex=0.6,cex=0.8,tweak=0.6)

### random forest -- which specific score affects the score rating?
library(randomForest)
set.seed(123)
rm_rating <- randomForest(review_scores_rating ~ review_scores_accuracy + 
                          review_scores_cleanliness + review_scores_checkin + 
                          review_scores_communication + review_scores_location + 
                          review_scores_value, data=airb, ntree=1000, importance=TRUE)
imp <- importance(rm_rating)
sorted_imp <- imp[order(imp[,1], decreasing=TRUE),]
sorted_imp

##### price strategies
# input selected final dataset
airb <- read.csv("D:\\Afile\\1硕士学业\\Data Mining\\group assign\\cleaned_listing_final_3.csv", header = TRUE, sep = ",")
View(airb)
# standardize the data
norm_value_1 <- preProcess(airb[, c(3:15,17:21,28,29)], method=c("center", "scale"))
airb[, c(3:15,17:21,28,29)] <- predict(norm_value_1, airb[, c(3:15,17:21,28,29)])

### knn -- predict price
library(dplyr)
library(class)
# Split data into training and testing sets (60% training, 40% testing)
set.seed(123)
index <- sample(1:nrow(airb), nrow(airb)*0.6) 
train <- airb[index, ] 
test <- airb[-index, ]

# select the columns
trainData <- train[,c('neighbourhood','room_type','accommodates','bedrooms','bathrooms','beds','price')]
testData <- test[,c('neighbourhood','room_type','accommodates','bedrooms','bathrooms','beds','price')]

# find best k
accuracy <- data.frame(k=seq(1,5,1), RMSE=rep(0,5))
for (k in 1:5){
  pred <- class::knn(train=trainData, 
                     test=testData, 
                     cl=trainData$room_type, k=k) 
  accuracy[k, 2] <- RMSE(as.numeric(as.character(pred)), testData[,7])
}
accuracy

# knn model
knn_model <- class::knn(train = trainData, test = testData, cl = trainData[,7], k = 5)
common_levels <- union(levels(knn_model), levels(testData))
data <- factor(knn_model, levels = common_levels)
reference <- factor(testData[,7], levels = common_levels)
confusionMatrix(data, reference)

### linear regression -- what influnces the price?
lm_price <- lm(price ~ host_response_time+host_response_rate+host_acceptance_rate+
               host_is_superhost+host_listings_count+host_has_profile_pic
               +host_identity_verified+neighbourhood+instant_bookable +
               room_type + accommodates + bathrooms + beds + review_scores_rating 
               + number_of_reviews + minimum_nights + maximum_nights + availability_365, data=airb)
summary(lm_price)

lm_price_adj <- lm(price ~ host_response_time+neighbourhood+instant_bookable +
                 room_type + accommodates + bathrooms + review_scores_rating 
               + number_of_reviews + minimum_nights + availability_365, data=airb)
summary(lm_price_adj)

