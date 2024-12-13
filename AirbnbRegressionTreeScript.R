library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)

# Load csv
airbnb.data <- read.csv("Airbnb_Data.csv")

# Drop unwanted columns
airbnb <- airbnb.data %>%
  select(-one_of(c("id",
                   "amenities", # i like what i see when i take this out.
                   "description",
                   "first_review",
                   # "host_has_profile_pic",
                   # "host_identity_verified",
                   "last_review",
                   "latitude",
                   "longitude",
                   "name",
                   "thumbnail_url",
                   "host_response_rate",
                   "host_since",
                   "neighbourhood",
                   "zipcode")))


colnames(airbnb)

# Ensure categorical variables are factors
airbnb$property_type <- as.factor(airbnb$property_type)
airbnb$room_type <- as.factor(airbnb$room_type)
airbnb$bed_type <- as.factor(airbnb$bed_type)
airbnb$cancellation_policy <- as.factor(airbnb$cancellation_policy)
airbnb$city <- as.factor(airbnb$city)

# Clean and make dollar variables numeric
airbnb$accommodates <- as.numeric(airbnb$accommodates)
airbnb$bathrooms <- as.numeric(airbnb$bathrooms)
airbnb$bedrooms <- as.numeric(airbnb$bedrooms)
airbnb$beds <- as.numeric(airbnb$beds)
airbnb$number_of_reviews <- as.numeric(airbnb$number_of_reviews)
airbnb$review_scores_rating <- as.numeric(airbnb$review_scores_rating)



airbnb <- airbnb[airbnb$beds > 0 &
                   airbnb$bathrooms > 0 &
                   airbnb$bedrooms > 0, ]

airbnb<- airbnb[!apply(is.na(airbnb) |
                         airbnb == "", 1, any), ]


# Create a new column 'price' by exponentiating 'log_price'
airbnb <- airbnb %>% mutate(price = exp(log_price))

# Price distribution
ggplot(airbnb, aes(x = price)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Prices", x = "Price", y = "Count")

# Price by property type
ggplot(airbnb, aes(x = property_type, y = price, fill = property_type)) +
  geom_boxplot() +
  labs(title = "Price by Property Type", x = "Property Type", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Price by room type
ggplot(airbnb, aes(x = room_type, y = price, fill = room_type)) +
  geom_boxplot() +
  labs(title = "Price by Room Type", x = "Room Type", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Price by city
ggplot(airbnb, aes(x = city, y = price, fill = city)) +
  geom_boxplot() +
  labs(title = "Price by City", x = "City", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatterplot of price vs. number of reviews
ggplot(airbnb, aes(x = number_of_reviews, y = price)) +
  geom_point(alpha = 0.3) +
  labs(title = "Price vs. Number of Reviews", x = "Number of Reviews", y = "Price")

# Faceted boxplots of price by room type and number of bathrooms
ggplot(airbnb, aes(x = room_type, y = price, fill = room_type)) +
  geom_boxplot() +
  facet_wrap(~ bathrooms, ncol = 2) +
  labs(title = "Price by Room Type and Number of Bathrooms", x = "Room Type", y = "Price")

# Scatterplot of price vs. number of bedrooms, colored by number of bathrooms
ggplot(airbnb, aes(x = bedrooms, y = price, color = factor(bathrooms))) +
  geom_point(alpha = 0.5) +
  labs(title = "Price vs. Number of Bedrooms by Number of Bathrooms",
       x = "Number of Bedrooms", y = "Price", color = "Number of Bathrooms")

# Faceted scatterplot of price vs. number of bedrooms, faceted by number of bathrooms
ggplot(airbnb, aes(x = bedrooms, y = price)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ bathrooms, ncol = 3) +
  labs(title = "Price vs. Number of Bedrooms, Faceted by Number of Bathrooms",
       x = "Number of Bedrooms", y = "Price") +
  scale_x_continuous(breaks = seq(1, 10, by = 1))

# Price histogram by property type
ggplot(airbnb, aes(x = price, fill = property_type)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  labs(title = "Price Distribution by Property Type", x = "Price", fill = "Property Type")

# Price histogram by room type
ggplot(airbnb, aes(x = price, fill = room_type)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  labs(title = "Price Distribution by Room Type", x = "Price", fill = "Room Type")
# Average price by city and room type (dodge)
ggplot(airbnb, aes(x = city, y = price, fill = room_type)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Price by City and Room Type", x = "City", y = "Average Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Average price by property type and room type (dodge)
ggplot(airbnb, aes(x = property_type, y = price, fill = room_type)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Price by Property Type and Room Type", x = "Property Type", y = "Average Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted scatterplots of price vs. number of reviews by city
ggplot(airbnb, aes(x = number_of_reviews, y = price)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ city, ncol = 3) +
  labs(title = "Price vs. Number of Reviews, Faceted by City",
       x = "Number of Reviews", y = "Price")


# Run one-way ANOVA
anova_result <- aov(log_price ~ city, data = airbnb)
summary(anova_result)


tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Check normality assumption
# shapiro.test(residuals(anova_result))

# Check homogeneity of variances assumption
bartlett.test(log_price ~ city, data = airbnb)


kruskal.test(log_price ~ city, data = airbnb)

# Remove the 'price' column before running the regression tree
airbnb <- airbnb[, !names(airbnb) %in% c("price")]
colnames(airbnb) # we have 15 predictor5 variables
summary(airbnb)
subset_airbnb <- airbnb[airbnb$bedrooms == 1 & airbnb$bathrooms == 8, ]
(subset_airbnb)
subset_airbnb <- airbnb[exp(airbnb$log_price) >= 400 & airbnb$bathrooms == 0.5, ]
(subset_airbnb)

# Load required libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data into train and validate sets
trainIndex <- createDataPartition(airbnb$log_price, p = 0.8, list = FALSE)
train_data <- airbnb[trainIndex, ]
validate_data <- airbnb[-trainIndex, ]

# Split the validate set into validate and test sets
validateIndex <- createDataPartition(validate_data$log_price, p = 0.5, list = FALSE)
validate_data <- validate_data[validateIndex, ]
test_data <- validate_data[-validateIndex, ]


print("Mean price for train data, validate data, and test data:")
mean(exp(train_data$log_price))
mean(exp(validate_data$log_price))
mean(exp(test_data$log_price))

# Load required libraries
library(rpart)
library(rpart.plot)

# Train the regression tree on the training data
tree_model <- rpart(log_price ~ ., data = train_data, method = "anova", control = rpart.control(cp = 0.001), minbucket=5, minsplit=20)

# Plot the tree
rpart.plot(tree_model, type = 3)
plotcp(tree_model)

(tree_model$variable.importance)

# Make predictions on the test data
test_predictions <- predict(tree_model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_data$log_price - test_predictions)^2))
cat("RMSE:", rmse, "\n")

# Calculate R-squared
r_squared <- 1 - sum((test_data$log_price - test_predictions)^2) / sum((test_data$log_price - mean(test_data$log_price))^2)
cat("R-squared:", r_squared, "\n")

# Set up cross-validation parameters
cv_params <- rpart.control(xval = 10, cp = 0.01)

# Perform cross-validation and prune the tree
pruned_tree <- rpart(log_price ~ ., data = validate_data, method = "anova", control = cv_params,minbucket= 5, minsplit=20)

# Plot the pruned tree
rpart.plot(pruned_tree, type = 3)
plotcp(pruned_tree)

(pruned_tree$variable.importance)

# Make predictions on the test data
test_predictions <- predict(pruned_tree, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_data$log_price - test_predictions)^2))
cat("RMSE:", rmse, "\n")

# Calculate R-squared
r_squared <- 1 - sum((test_data$log_price - test_predictions)^2) / sum((test_data$log_price - mean(test_data$log_price))^2)
cat("R-squared:", r_squared, "\n")
