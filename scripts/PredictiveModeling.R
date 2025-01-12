library(caret)
library(nnet) # Train a multinomial logistic regression model
library(ggcorrplot)
library(dplyr)
library(readxl)
library(knitr)             
library(broom) 
library(caTools)
library(ggplot2)
library(reshape2)
library(pROC)
library(car)

set.seed(123)

model_data <- read_excel("C:\\Users\\ievav\\OneDrive\\Documents\\Ironhack_Final_Project_Sentiment_Analysis\\data\\model_data.xlsx")

# BIT OF CLEANING
names(model_data) <- gsub(" ", "_", names(model_data))

# Define columns to exclude
columns_to_exclude <- c(
  "Sentiment_VADER", "RESULT_CREATED_AT", "ASIN_IN_URL", "ASIN", "PRODUCT_NAME", 
  "DOMAIN", "CURRENCY", "DESCRIPTION", "BULLET_POINTS", "PRICE_BUYBOX", "PRICING_COUNT", "PRICE", 
  "PRICE_INITIAL", "PRICE_SHIPPING", "PRICE_SNS", "PRICE_UPPER", "PRICING_STR", 
  "PRICING_URL", "STOCK", "DISCOUNT_END", "LIGHTNING_DEAL", "DELIVERY_BY", "DELIVERY_FROM", 
  "TOP_REVIEW", "COUPON", "IS_PRIME_PANTRY", "FEATURED_MERCHANT_IS_AMAZON_FULFILLED", 
  "FEATURED_MERCHANT_LINK", "FEATURED_MERCHANT_LINK_NAME", "FEATURED_MERCHANT_LINK_NAME_SELLER_ID", 
  "GEO_LOCATION", "SALESRANK_1_RANK", "SALESRANK_1_URL", "SALESRANK_1_NAME", "SALESRANK_2_RANK", 
  "SALESRANK_2_URL", "SALESRANK_2_NAME", "SALESRANK_3_RANK", "SALESRANK_3_URL", "SALESRANK_3_NAME", 
  "LADDER_1_NAME", "LADDER_1_URL", "LADDER_2_NAME", "LADDER_2_URL", "LADDER_3_NAME", 
  "LADDER_3_URL", "TIMESTAMP", "Stock_Quantity", "Days_to_Availability", "Delayed_Availability", "City", 
  "CLEANED_ITEM_CATEGORY", "Cleaned_Review", "PRICE_log", "PRICE_IN_EUROS", "DAY"
)

# Remove columns from the dataset
model_data <- model_data %>%
  select(-all_of(columns_to_exclude))

# Verify changes
glimpse(model_data)

  
  
  
  
  
  
  

# Prepping dependent variable
model_data$Sentiment_Category <- factor(model_data$Sentiment_Category, levels = c("Negative", "Neutral", "Positive"))
model_data$Sentiment_Category <- relevel(model_data$Sentiment_Category, ref = "Neutral")

# Sentiment counts
sentiment_counts <- model_data %>%
  group_by(Sentiment_Category) %>%
  summarise(count = n())

print(sentiment_counts)

model_data <- na.omit(model_data)





# Create a random split (80% train, 20% test)
split <- sample.split(model_data$Sentiment_Category, SplitRatio = 0.8)

# Split the data
train_data <- subset(model_data, split == TRUE)
test_data <- subset(model_data, split == FALSE)

# Identify binary columns to exclude from preprocessing
binary_cols <- names(model_data)[sapply(model_data, function(x) all(x %in% c(0, 1)))]

# Apply preprocessing only to continuous numeric columns
pre_process_train <- preProcess(train_data[, !names(train_data) %in% binary_cols], method = c("center", "scale"))
pre_process_test <- preProcess(test_data[, !names(test_data) %in% binary_cols], method = c("center", "scale"))

# Apply preprocessing only to continuous numeric columns
train_data[, !names(train_data) %in% binary_cols] <- predict(pre_process_train, train_data[, !names(train_data) %in% binary_cols])
test_data[, !names(test_data) %in% binary_cols] <- predict(pre_process_test, test_data[, !names(test_data) %in% binary_cols])

class_weights <- table(train_data$Sentiment_Category)
class_weights <- 1 / class_weights
weights <- class_weights[train_data$Sentiment_Category]
weights <- as.numeric(weights)

model <- multinom(Sentiment_Category ~ ., data = train_data, weights = weights)

# View model summary
summary(model)

# Predictions and accuracy
predictions <- predict(model, test_data, type = "class")
conf_matrix <- table(Predicted = predictions, Actual = test_data$Sentiment_Category)
print(conf_matrix)

accuracy <- mean(predictions == test_data$Sentiment_Category)
cat("Accuracy:", accuracy, "\n")



# Normalize confusion matrix to get percentages
conf_matrix <- as.table(conf_matrix)  # Ensure it's in table form
conf_matrix_perc <- prop.table(conf_matrix, 2) * 100  # Convert counts to percentages

# Convert confusion matrix to a data frame for plotting
conf_df <- as.data.frame(as.table(conf_matrix_perc))

# Plot heatmap with percentages and increased font size
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), color = "white", size = 6) +  # Increase the font size
  scale_fill_gradient(low = "lightblue", high = "darkblue", breaks = seq(0, 100, 20)) +
  labs(title = "Confusion Matrix (Relative Percentages)", x = "Actual", y = "Predicted", fill = "Percentage") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 26),    # Increase axis title font size
    axis.text = element_text(size = 24),     # Increase axis text font size
  )



# Extract coefficients
coef_df <- as.data.frame(coef(model))
coef_df$Feature <- rownames(coef_df)

# Melt data for ggplot2
coef_melt <- melt(coef_df, id.vars = "Feature", variable.name = "Class", value.name = "Coefficient")


# Sort coefficients by absolute value
coef_melt <- coef_melt %>%
  group_by(Class) %>%
  mutate(Abs_Coefficient = abs(Coefficient)) %>%
  arrange(Class, desc(Abs_Coefficient))
  
  # Plot coefficients
  ggplot(coef_melt, aes(x = reorder(Feature, Abs_Coefficient), y = Coefficient, fill = Class)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Feature Importance by Class",
         x = "Feature",
         y = "Coefficient",
         fill = "Class") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  



probs <- predict(model, test_data, type = "prob")

roc_curves <- lapply(levels(test_data$Sentiment_Category), function(class) {
  roc(as.numeric(test_data$Sentiment_Category == class), probs[, class])
})

# Plot ROC curves with correct legend
plot(roc_curves[[1]], col = "red", main = "ROC Curves for Multinomial Model", print.auc = TRUE)
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], col = i, lty = i, add = TRUE)
}

legend("bottomright", legend = levels(test_data$Sentiment_Category), col = 1:length(roc_curves), lty = 1:length(roc_curves), lwd = 2, title = "Sentiment Category")



sentiment_category_counts <- model_data %>%
  count(Sentiment_Category) %>%
  arrange(desc(n)) 



# Filter the data to select only the rows where Sentiment_VADER is 'Positive'
positive_sentiment_data <- data %>% 
  filter(Sentiment_Category == "Positive")

# Create a scatter plot of relative price vs. another variable (e.g., total_revenue)
ggplot(positive_sentiment_data, aes(x = PRICE_log, y = Sentiment_VADER)) +
  geom_point() +  # Add points to represent each observation
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a regression line
  labs(title = "Scatter Plot of Relative Price vs. Sentiment (Positive Sentiment)",
       x = "Price in Euros (log scale)", 
       y = "Sentiment") +
  theme_minimal()







model_data_reduced <- read_excel("C:\\Users\\ievav\\OneDrive\\Documents\\Ironhack\\Assignments\\Ironhack-FINAL-project\\model_data.xlsx")

# BIT OF CLEANING
names(model_data_reduced) <- gsub(" ", "_", names(model_data_reduced))

# Define columns to exclude
columns_to_exclude <- c(
  "Sentiment_VADER", "RESULT_CREATED_AT", "ASIN_IN_URL", "ASIN", "PRODUCT_NAME", 
  "DOMAIN", "CURRENCY", "DESCRIPTION", "BULLET_POINTS", "PRICE_BUYBOX", "PRICING_COUNT", "PRICE", 
  "PRICE_INITIAL", "PRICE_SHIPPING", "PRICE_SNS", "PRICE_UPPER", "PRICING_STR", 
  "PRICING_URL", "STOCK", "DISCOUNT_END", "LIGHTNING_DEAL", "DELIVERY_BY", "DELIVERY_FROM", 
  "TOP_REVIEW", "COUPON", "IS_PRIME_PANTRY", "FEATURED_MERCHANT_IS_AMAZON_FULFILLED", 
  "FEATURED_MERCHANT_LINK", "FEATURED_MERCHANT_LINK_NAME", "FEATURED_MERCHANT_LINK_NAME_SELLER_ID", 
  "GEO_LOCATION", "SALESRANK_1_RANK", "SALESRANK_1_URL", "SALESRANK_1_NAME", "SALESRANK_2_RANK", 
  "SALESRANK_2_URL", "SALESRANK_2_NAME", "SALESRANK_3_RANK", "SALESRANK_3_URL", "SALESRANK_3_NAME", 
  "LADDER_1_NAME", "LADDER_1_URL", "LADDER_2_NAME", "LADDER_2_URL", "LADDER_3_NAME", 
  "LADDER_3_URL", "TIMESTAMP", "Stock_Quantity", "Days_to_Availability", "Delayed_Availability", "City", 
  "CLEANED_ITEM_CATEGORY", "Cleaned_Review", "PRICE_log", "PRICE_IN_EUROS", "DAY"
)

  
  
  
# Filter to get Classes where Coefficient > 0.1 or Coefficient < -0.1
columns_to_include <- c(
  "relative_price", "CLEANED_ITEM_CATEGORYtoys_games", "CLEANED_ITEM_CATEGORYtools_home_improvement", "CLEANED_ITEM_CATEGORYhome_kitchen", 
  "CLEANED_ITEM_CATEGORYelectronics", "CLEANED_ITEM_CATEGORYclothing_shoes_jewelry", "CLEANED_ITEM_CATEGORYbeauty_personal_care", 
  "CLEANED_ITEM_CATEGORYautomotive", "Canada", "IS_ADDON_ITEM", "Sentiment_Category"
)

# Select these columns from model_data
model_data_reduced <- model_data %>%
  select(all_of(columns_to_include))





  
  
  
  # Prepping dependent variable
  model_data_reduced$Sentiment_Category <- factor(model_data_reduced$Sentiment_Category, levels = c("Negative", "Neutral", "Positive"))
  model_data_reduced$Sentiment_Category <- relevel(model_data_reduced$Sentiment_Category, ref = "Neutral")
  
  # Sentiment counts
  sentiment_counts <- model_data_reduced %>%
    group_by(Sentiment_Category) %>%
    summarise(count = n())
  
  print(sentiment_counts)
  
  model_data_reduced <- na.omit(model_data_reduced)
  
  
  
  
  
  # Create a random split (e.g., 70% train, 30% test)
  split <- sample.split(model_data_reduced$Sentiment_Category, SplitRatio = 0.8)
  
  # Split the data
  train_data <- subset(model_data_reduced, split == TRUE)
  test_data <- subset(model_data_reduced, split == FALSE)
  
  # Identify binary columns to exclude from preprocessing
  binary_cols <- names(model_data_reduced)[sapply(model_data_reduced, function(x) all(x %in% c(0, 1)))]
  
  # Apply preprocessing only to continuous numeric columns
  pre_process_train <- preProcess(train_data[, !names(train_data) %in% binary_cols], method = c("center", "scale"))
  pre_process_test <- preProcess(test_data[, !names(test_data) %in% binary_cols], method = c("center", "scale"))
  
  # Apply preprocessing only to continuous numeric columns
  train_data[, !names(train_data) %in% binary_cols] <- predict(pre_process_train, train_data[, !names(train_data) %in% binary_cols])
  test_data[, !names(test_data) %in% binary_cols] <- predict(pre_process_test, test_data[, !names(test_data) %in% binary_cols])
  
  class_weights <- table(train_data$Sentiment_Category)
  class_weights <- 1 / class_weights
  weights <- class_weights[train_data$Sentiment_Category]
  weights <- as.numeric(weights)
  
  model <- multinom(Sentiment_Category ~ ., data = train_data, weights = weights)
  
  # View model summary
  summary(model)
  
  # Predictions and accuracy
  predictions <- predict(model, test_data, type = "class")
  conf_matrix <- table(Predicted = predictions, Actual = test_data$Sentiment_Category)
  print(conf_matrix)
  
  accuracy <- mean(predictions == test_data$Sentiment_Category)
  cat("Accuracy:", accuracy, "\n")
  
  
  
  # Normalize confusion matrix to get percentages
  conf_matrix <- as.table(conf_matrix)  # Ensure it's in table form
  conf_matrix_perc <- prop.table(conf_matrix, 1) * 100  # Convert counts to percentages
  
  # Convert confusion matrix to a data frame for plotting
  conf_df <- as.data.frame(as.table(conf_matrix_perc))
  
  # Plot heatmap with percentages
  ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = round(Freq, 2)), color = "white", size = 8) +  # Double text size in the tiles
    scale_fill_gradient(low = "lightblue", high = "darkblue", breaks = seq(0, 100, 20)) +
    labs(title = "Confusion Matrix (Relative Percentages)", x = "Actual", y = "Predicted", fill = "Percentage") +
    theme_minimal() +
    theme(
      text = element_text(size = 32),               # Double general font size
      axis.text = element_text(size = 28),         # Double axis tick labels size
      axis.title = element_text(size = 30),        # Double axis titles size
      plot.title = element_text(size = 36, hjust = 0.5),  # Double title size and center it
      legend.title = element_text(size = 30),      # Double legend title size
      legend.text = element_text(size = 28)        # Double legend text size
    )
  
  # Extract coefficients
  coef_df <- as.data.frame(coef(model))
  coef_df$Feature <- rownames(coef_df)
  
  # Melt data for ggplot2
  coef_melt <- melt(coef_df, id.vars = "Feature", variable.name = "Class", value.name = "Coefficient")
  
  
  # Sort coefficients by absolute value
  coef_melt <- coef_melt %>%
    group_by(Class) %>%
    mutate(Abs_Coefficient = abs(Coefficient)) %>%
    arrange(Class, desc(Abs_Coefficient))
  
  # Plot coefficients
  ggplot(coef_melt, aes(x = reorder(Feature, Abs_Coefficient), y = Coefficient, fill = Class)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Feature Importance by Class",
         x = "Feature",
         y = "Coefficient",
         fill = "Class") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  probs <- predict(model, test_data, type = "prob")
  
  roc_curves <- lapply(levels(test_data$Sentiment_Category), function(class) {
    roc(as.numeric(test_data$Sentiment_Category == class), probs[, class])
  })
  
  # Plot ROC curves with correct legend
  plot(roc_curves[[1]], col = "red", main = "ROC Curves for Multinomial Model", print.auc = TRUE)
  for (i in 2:length(roc_curves)) {
    plot(roc_curves[[i]], col = i, lty = i, add = TRUE)
  }
  
  legend("bottomright", legend = levels(test_data$Sentiment_Category), col = 1:length(roc_curves), lty = 1:length(roc_curves), lwd = 2, title = "Sentiment Category")
  
  
  
  
  
  
  
  
  
  
  
  
  





























################# Cross-validation that did not work because it does not handle imbalanced data.
# Train with cross-validation
cv_model <- train(Sentiment_Category ~ ., data = model_data, 
                  method = "multinom", 
                  trControl = trainControl(method = "cv", number = 10))

print(cv_model)

# Extract predicted probabilities for cross-validated data
cv_predictions_prob <- predict(cv_model, model_data, type = "prob")

# Extract predicted classes
cv_predictions_class <- predict(cv_model, model_data)

# Combine actual classes, predicted classes, and probabilities into a data frame
confidence_table <- data.frame(
  Actual = model_data$Sentiment_Category,
  Predicted = cv_predictions_class,
  cv_predictions_prob
)

# Print the first few rows of the confidence table
print(head(confidence_table))

# Optional: Check the distribution of confidence scores
library(ggplot2)
ggplot(confidence_table, aes(x = Predicted, fill = Actual)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Predicted vs Actual Classes",
       x = "Predicted Class", y = "Count")









########################

library(randomForest)

# Fit a random forest model
rf_model <- randomForest(Sentiment_Category ~ . - Sentiment_VADER, data = model_data, ntree = 500)

# Get variable importance
importance(rf_model)
importance_values <- importance(rf_model)

# Plot the variable importance
varImpPlot(rf_model, main = "Variable Importance Plot")


############################


# Fit a linear regression model
lm_model <- lm(Sentiment_VADER ~ . - Sentiment_Category, data = model_data)

# Summarize the model
summary(lm_model)

#############################


#################################
lm_full <- lm(Sentiment_VADER ~ . -Sentiment_Category, data = model_data)  # Full model
stepwise_model <- step(lm_full, direction = "backward")
summary(stepwise_model)






# Select only numeric columns
num_cols <- sapply(model_data, is.numeric)
numerical_data <- model_data[, num_cols, drop = FALSE]

# Compute the correlation matrix, excluding missing values
cor_matrix <- cor(numerical_data, use = "complete.obs")

# Check for missing values and handle them
numerical_data <- na.omit(numerical_data)  # Remove rows with any NAs

# Visualize the correlation matrix
ggcorrplot(cor_matrix, 
           method = "circle",   # Circle visualization
           type = "lower",      # Lower triangle only
           lab = FALSE,          # Show correlation coefficients
           lab_size = 1,        # Size of labels
           colors = c("#6D9EC1", "white", "#E46726"), # Custom color palette
           title = "Correlation Heatmap",
           ggtheme = theme_minimal())





