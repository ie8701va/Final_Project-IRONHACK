library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)
library(readxl)
library(forecast)
library(tsibble)
library(fable)
library(skimr)
library(stringr)
library(cluster)
library(SnowballC)
library(tm)
library(tidyverse)
library(circlize)
library(sentimentr)
library(tidytext)
library(textstem)
library(text2vec)
library(FactoMineR)
library(plotly)  # For interactive 3D plots
library(caret)  # For data splitting

data1 <- read_tsv("C:/Users/ievav/OneDrive/Documents/Ironhack/Prep_Vinted/OxyLabs/E-commerce Product data Set.tsv")

## data1 OVERVIEW: 
str(data1)
summary(data1)
skim(data1)
glimpse(data1)

## VIZUALISING data1

numerical_columns <- sapply(data1, is.numeric)
numerical_data1 <- data1[, numerical_columns]

# Remove rows where the 'price' column is <= 0
data1 <- data1 %>% filter(PRICE > 0)


plot_list <- lapply(names(numerical_data1), function(col) {
  ggplot(data1, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
    theme_minimal()
})

# Print all plots
for (plot in plot_list) {
  print(plot)
}


############################# REVIEWING DATE COLUMN
str(data1$RESULT_CREATED_AT)
head(data1$RESULT_CREATED_AT)

data1 <- data1 %>%
  mutate(RESULT_CREATED_AT = ymd_hms(RESULT_CREATED_AT))

data1$TIMESTAMP <- as.POSIXct(data1$RESULT_CREATED_AT, format = "%Y-%m-%d %H:%M:%S")

data1 <- data1 %>%
  mutate(HOUR = hour(TIMESTAMP),
         DAY = weekdays(TIMESTAMP))
str(data1$RESULT_CREATED_AT)
summary(data1$RESULT_CREATED_AT)

# ONE HOT ENCODING FOR DAY OF THE WEEK: 
data1 <- data1 %>%
  mutate(
    MONDAY = as.numeric(DAY == "Monday"),
    TUESDAY = as.numeric(DAY == "Tuesday"),
    WEDNESDAY = as.numeric(DAY == "Wednesday"),
    THURSDAY = as.numeric(DAY == "Thursday"),
    FRIDAY = as.numeric(DAY == "Friday"),
    SATURDAY = as.numeric(DAY == "Saturday"),
    SUNDAY = as.numeric(DAY == "Sunday")
  )


# REVIEWING STOCK COLUMN, ONE HOT ENCODING FOR MULTINOMIAL REGRESSION


### DEALING WITH STOCK COLUMN ####
unique(data1$STOCK)

# Apply the function to the STOCK column
STOCK_FEATURES <- process_stock(data1$STOCK)

# Add processed features back to the dataset
data1 <- cbind(data1, STOCK_FEATURES)


#####################REVIEWING COUNTRY COLUMN

unique(data1$GEO_LOCATION)

geo_counts <- data1 %>%
  group_by(GEO_LOCATION) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Apply conditions to update 'geo_location' and create 'City' column
data1 <- data1 %>%
  mutate(
    # Update geo_location with country names where applicable
    GEO_LOCATION = case_when(
      GEO_LOCATION == "10004" ~ "US",
      GEO_LOCATION == "E1 8HZ" ~ "UK",
      GEO_LOCATION == "2000" ~ "Australia",
      GEO_LOCATION == "60607" ~ "US",
      GEO_LOCATION == "32003" ~ "US",
      GEO_LOCATION == "90201" ~ "US",
      GEO_LOCATION == "10001" ~ "US",
      GEO_LOCATION == "10115" ~ "Germany",
      TRUE ~ GEO_LOCATION  # Leave other values unchanged
    ),
    
    # Create a new column 'City' for numerical geo_location values
    City = case_when(
      GEO_LOCATION == "10004" ~ "NY",
      GEO_LOCATION == "E1 8HZ" ~ "London",
      GEO_LOCATION == "2000" ~ "Sydney",
      GEO_LOCATION == "60607" ~ "Chicago",
      GEO_LOCATION == "32003" ~ "Green Cove Springs",
      GEO_LOCATION == "90201" ~ "Bell",
      GEO_LOCATION == "10001" ~ "New York",
      GEO_LOCATION == "10115" ~ "Berlin",
      TRUE ~ NA_character_  # For other values, leave as NA
    )
  )
# Check the updated geo_location column
geo_counts_post <- data1 %>%
  group_by(GEO_LOCATION) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


data1$GEO_LOCATION[data1$GEO_LOCATION == "GB"] <- "UK"
data1$GEO_LOCATION[data1$GEO_LOCATION == "IT"] <- "Italy"
data1$GEO_LOCATION[data1$GEO_LOCATION == "DE"] <- "Germany"
data1$GEO_LOCATION[data1$GEO_LOCATION == "CA"] <- "Canada"
data1$GEO_LOCATION[data1$GEO_LOCATION == "JP"] <- "Japan"
data1$GEO_LOCATION[data1$GEO_LOCATION == "FR"] <- "France"
data1$GEO_LOCATION[data1$GEO_LOCATION == "ES"] <- "Spain"
data1$GEO_LOCATION[data1$GEO_LOCATION == "MX"] <- "Mexico"

# ONE HOT ENCODING UK US AND CANADA IN PREPARATION FOR 
data1 <- data1 %>%
  mutate(
    US = as.numeric(GEO_LOCATION == "US"),
    Canada = as.numeric(GEO_LOCATION == "Canada")
  )


# Check the updated geo_location column
geo_counts_post <- data1 %>%
  group_by(GEO_LOCATION) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


data2 <- data1[data1$GEO_LOCATION %in% c("US", "Canada"), ]


data <- data2[!is.na(data2$TOP_REVIEW), ]




################# DEALING WITH LADDER NAME TO CLUSTER INTO 10 BROAD CATEGORIES

# First, preprocess that column

data$CLEANED_ITEM_CATEGORY<- sapply(data$LADDER_1_NAME, clean_text)
length(unique(data$CLEANED_ITEM_CATEGORY)) # There are 250 unique categories

# Calculate category frequencies
category_freq <- table(data$CLEANED_ITEM_CATEGORY)


# Create a contingency table of country vs. category
country_category_freq <- table(data$GEO_LOCATION, data$LADDER_1_NAME)

# Convert the table to adata frame for easier handling
country_category_df <- as.data.frame(as.table(country_category_freq))

# Rename the columns for clarity
colnames(country_category_df) <- c("GEO_LOCATION", "CATEGORY", "Frequency")

# Print the first few rows of the result
head(country_category_df)


# Filter categories with more than 3000 frequencies
filtered_categories <- names(category_freq[category_freq > 1000])

# Filter the data to keep only these frequent categories
data <- data[data$CLEANED_ITEM_CATEGORY %in% filtered_categories, ]

# Visualize
ggplot(data, aes(x = CLEANED_ITEM_CATEGORY)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# One-hot encode the CLEANED_ITEM_CATEGORY column
category_data_encoded <- model.matrix(~ CLEANED_ITEM_CATEGORY - 1, data = data)
data <- cbind(data, category_data_encoded)
head(data)

################# REVIEW PRICE COLUMN / CONVERT TO EUROS


# API Key --> since limited requests - did not use it. 
# api_key <- "dd96dbe0545057bcfb602497dc4b4114"
# data <- data %>%
#   mutate(RESULT_CREATED_AT = ymd_hms(RESULT_CREATED_AT)) %>%  # Make sure RESULT_CREATED_AT is in datetime format
#   mutate(PRICE_EUR = mapply(convert_price_to_eur, PRICE, CURRENCY, RESULT_CREATED_AT, MoreArgs = list(api_key = api_key)))


# Load the exchange_rate sheet
exchange_rate <- read_excel("C:/Users/ievav/OneDrive/Documents/Ironhack_Final_Project_Sentiment_Analysis/data/Exchange Rate.xlsx")


# Create a named vector from the exchange_rate dataset for easy lookup
rate_vector <- setNames(exchange_rate$ExchangeRate, exchange_rate$Country)

# Multiply the PRICE column by the appropriate exchange rate
data <- data %>%
  mutate(PRICE_IN_EUROS = PRICE * rate_vector[GEO_LOCATION])


# Check if the 'PRICE' column contains any NA values
any(is.na(data$PRICE_IN_EUROS))


# Apply log transformation
data$PRICE_log <- log(data$PRICE_IN_EUROS + 1)

# Plot the normalized price distribution
ggplot(data, aes(x = PRICE_log)) +
  geom_histogram(binwidth = 0.2, fill = "blue", alpha = 0.7, color = "black") +
  labs(title = "Normalized Price Distribution", x = "Log(PRICE + 1)", y = "Frequency") +
  theme_minimal()






# VISUALIZING THE DISPTRIBUTION OF PRICE IN EUROS 
ggplot(data, aes_string(x = "PRICE_IN_EUROS")) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Log-Scale Distribution of PRICE_IN_EUROS", x = "PRICE_IN_EUROS", y = "Frequency") +
  theme_minimal()


ggplot(data, aes_string(x = "PRICE_IN_EUROS")) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Log-Scale Distribution of PRICE_IN_EUROS", x = "PRICE_IN_EUROS", y = "Frequency") +
  facet_wrap(~ GEO_LOCATION, scales = "free") +  # Create subplots for each country
  theme_minimal()



sales_count_per_country <- data %>%
  filter(PRICE_IN_EUROS > 0) %>%
  group_by(GEO_LOCATION) %>%
  summarise(sales_count = n())

# View the result
print(sales_count_per_country)



# Remove rows where GEO_LOCATION is NA
data <- data %>%
  filter(!is.na(GEO_LOCATION))

# Count the number of duplicate ASIN_IN_URL values
duplicate_asin_counts <- data %>%
  group_by(ASIN_IN_URL) %>%
  filter(n() > 1) %>%
  summarise(duplicate_count = n()) %>%
  arrange(desc(duplicate_count))

# View the result
head(duplicate_asin_counts)


# Get unique values in LADDER_2_NAME using dplyr
unique_categories <- data %>%
  distinct(LADDER_1_NAME) %>%
  pull(LADDER_1_NAME)

# View the unique categories
unique_categories

# Count the occurrences of each category in LADDER_2_NAME
category_counts <- data %>%
  count(LADDER_1_NAME) %>%
  arrange(desc(n))  # Optionally, sort in descending order of frequency

# View the result
category_counts

# Reorder LADDER_2_NAME by frequency
data$LADDER_1_NAME <- factor(data$LADDER_1_NAME, 
                             levels = names(sort(table(data$LADDER_1_NAME), decreasing = TRUE)))

# Create the bar plot
ggplot(data, aes(x = LADDER_1_NAME)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency of Categories in LADDER_2_NAME", 
       x = "Category", 
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability