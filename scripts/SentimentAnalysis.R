
library(topicmodels)
library(openxlsx)

# Apply the cleaning function to the TOP_REVIEW column
data$Cleaned_Review <- sapply(data$TOP_REVIEW, clean_text)

# Now you can apply VADER sentiment analysis
data$Sentiment_VADER <- sentiment(data$Cleaned_Review)$sentiment


ggplot(data, aes(x = Sentiment_VADER)) +
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Sentiment Scores", x = "Sentiment Score", y = "Frequency") +
  theme_minimal()







# Visualizing how sentiment correlates with price: 


# Plot using the cleaned data
ggplot(data, aes(x = PRICE_log, y = Sentiment_VADER)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Scatterplot of log-transformed price vs Sentiment_VADER",
    x = "Z-Scored Price in Euros",
    y = "Sentiment (VADER Score)"
  ) +
  theme_minimal()














# Categorizing the Sentiment into Positive Negative and Neutral

data <- data %>%
  mutate(Sentiment_Category = case_when(
    Sentiment_VADER > 0.1 ~ "Positive",
    Sentiment_VADER < -0.1 ~ "Negative",
    TRUE ~ "Neutral"
  ))


sentiment_counts <- data %>%
  count(Sentiment_Category)

sentiment_counts


data %>%
  group_by(GEO_LOCATION, Sentiment_Category) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = GEO_LOCATION, y = Count, fill = Sentiment_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment Distribution by Location", x = "Location", y = "Count") +
  theme_minimal()



# Calculating the relative contirbution of every item to see whether there are sign. diff between them

data <- data %>%
  mutate(relative_price = case_when(
    Sentiment_Category == "Positive" ~ (PRICE_IN_EUROS / sum(Sentiment_Category == "Positive")),
    Sentiment_Category == "Negative" ~ (PRICE_IN_EUROS / sum(Sentiment_Category == "Negative")),
    Sentiment_Category == "Neutral" ~ (PRICE_IN_EUROS / sum(Sentiment_Category == "Neutral")),
    TRUE ~ NA_real_  # Default value for unmatched categories
  ))

# Scatter plot between Sentiment_VADER and relative_price
ggplot(data, aes(x = Sentiment_VADER, y = relative_price)) +
  geom_point(alpha = 0.5, color = "blue") +  # Transparent points for better visualization
  labs(title = "Scatter Plot of Sentiment VADER vs. Relative Price",
       x = "Sentiment VADER",
       y = "Relative Price") +
  theme_minimal()


# Custom color palette
distinct_blues <- c("#345ee9", "#1f77b4", "#66c2ff", "#003f88", "#1e90ff", "#4682b4", "#5dade2", "#154360")

# Plot average price by sentiment category with custom colors and legend title
data %>%
  group_by(Sentiment_Category) %>%
  summarise(avg_price = mean(PRICE_IN_EUROS, na.rm = TRUE)) %>%
  ggplot(aes(x = Sentiment_Category, y = avg_price, fill = Sentiment_Category)) +
  geom_bar(stat = "identity", , width = 0.7) +
  scale_fill_manual(values = distinct_blues) +  # Apply custom colors
  labs(title = "Average Price by Sentiment Category", 
       x = "Sentiment Category", 
       y = "Average Price",
       fill = "Sentiment Category") +  # Set legend title
  theme_minimal() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        aspect.ratio = 1)  # Rotate x-axis labels


# Bar plot showing count of each Sentiment Category with top-left legend, narrower bars, tighter spacing, and square shape
# Bar plot showing count of each Sentiment Category with top-left legend, narrower bars, tighter spacing, and square shape
data %>%
  group_by(Sentiment_Category) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Sentiment_Category, y = count, fill = Sentiment_Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Narrower bars
  scale_fill_manual(values = distinct_blues) +  # Use custom colors
  theme_minimal() +
  theme(legend.position = "top",  # Move legend to top
        legend.text = element_text(size = 26),  # Increase legend text size
        legend.title = element_blank(),  # Increase legend title size
        axis.text.x = element_text(hjust = 1, size = 26),  # Increase x-axis label size
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank(),  # Remove y-axis title
        plot.title = element_blank(),  # Remove plot title
        panel.spacing = unit(0.2, "lines"),  # Reduce spacing between panels
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),  # Tighten plot margins
        aspect.ratio = 1)  # Set the aspect ratio to 1 (square plot)




# Exporting to Excel
write.xlsx(data, "model_data.xlsx")


# Prepare the data for chord diagram, grouping by LADDER_1_NAME and country, sentiment category
reviews_count <- data %>%
  count(LADDER_1_NAME, GEO_LOCATION, Sentiment_Category, name = "Review_Count")

# Assign colors to the sentiment categories
# Define custom colors
sentiment_colors <- c("Positive" = "#00bf63",  # Green
                      "Negative" = "#e27354",  # Red
                      "Neutral" = "#345ee9")   # Blue


# Create a color vector for sentiment categories (this will map colors to sentiment categories)
grid_col <- sentiment_colors[reviews_count$Sentiment_Category]

# Set the global parameters for the circular layout
circos.par(gap.after = c(rep(5, length(unique(reviews_count$LADDER_1_NAME)) - 1), 15,
                         rep(5, length(unique(reviews_count$Sentiment_Category)) - 1), 15))

# Clear previous layout
circos.clear()

# Draw the chord diagram using the prepared data
chordDiagram(reviews_count[, c("LADDER_1_NAME", "GEO_LOCATION", "Review_Count")], 
             grid.col = grid_col, transparency = 0.2)

# Add title to the chart
title("Sentiment Analysis by LADDER_1_NAME and Country")






# Identify top 5 most frequent LADDER_1_NAME categories
top_ladder_names <- category_counts %>%
  arrange(desc(n)) %>%
  head(5)  # Top 5 categories

# Filter reviews_count to include only top LADDER_1_NAME categories
reviews_count_top <- reviews_count %>%
  filter(LADDER_1_NAME %in% top_ladder_names$LADDER_1_NAME)

# Assign colors to the sentiment categories
sentiment_colors <- c("Positive" = "#00bf63",  # Green
                      "Negative" = "#e27354",  # Red
                      "Neutral" = "#345ee9")   # Blue

# Create a color vector for sentiment categories
grid_col <- c(sentiment_colors)

# Set the global parameters for the circular layout
circos.par(gap.after = c(rep(5, length(unique(reviews_count_top$LADDER_1_NAME)) - 1), 15,
                         rep(5, length(unique(reviews_count_top$Sentiment_Category)) - 1), 15))

# Clear previous layout
circos.clear()

# Draw the chord diagram focusing on top LADDER_1_NAME and Sentiment Category
chordDiagram(reviews_count_top, grid.col = grid_col, transparency = 0.2)

# Adjust the text properties to display LADDER_1_NAME better
# Rotate and adjust label position
circos.text(sector.index = get.panel.text("LADDER_1_NAME", major.index = 1), 
            cell.index = 1:length(unique(reviews_count_top$LADDER_1_NAME)), 
            labels = unique(reviews_count_top$LADDER_1_NAME), 
            facing = "clockwise", 
            niceFacing = TRUE, 
            cex = 0.8)  # Adjust the font size of labels

# Add title to the chart
title("Sentiment Analysis by Top 5 LADDER_1_NAME and Sentiment Category")













# Filter the data for US and Canada
us_data <- reviews_count %>% filter(GEO_LOCATION == "US")
canada_data <- reviews_count %>% filter(GEO_LOCATION == "Canada")

# Calculate proportions for each LADDER_1_NAME
us_data_proportions <- us_data %>%
  filter(GEO_LOCATION == "US") %>%
  group_by(LADDER_1_NAME) %>%
  mutate(Proportion = Review_Count / sum(Review_Count))

canada_data_proportions <- canada_data %>%
  filter(GEO_LOCATION == "Canada") %>%
  group_by(LADDER_1_NAME) %>%
  mutate(Proportion = Review_Count / sum(Review_Count))

# Plot for US with proportions
ggplot(us_data_proportions, aes(x = LADDER_1_NAME, y = Proportion, fill = Sentiment_Category)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Distribution by Category (US)", y = "Proportion", x = "Category", fill = "Sentiment Category") +
  scale_fill_manual(values = sentiment_colors) +
  theme_minimal()+
  theme(legend.position = "top")

# Plot for Canada with proportions
ggplot(canada_data_proportions, aes(x = LADDER_1_NAME, y = Proportion, fill = Sentiment_Category)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Distribution by Category (Canada)", y = "Proportion", , x = "Category", fill = "Sentiment Category") +
  scale_fill_manual(values = sentiment_colors) +
  theme_minimal() +
  theme(legend.position = "top")













###################
## SANKEY PLOT ####
###################
# Load required libraries
library(dplyr)
library(tm)
library(networkD3)


# Filter reviews based on sentiment category
positive_reviews <- data %>% filter(Sentiment_Category == "Positive")
negative_reviews <- data %>% filter(Sentiment_Category == "Negative")
neutral_reviews <- data %>% filter(Sentiment_Category == "Neutral")

# Create corpora for each sentiment category
positive_corpus <- Corpus(VectorSource(positive_reviews$Cleaned_Review))
negative_corpus <- Corpus(VectorSource(negative_reviews$Cleaned_Review))
neutral_corpus <- Corpus(VectorSource(neutral_reviews$Cleaned_Review))


# Process each sentiment category
positive_top_words <- get_top_words(positive_corpus)
negative_top_words <- get_top_words(negative_corpus)
neutral_top_words <- get_top_words(neutral_corpus)

# Combine results for all sentiment categories
all_top_words <- list(
  Positive = positive_top_words,
  Negative = negative_top_words,
  Neutral = neutral_top_words
)

# View top words
print(all_top_words)


# Combine results into a single data frame for Sankey diagram
sankey_data <- bind_rows(
  positive_top_words %>% mutate(Sentiment = "Positive"),
  negative_top_words %>% mutate(Sentiment = "Negative"),
  neutral_top_words %>% mutate(Sentiment = "Neutral")
)

# Example output for visualization
print(sankey_data)






# Prepare data for Sankey diagram
nodes <- data.frame(name = unique(c(sankey_data$Word, sankey_data$Sentiment)))

# Map words and sentiment to node indices
sankey_data <- sankey_data %>%
  mutate(
    Source = match(Sentiment, nodes$name) - 1,  # Map sentiment to node indices
    Target = match(Word, nodes$name) - 1          # Map words to node indices
  )

# Add a 'Group' column for coloring
links <- sankey_data %>%
  mutate(Group = case_when(
    Sentiment == "Positive" ~ "Green",
    Sentiment == "Negative" ~ "Red",
    Sentiment == "Neutral" ~ "Blue"
  ))

# Define custom color scale with toned-down colors
color_scale <- 'd3.scaleOrdinal()
                 .domain(["Green", "Red", "Blue"])
                 .range(["#00BF63", "#E27354", "#345EE9"])'

# Create the Sankey diagram
sankey <- sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "Source", Target = "Target", Value = "Frequency",
  NodeID = "name", fontSize = 20, nodeWidth = 50,
  LinkGroup = "Group", # Assign the group column for links
  colourScale = color_scale # Define the color scale
)

# Plot
sankey










library(wordcloud)
library(tm)
library(RColorBrewer)

# Combine all text from the Cleaned_Review column into one string
all_text <- paste(data$Cleaned_Review, collapse = " ")

# Create a Corpus
corpus <- Corpus(VectorSource(all_text))

# Clean the text
corpus <- tm_map(corpus, content_transformer(tolower))       # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)                  # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                      # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))       # Remove common stopwords
corpus <- tm_map(corpus, stripWhitespace)                    # Remove extra whitespace

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert the matrix to a data frame of word frequencies
matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)
# Define a custom palette with distinct shades of blue
distinct_blues <- c("#345ee9", "#1f77b4", "#66c2ff", "#003f88", "#1e90ff", 
                    "#4682b4", "#5dade2", "#154360")

# Generate the Word Cloud
set.seed(123)  # For reproducibility
# Generate the word cloud with circular shape
wordcloud(words = word_freq_df$word, 
          freq = word_freq_df$freq, 
          min.freq = 10,                # Minimum frequency of words to be included
          max.words = 500,             # Maximum number of words to display
          random.order = FALSE,        # Order by frequency
          colors = distinct_blues,     # Custom blue shades
          rot.per = 0.35,              # Rotating about 35% of words
          scale = c(4, 0.7),           # Size scaling for words
          circular = TRUE)  