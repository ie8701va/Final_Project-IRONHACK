
#%%# CUSTOM FUNCTIONS FOR THE PROJECT

get_exchange_rate_ecb <- function(currency, date, api_key) {
  formatted_date <- format(date, "%Y-%m-%d")  # Format the date to YYYY-MM-DD
  url <- paste0("https://api.exchangeratesapi.io/", formatted_date, 
                "?base=EUR&symbols=", currency, "&access_key=", api_key)
  
  # GET request to fetch data
  response <- GET(url)
  
  # Print the status code and raw content for debugging
  print(status_code(response))
  print(content(response, "text"))
  
  # Parse response to JSON if successful
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    
    # Check if the 'rates' data is available
    if (!is.null(data$rates[[currency]])) {
      return(data$rates[[currency]])  # Return the exchange rate for the requested currency
    } else {
      return(NA)  # If no data for that currency, return NA
    }
  } else {
    return(NA)  # In case of a request failure
  }
}# Function to get exchange rate from the API of European Central Bank

get_exchange_rate_ecb <- function(date, base_currency = "EUR", target_currency = "USD") {
  # Ensure target_currency is in uppercase
  target_currency <- toupper(target_currency) 
  
  url <- paste0("https://api.exchangeratesapi.io/", date, "?base=", base_currency, "&symbols=", target_currency)
  response <- GET(url)
  
  # Check the response status
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    
    # Check if the 'rates' section is available
    if (!is.null(data$rates[[target_currency]])) {
      return(data$rates[[target_currency]])  # Return the exchange rate for the specified currency
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

convert_price_to_eur <- function(price, currency, created_at, api_key) {
  if (currency == "EUR") {
    return(price)  # No conversion needed if currency is already EUR
  } else {
    exchange_rate <- get_exchange_rate_ecb(currency, created_at, api_key)
    if (!is.na(exchange_rate)) {
      return(price / exchange_rate)  # Convert price to EUR
    } else {
      return(NA)  # If no exchange rate found, return NA
    }
  }
}# Function to convert price to EUR using API of European Central Bank

process_stock <- function(stock_column) {
  stock_column <- as.character(stock_column)
  
  # Extract numerical stock quantity
  stock_quantity <- str_extract(stock_column, "\\d+(?=\\s+left in stock)") %>% as.numeric()
  
  # Extract days to availability (e.g., "Usually ships within X days")
  days_to_availability <- str_extract(stock_column, "\\d+(?=\\s+days)") %>% as.numeric()
  
  # Identify items in stock on a specific date
  delayed_availability <- grepl("In stock on", stock_column)
  
  # Create a binary feature for immediate availability
  immediate_availability <- ifelse(grepl("left in stock", stock_column), 1, 0)
  
  # Combine into a data frame
  data.frame(
    Stock_Quantity = stock_quantity,
    Days_to_Availability = days_to_availability,
    Delayed_Availability = as.numeric(delayed_availability),
    Immediate_Availability = immediate_availability
  )
} # Function to process stock information

clean_text <- function(text) {
  # Define custom stopwords
  custom_stopwords <- c(stopwords("en"), "read", "can", "will", "just", "one", "like", "get", "use", "also", "really", "dont", "time", "book", "much")
  
  # Perform text cleaning
  text %>%
    tolower() %>%                      # Convert to lowercase
    removePunctuation() %>%            # Remove punctuation
    gsub("['’\";]", "", .) %>%         # Remove single quotes, double quotes, and semicolons
    removeNumbers() %>%                # Remove numbers
    removeWords(custom_stopwords) %>%  # Remove stop words (including custom ones)
    stripWhitespace() %>%              # Remove extra white spaces
    gsub("http\\S+|www\\S+", "", .)    # Remove URLs
}

get_top_words <- function(corpus, num_words = 10) {
  # Create a Term-Document Matrix (TDM)
  tdm <- TermDocumentMatrix(corpus)
  
  # Convert TDM to a matrix
  tdm_matrix <- as.matrix(tdm)
  
  # Calculate word frequencies
  word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
  
  # Create a data frame of top words
  top_words <- data.frame(
    Word = names(word_freq),
    Frequency = word_freq
  )
  
  # Return the top N words
  head(top_words, num_words)
} # Function to preprocess and get top words
