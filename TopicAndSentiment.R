# Assuming 'truths' has columns author, text, and other necessary metadata
library(tidytext)
library(dplyr)
library(readr)
library(stringr)
library(tm)
library(tidyr)
library(ggplot2)
library(igraph)
library(topicmodels)
library(lubridate)
library(ggraph)
library(wordcloud)
library(syuzhet)


truths <- read_tsv("truths-cleaned.tsv", quote = "")
users <- read_tsv("users.tsv")

truths <- truths %>%
  mutate(truth_retruthed = ifelse(truth_retruthed == "", "-1", truth_retruthed))


# Preprocess data (assuming `truths` is already loaded)
truths_clean <- truths %>%
  mutate(
    text_clean = text %>%
      tolower() %>%
      gsub("[^a-zA-Z\s]", "", .) %>%
      str_squish()
  ) %>%
  unnest_tokens(word, text_clean)

# Define keyword sets for ideological alignment
left_keywords <- c("progressive", "liberal", "democrat")
right_keywords <- c("conservative", "republican", "maga")
political_keywords <- c(left_keywords, right_keywords)

# Add ideology column
truths_clean <- truths_clean %>%
  mutate(ideology = case_when(
    str_detect(text_clean, str_c(left_keywords, collapse = "|")) ~ "left",
    str_detect(text_clean, str_c(right_keywords, collapse = "|")) ~ "right",
    TRUE ~ "neutral"
  ))

# Add political mention column
truths_clean <- truths_clean %>%
  mutate(political_mention = str_detect(text_clean, str_c(political_keywords, collapse = "|")))

communities <- cluster_louvain(truths_clean)

# Extract community membership and create community_data dataframe
community_data <- data.frame(
  author = V(graph_object)$name,
  community = membership(communities)
)

truths_clean <- truths_clean %>% left_join(community_data, by = "author")

# Define functions for various analyses

# 1. Topic Modeling
topic_modeling <- function(data, num_topics = 5) {
  dtm <- DocumentTermMatrix(VectorSource(data$text_clean))
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  terms(lda_model, 10) # Top 10 terms for each topic
}

# 2. Sentiment and Emotion Analysis
sentiment_analysis <- function(data) {
  bing_sentiment <- get_sentiments("bing")
  sentiment_scores <- data %>%
    inner_join(bing_sentiment, by = "word") %>%
    group_by(author) %>%
    summarize(avg_sentiment = mean(score, na.rm = TRUE))
  sentiment_scores
}

emotion_analysis <- function(data) {
  nrc_sentiments <- get_sentiments("nrc")
  emotion_counts <- data %>%
    inner_join(nrc_sentiments, by = "word") %>%
    count(author, sentiment)
  emotion_counts
}

# 3. Network Analysis
network_analysis <- function(graph) {
  # Assuming `graph` is an igraph object
  centrality_measures <- data.frame(
    user_id = V(graph)$name,
    degree = degree(graph),
    betweenness = betweenness(graph),
    closeness = closeness(graph),
    eigenvector = eigen_centrality(graph)$vector
  )
  centrality_measures
}

# 4. Temporal Analysis
temporal_analysis <- function(data) {
  data %>%
    mutate(date = as.Date(created_at)) %>%
    group_by(date) %>%
    summarize(post_count = n()) %>%
    ggplot(aes(x = date, y = post_count)) +
    geom_line() +
    theme_minimal()
}

# 5. Visualization
create_wordcloud <- function(data) {
  word_counts <- data %>%
    filter(!word %in% stop_words$word) %>%
    count(word, sort = TRUE)
  wordcloud(words = word_counts$word, freq = word_counts$n, max.words = 100)
}

# Run the analyses

# Topic Modeling
topic_modeling_results <- topic_modeling(truths_clean)
print(topic_modeling_results)

# Sentiment Analysis
sentiment_results <- sentiment_analysis(truths_clean)
print(sentiment_results)

# Emotion Analysis
emotion_results <- emotion_analysis(truths_clean)
print(emotion_results)

# Network Analysis
# Assuming `graph_object` is an igraph network object created separately
# centrality_results <- network_analysis(graph_object)
# print(centrality_results)

# Temporal Analysis
temporal_plot <- temporal_analysis(truths_clean)
print(temporal_plot)

# Word Cloud
create_wordcloud(truths_clean)
