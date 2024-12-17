# Install required libraries if not already installed
if (!require("igraph")) install.packages("igraph")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggraph")) install.packages("ggraph")

library(igraph)
library(tidyverse)
library(lubridate)
library(ggraph)
library(ggrepel)

con <- file("eventsv2.txt")
sink(con, append=TRUE)

# Load the dataset (assume a CSV file with columns: source, target, timestamp, type)
# source = user who posts or interacts, target = recipient, type = interaction type (e.g., "truth", "retruth"), timestamp = event time
data <- read_tsv("truths-cleaned.tsv", quote = "")

data <- data %>%
  mutate(truth_retruthed = ifelse(truth_retruthed == "", "-1", truth_retruthed))

# Filter out rows with invalid timestamps (-1)
data <- data %>% filter(timestamp != -1)

# Parse the timestamp column and handle parsing errors
data <- data %>%
  mutate(timestamp_parsed = ymd_hms(timestamp, quiet = TRUE)) %>%
  filter(!is.na(timestamp_parsed))  # Remove rows with unparsable timestamps

# Overwrite original timestamp column with the parsed version
data$timestamp <- data$timestamp_parsed
data <- select(data, -timestamp_parsed)  # Drop temporary parsed column


# Filter data into time windows for the overturning of Roe v. Wade
roe_baseline_data <- data %>% filter(timestamp >= "2022-06-10" & timestamp < "2022-06-24")
roe_event_data <- data %>% filter(timestamp >= "2022-06-24" & timestamp <= "2022-06-30")
roe_post_event_data <- data %>% filter(timestamp > "2022-06-30")

# Filter data into time windows for the FBI search of Mar-a-Lago
fbi_baseline_data <- data %>% filter(timestamp >= "2022-07-25" & timestamp < "2022-08-08")
fbi_event_data <- data %>% filter(timestamp >= "2022-08-08" & timestamp <= "2022-08-14")
fbi_post_event_data <- data %>% filter(timestamp > "2022-08-14")

# Function to create an igraph object from the dataset
create_network <- function(data) {
  graph <- graph_from_data_frame(data, directed = TRUE)
  return(graph)
}

# Create networks for each event
roe_baseline_network <- create_network(roe_baseline_data)
roe_event_network <- create_network(roe_event_data)
roe_post_event_network <- create_network(roe_post_event_data)

fbi_baseline_network <- create_network(fbi_baseline_data)
fbi_event_network <- create_network(fbi_event_data)
fbi_post_event_network <- create_network(fbi_post_event_data)


## Network Structure Analysis 

# Function to calculate network metrics
calculate_metrics <- function(graph) {
  list(
    num_nodes = vcount(graph),
    num_edges = ecount(graph),
    density = edge_density(graph),
    avg_path_length = mean_distance(graph, directed = TRUE),
    diameter = diameter(graph),
    clustering_coefficient = transitivity(graph, type = "global"),
    connected_components = components(graph)
  )
}

# Calculate metrics for each event
roe_baseline_metrics <- calculate_metrics(roe_baseline_network)
roe_event_metrics <- calculate_metrics(roe_event_network)
roe_post_event_metrics <- calculate_metrics(roe_post_event_network)

fbi_baseline_metrics <- calculate_metrics(fbi_baseline_network)
fbi_event_metrics <- calculate_metrics(fbi_event_network)
fbi_post_event_metrics <- calculate_metrics(fbi_post_event_network)

## Engagement Analysis

# Function to count daily interactions for activity trend analysis
activity_trends <- function(data, event_date, event_label, days_before = 30, days_after = 30) {
  # Convert event_date to Date format
  event_date <- as.Date(event_date)
  
  # Set the date range around the event
  start_date <- event_date - days_before
  end_date <- event_date + days_after
  
  # Filter data for the selected range
  filtered_data <- data %>%
    mutate(date = as.Date(timestamp)) %>%
    filter(date >= start_date & date <= end_date) %>%
    group_by(date) %>%
    summarise(num_interactions = n())
  
  # Plot with the event date centered and trend line added
  ggplot(filtered_data, aes(x = date, y = num_interactions)) +
    geom_line(color = "blue", alpha = 0.7, size = 1) + # Original interaction trends
    geom_smooth(method = "loess", color = "orange", se = TRUE, linetype = "solid", linewidth = 1.2) + # LOESS trend line
    geom_vline(xintercept = event_date, linetype = "dashed", color = "red", linewidth = 1) + # Event date
    scale_x_date(limits = c(start_date, end_date), date_labels = "%Y-%m-%d") +
    labs(
      title = paste("Engagement Trends Around", event_label),
      x = "Date",
      y = "Number of Interactions"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
}

# Example Usage
activity_trends(data, "2022-06-24", "Roe v. Wade Overturning", days_before = 50, days_after = 50)
activity_trends(data, "2022-08-08", "FBI Search of Mar-a-Lago", days_before = 50, days_after = 50)


## Community Detection

# Function to detect communities using the Louvain method
detect_communities <- function(graph) {
  # Convert the graph to undirected
  undirected_graph <- as.undirected(graph, mode = "collapse")
  
  # Detect communities
  community <- cluster_louvain(undirected_graph)
  
  # Get community membership and sizes
  membership <- membership(community)
  sizes <- sizes(community)
  modularity <- modularity(community)
  
  list(community = community, membership = membership, sizes = sizes, modularity = modularity)
}

# Detect communities for each event
roe_baseline_communities <- detect_communities(roe_baseline_network)
roe_event_communities <- detect_communities(roe_event_network)
roe_post_event_communities <- detect_communities(roe_post_event_network)

fbi_baseline_communities <- detect_communities(fbi_baseline_network)
fbi_event_communities <- detect_communities(fbi_event_network)
fbi_post_event_communities <- detect_communities(fbi_post_event_network)

# Function to print detailed community information
print_community_info <- function(community_results, event_name) {
  cat("\n---", event_name, "Community Information ---\n")
  
  # Print the number of communities detected
  num_communities <- length(community_results$sizes)
  cat("Number of communities detected:", num_communities, "\n")
  
  # Calculate and print the average community size
  avg_community_size <- mean(community_results$sizes)
  cat("Average community size:", round(avg_community_size, 2), "\n")
  
  # Print the modularity score of the community detection
  cat("Modularity of the community structure:", community_results$modularity, "\n\n")
}

# Print community information for each event (Roe v. Wade and FBI Search)
print_community_info(roe_baseline_communities, "Roe v. Wade Baseline")
print_community_info(roe_event_communities, "Roe v. Wade Event")
print_community_info(roe_post_event_communities, "Roe v. Wade Post-Event")

print_community_info(fbi_baseline_communities, "FBI Search Baseline")
print_community_info(fbi_event_communities, "FBI Search Event")
print_community_info(fbi_post_event_communities, "FBI Search Post-Event")


## Centrality Measures

# Function to create an igraph object from the dataset
create_network <- function(data) {
  # Process the 'truths' data to join on retruthed truths (if a truth has been retruthed)
  data <- data %>%
    mutate(truth_retruthed = ifelse(truth_retruthed == "", "-1", truth_retruthed)) %>%
    filter(truth_retruthed != -1) %>%  # Keep only retruthed truths
    mutate(truth_retruthed = as.integer(truth_retruthed)) %>%
    left_join(data, by = c("truth_retruthed" = "id"), suffix = c("_reposter", "_original")) %>%
    select(from = author_original, to = author_reposter) %>%  # Select the columns we need (author IDs)
    filter(!is.na(from))  # Keep only rows where 'from' is not NA
  
  # Check if 'from' and 'to' columns exist
  if (!all(c("from", "to") %in% colnames(data))) {
    stop("Columns 'from' and 'to' are missing in the data. Please check the dataset structure.")
  }
  
  # Create vertices data frame with unique user IDs from both 'from' and 'to' columns
  vertices <- data.frame(name = unique(c(data$from, data$to)))
  
  # Create graph from the edges (from 'author' to 'reposter')
  graph <- graph_from_data_frame(data, directed = TRUE, vertices = vertices)
  
  # Check the number of vertices in the graph
  if (vcount(graph) == 0) {
    stop("The graph has no vertices. Please check the dataset or the processing steps.")
  }
  
  # Explicitly assign the user_id column to the graph vertices
  V(graph)$user_id <- V(graph)$name
  
  return(graph)
}

# Function to calculate centralities
calculate_centralities <- function(graph) {
  # Check if the graph has vertices
  if (vcount(graph) == 0) {
    stop("Graph has no vertices, cannot calculate centralities.")
  }
  
  # Extract centrality measures: degree, betweenness, closeness, eigenvector
  degree_values <- degree(graph, mode = "all")
  betweenness_values <- betweenness(graph)
  closeness_values <- closeness(graph)
  eigenvector_values <- eigen_centrality(graph)$vector
  
  # Check the length of the centrality measures
  if (length(degree_values) != vcount(graph) || 
      length(betweenness_values) != vcount(graph) ||
      length(closeness_values) != vcount(graph) ||
      length(eigenvector_values) != vcount(graph)) {
    stop("Mismatch in the number of centrality values and vertices.")
  }
  
  # Create a data frame with 'user_id' (vertex names) and centrality measures
  centralities <- data.frame(
    user_id = V(graph)$user_id,  # Use the user_id explicitly assigned to vertices
    degree = degree_values,
    betweenness = betweenness_values,
    closeness = closeness_values,
    eigenvector = eigenvector_values
  )
  
  return(centralities)
}

# recalculating centralities for the cleaned event networks
roe_baseline_network <- create_network(roe_baseline_data)
roe_event_network <- create_network(roe_event_data)
roe_post_event_network <- create_network(roe_post_event_data)

fbi_baseline_network <- create_network(fbi_baseline_data)
fbi_event_network <- create_network(fbi_event_data)
fbi_post_event_network <- create_network(fbi_post_event_data)

# safely calculate centralities for each event after filtering out invalid user IDs
roe_baseline_centrality <- calculate_centralities(roe_baseline_network)
roe_event_centrality <- calculate_centralities(roe_event_network)
roe_post_event_centrality <- calculate_centralities(roe_post_event_network)

fbi_baseline_centrality <- calculate_centralities(fbi_baseline_network)
fbi_event_centrality <- calculate_centralities(fbi_event_network)
fbi_post_event_centrality <- calculate_centralities(fbi_post_event_network)

# Print the top users by degree centrality for Roe and FBI events
top_roe_users <- roe_event_centrality %>%
  arrange(desc(degree)) %>%
  head(10)

top_roe_baseline_users <- roe_baseline_centrality %>%
  arrange(desc(degree)) %>%
  head(10)

top_roe_post_event_users <- roe_post_event_centrality %>%
  arrange(desc(degree)) %>%
  head(10)

top_fbi_users <- fbi_event_centrality %>%
  arrange(desc(degree)) %>%
  head(10)

top_fbi_baseline_users <- fbi_baseline_centrality %>%
  arrange(desc(degree)) %>%
  head(10)

top_fbi_post_event_users <- fbi_post_event_centrality %>%
  arrange(desc(degree)) %>%
  head(10)

# Print the top users for each stage
cat("Top Users Before, During, and After the Event (Roe):\n")
cat("Before Event:\n")
print(top_roe_baseline_users)

cat("\nDuring Event:\n")
print(top_roe_users)

cat("\nAfter Event:\n")
print(top_roe_post_event_users)

cat("\nTop Users Before, During, and After the Event (FBI):\n")
cat("Before Event:\n")
print(top_fbi_baseline_users)

cat("\nDuring Event:\n")
print(top_fbi_users)

cat("\nAfter Event:\n")
print(top_fbi_post_event_users)

# Combine the top users from each stage and make sure we have all unique users
top_roe_combined <- bind_rows(
  mutate(top_roe_baseline_users, stage = "Before Event"),
  mutate(top_roe_users, stage = "During Event"),
  mutate(top_roe_post_event_users, stage = "After Event")
) %>%
  distinct(user_id, .keep_all = TRUE)  # Ensure unique users across stages

top_fbi_combined <- bind_rows(
  mutate(top_fbi_baseline_users, stage = "Before Event"),
  mutate(top_fbi_users, stage = "During Event"),
  mutate(top_fbi_post_event_users, stage = "After Event")
) %>%
  distinct(user_id, .keep_all = TRUE)  # Ensure unique users across stages

# Print the top users for each stage, making sure duplicates are included
cat("Top Users Before, During, and After the Event (Roe):\n")
print(top_roe_combined)

cat("\nTop Users Before, During, and After the Event (FBI):\n")
print(top_fbi_combined)

# Analyze the differences in centrality measures before, during, and after the event for Roe
roe_diff_combined <- full_join(top_roe_baseline_users, top_roe_users, by = "user_id", suffix = c("_baseline", "_event")) %>%
  full_join(top_roe_post_event_users, by = "user_id") %>%
  rename(degree_baseline = degree_baseline, degree_event = degree_event, degree_post_event = degree)

cat("\nCentrality Differences (Roe):\n")
print(roe_diff_combined)

# Analyze the differences in centrality measures before, during, and after the event for FBI
fbi_diff_combined <- full_join(top_fbi_baseline_users, top_fbi_users, by = "user_id", suffix = c("_baseline", "_event")) %>%
  full_join(top_fbi_post_event_users, by = "user_id") %>%
  rename(degree_baseline = degree_baseline, degree_event = degree_event, degree_post_event = degree)

cat("\nCentrality Differences (FBI):\n")
print(fbi_diff_combined)

# Before and After the Event Visualizations for Roe
# Merge the top users for before and after the event for Roe
roe_diff_combined <- full_join(top_roe_baseline_users, top_roe_post_event_users, by = "user_id", suffix = c("_baseline", "_post_event"))

# Plot Degree Centrality Comparison for Roe (Before vs. After)
ggplot(roe_diff_combined, aes(x = degree_baseline, y = degree_post_event, label = user_id)) +
  geom_point(color = "blue", size = 3) +
  geom_text_repel(aes(label = user_id), box.padding = 0.3, point.padding = 0.5, size = 3) +
  labs(title = "Degree Centrality Comparison (Roe): Before vs After Event",
       x = "Degree Centrality (Before Event)",
       y = "Degree Centrality (After Event)") +
  theme_minimal()

# Before and After the Event Visualizations for FBI
# Merge the top users for before and after the event for FBI
fbi_diff_combined <- full_join(top_fbi_baseline_users, top_fbi_post_event_users, by = "user_id", suffix = c("_baseline", "_post_event"))

# Plot Degree Centrality Comparison for FBI (Before vs. After)
ggplot(fbi_diff_combined, aes(x = degree_baseline, y = degree_post_event, label = user_id)) +
  geom_point(color = "red", size = 3) +
  geom_text_repel(aes(label = user_id), box.padding = 0.3, point.padding = 0.5, size = 3) +
  labs(title = "Degree Centrality Comparison (FBI): Before vs After Event",
       x = "Degree Centrality (Before Event)",
       y = "Degree Centrality (After Event)") +
  theme_minimal()

# Visualize Degree Centrality for Each Stage (Before, Event, After) for Roe
roe_combined_all_stages <- bind_rows(
  mutate(top_roe_baseline_users, stage = "Before Event"),
  mutate(top_roe_users, stage = "During Event"),
  mutate(top_roe_post_event_users, stage = "After Event")
) 

ggplot(roe_combined_all_stages, aes(x = user_id, y = degree, color = stage)) +
  geom_point() +
  labs(title = "Degree Centrality for Each Stage (Roe)",
       x = "User ID", y = "Degree Centrality") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +  # Hide x-axis labels for clarity
  theme_minimal()

# Visualize Degree Centrality for Each Stage (Before, Event, After) for FBI
fbi_combined_all_stages <- bind_rows(
  mutate(top_fbi_baseline_users, stage = "Before Event"),
  mutate(top_fbi_users, stage = "During Event"),
  mutate(top_fbi_post_event_users, stage = "After Event")
)

ggplot(fbi_combined_all_stages, aes(x = user_id, y = degree, color = stage)) +
  geom_point() +
  labs(title = "Degree Centrality for Each Stage (FBI)",
       x = "User ID", y = "Degree Centrality") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +  # Hide x-axis labels for clarity
  theme_minimal()


## Network Visualization

# Visualize event networks with ggraph for both events

# Before Event Network Visualization for Roe
ggraph(roe_baseline_network, layout = "fr") +
  geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = degree(roe_baseline_network)), color = "blue") +
  theme_minimal() +
  labs(title = "Network Structure Before Roe v. Wade Overturning")

# During Event Network Visualization for Roe
ggraph(roe_event_network, layout = "fr") +
  geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = degree(roe_event_network)), color = "blue") +
  theme_minimal() +
  labs(title = "Network Structure During Roe v. Wade Overturning")

# After Event Network Visualization for Roe
ggraph(roe_post_event_network, layout = "fr") +
  geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = degree(roe_post_event_network)), color = "blue") +
  theme_minimal() +
  labs(title = "Network Structure After Roe v. Wade Overturning")

# Before Event Network Visualization for FBI
ggraph(fbi_baseline_network, layout = "fr") +
  geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = degree(fbi_baseline_network)), color = "blue") +
  theme_minimal() +
  labs(title = "Network Structure Before FBI Search of Mar-a-Lago")

# During Event Network Visualization for FBI
ggraph(fbi_event_network, layout = "fr") +
  geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = degree(fbi_event_network)), color = "blue") +
  theme_minimal() +
  labs(title = "Network Structure During FBI Search of Mar-a-Lago")

# After Event Network Visualization for FBI
ggraph(fbi_post_event_network, layout = "fr") +
  geom_edge_link(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = degree(fbi_post_event_network)), color = "blue") +
  theme_minimal() +
  labs(title = "Network Structure After FBI Search of Mar-a-Lago")

### ROE

# Combine the edge lists from all stages for Roe with the updated stage schema
combined_roe_data <- bind_rows(
  roe_baseline_network %>% 
    igraph::as_data_frame() %>%
    mutate(stage = "Before"),
  
  roe_event_network %>% 
    igraph::as_data_frame() %>%
    mutate(stage = "During"),
  
  roe_post_event_network %>% 
    igraph::as_data_frame() %>%
    mutate(stage = "After")
)

# Ensure each user appears only once with a combined stage column reflecting participation in multiple stages
combined_roe_data_summary <- combined_roe_data %>%
  group_by(from, to) %>%
  summarize(stages = paste(unique(stage), collapse = " & "), .groups = "drop")

# Create a combined graph for Roe based on this summarized stage information
combined_roe_graph <- graph_from_data_frame(combined_roe_data_summary, directed = TRUE)

# Add vertex attributes for combined stages
V(combined_roe_graph)$stage_combined <- V(combined_roe_graph)$name %>%
  sapply(function(user) {
    user_stages <- combined_roe_data %>%
      filter(from == user | to == user) %>%
      pull(stage) %>%
      unique() %>%
      paste(collapse = " & ")
    user_stages
  })

# Identify top users (e.g., top 10 users during the event)
top_roe_users <- roe_event_centrality %>%
  arrange(desc(degree)) %>%
  head(10)

# Mark top users and users involved in all three stages
V(combined_roe_graph)$is_top_user <- ifelse(V(combined_roe_graph)$name %in% top_roe_users$user_id, "Top 10", "Other")
V(combined_roe_graph)$is_missing <- ifelse(is.na(V(combined_roe_graph)$stage_combined), "Missing", "Present")

# Visualization of the combined network with visual distinctions for missing and top users
ggraph(combined_roe_graph, layout = "fr") +
  geom_edge_link(aes(alpha = 0.5, color = E(combined_roe_graph)$stage_combined), show.legend = TRUE) +
  geom_node_point(aes(size = degree(combined_roe_graph),
                      color = V(combined_roe_graph)$stage_combined,
                      shape = V(combined_roe_graph)$is_missing), 
                  show.legend = TRUE) +
  theme_minimal() +
  labs(title = "Network Structure Before, During, and After Roe v. Wade Overturning") +
  scale_color_manual(values = c("Before" = "blue", 
                                "During" = "red", 
                                "After" = "green",
                                "Before & During" = "purple", 
                                "Before & After" = "orange", 
                                "During & After" = "yellow", 
                                "All Stages" = "black",
                                "Other" = "gray")) +
  scale_shape_manual(values = c("Present" = 16, "Missing" = 17)) +  # 16 = circle, 17 = triangle
  scale_size_continuous(range = c(2, 6)) +  # Adjust the size range for nodes
  theme(legend.position = "right") +
  guides(shape = guide_legend(title = "User Status"), 
         color = guide_legend(title = "Stage Category"))

# Ensure the stage_combined attribute is assigned to each vertex
V(combined_roe_graph)$stage_combined <- V(combined_roe_graph)$name %>%
  sapply(function(user) {
    user_stages <- combined_roe_data %>%
      filter(from == user | to == user) %>%
      pull(stage) %>%
      unique() %>%
      paste(collapse = " & ")
    user_stages
  })

# Calculate centrality measures for each user
V(combined_roe_graph)$user_id <- V(combined_roe_graph)$name
centralities <- calculate_centralities(combined_roe_graph)

# Merge the centralities with the stage_combined information
user_data <- centralities %>%
  left_join(data.frame(user_id = V(combined_roe_graph)$name, 
                       stage_combined = V(combined_roe_graph)$stage_combined), 
            by = c("user_id" = "user_id"))

# Filter users by stage group
before_users <- user_data %>% filter(stage_combined == "Before")
during_users <- user_data %>% filter(stage_combined == "During")
after_users <- user_data %>% filter(stage_combined == "After")
before_during_users <- user_data %>% filter(stage_combined == "Before & During")
before_after_users <- user_data %>% filter(stage_combined == "Before & After")
during_after_users <- user_data %>% filter(stage_combined == "During & After")
all_stages_users <- user_data %>% filter(stage_combined == "All Stages")

# Perform statistical analysis to compare centrality measures across different stage groups

# Degree centrality comparison using ANOVA
anova_degree <- aov(degree ~ stage_combined, data = user_data)
cat("Degree Centrality ANOVA Results:\n")
print(summary(anova_degree))

# Closeness centrality comparison using ANOVA
anova_closeness <- aov(closeness ~ stage_combined, data = user_data)
cat("\nCloseness Centrality ANOVA Results:\n")
print(summary(anova_closeness))

# Pairwise comparison of degree centrality between stages
cat("\nPairwise Comparison of Degree Centrality Between Stages:\n")
pairwise_degree <- pairwise.t.test(user_data$degree, user_data$stage_combined)
print(pairwise_degree)

# Summary statistics for centralities in each group (degree, betweenness, closeness, eigenvector)
cat("\nSummary Statistics for Degree Centrality, Betweenness, Closeness, and Eigenvector by Stage:\n")
summary_by_group <- user_data %>%
  group_by(stage_combined) %>%
  summarize(
    mean_degree = mean(degree),
    sd_degree = sd(degree),
    mean_betweenness = mean(betweenness),
    sd_betweenness = sd(betweenness),
    mean_closeness = mean(closeness),
    sd_closeness = sd(closeness),
    mean_eigenvector = mean(eigenvector),
    sd_eigenvector = sd(eigenvector)
  )

print(summary_by_group)

### FBI

# Combine the edge lists from all stages for the FBI event with the updated stage schema
combined_fbi_data <- bind_rows(
  fbi_baseline_network %>% 
    igraph::as_data_frame() %>%
    mutate(stage = "Before"),
  
  fbi_event_network %>% 
    igraph::as_data_frame() %>%
    mutate(stage = "During"),
  
  fbi_post_event_network %>% 
    igraph::as_data_frame() %>%
    mutate(stage = "After")
)

# Ensure each user appears only once with a combined stage column reflecting participation in multiple stages
combined_fbi_data_summary <- combined_fbi_data %>%
  group_by(from, to) %>%
  summarize(stages = paste(unique(stage), collapse = " & "), .groups = "drop")

# Create a combined graph for the FBI event based on this summarized stage information
combined_fbi_graph <- graph_from_data_frame(combined_fbi_data_summary, directed = TRUE)

# Add vertex attributes for combined stages
V(combined_fbi_graph)$stage_combined <- V(combined_fbi_graph)$name %>%
  sapply(function(user) {
    user_stages <- combined_fbi_data %>%
      filter(from == user | to == user) %>%
      pull(stage) %>%
      unique() %>%
      paste(collapse = " & ")
    user_stages
  })

# Identify top users (e.g., top 10 users during the event)
top_fbi_users <- fbi_event_centrality %>%
  arrange(desc(degree)) %>%
  head(10)

# Mark top users and users involved in all three stages
V(combined_fbi_graph)$is_top_user <- ifelse(V(combined_fbi_graph)$name %in% top_fbi_users$user_id, "Top 10", "Other")
V(combined_fbi_graph)$is_missing <- ifelse(is.na(V(combined_fbi_graph)$stage_combined), "Missing", "Present")

# Visualization of the combined network with visual distinctions for missing and top users
ggraph(combined_fbi_graph, layout = "fr") +
  geom_edge_link(aes(alpha = 0.5, color = E(combined_fbi_graph)$stage_combined), show.legend = TRUE) +
  geom_node_point(aes(size = degree(combined_fbi_graph),
                      color = V(combined_fbi_graph)$stage_combined,
                      shape = V(combined_fbi_graph)$is_missing), 
                  show.legend = TRUE) +
  theme_minimal() +
  labs(title = "Network Structure Before, During, and After FBI Search of Mar-a-Lago") +
  scale_color_manual(values = c("Before" = "blue", 
                                "During" = "red", 
                                "After" = "green",
                                "Before & During" = "purple", 
                                "Before & After" = "orange", 
                                "During & After" = "yellow", 
                                "All Stages" = "black",
                                "Other" = "gray")) +
  scale_shape_manual(values = c("Present" = 16, "Missing" = 17)) +  # 16 = circle, 17 = triangle
  scale_size_continuous(range = c(2, 6)) +  # Adjust the size range for nodes
  theme(legend.position = "right") +
  guides(shape = guide_legend(title = "User Status"), 
         color = guide_legend(title = "Stage Category"))

# Calculate centrality measures for each user
V(combined_fbi_graph)$user_id <- V(combined_fbi_graph)$name
centralities <- calculate_centralities(combined_fbi_graph)

# Merge the centralities with the stage_combined information
user_data <- centralities %>%
  left_join(data.frame(user_id = V(combined_fbi_graph)$name, 
                       stage_combined = V(combined_fbi_graph)$stage_combined), 
            by = c("user_id" = "user_id"))

# Filter users by stage group
before_users <- user_data %>% filter(stage_combined == "Before")
during_users <- user_data %>% filter(stage_combined == "During")
after_users <- user_data %>% filter(stage_combined == "After")
before_during_users <- user_data %>% filter(stage_combined == "Before & During")
before_after_users <- user_data %>% filter(stage_combined == "Before & After")
during_after_users <- user_data %>% filter(stage_combined == "During & After")
all_stages_users <- user_data %>% filter(stage_combined == "All Stages")

# Perform statistical analysis to compare centrality measures across different stage groups

# Degree centrality comparison using ANOVA
anova_degree <- aov(degree ~ stage_combined, data = user_data)
cat("Degree Centrality ANOVA Results:\n")
print(summary(anova_degree))

# Closeness centrality comparison using ANOVA
anova_closeness <- aov(closeness ~ stage_combined, data = user_data)
cat("\nCloseness Centrality ANOVA Results:\n")
print(summary(anova_closeness))

# Pairwise comparison of degree centrality between stages
cat("\nPairwise Comparison of Degree Centrality Between Stages:\n")
pairwise_degree <- pairwise.t.test(user_data$degree, user_data$stage_combined)
print(pairwise_degree)

# Summary statistics for centralities in each group (degree, betweenness, closeness, eigenvector)
cat("\nSummary Statistics for Degree Centrality, Betweenness, Closeness, and Eigenvector by Stage:\n")
summary_by_group <- user_data %>%
  group_by(stage_combined) %>%
  summarize(
    mean_degree = mean(degree),
    sd_degree = sd(degree),
    mean_betweenness = mean(betweenness),
    sd_betweenness = sd(betweenness),
    mean_closeness = mean(closeness),
    sd_closeness = sd(closeness),
    mean_eigenvector = mean(eigenvector),
    sd_eigenvector = sd(eigenvector)
  )

print(summary_by_group)


sink()
