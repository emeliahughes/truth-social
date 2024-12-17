library(readr)
library(dplyr)
library(igraph)
library(ggplot2)

# Assign the cleaned data to the 'truths' object
truths <- read_tsv("truths-cleaned.tsv", quote = "")

# Load the files
users <- read_tsv("users.tsv")
follows <- read_tsv("follows.tsv")
quotes <- read_tsv("quotes.tsv")
replies <- read_tsv("replies.tsv")
media <- read_tsv("media.tsv")
hashtags <- read_tsv("hashtags.tsv")
external_urls <- read_tsv("external_urls.tsv")
truth_hashtag_edges <- read_tsv("truth_hashtag_edges.tsv")
truth_media_edges <- read_tsv("truth_media_edges.tsv")
truth_external_url_edges <- read_tsv("truth_external_url_edges.tsv")
truth_user_tag_edges <- read_tsv("truth_user_tag_edges.tsv")

truths <- truths %>%
  mutate(truth_retruthed = ifelse(truth_retruthed == "", "-1", truth_retruthed))



# Join the truths table to itself on the condition that a truth is retruthed (truth_retruthed == id)
truths_joined <- truths %>%
  filter(truth_retruthed != -1) %>%  # Keep only retruthed truths
  mutate(truth_retruthed = as.integer(truth_retruthed)) %>%
  left_join(truths, by = c("truth_retruthed" = "id"), suffix = c("_reposter", "_original")) %>%
  select(from = author_original, to = author_reposter) %>%  # Select the columns we need (author IDs)
  filter(!is.na(from))  # Keep only rows where 'from' is not NA


# Count how many times one author has reposted another author's truth
edges <- truths_joined %>%
  group_by(from, to) %>%
  summarise(reposts = n(), .groups = "drop")  # Count reposts per author pair

top_from_ids <- edges %>%
  count(to) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  pull(to)  # Extract the 'from' IDs as a vector

# Filter the edges to include only rows where 'from' or 'to' is in the top X
filtered_edges <- edges %>%
  filter(from %in% top_from_ids | to %in% top_from_ids) %>%
  arrange(desc(reposts)) %>%
  slice_head(n = 250)

# Create a directed graph from the author-to-author edge list
filtered_g <- graph_from_data_frame(edges, directed = TRUE)

# Plot the network
plot(filtered_g, 
     vertex.size = E(g)$reposts / 10, 
     vertex.label.cex = 0.5, 
     edge.arrow.size = 0.2,
     edge.width = 1,  # Adjust edge width based on reposts count
     vertex.label.color = "black", 
     vertex.color = "lightblue", 
     main = "Repost Network")

# Unfiltered network
g <- graph_from_data_frame(edges, directed = TRUE)

users_for_graph <- users %>%
  filter(id %in% V(g)$name)  # Ensure only users present in the graph are included

# Create a vector for user names, with a default value for all vertices
user_names <- rep("Unknown", length(V(g)$name))  # Default "Unknown" for missing users
followers_count <- rep(0, length(V(g)$name))  # Default to 0 if no match
following_count <- rep(0, length(V(g)$name))  # Default to 0 if no match

# Match users from the graph with the users_for_graph and assign their follower and following counts
matches <- match(V(g)$name, users_for_graph$id)

# Only update the follower and following counts where there's a valid match (i.e., not NA)
valid_matches <- !is.na(matches)
user_names[valid_matches] <- users_for_graph$username[matches[valid_matches]]
followers_count[valid_matches] <- users_for_graph$follower_count[matches[valid_matches]]
following_count[valid_matches] <- users_for_graph$following_count[matches[valid_matches]]

# Assign the follower and following counts as vertex attributes
V(g)$user_name <- user_names
V(g)$user_followers <- followers_count
V(g)$user_following <- following_count

### ANALYSIS ###

# Degree of each node (in-degree and out-degree)
deg_in <- degree(g, mode = "in")  # In-degree (how many times an author is reposted by others)
deg_out <- degree(g, mode = "out")  # Out-degree (how many times an author reposts others)
deg_total <- degree(g, mode = "all")  # Total degree

cat("Degree Centrality (In-degree, Out-degree, and Total Degree):\n")
print(data.frame(user_id = V(g)$name, degree_in = deg_in, degree_out = deg_out, degree_total = deg_total))

# Network Density (The proportion of possible edges that exist in the graph)
density_value <- edge_density(g)
cat("\nNetwork Density:\n")
print(density_value)

# Graph Summary
cat("\nGraph Summary:\n")
summary(g)

# Betweenness Centrality (Measures the number of times a node acts as a bridge along the shortest path between two other nodes)
betweenness_centrality <- betweenness(g, directed = TRUE)
cat("\nBetweenness Centrality:\n")
print(data.frame(user_id = V(g)$name, betweenness = betweenness_centrality))

# Closeness Centrality (Measures how close a node is to all other nodes in the network)
closeness_centrality <- closeness(g, mode = "all", weights = NA)
cat("\nCloseness Centrality:\n")
print(data.frame(user_id = V(g)$name, closeness = closeness_centrality))

# Eigenvector Centrality (Measures the influence of a node in a network based on the connections to other highly connected nodes)
eigenvector_centrality <- eigen_centrality(g)$vector
cat("\nEigenvector Centrality:\n")
print(data.frame(user_id = V(g)$name, eigenvector = eigenvector_centrality))

# Degree Centrality
degree_centrality <- degree(g)
cat("\nDegree Centrality:\n")
print(data.frame(user_id = V(g)$name, degree = degree_centrality))

# Combine centrality scores with user names and IDs
centrality_scores <- data.frame(
  user_id = V(g)$name,  # Assuming 'name' is the user ID field
  user_name = V(g)$user_name,  # Assuming 'username' is the user name field
  degree = degree_centrality,
  betweenness = betweenness_centrality,
  closeness = closeness_centrality,
  eigenvector = eigenvector_centrality
)

# Rank users by combined centrality measures (e.g., average centrality score)
centrality_scores$avg_score <- rowMeans(centrality_scores[, 3:6])

# Sort by highest average score
top_influential_users <- centrality_scores[order(-centrality_scores$avg_score), ]
cat("\nTop 5 Influential Users (by average centrality score): \n")
print(top_influential_users[1:5, c("user_id", "user_name", "avg_score")])

# Degree Centrality - Top Users with user name and id
cat("\nTop 5 nodes by Degree Centrality:\n")
degree_centrality_top <- centrality_scores[order(-centrality_scores$degree), ]
print(degree_centrality_top[1:5, c("user_id", "user_name", "degree")])

# Betweenness Centrality - Top Users with user name and id
cat("\nTop 5 nodes by Betweenness Centrality:\n")
betweenness_centrality_top <- centrality_scores[order(-centrality_scores$betweenness), ]
print(betweenness_centrality_top[1:5, c("user_id", "user_name", "betweenness")])

# Closeness Centrality - Top Users with user name and id
cat("\nTop 5 nodes by Closeness Centrality:\n")
closeness_centrality_top <- centrality_scores[order(-centrality_scores$closeness), ]
print(closeness_centrality_top[1:5, c("user_id", "user_name", "closeness")])

# Eigenvector Centrality - Top Users with user name and id
cat("\nTop 5 nodes by Eigenvector Centrality:\n")
eigenvector_centrality_top <- centrality_scores[order(-centrality_scores$eigenvector), ]
print(eigenvector_centrality_top[1:5, c("user_id", "user_name", "eigenvector")])

# PageRank (A variation of eigenvector centrality)
pagerank_centrality <- page_rank(g)$vector

# Strongly Connected Components (SCCs)
scc <- components(g, mode = "strong")  # For directed graphs
num_scc <- scc$no  # Number of strongly connected components
cat("\nStrongly Connected Components (SCCs):\n")
cat("Number of SCCs: ", num_scc, "\n")

# Weakly Connected Components (For undirected connectivity)
wcc <- components(g, mode = "weak")
num_wcc <- wcc$no  # Number of weakly connected components
cat("\nWeakly Connected Components (WCCs):\n")
cat("Number of WCCs: ", num_wcc, "\n")

# Shortest Paths Between All Pairs of Nodes
pairwise_distances <- distances(g)
cat("\nShortest Paths Between All Pairs of Nodes:\n")
print(pairwise_distances)

# Average Path Length
avg_path_length <- mean(pairwise_distances[is.finite(pairwise_distances)])
cat("\nAverage Path Length:\n")
print(avg_path_length)

# Diameter of the graph
graph_diameter <- diameter(g, directed = TRUE, weights = NA)
cat("\nGraph Diameter:\n")
print(graph_diameter)

# Eccentricity (Furthest distance from a node to any other node)
eccentricity_value <- eccentricity(g)
cat("\nEccentricity:\n")
print(eccentricity_value)

# Geodesic Distance (Shortest path between any two nodes)
geodesic_distance <- distances(g, v = V(g), to = V(g), weights = NA)
cat("\nGeodesic Distance:\n")
print(geodesic_distance)

# Community Detection (Using the Louvain method)
g_undirected <- as.undirected(g, mode = "collapse")  # Combine edge weights for undirected representation

louvain_community <- cluster_louvain(g_undirected)
louvain_modularity <- modularity(louvain_community)

# Print Community Detection Summary
cat("Community Detection Results (Louvain Method):\n")
cat("Number of Communities Detected:", length(communities(louvain_community)), "\n")
cat("Modularity Value:", louvain_modularity, "\n\n")

# Print the communities
for (i in seq_along(communities(louvain_community))) {
  cat("Community", i, ": ", paste(communities(louvain_community)[[i]], collapse = ", "), "\n")
}

for (i in seq_along(communities(louvain_community))) {
  community_size <- length(communities(louvain_community)[[i]])  # Get the size of each community
  cat("Community", i, "has", community_size, "members.\n")
}


# Visualize Centrality Distributions with Histograms
cat("\n\nHistograms for Centrality Measures:\n")

# Degree Centrality Histogram
ggplot(data.frame(degree = degree_centrality), aes(x = degree)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  ggtitle("Histogram of Degree Centrality") +
  xlab("Degree") +
  ylab("Frequency") +
  theme_minimal()

# Betweenness Centrality Histogram
ggplot(data.frame(betweenness = betweenness_centrality), aes(x = betweenness)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "red", alpha = 0.7) +
  ggtitle("Histogram of Betweenness Centrality") +
  xlab("Betweenness") +
  ylab("Frequency") +
  theme_minimal()

# Closeness Centrality Histogram
ggplot(data.frame(closeness = closeness_centrality), aes(x = closeness)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "green", alpha = 0.7) +
  ggtitle("Histogram of Closeness Centrality") +
  xlab("Closeness") +
  ylab("Frequency") +
  theme_minimal()

# Eigenvector Centrality Histogram
ggplot(data.frame(eigenvector = eigenvector_centrality), aes(x = eigenvector)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "purple", alpha = 0.7) +
  ggtitle("Histogram of Eigenvector Centrality") +
  xlab("Eigenvector Centrality") +
  ylab("Frequency") +
  theme_minimal()

