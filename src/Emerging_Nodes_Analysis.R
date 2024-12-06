
install.packages("igraph")
library(igraph)

########################----Loading Nodes----########################

nodes_2018 <- read.csv("data/2018_top_30000_indegrees_nodes.csv", header = TRUE)
edges_2018 <- read.csv("data/2018_top_30000_indegrees_edges.csv", header = TRUE)
nodes_2024 <- read.csv("data/2024_top_30000_indegrees_nodes.csv", header = TRUE)
edges_2024 <- read.csv("data/2024_top_30000_indegrees_edges.csv", header = TRUE)

graph_2018 <- graph_from_data_frame(d = edges_2018, vertices = nodes_2018, directed = TRUE)
graph_2024 <- graph_from_data_frame(d = edges_2024, vertices = nodes_2024, directed = TRUE)

# Compute centrality metrics
degree_2018 <- degree(graph_2018, mode = "all")
betweenness_2018 <- betweenness(graph_2018, directed = TRUE)
pagerank_2018 <- page_rank(graph_2018)$vector

degree_2024 <- degree(graph_2024, mode = "all")
betweenness_2024 <- betweenness(graph_2024, directed = TRUE)
pagerank_2024 <- page_rank(graph_2024)$vector


centrality_2018 <- data.frame(
  domain = V(graph_2018)$name,
  degree = degree_2018,
  betweenness = betweenness_2018,
  pagerank = pagerank_2018
)

centrality_2024 <- data.frame(
  domain = V(graph_2024)$name,
  degree = degree_2024,
  betweenness = betweenness_2024,
  pagerank = pagerank_2024
)

########################----Definition of Emerging Nodes----########################

# moderate degree centrality in 2018
degree_25 <- quantile(centrality_2018$degree, 0.25)
degree_75 <- quantile(centrality_2018$degree, 0.75)

# high betweenness and PageRank in 2018
betweenness_90 <- quantile(centrality_2018$betweenness, 0.90)
pagerank_90 <- quantile(centrality_2018$pagerank, 0.90)

centrality_2018 <- centrality_2018 %>%
  mutate(
    betweenness = as.numeric(as.character(betweenness)),
    pagerank = as.numeric(as.character(pagerank))
  )

install.packages("dplyr")
library(dplyr)

# Filter nodes
emerging_nodes_2018 <- centrality_2018 %>%
  filter(
    degree > degree_25 & degree < degree_75,
    betweenness > betweenness_90 | pagerank > pagerank_90
  )

# popular nodes (top 10% in 2024)
degree_90_2024 <- quantile(centrality_2024$degree, 0.90)

# Filter popular nodes
popular_nodes_2024 <- centrality_2024 %>%
  filter(degree > degree_90_2024)

# Merge the two datasets
comparison <- merge(emerging_nodes_2018, popular_nodes_2024, by = "domain")

# Count the number of nodes
num_popular_in_2024 <- nrow(comparison)

# Total number of emerging nodes in 2018
total_emerging_2018 <- nrow(emerging_nodes_2018)

# Calculate the percentage
percentage_popular <- (num_popular_in_2024 / total_emerging_2018) * 100

#top20 in emerging nodes(2018) and popular nodes(2024) with high centrality
sorted_emerging_2018 <- emerging_nodes_2018 %>%arrange(desc(betweenness), desc(pagerank), desc(degree))

print(head(sorted_emerging_2018, 20))

sorted_popular_2024 <- popular_nodes_2024 %>%arrange(desc(betweenness), desc(pagerank), desc(degree))

print(head(sorted_popular_2024, 20))

# common nodes in top20 in emerging nodes(2018) and popular nodes(2024)
high_centrality_common <- merge(head(sorted_emerging_2018, 20), head(sorted_popular_2024, 20), by = "domain")

########################----Growth Comparison----########################

growth <- comparison %>%
  mutate(
    degree_growth = (degree.y - degree.x) / degree.x * 100,
    betweenness_growth = (betweenness.y - betweenness.x) / betweenness.x * 100,
    pagerank_growth = (pagerank.y - pagerank.x) / pagerank.x * 100
  )

overall_growth <- centrality_2024 %>%
  summarise(
    degree_growth = mean(
      ifelse(centrality_2018$degree == 0, NA, (degree - centrality_2018$degree) / centrality_2018$degree * 100),
      na.rm = TRUE
    ),
    betweeness_growth = mean(
      ifelse(centrality_2018$betweenness == 0, NA, (betweenness - centrality_2018$betweenness) / centrality_2018$betweenness * 100),
      na.rm = TRUE
    ),
    pageRank_growth = mean(
      ifelse(centrality_2018$pagerank == 0, NA, (pagerank - centrality_2018$pagerank) / centrality_2018$pagerank * 100),
      na.rm = TRUE
    )
  )

library(ggplot2)
library(tidyr)

# Reshape the data
growth_long <- growth %>%
  select(degree_growth, betweenness_growth, pagerank_growth) %>%
  gather(key = "metric", value = "growth") %>%
  mutate(type = "Emerging Nodes")

overall_growth_long <- overall_growth %>%
  gather(key = "metric", value = "growth") %>%
  mutate(type = "Overall Network")

combined_growth <- bind_rows(growth_long, overall_growth_long)

ggplot(combined_growth, aes(x = metric, y = growth, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Growth Comparison: Emerging Nodes vs Overall Network (2018-2024)",
       x = "Metric", y = "Percentage Growth") +
  theme_minimal() +
  scale_fill_manual(values = c("Emerging Nodes" = "orange", "Overall Network" = "skyblue"))

########################----Popular but Not in Emerging Nodes----########################

common_nodes <- merge(emerging_nodes_2018, popular_nodes_2024, by = "domain")

not_emerging_but_popular <- anti_join(popular_nodes_2024, emerging_nodes_2018, by = "domain")
nrow(not_emerging_but_popular)

comparison_not_emerging_but_popular <- merge(not_emerging_but_popular, centrality_2018, by = "domain")
nrow(comparison_not_emerging_but_popular)

degree_90 <- quantile(centrality_2018$degree, 0.90)
betweenness_90 <- quantile(centrality_2018$betweenness, 0.90)
pagerank_90 <- quantile(centrality_2018$pagerank, 0.90)

top_degree_2018 <- centrality_2018 %>%
  filter(
    degree > degree_90)

top_betweeness_2018 <- centrality_2018 %>%
  filter(
    betweenness > betweenness_90)

top_pagerank_2018 <- centrality_2018 %>%
  filter(
    pagerank > pagerank_90)

find_in_top_degree <- merge(not_emerging_but_popular, top_degree_2018, by = "domain")
percentage_in_top_degree <- (nrow(find_in_top_degree) / nrow(not_emerging_but_popular)) * 100
percentage_in_top_degree

find_in_top_betweenness <- merge(not_emerging_but_popular, top_betweeness_2018, by = "domain")
percentage_in_top_betweenness <- (nrow(find_in_top_betweenness) / nrow(not_emerging_but_popular)) * 100
percentage_in_top_betweenness

find_in_top_pagerank <- merge(not_emerging_but_popular, top_pagerank_2018, by = "domain")
percentage_in_top_pagerank <- (nrow(find_in_top_pagerank) / nrow(not_emerging_but_popular)) * 100
percentage_in_top_pagerank

########################----Analysis on Community Evolution of Non-Emerging Nodes in 2018----########################

# Non-emerging nodes in 2018
non_emerging_nodes_2018 <- centrality_2018 %>%
  filter(
    degree <= degree_25 | degree >= degree_75,  # not moderate
    betweenness <= betweenness_90 & pagerank <= pagerank_90  # low betweenness and PageRank
  )

# Filter edges that involve any of the selected nodes (non-emerging nodes in 2018)
edges_involving_non_emerging_2018 <- edges_2018[edges_2018$from_domain %in% non_emerging_nodes_2018$domain | 
                                                  edges_2018$to_domain %in% non_emerging_nodes_2018$domain, ]

# Create graph from filtered edges (2018)
graph_filtered_2018 <- graph_from_data_frame(edges_involving_non_emerging_2018, directed = FALSE)

# Use the Louvain method for community detection (2018)
community_detection_2018 <- cluster_louvain(graph_filtered_2018)

# View the community structure (2018)
community_structure_2018 <- community_detection_2018$membership

# Calculate centrality measures for the 2018 graph
degree_centrality_2018 <- degree(graph_filtered_2018)
betweenness_centrality_2018 <- betweenness(graph_filtered_2018)
pagerank_2018 <- page_rank(graph_filtered_2018)$vector

# Get community membership for each node (2018)
node_communities_2018 <- community_detection_2018$membership

# Ensure node_communities is a named vector (2018)
node_communities_2018 <- as.integer(node_communities_2018)
names(node_communities_2018) <- names(degree_centrality_2018)

# Create a data frame of nodes with their centrality measures (2018)
node_data_2018 <- data.frame(
  node = names(node_communities_2018),
  community = node_communities_2018,
  degree = degree_centrality_2018,
  betweenness = betweenness_centrality_2018,
  pagerank = pagerank_2018
)

# View the data to check if clusters with high centrality are more connected (2018)
summary(node_data_2018)

# Investigate community centrality (2018)
community_centrality_2018 <- aggregate(cbind(degree, betweenness, pagerank) ~ community, data = node_data_2018, FUN = mean)

# View communities with highest centrality scores (2018)
community_centrality_2018[order(community_centrality_2018$degree, decreasing = TRUE), ]

### These communities are likely to have played a crucial role in the rise of non-emerging nodes by 2024. ###

################################

# Filter edges that involve any of the selected nodes (non-emerging nodes in 2018) for 2024
edges_involving_non_emerging_2024 <- edges_2024[edges_2024$from_domain %in% non_emerging_nodes_2018$domain | 
                                                  edges_2024$to_domain %in% non_emerging_nodes_2018$domain, ]

# Create graph from filtered edges (2024)
graph_filtered_2024 <- graph_from_data_frame(edges_involving_non_emerging_2024, directed = FALSE)

# Use the Louvain method for community detection (2024)
community_detection_2024 <- cluster_louvain(graph_filtered_2024)

# View the community structure (2024)
community_structure_2024 <- community_detection_2024$membership

# Calculate centrality measures for the 2024 graph
degree_centrality_2024 <- degree(graph_filtered_2024)
betweenness_centrality_2024 <- betweenness(graph_filtered_2024)
pagerank_2024 <- page_rank(graph_filtered_2024)$vector

# Get community membership for each node (2024)
node_communities_2024 <- community_detection_2024$membership

# Ensure node_communities is a named vector (2024)
node_communities_2024 <- as.integer(node_communities_2024)
names(node_communities_2024) <- names(degree_centrality_2024)

# Create a data frame of nodes with their centrality measures (2024)
node_data_2024 <- data.frame(
  node = names(node_communities_2024),
  community = node_communities_2024,
  degree = degree_centrality_2024,
  betweenness = betweenness_centrality_2024,
  pagerank = pagerank_2024
)

# View the data to check if clusters with high centrality are more connected (2024)
summary(node_data_2024)

# Investigate community centrality (2024)
community_centrality_2024 <- aggregate(cbind(degree, betweenness, pagerank) ~ community, data = node_data_2024, FUN = mean)

# View communities with highest centrality scores (2024)
community_centrality_2024[order(community_centrality_2024$degree, decreasing = TRUE), ]







