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