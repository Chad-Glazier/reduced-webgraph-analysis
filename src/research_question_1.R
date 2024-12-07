#' This file includes the code used to analyze the categories in the reduced
#' sub-graphs. To analyze the 2018 or 2024 graphs, uncomment the appropriate
#' `read.csv` lines below.
#'
#' All CSV files used can be found in a folder called
#' "domain_categorizations.zip" at
#' https://drive.google.com/drive/folders/1odjh6_URj1K8rUjA6yI06YoV7yLSN79e?dmr=1&ec=wgc-drive-hero-goto. # nolint
#' Begin by putting those files into the top-level `data` directory, then
#' you may run this code.
library(igraph)

# 2018 Data
# nodes <- read.csv("../Cateogrized_Nodes_Edges_2018_2024/2018_top_200_categorized_nodes.csv")
# edges <- read.csv("../Cateogrized_Nodes_Edges_2018_2024/2018_top_200_categorized_edges.csv")

# 2024 Data
# edges <- read.csv("../Cateogrized_Nodes_Edges_2018_2024/2024_top_200_categorized_edges.csv", header = TRUE)
# nodes <- read.csv("../Cateogrized_Nodes_Edges_2018_2024/2024_top_200_categorized_nodes.csv", header = TRUE)

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

#community detection graph

community_detection <- cluster_louvain(g)
nodes$community <- community_detection$membership

community_sizes <- table(nodes$community)
communities_larger_than_one <- names(community_sizes[community_sizes > 1])

nodes_filtered <- nodes[nodes$community %in% communities_larger_than_one, ]

community_category_counts <- table(nodes_filtered$community, nodes_filtered$category)

community_category_matrix <- as.matrix(community_category_counts)

par(mar = c(6, 4, 4, 2))

barplot(community_category_matrix, 
        beside = FALSE, 
        col = rainbow(ncol(community_category_matrix)), 
        main = "Category Distribution Across Communities", 
        ylab = "Count", 
        cex.names = 1, 
        las = 2, 
        cex.axis = 0.8)

colnames(community_category_matrix) <- gsub(" ", "\n", colnames(community_category_matrix))

barplot(community_category_matrix, 
        beside = FALSE, 
        col = rainbow(ncol(community_category_matrix)), 
        legend = rownames(community_category_matrix), 
        main = "Category Distribution Across Communities", 
        ylab = "Count", 
        cex.names = 1, 
        las = 2, 
        cex.axis = 0.8)

#highest number of edges between two categories

categories <- unique(nodes_filtered$category)
category_matrix <- matrix(0, nrow = length(categories), ncol = length(categories))
rownames(category_matrix) <- categories
colnames(category_matrix) <- categories

for (i in 1:nrow(edges)) {
  node1 <- edges$from[i]
  node2 <- edges$to[i]
  cat1 <- nodes$category[nodes$node_id == node1]
  cat2 <- nodes$category[nodes$node_id == node2]
  if (cat1 != cat2) {
    category_matrix[cat1, cat2] <- category_matrix[cat1, cat2] + 1
    category_matrix[cat2, cat1] <- category_matrix[cat2, cat1] + 1
  }
}

max_connections <- max(category_matrix)
most_connected <- which(category_matrix == max_connections, arr.ind = TRUE)

cat("Most connected categories are:\n")
cat("Category 1:", rownames(category_matrix)[most_connected[1, 1]], "\n")
cat("Category 2:", colnames(category_matrix)[most_connected[1, 2]], "\n")
cat("Number of connections:", max_connections, "\n")

#top 3 indegree betweeness and closeness

get_top_3 <- function(centrality, centrality_name) {
  aggregate_values <- aggregate(centrality, by = list(Category = V(g)$category), mean)
  colnames(aggregate_values) <- c("Category", "Value")
  top_3 <- head(aggregate_values[order(-aggregate_values$Value), ], 3)
  data.frame(
    CentralityType = rep(centrality_name, nrow(top_3)),
    Category = top_3$Category,
    Value = top_3$Value
  )
}

indegree_top <- get_top_3(indegree_centrality, "InDegree")
betweenness_top <- get_top_3(betweenness_centrality, "Betweenness")
closeness_top <- get_top_3(closeness_centrality, "Closeness")

top_centrality <- rbind(indegree_top, betweenness_top, closeness_top)

options(scipen = 999)
top_centrality$Value <- round(top_centrality$Value, 4)

print(top_centrality)

#pie chart

category_counts <- table(nodes$category)
top_categories <- head(sort(category_counts, decreasing = TRUE), 15)
percentages <- prop.table(top_categories) * 100
pie(percentages, labels = paste(names(percentages), round(percentages, 1), "%"), main = "Top 15 Categories by Node Count", cex = 0.8)

#k core decomposition

k_core_decomposition <- coreness(g)
max_k_core <- max(k_core_decomposition)
core_nodes <- V(g)[coreness(g) == max_k_core]
core_subgraph <- induced_subgraph(g, vids = core_nodes)

core_categories <- nodes$category[nodes$node_id %in% core_nodes$name]
unique_core_categories <- unique(core_categories)

print(unique_core_categories)
