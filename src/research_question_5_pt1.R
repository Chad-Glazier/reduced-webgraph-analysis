#' This file is used to perform part of the analysis to answer research
#' question 5. This one loads the CSV files, so it should be run before
#' the second part.
#'
#' All CSV files used can be found in a folder called "domain_only_graphs.zip"
#' at
#' https://drive.google.com/drive/folders/1odjh6_URj1K8rUjA6yI06YoV7yLSN79e?dmr=1&ec=wgc-drive-hero-goto. # nolint
#' Begin by putting those files into the top-level `data` directory, then
#' you may run this code.
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

