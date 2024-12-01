library(tidyverse)
library(purrr)

source("src/lib/top_n_by_pagerank.R")
source("src/lib/plot_top_n.R")
source("src/lib/cumulative_indegree_share_distribution.R")
source("src/lib/indegree_share_distribution.R")

# columns for "*_edges.csv" files are "from,to" (domain names)
# columns for "*_nodes.csv" files are "domain,pagerank,indegree_share"
#
# - "domain" refers to the website's domain name, in the order of
# top-level.second-level.... E.g., "com.facebook".
#
# - "pagerank" refers to the PageRank score of the domain, when
# computed on the subgraph of the top 30,000 nodes ranked by indegree.
# Since this subgraph does not include all 30,000 of those nodes, the
# sum total "pagerank" for all nodes will be less than 1.
#
# - "indegree_share" refers to the proportion of all edges (excluding
# parallel edges and loops) that point to this node.

nodes_2018 <- read_csv("data/2018_top_10000_nodes.csv")
nodes_2020 <- read_csv("data/2020_top_10000_nodes.csv")
nodes_2022 <- read_csv("data/2022_top_10000_nodes.csv")
nodes_2024 <- read_csv("data/2024_top_10000_nodes.csv")

# Here, we create some data that represents the history of pagerank
# scores from prominent, end-user websites.
top_10_nodes_data_end_user_only <- top_n_by_pagerank(10, list( # nolint
    "2018" = nodes_2018,
    "2020" = nodes_2020,
    "2022" = nodes_2022,
    "2024" = nodes_2024
), end_users_only = TRUE, subdomains_allowed = 2)

# Here, we create some similar data, except that domains which are not
# meant for end-users are still included (e.g., aws domains).
top_10_nodes_data_all <- top_n_by_pagerank(10, list(
    "2018" = nodes_2018,
    "2020" = nodes_2020,
    "2022" = nodes_2022,
    "2024" = nodes_2024
), end_users_only = FALSE, subdomains_allowed = 2)

# Now we turn those tibbles into plots.
plot_top_n(
    top_10_nodes_data_end_user_only,
    "produced_images/prominent_websites_pagerank_end_user_only.png"
)
plot_top_n(
    top_10_nodes_data_all, 
    "produced_images/prominent_websites_pagerank_all.png"
)

# Another metric we've preserved throughout this process is the indegree_share.
# We'll plot that next.
plot_top_n(
    top_10_nodes_data_end_user_only,
    "produced_images/prominent_websites_indegree_share_end_user_only.png",
    use_indegree = TRUE
)
plot_top_n(
    top_10_nodes_data_all,
    "produced_images/prominent_websites_indegree_share_all.png",
    use_indegree = TRUE
)

# From the indegree_share plots, it looks as though the most important nodes
# are slowly losing their share of the total indegrees. This might suggest
# that the internet is becoming less centralized. But is this really the case?
# To answer that, we can look to the overall indegree distribution.

# First, we plot the cumulative indegree distribution per node for each year.
list(
    "2018" = nodes_2018,
    "2020" = nodes_2020,
    "2022" = nodes_2022,
    "2024" = nodes_2024
) |>
    cumulative_indegree_share_distribution(
        "produced_images/cumulative_indegree_share_distribution.png"
    )

# Then, we plot the actual indegree distribution for the top 100 nodes (beyond
# which, the indegree shares get too small to be noticeable on the plot).
list(
    "2018" = nodes_2018 |> slice_head(n = 100),
    "2020" = nodes_2020 |> slice_head(n = 100),
    "2022" = nodes_2022 |> slice_head(n = 100),
    "2024" = nodes_2024 |> slice_head(n = 100)
) |>
    indegree_share_distribution(
        "produced_images/indegree_share_distribution.png"
    )
