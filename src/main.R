library(tidyverse)
library(purrr)

# columns for "*_edges.csv" files are "from,to"
# columns for "*_nodes.csv" files are "domain,pagerank,indegree,outdegree"
y2018_nodes <- read_csv("data/2018_top_10000_nodes.csv")
