source("src/libraries.R")

y2018_edges <- read_csv("data/2018_top_10000_pagerank_edges.csv")
y2018_nodes <- read_csv("data/2018_top_10000_pagerank_nodes.csv")

y2018_edges |> glimpse() |> print()
y2018_nodes |> glimpse() |> print()
