#' Produces a histogram that represents the distribution of indegree share
#' across nodes for a given year's node data (like one formed by reading a
#' "*_nodes.csv" file). The produced plot is written to a file.
#'
#' @param node_data The tibble representing a set of nodes.
#' @param output_file The path of the new image to produce.
cumulative_indegree_share_distribution <- function(
    year_to_year_node_data,
    output_file
) {
    formatted_data <- NULL
    len_of_year_data <- nrow(year_to_year_node_data[[1]])
    for (year in names(year_to_year_node_data)) {
        this_years_data <- year_to_year_node_data[[year]] |>
            select(-domain, -pagerank) |>
            mutate(
                year = rep(year, len_of_year_data)
            )
        if (length(formatted_data) == 0) {
            formatted_data <- this_years_data
        } else {
            formatted_data <- bind_rows(formatted_data, this_years_data)
        }
    }
    new_plot <- formatted_data |>
        group_by(year) |>
        arrange(desc(indegree_share)) |>
        mutate(
            nth = 1:len_of_year_data,
            cumulative_indegree_share_percent = cumsum(indegree_share)
        ) |>
        ggplot(aes(
            x = nth,
            y = cumulative_indegree_share_percent,
            group = year,
            color = year
        )) +
        geom_line() +
        labs(
            x = "Top x Nodes, Ordered by Descending Indegree",
            y = "Cumulative Indegree Share",
            color = "Years"
        ) +
        theme_classic()
    ggsave(output_file, plot = new_plot, width = 8, height = 6, dpi = 400)
}
