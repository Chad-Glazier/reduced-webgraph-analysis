#' This function is meant to plot the tibble resulting from a call to the
#' `top_n_by_pagerank` function.
#'
#' @param top_n_by_pagerank_result a tibble like the one returned from a
#' call to `top_n_by_pagerank`.
#' @param output_file a string representing the path to a file where the
#' new image should be put (should end in ".png", since the plot will be a
#' PNG).
#' @param use_indegree plot the indegree share of each domain instead of
#' their PageRank score.
plot_top_n <- function(
    top_n_by_pagerank_result, output_file,
    use_indegree = FALSE
) {
    new_plot <- c()
    if (use_indegree) {
        new_plot <- top_n_by_pagerank_result |>
            # widen the data first to make years numeric
            select(!starts_with("page_rank_in_")) |>
            pivot_longer(
                cols = starts_with("indegree_share_in_"),
                names_to = "year",
                values_to = "indegree_share"
            ) |>
            mutate(
                year = str_remove(year, "indegree_share_in_") |> as.numeric()
            ) |>
            # make the ggplot
            ggplot(aes(
                x = year,
                y = indegree_share,
                color = domain,
                group = domain
            )) +
            geom_point(size = 1) +
            geom_line() +
            labs(
                x = "Year",
                y = "Indegree Share",
                color = "Domain"
            ) +
            theme_classic()
    } else {
        new_plot <- top_n_by_pagerank_result |>
            # widen the data first to make years numeric
            pivot_longer(
                cols = starts_with("pagerank_in_"),
                names_to = "year",
                values_to = "pagerank"
            ) |>
            mutate(
                year = str_remove(year, "pagerank_in_") |> as.numeric()
            ) |>
            # make the ggplot
            ggplot(aes(
                x = year,
                y = pagerank,
                color = domain,
                group = domain
            )) +
            geom_point(size = 1) +
            geom_line() +
            labs(
                x = "Year",
                y = "PageRank",
                color = "Domain"
            ) +
            theme_classic()
    }
    ggsave(output_file, plot = new_plot, width = 8, height = 6, dpi = 400)
}