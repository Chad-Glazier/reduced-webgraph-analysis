#' A set of the most notable domains that aren't meant for end-users.
excluded_domains <- c(
    "com.googleapis.fonts", "com.macromedia.download", "com.tinypic",
    "com.googletagmanager", "com.gpogle.lh3", "us.imageshack",
    "org.fedoraproject", "net.nginx", "com.cloudflare.cdnjs",
    "ly.bit", "ru.fastpic", "com.googleusercontent.lh3",
    "com.bootstrapcdn.maxcdn", "com.imageshack", "com.googleapis.ajax",
    "com.gstatic.fonts", "com.cloudflare", "com.google-analytics",
    "com.jquery.code", "com.cloudflare.support", "com.vimeo.player",
    "com.google.apis", "com.amazonaws.s3", "com.gstatic",
    "net.ac22.888", "net.jsdelivr.cdn", "com.amazonaws", "com.twimg",
    "com.adobe"
)

#' For each year of data included, this function will record the top n nodes
#' and then form a tibble that include's that node's PageRank score throughout
#' each year (if there is a year where it is not found in the dataset, it will
#' be set to 0). Since very popular domains tend to remain popular, it's unlike-
#' ly that the produced tibble will actually have n rows for each year.
#'
#' The domains get preprocessed a little:
#' - First of all, each domain is reduced to the top two subdomains. E.g.,
#' the domains "com.facebook" and "com.facebook.m" will be combined into
#' "com.facebook". The PageRank scores will be summed. This led to some
#' problems before, so this is restricted to the top 100 nodes, sorted by
#' PageRank.
#' - Certain domains which are not meant for end users (CDN's, font services,
#' proxies, VPN's, etc.) are excluded.
#'
#' @param n the number of nodes to take from each year's data. The top n are
#' chosen by their PageRank.
#' @param node_data a named list of tibbles containing node data, like the
#' ones found in the *_nodes.csv files. The name of each entry is the year
#' that the node_data is associated with.
#' @param subdomains_allowed defaults to 2; specifies the number of subdomains
#' to allow. E.g., with 2 subdomains allowed, any domains "a.b.x" for any "x"
#' will be combined into "a.b".
#' @param end_users_only defaults to TRUE; specifies whether or not to exclude
#' certain domains which are not meant for end users. If TRUE, such domains will
#' be excluded.
#' @return a tibble with the following columns.
#' - `domain <chr>`: the domain name.
#' - `pagerank_in_i <number>`: the PageRank score for the domain in
#' the year i. There will be one such column for each year.
top_n_by_pagerank <- function(
    n, node_data,
    subdomains_allowed = 2,
    end_users_only = TRUE
) {
    if (!end_users_only) excluded_domains <- c()
    result <- list()
    included_domains <- c()
    original_data <- node_data
    for (year in names(node_data)) {
        node_data[[year]] <- node_data[[year]] |>
            filter(!(domain %in% excluded_domains)) |>
            slice_head(n = 100) |>
            mutate(domain =
                    sapply(
                        strsplit(domain, ".", fixed = TRUE),
                        function(x) {
                            paste(head(x, subdomains_allowed), collapse = ".")
                        }
                    )
            ) |>
            group_by(domain) |>
            summarise(
                pagerank = sum(pagerank),
                indegree_share = sum(indegree_share)
            )
        entries_for_this_year <- node_data[[year]] |>
            filter(!(domain %in% excluded_domains)) |>
            arrange(desc(pagerank)) |>
            slice_head(n = n) |>
            pull(domain)
        included_domains <- c(included_domains, entries_for_this_year)
    }
    included_domains <- unique(included_domains)

    result[["domain"]] <- included_domains
    for (year in names(node_data)) {
        result[[paste("pagerank_in_", year, sep = "")]] <-
            rep(0, length(included_domains))
        result[[paste("indegree_share_in_", year, sep = "")]] <-
            rep(0, length(included_domains))
    }

    for (i in 1:length(included_domains)) { # nolint: seq_linter.
        current_domain <- result[["domain"]][i]
        for (year in names(node_data)) {
            pagerank_in_this_year <- node_data[[year]] |>
                filter(domain == current_domain) |>
                pull(pagerank)
            indegree_share_in_this_year <- node_data[[year]] |>
                filter(domain == current_domain) |>
                pull(indegree_share)
            if (length(pagerank_in_this_year) == 0) {
                original_pagerank <- original_data[[year]] |>
                    filter(grepl(paste("^", current_domain), domain)) |>
                    slice_head(n = 1) |>
                    pull(pagerank)
                original_indegree_share <- original_data[[year]] |>
                    filter(grepl(paste("^", current_domain), domain)) |>
                    slice_head(n = 1) |>
                    pull(indegree_share)
                if (length(original_pagerank) != 0) {
                    result[[paste("pagerank_in_", year, sep = "")]][i] <-
                        original_pagerank
                    result[[paste("indegree_share_in_", year, sep = "")]][i] <-
                        original_indegree_share
                }
                result[[paste("pagerank_in_", year, sep = "")]][i] <- NA
                result[[paste("indegree_share_in_", year, sep = "")]][i] <- NA
            } else {
                result[[paste("pagerank_in_", year, sep = "")]][i] <-
                    pagerank_in_this_year
                result[[paste("indegree_share_in_", year, sep = "")]][i] <-
                    indegree_share_in_this_year
            }
        }
    }

    return(as_tibble(result))
}