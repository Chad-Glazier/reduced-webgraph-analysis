reverse_subdomain_order <- function(domain) {
    reversed_domain <- domain |>
        strsplit(".", fixed = TRUE) |>
        purrr::pluck(1) |>
        rev() |>
        paste(collapse = ".")
    return(reversed_domain)
}
