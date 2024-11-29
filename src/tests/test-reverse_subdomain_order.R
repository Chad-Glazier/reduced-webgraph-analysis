library(testthat)

source("../lib/reverse_subdomain_order.R")

test_that("reverse_subdomain_order function works", {
    expect_equal(
        reverse_subdomain_order("com.facebook"),
        "facebook.com"
    )
    expect_equal(
        reverse_subdomain_order("apple"),
        "apple"
    )
    expect_equal(
        reverse_subdomain_order("com.facebook.en"),
        "en.facebook.com"
    )
    expect_equal(
        reverse_subdomain_order("f.e.d.c.b.a"),
        "a.b.c.d.e.f"
    )
})
