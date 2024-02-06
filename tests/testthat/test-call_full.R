test_that("Support function call composition", {
  call_full("cautiously") |>
    expect_identical(quote(cautiously(.f = .f, otherwise = otherwise)))
})
