test_that("Support function call composition", {
  call_full("cautiously") |>
    expect_identical(quote(cautiously(.f = .f, otherwise = otherwise)))
  call_full("cautiously", otherwise = NULL) |>
    expect_identical(quote(cautiously(.f = .f, otherwise = NULL)))
})
