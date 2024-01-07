test_that("Smoke test", {
  x <- list(
    test_1_b = data.frame(z = 1:2),
    test_2_c = data.frame(z = 2)
  )
  out <- tibble::tibble(
    x = c(1L, 1L, 2L),
    y = c("b", "b", "c"),
    z = c(1, 2, 2)
  )
  bind_rows_meta(
    !!!x,
    .names_meta = c("x", "y"),
    .prefix = "test"
  ) |>
    expect_identical(out)
})
