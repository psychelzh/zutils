test_that("One piece of meta", {
  x <- list(
    test_1 = data.frame(z = 1:2),
    test_2 = data.frame(z = 2)
  )
  out <- tibble::tibble(
    x = c(1L, 1L, 2L),
    z = c(1, 2, 2)
  )
  bind_rows_meta(
    !!!x,
    .names_meta = "x",
    .prefix = "test"
  ) |>
    expect_equal(out, ignore_attr = TRUE)
})

test_that("Two pieces of meta", {
  x <- list(
    test_1_a = data.frame(z = 1:2),
    test_2_b = data.frame(z = 2)
  )
  out <- tibble::tibble(
    x = c(1L, 1L, 2L),
    y = c("a", "a", "b"),
    z = c(1, 2, 2)
  )
  bind_rows_meta(
    !!!x,
    .names_meta = c("x", "y"),
    .prefix = "test"
  ) |>
    expect_equal(out, ignore_attr = TRUE)
})

test_that("No prefix", {
  x <- list(
    a = data.frame(z = 1:2),
    b = data.frame(z = 2)
  )
  out <- tibble::tibble(
    x = c("a", "a", "b"),
    z = c(1, 2, 2)
  )
  bind_rows_meta(
    !!!x,
    .names_meta = "x"
  ) |>
    expect_equal(out, ignore_attr = TRUE)
})

test_that("Ignore meta", {
  x <- list(
    a = data.frame(z = 1:2),
    b = data.frame(z = 2)
  )
  out <- tibble::tibble(
    z = c(1, 2, 2)
  )
  bind_rows_meta(!!!x) |>
    expect_equal(out, ignore_attr = TRUE)
})
