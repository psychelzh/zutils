test_that("One piece of meta", {
  data <- data.frame(val = c("1", "2"))
  out <- data.frame(x = c(1L, 2L))
  separate_wider_dsv(data, val, "x") |>
    expect_equal(out, ignore_attr = TRUE)
})

test_that("Two pieces of meta", {
  data <- data.frame(val = c("1_a", "2_b"))
  out <- data.frame(x = c(1L, 2L), y = c("a", "b"))
  separate_wider_dsv(data, val, c("x", "y")) |>
    expect_equal(out, ignore_attr = TRUE)
})

test_that("With prefix and suffix", {
  data <- data.frame(val = c("test_1_a_test", "test_2_b_test"))
  out <- data.frame(x = c(1L, 2L), y = c("a", "b"))
  separate_wider_dsv(
    data, val, c("x", "y"),
    prefix = "test",
    suffix = "test"
  ) |>
    expect_equal(out, ignore_attr = TRUE)
})
