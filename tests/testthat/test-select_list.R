test_that("Support basic tidy select", {
  l <- list(x = 1, x_1 = 2, a_1 = 3, x_2 = 4)
  select_list(l, "x") |>
    expect_equal(list(x = 1))
  select_list(l, starts_with("x")) |>
    expect_equal(list(x = 1, x_1 = 2, x_2 = 4))
  select_list(l, ends_with("1")) |>
    expect_equal(list(x_1 = 2, a_1 = 3))
})
