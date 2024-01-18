test_that("cautiously returns default value and emits a warning on failure", {
  f <- function(...) stop("tilt")
  cautiously(f, NA_real_)() |>
    expect_identical(NA_real_) |>
    expect_warning("tilt")
})
