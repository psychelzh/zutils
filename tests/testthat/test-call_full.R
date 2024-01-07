test_that("Support function call composition", {
  call <- quote(call_full(.fn = .fn))
  call_full(call_full) |>
    expect_identical(call)
  call_full("call_full") |>
    expect_identical(call)
})
