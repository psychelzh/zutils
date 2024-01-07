test_that("Support function call composition", {
  call_full("call_full") |>
    expect_identical(quote(call_full(.fn = .fn)))
})
